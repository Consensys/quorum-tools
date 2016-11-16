#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-- OR, to pre-compile this:
-- $ stack ghc -- -O2 -threaded cluster.hs

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

import           Control.Concurrent.Async   (Async, forConcurrently)
import           Control.Concurrent.MVar    (isEmptyMVar, newEmptyMVar, putMVar,
                                             takeMVar)
import qualified Control.Foldl              as Fold
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Control.Monad.Reader.Class (MonadReader (ask, reader))
import           Data.Aeson
import           Data.Foldable              (traverse_)
import           Data.Functor               (($>))
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (isInfixOf)
import qualified Data.Text.IO               as T
import           Data.Text.Lazy             (toStrict)
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           Prelude                    hiding (FilePath)
import           System.IO                  (BufferMode (..), hClose,
                                             hSetBuffering)
import           Turtle

main :: IO ()
main = sh $ flip runReaderT defaultClusterEnv $ do
  geths <- setupNodes [GethId 1, GethId 2, GethId 3]
  runNodes geths

--

newtype Verbosity = Verbosity Int deriving (Eq, Show, Num, Enum, Ord, Real, Integral)

data ClusterEnv
  = ClusterEnv { clusterDataRoot     :: FilePath
               , clusterPassword     :: Text
               , clusterNetworkId    :: Int
               , clusterGenesisJson  :: FilePath
               , clusterBaseHttpPort :: Int
               , clusterBaseRpcPort  :: Int
               , clusterVerbosity    :: Verbosity
               }
  deriving (Eq, Show)

type HasEnv = MonadReader ClusterEnv

defaultClusterEnv :: ClusterEnv
defaultClusterEnv = ClusterEnv { clusterDataRoot     = "gdata"
                               , clusterPassword     = "abcd"
                               , clusterNetworkId    = 1418
                               , clusterGenesisJson  = "genesis.json"
                               , clusterBaseHttpPort = 30400
                               , clusterBaseRpcPort  = 40400
                               , clusterVerbosity    = 3
                               }

data GethId = GethId { gId :: Int }
  deriving (Show, Eq)

data EnodeId = EnodeId Text
  deriving (Show, Eq)

instance ToJSON EnodeId where
  toJSON (EnodeId eid) = String eid

data Geth =
  Geth { gethId      :: GethId
       , gethEnodeId :: EnodeId
       }
  deriving (Show, Eq)

httpPort :: HasEnv m => GethId -> m Int
httpPort (GethId gid) = (gid +) <$> reader clusterBaseHttpPort

rpcPort :: HasEnv m => GethId -> m Int
rpcPort (GethId gid) = (gid +) <$> reader clusterBaseRpcPort

gethBinary :: HasEnv m => GethId -> m Text
gethBinary gid = mkCommand <$> dataDir gid
                           <*> httpPort gid
                           <*> rpcPort gid
                           <*> reader clusterNetworkId
                           <*> reader clusterVerbosity
  where
    mkCommand = format $ "geth --datadir "%fp       %
                             " --port "%d           %
                             " --rpcport "%d        %
                             " --networkid "%d      %
                             " --verbosity "%d      %
                             " --nodiscover"        %
                             " --maxpeers 10"       %
                             " --rpc"               %
                             " --rpccorsdomain '*'" %
                             " --rpcaddr localhost"

gethCommand :: HasEnv m => GethId -> m (Text -> Text)
gethCommand gid = do
  binary <- gethBinary gid
  return $ format (s % " " % s) binary

dataDir :: HasEnv m => GethId -> m FilePath
dataDir geth = do
  dataDirRoot <- reader clusterDataRoot
  let nodeName = format ("geth"%d) (gId geth)
  pure $ dataDirRoot </> fromText nodeName

wipeDataDirs :: (MonadIO m, HasEnv m) => m ()
wipeDataDirs = rmtree =<< reader clusterDataRoot

initNode :: (MonadIO m, HasEnv m) => GethId -> m ()
initNode geth = do
  genesisJson <- reader clusterGenesisJson
  cmd <- gethCommand geth <*> pure (format ("init "%fp) genesisJson)
  shells cmd empty

createAccount :: (MonadIO m, HasEnv m) => GethId -> m ()
createAccount gid = do
  cmd <- gethCommand gid <*> pure "account new"
  pw <- reader clusterPassword
  -- Enter pw twice in response to "Passphrase:" and "Repeat passphrase:"
  shells cmd $ return pw <|> return pw

fileContaining :: Shell Text -> Managed FilePath
fileContaining contents = do
  dir <- using $ mktempdir "/tmp" "geth"
  (path, handle) <- using $ mktemp dir "geth"
  liftIO $ outhandle handle contents
  liftIO $ hClose handle
  return path

getEnodeId :: (MonadIO m, HasEnv m) => GethId -> m EnodeId
getEnodeId gid = do
  mkCmd <- gethCommand gid

  let enodeIdShell :: Shell Text
      enodeIdShell = sed (begins "enode") $ do
        jsPath <- using $ fileContaining jsPayload
        let cmd = mkCmd $ format ("js "%fp) jsPath
        inshell cmd empty

  EnodeId . forceMaybe <$> fold enodeIdShell Fold.head

  where
    jsPayload = return "console.log(admin.nodeInfo.enode)"
    forceMaybe = fromMaybe $ error "unable to extract enode ID"

-- > allSiblings [1,2,3]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
--
allSiblings :: (Alternative m, Monad m, Eq a) => m a -> m (a, m a)
allSiblings as = (\a -> (a, sibs a)) <$> as
  where
    sibs me = do
      sib <- as
      guard $ me /= sib
      pure sib

writeStaticNodes :: (MonadIO m, HasEnv m) => [Geth] -> Geth -> m ()
writeStaticNodes sibs geth = do
  nodeDataDir <- dataDir $ gethId geth
  let jsonPath = nodeDataDir </> "static-nodes.json"
  output jsonPath contents

  where
    contents = return $ toStrict . decodeUtf8 . encode $ gethEnodeId <$> sibs

mkGeth :: (MonadIO m, HasEnv m) => GethId -> m Geth
mkGeth gid = Geth gid <$> getEnodeId gid

setupNodes :: forall m. (MonadIO m, HasEnv m) => [GethId] -> m [Geth]
setupNodes gids = do
  cEnv <- ask

  wipeDataDirs

  geths <- liftIO $ forConcurrently gids $ \gid -> flip runReaderT cEnv $ do
    initNode gid
    createAccount gid
    mkGeth gid

  void $ liftIO $ forConcurrently (allSiblings geths) $ \(geth, sibs) ->
    flip runReaderT cEnv $ writeStaticNodes sibs geth

  pure geths

inshellWithJoinedErr :: Text -> Shell Text -> Shell Text
inshellWithJoinedErr cmd inputShell = do
  line <- inshellWithErr cmd inputShell
  case line of
    Left txt  -> return txt
    Right txt -> return txt

data NodeOnline = NodeOnline -- IPC is up; ready for us to start raft
data NodeTerminated = NodeTerminated

runNode :: forall m. (MonadManaged m, HasEnv m)
        => Geth
        -> m (Async NodeOnline, Async NodeTerminated)
runNode geth = do
  let gid = gethId geth

  command <- gethCommand gid <*> pure ""
  mvar <- liftIO newEmptyMVar

  let started :: m (Async NodeOnline)
      started = fork $ do
                  _ <- takeMVar mvar
                  return NodeOnline

      outputPath :: FilePath
      outputPath = fromText $ format ("geth"%d%".out") $ gId gid

      nodeShell :: Shell Text
      nodeShell = inshellWithJoinedErr command empty

      terminated :: m (Async NodeTerminated)
      terminated = fmap ($> NodeTerminated) $
        using $ fork $ runManaged $ do
          handle <- using (writeonly outputPath)
          liftIO $ hSetBuffering handle LineBuffering

          foldIO nodeShell $ Fold.mapM_ $ \line -> do
            liftIO $ T.hPutStrLn handle line
            guard =<< isEmptyMVar mvar
            when ("IPC endpoint opened:" `isInfixOf` line) $
              putMVar mvar NodeOnline

  (,) <$> started <*> terminated

startRaft :: (MonadIO m, HasEnv m) => Geth -> m ()
startRaft geth = do
  nodeDataDir <- dataDir (gethId geth)
  let ipcEndpoint = format ("ipc:"%fp) $ nodeDataDir </> "geth.ipc"
  command <- gethCommand (gethId geth) <*>
    pure (format ("--exec 'raft.startNode();' attach "%s) ipcEndpoint)
  shells command empty

awaitAll :: (MonadIO m, Traversable t) => t (Async a) -> m ()
awaitAll = liftIO . traverse_ wait

runNodes :: (MonadManaged m, HasEnv m) => [Geth] -> m ()
runNodes geths = do
  (readyAsyncs, terminatedAsyncs) <- unzip <$> traverse runNode geths

  awaitAll readyAsyncs
  traverse_ startRaft geths
  awaitAll terminatedAsyncs

--
--


-- TODO: use this
--
-- type IdProducer = MonadState GethId
--
-- produceId :: IdProducer m => m GethId
-- produceId = do
--   id@(GethId i) <- S.get
--   S.put (GethId (i + 1))
--   return id


-- TODO: use this
--
-- postTx :: MonadIO io => Int -> io ()
-- postTx gethId =
--   let port = 40400 + gethId
--       route = format ("http://localhost:"%d) show port
--
--       t :: Text -> Text
--       t = id
--
--       body :: Value
--       body = object
--         [ "id" .= (1 :: Int)
--         , "jsonrpc" .= t "2.0"
--         , "method" .= t "eth_sendTransaction"
--         , "params" .= object
--           [ "from" .= t "joel"
--           , "to" .= t "jpm"
--           ]
--         ]
--   in liftIO $ post route body >> return ()
