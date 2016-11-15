#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-- OR, to pre-compile this:
-- $ stack ghc -- -O2 -threaded cluster.hs

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Concurrent.Async (Async, forConcurrently,
                                           mapConcurrently)
import           Control.Concurrent.MVar  (isEmptyMVar, newEmptyMVar, putMVar,
                                           takeMVar)
import qualified Control.Foldl            as Fold
import           Control.Monad.Managed    (MonadManaged)
import           Data.Aeson
import           Data.Foldable            (traverse_)
import           Data.Functor             (($>))
import           Data.Maybe               (fromMaybe)
import           Data.Text                (isInfixOf)
import qualified Data.Text.IO             as T
import           Data.Text.Lazy           (toStrict)
import           Data.Text.Lazy.Encoding  (decodeUtf8)
import           Prelude                  hiding (FilePath)
import           System.IO                (BufferMode (..), hClose,
                                           hSetBuffering)
import           Turtle

httpPort :: ClusterEnv -> GethId -> Int
httpPort cEnv (GethId gid) = (clusterBaseHttpPort cEnv) + gid

rpcPort :: ClusterEnv -> GethId -> Int
rpcPort cEnv (GethId gid) = (clusterBaseRpcPort cEnv) + gid

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

defaultClusterEnv :: ClusterEnv
defaultClusterEnv = ClusterEnv { clusterDataRoot     = "gdata"
                               , clusterPassword     = "abcd"
                               , clusterNetworkId    = 1418
                               , clusterGenesisJson  = "genesis.json"
                               , clusterBaseHttpPort = 30400
                               , clusterBaseRpcPort  = 40400
                               , clusterVerbosity    = 3
                               }

main :: IO ()
main = sh $ setupNodes cEnv [GethId 1, GethId 2, GethId 3] >>= runNodes cEnv
  where
    cEnv = defaultClusterEnv

--

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

gethBinary :: ClusterEnv -> GethId -> Text
gethBinary cEnv gid = mkCommand (dataDir cEnv gid)
                                (httpPort cEnv gid)
                                (rpcPort cEnv gid)
                                (clusterNetworkId cEnv)
                                (clusterVerbosity cEnv)
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

gethCommand :: ClusterEnv -> Text -> GethId -> Text
gethCommand cEnv cmd gid = format (s % " " % s) (gethBinary cEnv gid) cmd

dataDir :: ClusterEnv -> GethId -> FilePath
dataDir cEnv geth = (clusterDataRoot cEnv) </> fromText nodeName
  where
    nodeName = format ("geth"%d) (gId geth)

wipeDataDirs :: MonadIO m => ClusterEnv -> m ()
wipeDataDirs cEnv = rmtree (clusterDataRoot cEnv)

initNode :: MonadIO m => ClusterEnv -> GethId -> m ()
initNode cEnv geth = shells initCommand empty
  where
    initCommand :: Text
    initCommand = gethCommand cEnv
                              (format ("init "%fp) (clusterGenesisJson cEnv))
                              geth

createAccount :: MonadIO m => ClusterEnv -> GethId -> m ()
createAccount cEnv gid = shells createCommand inputPasswordTwice
  where
    createCommand :: Text
    createCommand = gethCommand cEnv "account new" gid

    -- For responding to "Passphrase:" and "Repeat passphrase:"
    inputPasswordTwice :: Shell Text
    inputPasswordTwice = let pw = clusterPassword cEnv
                         in return pw <|> return pw

fileContaining :: Shell Text -> Managed FilePath
fileContaining contents = do
  dir <- using $ mktempdir "/tmp" "geth"
  (path, handle) <- using $ mktemp dir "geth"
  liftIO $ outhandle handle contents
  liftIO $ hClose handle
  return path

getEnodeId :: MonadIO m => ClusterEnv -> GethId -> m EnodeId
getEnodeId cEnv gid = EnodeId . forceMaybe <$> fold enodeIdShell Fold.head
  where
    jsCommand jsPath = gethCommand cEnv (format ("js "%fp) jsPath) gid
    jsPayload = return "console.log(admin.nodeInfo.enode)"
    forceMaybe = fromMaybe $ error "unable to extract enode ID"

    enodeIdShell :: Shell Text
    enodeIdShell = sed (begins "enode") $ do
      jsPath <- using $ fileContaining jsPayload
      inshell (jsCommand jsPath) empty

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

writeStaticNodes :: (MonadIO m) => ClusterEnv -> [Geth] -> Geth -> m ()
writeStaticNodes cEnv sibs geth = output jsonPath contents
  where
    jsonPath = dataDir cEnv (gethId geth) </> "static-nodes.json"
    contents = return $ toStrict . decodeUtf8 . encode $ gethEnodeId <$> sibs

mkGeth :: MonadIO m => ClusterEnv -> GethId -> m Geth
mkGeth cEnv gid = Geth gid <$> getEnodeId cEnv gid

setupNodes :: MonadIO m => ClusterEnv -> [GethId] -> m [Geth]
setupNodes cEnv gids = do
  wipeDataDirs cEnv

  void $ liftIO $ forConcurrently gids $ \gid -> do
    initNode cEnv gid
    createAccount cEnv gid

  geths <- liftIO $ mapConcurrently (mkGeth cEnv) gids

  liftIO $ forConcurrently (allSiblings geths) $ \(geth, sibs) -> do
    writeStaticNodes cEnv sibs geth
    pure geth

inshellWithJoinedErr :: Text -> Shell Text -> Shell Text
inshellWithJoinedErr cmd inputShell = do
  line <- inshellWithErr cmd inputShell
  case line of
    Left txt  -> return txt
    Right txt -> return txt

data NodeReady = NodeReady -- online, and ready for us to start raft
data NodeTerminated = NodeTerminated

runNode :: forall m. MonadManaged m
        => ClusterEnv
        -> Geth
        -> m (Async NodeReady, Async NodeTerminated)
runNode cEnv geth = do
  mvar <- liftIO newEmptyMVar

  let started :: m (Async NodeReady)
      started = fork $ do
                  _ <- takeMVar mvar
                  return NodeReady

      gid = gethId geth

      outputPath :: FilePath
      outputPath = fromText $ format ("geth"%d%".out") (gId gid)

      nodeShell :: Shell Text
      nodeShell = inshellWithJoinedErr (gethCommand cEnv "" gid) empty

      terminated :: m (Async NodeTerminated)
      terminated = fmap ($> NodeTerminated) $
        using $ fork $ runManaged $ do
          handle <- using (writeonly outputPath)
          liftIO $ hSetBuffering handle LineBuffering

          foldIO nodeShell $ Fold.mapM_ $ \line -> do
            liftIO $ T.hPutStrLn handle line
            guard =<< isEmptyMVar mvar
            when ("IPC endpoint opened:" `isInfixOf` line) $
              putMVar mvar NodeReady

  (,) <$> started <*> terminated

startRaft :: MonadIO m => ClusterEnv -> Geth -> m ()
startRaft cEnv geth = shells startCommand empty
  where
    startCommand =
      gethCommand cEnv
                  (format ("--exec 'raft.startNode();' attach "%s) ipcEndpoint)
                  (gethId geth)

    ipcPath = dataDir cEnv (gethId geth) </> "geth.ipc"
    ipcEndpoint = format ("ipc:"%fp) ipcPath

awaitAll :: (MonadIO m, Traversable t) => t (Async a) -> m ()
awaitAll = liftIO . traverse_ wait

runNodes :: MonadManaged m => ClusterEnv -> [Geth] -> m ()
runNodes cEnv geths = do
  (readyAsyncs, terminatedAsyncs) <- unzip <$> traverse (runNode cEnv) geths

  awaitAll readyAsyncs
  traverse_ (startRaft cEnv) geths
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
