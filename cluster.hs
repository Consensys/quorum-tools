#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-- OR, to pre-compile this:
-- $ stack ghc -- -O2 -threaded cluster.hs

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

import           Control.Concurrent.Async   (Async, forConcurrently)
import           Control.Concurrent.MVar    (isEmptyMVar, newEmptyMVar, putMVar,
                                             takeMVar)
import qualified Control.Foldl              as Fold
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Control.Monad.Reader.Class (MonadReader (ask, reader))
import           Data.Aeson
import           Data.Bifunctor             (first)
import           Data.Foldable              (traverse_)
import           Data.Functor               (($>))
import           Data.Maybe                 (fromMaybe, isJust)
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

data AccountId = AccountId Text
  deriving (Show, Eq)

data Geth =
  Geth { gethId        :: GethId
       , gethEnodeId   :: EnodeId
       , gethAccountId :: AccountId
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

createAccount :: (MonadIO m, HasEnv m) => GethId -> m AccountId
createAccount gid = do
  cmd <- gethCommand gid <*> pure "account new"
  pw <- reader clusterPassword
  -- Enter pw twice in response to "Passphrase:" and "Repeat passphrase:"
  let acctShell = inshell cmd (return pw <|> return pw)
                & grep (begins "Address: ")
                & sed (chars *> between (char '{') (char '}') chars)

  AccountId . forceMaybe <$> fold acctShell Fold.head

  where
    forceMaybe = fromMaybe $ error "unable to extract account ID"

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
      enodeIdShell = do
                       jsPath <- using $ fileContaining jsPayload
                       let cmd = mkCmd $ format ("js "%fp) jsPath
                       inshell cmd empty
                   & grep (begins "enode")

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

mkGeth :: (MonadIO m, HasEnv m) => GethId -> AccountId -> m Geth
mkGeth gid aid = Geth gid <$> getEnodeId gid <*> pure aid

setupNodes :: forall m. (MonadIO m, HasEnv m) => [GethId] -> m [Geth]
setupNodes gids = do
  cEnv <- ask

  wipeDataDirs

  geths <- liftIO $ forConcurrently gids $ \gid -> flip runReaderT cEnv $ do
    initNode gid
    aid <- createAccount gid
    mkGeth gid aid

  void $ liftIO $ forConcurrently (allSiblings geths) $ \(geth, sibs) ->
    flip runReaderT cEnv $ writeStaticNodes sibs geth

  pure geths

inshellWithJoinedErr :: Text -> Shell Text -> Shell Text
inshellWithJoinedErr cmd inputShell = do
  line <- inshellWithErr cmd inputShell
  case line of
    Left txt  -> return txt
    Right txt -> return txt

gethShell :: HasEnv m
          => Geth
          -> m (Shell Text)
gethShell geth = do
  let gid = gethId geth
  command <- gethCommand gid <*> pure ""
  return $ inshellWithJoinedErr command empty

tee :: FilePath -> Shell Text -> Shell Text
tee filepath lines = do
  handle <- using $ writeonly filepath
  liftIO $ hSetBuffering handle LineBuffering
  line <- lines
  liftIO $ T.hPutStrLn handle line
  return line

data Transitioned = PreTransition | PostTransition

observingTransition :: (a -> Bool) -> Shell a -> Shell (Transitioned, a)
observingTransition test lines = do
  mvar <- liftIO newEmptyMVar
  line <- lines
  isEmpty <- liftIO $ isEmptyMVar mvar

  let thisIsTheLine = test line
  when (isEmpty && thisIsTheLine) $ liftIO $ putMVar mvar ()

  let isPost = not isEmpty || thisIsTheLine
  return (if isPost then PostTransition else PreTransition, line)

data NodeOnline = NodeOnline -- IPC is up; ready for us to start raft
data NodeTerminated = NodeTerminated

-- TODO: move from tuple to first-class data type
-- TODO: once we have >1 data type (e.g. regular vs partition testing), we can
--       use a typeclass to access the (Maybe NodeOnline) field.
observingBoot :: Shell Text -> Shell (Maybe NodeOnline, Text)
observingBoot shell = first isOnline <$> observingTransition ipcOpened shell
  where
    ipcOpened = ("IPC endpoint opened:" `isInfixOf`)
    isOnline = \case
      PreTransition -> Nothing
      PostTransition -> Just NodeOnline

instrumentedGethShell :: HasEnv m => Geth -> m (Shell (Maybe NodeOnline, Text))
instrumentedGethShell geth = observingBoot . tee logPath <$> gethShell geth
  where
    logPath = fromText $ format ("geth"%d%".out") $ gId . gethId $ geth

runNode :: forall m. (MonadManaged m, HasEnv m)
        => Geth
        -> m (Async NodeOnline, Async NodeTerminated)
runNode geth = do
  shell <- instrumentedGethShell geth
  onlineMvar <- liftIO newEmptyMVar

  let started :: m (Async NodeOnline)
      started = fork $ NodeOnline <$ takeMVar onlineMvar

      terminated :: m (Async NodeTerminated)
      terminated = fmap ($> NodeTerminated) $
        fork $ foldIO shell $ Fold.mapM_ $ \(mOnline, _line) -> do
          isEmpty <- isEmptyMVar onlineMvar
          when (isEmpty && isJust mOnline) $
            putMVar onlineMvar ()

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
