#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package aeson

-- OR, to pre-compile this:
-- $ stack ghc -- -O2 -threaded cluster.hs

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (Async, forConcurrently)
import           Control.Concurrent.MVar    (isEmptyMVar, newEmptyMVar, putMVar,
                                             takeMVar)
import           Control.Exception          (bracket)
import qualified Control.Foldl              as Fold
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Control.Monad.Reader.Class (MonadReader (ask, reader))
import           Control.Monad.State        (evalStateT, modify, get)
import           Control.Monad.Trans.Class  (lift)
import           Data.Aeson
import           Data.Bifunctor             (first)
import           Data.Foldable              (traverse_)
import           Data.Functor               (($>))
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Text                  (isInfixOf, pack, replace)
import qualified Data.Text.IO               as T
import qualified Data.Text                  as T
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

newtype Verbosity = Verbosity Int
  deriving (Eq, Show, Num, Enum, Ord, Real, Integral)

newtype Millis = Millis Int deriving Num
newtype Port = Port Int deriving (Eq, Show, Enum, Ord, Num, Real, Integral)

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

newtype GethId = GethId { gId :: Int }
  deriving (Show, Eq, Num)

data EnodeId = EnodeId Text
  deriving (Show, Eq)

instance ToJSON EnodeId where
  toJSON (EnodeId eid) = String eid

data AccountId = AccountId Text
  deriving (Show, Eq)

data Geth =
  Geth { gethId        :: GethId
       , gethEnodeId   :: EnodeId
       , gethHttpPort  :: Port
       , gethRpcPort   :: Port
       , gethAccountId :: AccountId
       , gethPassword  :: Text
       , gethNetworkId :: Int
       , gethVerbosity :: Verbosity
       , gethDataDir   :: FilePath
       }
  deriving (Show, Eq)

dataDir :: HasEnv m => GethId -> m FilePath
dataDir gid = do
  dataDirRoot <- reader clusterDataRoot
  let nodeName = format ("geth"%d) (gId gid)
  pure $ dataDirRoot </> fromText nodeName

httpPort :: HasEnv m => GethId -> m Port
httpPort (GethId gid) = Port . (gid +) <$> reader clusterBaseHttpPort

rpcPort :: HasEnv m => GethId -> m Port
rpcPort (GethId gid) = Port . (gid +) <$> reader clusterBaseRpcPort

setupCommand :: HasEnv m => GethId -> m (Text -> Text)
setupCommand gid = format ("geth --datadir "%fp%
                               " --port "%d    %
                               " --nodiscover" %
                               " "% s)
                      <$> dataDir gid
                      <*> httpPort gid

gethCommand :: Geth -> Text -> Text
gethCommand geth = format ("geth --datadir "%fp       %
                               " --port "%d           %
                               " --rpcport "%d        %
                               " --networkid "%d      %
                               " --verbosity "%d      %
                               " --nodiscover"        %
                               " --rpc"               %
                               " --rpccorsdomain '*'" %
                               " --rpcaddr localhost" %
                               " "%s)
                          (gethDataDir geth)
                          (gethHttpPort geth)
                          (gethRpcPort geth)
                          (gethNetworkId geth)
                          (gethVerbosity geth)

wipeDataDirs :: (MonadIO m, HasEnv m) => m ()
wipeDataDirs = rmtree =<< reader clusterDataRoot

initNode :: (MonadIO m, HasEnv m) => GethId -> m ()
initNode gid = do
  genesisJson <- reader clusterGenesisJson
  cmd <- setupCommand gid <*> pure (format ("init "%fp) genesisJson)
  shells cmd empty

createAccount :: (MonadIO m, HasEnv m) => GethId -> m AccountId
createAccount gid = do
  cmd <- setupCommand gid <*> pure "account new"
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
  mkCmd <- setupCommand gid

  let enodeIdShell = do
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

mkGeth :: (MonadIO m, HasEnv m) => GethId -> EnodeId -> AccountId -> m Geth
mkGeth gid eid aid = Geth <$> pure gid
                          <*> pure eid
                          <*> httpPort gid
                          <*> rpcPort gid
                          <*> pure aid
                          <*> reader clusterPassword
                          <*> reader clusterNetworkId
                          <*> reader clusterVerbosity
                          <*> dataDir gid

createNode :: (MonadIO m, HasEnv m) => GethId -> m Geth
createNode gid = do
  initNode gid
  aid <- createAccount gid
  eid <- getEnodeId gid
  mkGeth gid eid aid

writeStaticNodes :: MonadIO m => [Geth] -> Geth -> m ()
writeStaticNodes sibs geth = output jsonPath contents
  where
    jsonPath = gethDataDir geth </> "static-nodes.json"
    contents = return $ toStrict . decodeUtf8 . encode $ gethEnodeId <$> sibs

setupNodes :: (MonadIO m, HasEnv m) => [GethId] -> m [Geth]
setupNodes gids = do
  cEnv <- ask

  wipeDataDirs

  geths <- liftIO $ forConcurrently gids $ \gid ->
    runReaderT (createNode gid) cEnv

  void $ liftIO $ forConcurrently (allSiblings geths) $ \(geth, sibs) ->
    writeStaticNodes sibs geth

  pure geths

inshellWithJoinedErr :: Text -> Shell Text -> Shell Text
inshellWithJoinedErr cmd inputShell = do
  line <- inshellWithErr cmd inputShell
  case line of
    Left txt  -> return txt
    Right txt -> return txt

gethShell :: Geth -> Shell Text
gethShell geth = inshellWithJoinedErr (gethCommand geth "") empty

tee :: FilePath -> Shell Text -> Shell Text
tee filepath lines = do
  handle <- using $ writeonly filepath
  liftIO $ hSetBuffering handle LineBuffering
  line <- lines
  liftIO $ T.hPutStrLn handle line
  return line

data Transitioned
  = PreTransition
  | PostTransition

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
--       use a typeclass ReportsOnline/ReportsBooted/HasBooted to access the
--       (Maybe NodeOnline) field.
observingBoot :: Shell Text -> Shell (Maybe NodeOnline, Text)
observingBoot shell = first isOnline <$> observingTransition ipcOpened shell
  where
    ipcOpened = ("IPC endpoint opened:" `isInfixOf`)
    isOnline = \case
      PreTransition -> Nothing
      PostTransition -> Just NodeOnline

data LastBlock
  = NoneSeen
  | LastBlock Text
  | Panic
  deriving Eq

almostLattice :: LastBlock -> LastBlock -> LastBlock
almostLattice NoneSeen x = x
almostLattice _ Panic = Panic
almostLattice _b1 b2 = b2

infixl 5 <^<
(<^<) = almostLattice

-- prototype: "I1107 15:40:34.895541 raft/handler.go:537] Successfully extended chain: d7895e144053e4e8980141cbf8d190506864c3963b970b04585509823864f618"
extractHash :: Pattern LastBlock
extractHash = has $
  LastBlock . pack <$> ("Successfully extended chain: " *> count 64 hexDigit)
  <|> Panic <$ "panic:"

trackLastBlock :: Shell (Maybe NodeOnline, Text)
               -> Shell (Maybe NodeOnline, LastBlock, Text)
trackLastBlock incoming = flip evalStateT NoneSeen $ do
  (online, line) <- lift incoming
  case match extractHash line of
    [block] -> modify (<^< block)
    _ -> pure ()
  (online, ,line) <$> get

instrumentedGethShell :: Geth -> Shell (Maybe NodeOnline, Text)
instrumentedGethShell geth = gethShell geth
                           & tee logPath
                           & observingBoot
                           -- & trackLastBlock
  where
    logPath = fromText $ format ("geth"%d%".out") $ gId . gethId $ geth

-- TODO: take a shell which supports ReportsOnline/ReportsBooted/HasBooted
-- instead of hard-coding to build an instrumentedGethShell
runNode :: forall m. (MonadManaged m)
        => Geth
        -> m (Async NodeOnline, Async NodeTerminated)
runNode geth = do
  -- TODO: take as arg:
  let shell = instrumentedGethShell geth

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

shellEscapeSingleQuotes :: Text -> Text
shellEscapeSingleQuotes = replace "'" "'\"'\"'" -- see http://bit.ly/2eKRS6W

jsEscapeSingleQuotes :: Text -> Text
jsEscapeSingleQuotes = replace "'" "\\'"

sendJs :: MonadIO m => Geth -> Text -> m ()
sendJs geth js = shells (gethCommand geth subcmd) empty
  where
    ipcEndpoint = format ("ipc:"%fp) $ gethDataDir geth </> "geth.ipc"
    subcmd = format ("--exec '"%s%"' attach "%s)
                    (shellEscapeSingleQuotes js)
                    ipcEndpoint

startRaft :: MonadIO m => Geth -> m ()
startRaft geth = sendJs geth "raft.startNode();"

unlockAccount :: MonadIO m => Geth -> m ()
unlockAccount geth = sendJs geth js
  where
    tenYears = 10 * 365 * 24 * 60 * 60 :: Int
    js = format ("personal.unlockAccount(eth.accounts[0], '"%s%"', "%d%");")
                (jsEscapeSingleQuotes $ gethPassword geth)
                tenYears

awaitAll :: (MonadIO m, Traversable t) => t (Async a) -> m ()
awaitAll = liftIO . traverse_ wait

runNodes :: MonadManaged m => [Geth] -> m ()
runNodes geths = do
  (readyAsyncs, terminatedAsyncs) <- unzip <$> traverse runNode geths

  awaitAll readyAsyncs
  void $ liftIO $ forConcurrently geths unlockAccount
  void $ liftIO $ forConcurrently geths startRaft
  awaitAll terminatedAsyncs

-- | Make a packet filter rule to block a specific port.
blockPortRule :: Port -> Text
blockPortRule (Port i) = format
  ("block in quick inet proto { tcp, udp } from any to any port "%d) i

-- | Execute an action before exiting. Exception safe.
--
-- @
--     onExit (putStrLn "exited!") $ \_ -> { code }
-- @
onExit :: IO () -> (() -> IO r) -> IO r
onExit action cb = bracket (pure ()) (\_ -> action) cb

-- | Partition some geth node for a number of milliseconds.
--
-- TODO: This will currently only work for partitioning a single node.
partition :: (MonadManaged m, HasEnv m) => Millis -> GethId -> m ()
partition (Millis ms) g = do
  pfConf <- fold (input "/etc/pf.conf") Fold.mconcat
  port <- httpPort g

  ruleFile <- using $ fileContaining $ pure $ T.unlines
    [ pfConf
    , blockPortRule port
    ]

  -- make sure to reset pf.conf on exit
  _ <- using $ managed (onExit (sh $ inshell "sudo pfctl -f /etc/pf.conf" ""))

  view $ inshell (format ("sudo pfctl -f "%fp) ruleFile) ""
  liftIO $ threadDelay (1000 * ms)


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
