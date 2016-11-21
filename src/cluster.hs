{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Cluster where

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (Async, forConcurrently)
import           Control.Concurrent.MVar    (MVar, isEmptyMVar, newEmptyMVar,
                                             putMVar, swapMVar, takeMVar)
import           Control.Exception          (bracket)
import qualified Control.Foldl              as Fold
import           Control.Lens               (to, (^.), (^?))
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Control.Monad.Reader.Class (MonadReader (ask, reader))
import           Control.Monad.State        (evalStateT, get, modify)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import           Data.Aeson                 (ToJSON (toJSON), Value (String),
                                             encode, object, (.=))
import           Data.Aeson.Lens            (key, _String)
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Lazy       as LSB
import           Data.Foldable              (traverse_)
import           Data.Functor               (($>))
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Text                  (isInfixOf, pack, replace)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Text.Lazy             (toStrict)
import qualified Data.Text.Lazy.Encoding    as LT
import           Network.Wreq               (Response, post, responseBody)
import           Prelude                    hiding (FilePath)
import           Safe                       (headMay)
import           System.IO                  (BufferMode (..), hClose,
                                             hSetBuffering)
import           Turtle

main :: IO ()
main = sh $ flip runReaderT defaultClusterEnv $ do
  geths <- setupNodes [1, 2, 3]
  runNodesIndefinitely geths

--

newtype Verbosity = Verbosity Int
  deriving (Eq, Show, Num, Enum, Ord, Real, Integral)

newtype Millis = Millis Int
  deriving Num

newtype Seconds = Seconds Int
  deriving Num

newtype Port = Port { getPort :: Int }
  deriving (Eq, Show, Enum, Ord, Num, Real, Integral)

newtype Hostname = Hostname Text
  deriving (Eq, Show)

data ClusterEnv
  = ClusterEnv { clusterDataRoot     :: FilePath
               , clusterPassword     :: Text
               , clusterNetworkId    :: Int
               , clusterGenesisJson  :: FilePath
               , clusterBaseHttpPort :: Port
               , clusterBaseRpcPort  :: Port
               , clusterVerbosity    :: Verbosity
               , clusterHostnames    :: Map.Map GethId Hostname
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
                               , clusterHostnames    =
                                   Map.fromList [ (1, Hostname "localhost")
                                                , (2, Hostname "localhost")
                                                , (3, Hostname "localhost")]
                               }

newtype GethId = GethId { gId :: Int }
  deriving (Show, Eq, Num, Ord, Enum)

data EnodeId = EnodeId Text
  deriving (Show, Eq)

instance ToJSON EnodeId where
  toJSON (EnodeId eid) = String eid

data AccountId = AccountId { accountId :: Text }
  deriving (Show, Eq)

data TxId = TxId { txId :: Text }
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
       , gethUrl       :: Text
       }
  deriving (Show, Eq)

dataDir :: HasEnv m => GethId -> m FilePath
dataDir gid = do
  dataDirRoot <- reader clusterDataRoot
  let nodeName = format ("geth"%d) (gId gid)
  pure $ dataDirRoot </> fromText nodeName

httpPort :: HasEnv m => GethId -> m Port
httpPort (GethId gid) = (fromIntegral gid +) <$> reader clusterBaseHttpPort

rpcPort :: HasEnv m => GethId -> m Port
rpcPort (GethId gid) = (fromIntegral gid +) <$> reader clusterBaseRpcPort

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
  liftIO $ do
    outhandle handle contents
    hClose handle
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
mkGeth gid eid aid = do
  rpcPort' <- rpcPort gid

  Geth <$> pure gid
       <*> pure eid
       <*> httpPort gid
       <*> pure rpcPort'
       <*> pure aid
       <*> reader clusterPassword
       <*> reader clusterNetworkId
       <*> reader clusterVerbosity
       <*> dataDir gid
       <*> pure (format ("http://localhost:"%d) rpcPort')

createNode :: (MonadIO m, HasEnv m) => GethId -> m Geth
createNode gid = do
  initNode gid
  aid <- createAccount gid
  eid <- getEnodeId gid
  mkGeth gid eid aid

shellEscapeSingleQuotes :: Text -> Text
shellEscapeSingleQuotes = replace "'" "'\"'\"'" -- see http://bit.ly/2eKRS6W

jsEscapeSingleQuotes :: Text -> Text
jsEscapeSingleQuotes = replace "'" "\\'"

sendJsSubcommand :: FilePath -> Text -> Text
sendJsSubcommand nodeDataDir js = format ("--exec '"%s%"' attach "%s)
                                         (shellEscapeSingleQuotes js)
                                         ipcEndpoint
  where
    ipcEndpoint = format ("ipc:"%fp) $ nodeDataDir </> "geth.ipc"

loadNode :: (MonadIO m, HasEnv m) => GethId -> m Geth
loadNode gid = do
  nodeDataDir <- dataDir gid
  let js = "console.log(eth.accounts[0] + '!' + admin.nodeInfo.enode)"
  cmd <- setupCommand gid <*> pure (sendJsSubcommand nodeDataDir js)

  let pat :: Pattern (AccountId, EnodeId)
      pat = (pure (,)) <*> fmap (AccountId . T.pack) ("0x" *> count 40 hexDigit)
                       <*> fmap EnodeId ("!" *> begins "enode")

  (aid, eid) <- fmap forceMaybe $ runMaybeT $ do
    t <- MaybeT $ fold (inshell cmd empty) Fold.head
    MaybeT $ return $ headMay $ match pat t

  mkGeth gid eid aid

  where
    forceMaybe = fromMaybe $ error "unable to extract account and enode ID"

-- TODO: switch to a more efficient version
textEncode :: ToJSON a => a -> Text
textEncode = toStrict . LT.decodeUtf8 . encode

writeStaticNodes :: MonadIO m => [Geth] -> Geth -> m ()
writeStaticNodes sibs geth = output jsonPath contents
  where
    jsonPath = gethDataDir geth </> "static-nodes.json"
    contents = return $ textEncode $ gethEnodeId <$> sibs

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
almostLattice _ Panic    = Panic
almostLattice _b1 b2     = b2

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
    _       -> pure ()
  (online, ,line) <$> get

instrumentedGethShell :: Geth -> Shell (Maybe NodeOnline, LastBlock, Text)
instrumentedGethShell geth = gethShell geth
                           & tee logPath
                           & observingBoot
                           & trackLastBlock
  where
    logPath = fromText $ format ("geth"%d%".out") $ gId . gethId $ geth

-- TODO: take a shell which supports ReportsOnline/ReportsBooted/HasBooted
-- instead of hard-coding to build an instrumentedGethShell
runNode :: forall m. (MonadManaged m)
        => Geth
        -> m (Async NodeOnline, Async NodeTerminated, MVar LastBlock)
runNode geth = do
  -- TODO: take as arg:
  let shell = instrumentedGethShell geth

  (onlineMvar, lastBlockMvar) <- liftIO $ (,) <$> newEmptyMVar <*> newEmptyMVar

  let started :: m (Async NodeOnline)
      started = fork $ NodeOnline <$ takeMVar onlineMvar

      terminated :: m (Async NodeTerminated)
      terminated = fmap ($> NodeTerminated) processor

      processor :: m (Async ())
      processor = fork $ foldIO shell $ Fold.mapM_ $
        \(mOnline, lastBlock, _line) -> do
          isEmpty <- isEmptyMVar onlineMvar
          swapMVar lastBlockMvar lastBlock
          when (isEmpty && isJust mOnline) $
            putMVar onlineMvar ()

  (,, lastBlockMvar) <$> started <*> terminated



sendJs :: MonadIO m => Geth -> Text -> m ()
sendJs geth js = shells (gethCommand geth subcmd) empty
  where
    subcmd = sendJsSubcommand (gethDataDir geth) js

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

runNodesIndefinitely :: MonadManaged m => [Geth] -> m ()
runNodesIndefinitely geths = do
  (readyAsyncs, terminatedAsyncs, _lastBlocks) <-
    unzip3 <$> traverse runNode geths

  awaitAll readyAsyncs
  void $ liftIO $ do
    forConcurrently geths unlockAccount
    forConcurrently geths startRaft

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


txRpcBody :: Geth -> Value
txRpcBody geth = object
  [ "id"      .= (1 :: Int)
  , "jsonrpc" .= t "2.0"
  , "method"  .= t "raft_sendTransaction"
  , "params"  .=
    [ object
      [ "from" .= (accountId . gethAccountId $ geth)
      , "to"   .= t "0000000000000000000000000000000000000000"
      ]
    ]
  ]

  where
    t :: Text -> Text
    t = id

sendTx :: MonadIO io => Geth -> io (Either Text TxId)
sendTx geth = liftIO $ parse <$> post (T.unpack $ gethUrl geth) (txRpcBody geth)
  where
    parse :: Response LSB.ByteString -> Either Text TxId
    parse r = fromMaybe parseFailure mParsed
      where
        parseFailure = Left $ toStrict $
          "failed to parse RPC response: " <> LT.decodeUtf8 (r^.responseBody)
        mParsed :: Maybe (Either Text TxId)
        mParsed = (r^?responseBody.key "result"._String.to (Right . TxId))
              <|> (r^?responseBody.key "error".key "message"._String.to Left)

-- | Continuously send transaction requests in a round-robin order. This runs
--   indefinitely.
--
-- Invariant: list has at least one element
spamTransactions :: MonadIO m => [Geth] -> m ()
spamTransactions geths =
  let spamInfinite (g:gs) = sendTx g >> spamInfinite gs
  in spamInfinite (cycle geths)

bench :: MonadIO m => Geth -> Seconds -> m ()
bench geth (Seconds seconds) = view benchShell
  where
    luaEscapeSingleQuotes = jsEscapeSingleQuotes
    lua = format ("wrk.method = 'POST'"  %
                  "wrk.body   = '"%s%"'" %
                  "wrk.headers['Content-Type'] = 'application/json'")
                 (luaEscapeSingleQuotes $ textEncode $ txRpcBody geth)

    benchShell = do
      luaPath <- using $ fileContaining $ return lua
      let cmd = format ("wrk -s "%fp%" -c 1 -d "%d%"s -t 1 "%s)
                       luaPath
                       seconds
                       (gethUrl geth)
      inshell cmd empty

-- TODO: potentially use this
--
-- type IdProducer = MonadState GethId
--
-- produceId :: IdProducer m => m GethId
-- produceId = do
--   id@(GethId i) <- S.get
--   S.put (GethId (i + 1))
--   return id
