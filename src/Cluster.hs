{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Cluster where

import           Control.Arrow              ((>>>))
import           Control.Concurrent.Async   (Async, forConcurrently)
import           Control.Concurrent.MVar    (MVar, isEmptyMVar, modifyMVar_,
                                             newEmptyMVar, newMVar, putMVar,
                                             readMVar, swapMVar)
import qualified Control.Foldl              as Fold
import           Control.Lens               (to, (<&>), (^.), (^?))
import           Control.Monad              (replicateM)
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Control.Monad.Reader.Class (MonadReader (ask, reader))
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import           Data.Aeson                 (ToJSON (toJSON), Value (String),
                                             encode, object, (.=))
import           Data.Aeson.Lens            (key, _String)
import           Data.Bifunctor             (first, second)
import qualified Data.ByteString.Lazy       as LSB
import           Data.Functor               (($>))
import           Data.List                  (unzip5)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Monoid                (Last, (<>))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (isInfixOf, pack, replace)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Text.Lazy             (toStrict)
import qualified Data.Text.Lazy.Encoding    as LT
import           Network.Wreq               (Response, post, responseBody)
import           Prelude                    hiding (FilePath, lines)
import           Safe                       (headMay)
import           System.IO                  (BufferMode (..), hClose,
                                             hSetBuffering)
import           Turtle

import           Control
import           Checkpoint

newtype Verbosity = Verbosity Int
  deriving (Eq, Show, Num, Enum, Ord, Real, Integral)

newtype Millis = Millis Int
  deriving Num

newtype Seconds = Seconds Int
  deriving Num

newtype Port = Port { getPort :: Int }
  deriving (Eq, Show, Enum, Ord, Num, Real, Integral)

newtype Ip = Ip { getIp :: Text }
  deriving (Eq, Show)

data ClusterEnv
  = ClusterEnv { clusterDataRoot     :: FilePath
               , clusterPassword     :: Text
               , clusterNetworkId    :: Int
               , clusterBaseHttpPort :: Port
               , clusterBaseRpcPort  :: Port
               , clusterVerbosity    :: Verbosity
               , clusterIps          :: Map.Map GethId Ip
               }
  deriving (Eq, Show)

type HasEnv = MonadReader ClusterEnv

defaultClusterEnv :: ClusterEnv
defaultClusterEnv = ClusterEnv { clusterDataRoot     = "gdata"
                               , clusterPassword     = "abcd"
                               , clusterNetworkId    = 1418
                               , clusterBaseHttpPort = 30400
                               , clusterBaseRpcPort  = 40400
                               , clusterVerbosity    = 3
                               , clusterIps    =
                                   Map.fromList [ (1, Ip "127.0.0.1")
                                                , (2, Ip "127.0.0.1")
                                                , (3, Ip "127.0.0.1")]
                               }

data EnodeId = EnodeId Text
  deriving (Show, Eq)

instance ToJSON EnodeId where
  toJSON (EnodeId eid) = String eid

data AccountId = AccountId { accountId :: Text }
  deriving (Show, Eq)

data AccountKey = AccountKey { akAccountId :: AccountId
                             , akKey       :: T.Text
                             }
  deriving (Show, Eq)

data TxId = TxId { txId :: Text }
  deriving (Show, Eq)

data Block = Block Text
  deriving (Eq, Show)

data DataDir
  = DataDir { dataDirPath :: FilePath }
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
       , gethDataDir   :: DataDir
       , gethUrl       :: Text
       }
  deriving (Show, Eq)

data RaftRole
  = Leader
  | Candidate
  | Follower
  deriving (Eq, Show)

data RaftStatus
  = RaftStatus { raftRole :: RaftRole
               , raftTerm :: Int }
  deriving Show

nodeName :: GethId -> T.Text
nodeName gid = format ("geth"%d) (gId gid)

gidDataDir :: HasEnv m => GethId -> m DataDir
gidDataDir gid = do
  dataDirPathRoot <- reader clusterDataRoot
  pure . DataDir $ dataDirPathRoot </> fromText (nodeName gid)

httpPort :: HasEnv m => GethId -> m Port
httpPort (GethId gid) = (fromIntegral gid +) <$> reader clusterBaseHttpPort

rpcPort :: HasEnv m => GethId -> m Port
rpcPort (GethId gid) = (fromIntegral gid +) <$> reader clusterBaseRpcPort

rawCommand :: DataDir -> Text -> Text
rawCommand dir = format ("geth --datadir "%fp%" "%s) (dataDirPath dir)

setupCommand :: HasEnv m => GethId -> m (Text -> Text)
setupCommand gid = format ("geth --datadir "%fp%
                               " --port "%d    %
                               " --nodiscover" %
                               " "% s)
                      <$> fmap dataDirPath (gidDataDir gid)
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
                               " --raft"              %
                               " "%s)
                          (dataDirPath (gethDataDir geth))
                          (gethHttpPort geth)
                          (gethRpcPort geth)
                          (gethNetworkId geth)
                          (gethVerbosity geth)

wipeDataDirs :: (MonadIO m, HasEnv m) => m ()
wipeDataDirs = do
  gdata <- reader clusterDataRoot
  dirExists <- testdir gdata
  when dirExists $ rmtree gdata
  mktree gdata

initNode :: (MonadIO m, HasEnv m) => FilePath -> GethId -> m ()
initNode genesisJsonPath gid = do
  cmd <- setupCommand gid <*> pure (format ("init "%fp) genesisJsonPath)
  shells cmd empty

readAccountKey :: MonadIO m => DataDir -> AccountId -> m (Maybe AccountKey)
readAccountKey dir acctId@(AccountId aid) = do
    paths <- fold (ls $ dataDirPath dir <> "keystore") Fold.list
    let mPath = headMay $ filter (format fp
                                    >>> match (contains $ text aid)
                                    >>> null
                                    >>> not)
                                 paths

    fmap (AccountKey acctId) <$> sequence (strict . input <$> mPath)

inshellWithJoinedErr :: Text -> Shell Line -> Shell Line
inshellWithJoinedErr cmd inputShell = do
  line <- inshellWithErr cmd inputShell
  case line of
    Left txt  -> return txt
    Right txt -> return txt

createAccount :: (MonadIO m, HasEnv m) => DataDir -> m AccountKey
createAccount dir = do
    let cmd = rawCommand dir "account new"
    pw <- reader clusterPassword
    -- Enter pw twice in response to "Passphrase:" and "Repeat passphrase:"
    let acctShell = inshellWithJoinedErr cmd (select $ textToLines pw
                                                    <> textToLines pw)
                  & grep (begins "Address: ")
                  & sed (chars *> between (char '{') (char '}') chars)
    aid <- AccountId . lineToText . forceAcctId <$> fold acctShell Fold.head
    mKey <- readAccountKey dir aid
    return $ forceKey mKey

  where
    forceAcctId = fromMaybe $ error "unable to extract account ID"
    forceKey = fromMaybe $ error "unable to find key in keystore"

fileContaining :: Shell Line -> Managed FilePath
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
  mHost <- Map.lookup gid <$> reader clusterIps

  let host = getIp $ forceIp mHost
      enodeIdShell = do
                       jsPath <- using $ fileContaining jsPayload
                       let cmd = mkCmd $ format ("js "%fp) jsPath
                       inshell cmd empty
                   & grep (begins "enode")
                   & sed (fmap (\a b -> a <> host <> b) chars
                           <*  text "[::]"
                           <*> chars)

  EnodeId . lineToText . forceNodeId <$> fold enodeIdShell Fold.head

  where
    jsPayload = return "console.log(admin.nodeInfo.enode)"
    forceNodeId = fromMaybe $ error "unable to extract enode ID"
    forceIp = fromMaybe $ error $ "no IP found for " <> show gid

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
       <*> gidDataDir gid
       <*> pure (format ("http://localhost:"%d) rpcPort')

installAccountKey :: (MonadIO m, HasEnv m) => GethId -> AccountKey -> m ()
installAccountKey gid acctKey = do
  dir <- gidDataDir gid
  let keystoreDir = (dataDirPath dir) </> "keystore"
      jsonPath = keystoreDir </> fromText (nodeName gid)
  output jsonPath (select $ textToLines $ akKey acctKey)

createNode :: (MonadIO m, HasEnv m)
           => FilePath
           -> GethId
           -> AccountKey
           -> m Geth
createNode genesisJsonPath gid acctKey = do
  initNode genesisJsonPath gid
  installAccountKey gid acctKey
  eid <- getEnodeId gid
  mkGeth gid eid (akAccountId acctKey)

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
  nodeDataDir <- gidDataDir gid
  let js = "console.log(eth.accounts[0] + '!' + admin.nodeInfo.enode)"
  cmd <- setupCommand gid <*> pure (sendJsSubcommand (dataDirPath nodeDataDir) js)

  let pat :: Pattern (AccountId, EnodeId)
      pat = pure (,) <*> fmap (AccountId . T.pack) ("0x" *> count 40 hexDigit)
                     <*> fmap EnodeId ("!" *> begins "enode")

  (aid, eid) <- fmap forceMaybe $ runMaybeT $ do
    line <- MaybeT $ fold (inshell cmd empty) Fold.head
    MaybeT $ return $ headMay $ match pat (lineToText line)

  mkGeth gid eid aid

  where
    forceMaybe = fromMaybe $ error "unable to extract account and enode ID"

-- TODO: switch to a more efficient version
textEncode :: ToJSON a => a -> Text
textEncode = toStrict . LT.decodeUtf8 . encode

writeStaticNodes :: MonadIO m => [Geth] -> Geth -> m ()
writeStaticNodes sibs geth = output jsonPath contents
  where
    jsonPath = dataDirPath (gethDataDir geth) </> "static-nodes.json"
    contents = select $ textToLines $ textEncode $ gethEnodeId <$> sibs

generateAccountKeys :: (MonadIO m, HasEnv m) => Int -> m [AccountKey]
generateAccountKeys numAccts = do
  clusterEnv <- ask
  liftIO $ with (DataDir <$> mktempdir "/tmp" "geth") $ \tmpDataDir ->
    runReaderT (replicateM numAccts $ createAccount tmpDataDir) clusterEnv

createGenesisJson :: (MonadIO m, HasEnv m) => [AccountId] -> m FilePath
createGenesisJson acctIds = do
    jsonPath <- reader clusterDataRoot <&> (</> "genesis.json")
    output jsonPath contents
    return jsonPath

  where
    contents :: Shell Line
    contents = select $ textToLines $ textEncode $ object
      [ "alloc"      .= object
        (fmap (\(AccountId aid) ->
                 ("0x" <> aid) .= object [ "balance" .= t "0" ])
              acctIds)
      , "coinbase"   .= t "0x0000000000000000000000000000000000000000"
      , "config"     .= object
        [ "homesteadBlock" .= (0 :: Int) ]
      , "difficulty" .= t "0x0"
      , "extraData"  .= t "0x0"
      , "gasLimit"   .= t "0x2FEFD800"
      , "mixhash"    .= t "0x0000000000000000000000000000000000000000000000000000000000000000"
      , "nonce"      .= t "0x0"
      , "parentHash" .= t "0x0000000000000000000000000000000000000000000000000000000000000000"
      , "timestamp"  .= t "0x0"
      ]

    t = id :: Text -> Text

setupNodes :: (MonadIO m, HasEnv m) => [GethId] -> m [Geth]
setupNodes gids = do
  wipeDataDirs

  acctKeys <- generateAccountKeys (length gids)
  genesisJsonPath <- createGenesisJson $ akAccountId <$> acctKeys

  clusterEnv <- ask
  geths <- liftIO $ forConcurrently (zip gids acctKeys) $ \(gid, acctKey) ->
    runReaderT (createNode genesisJsonPath gid acctKey) clusterEnv

  void $ liftIO $ forConcurrently (allSiblings geths) $ \(geth, sibs) ->
    writeStaticNodes sibs geth

  pure geths

gethShell :: Geth -> Shell Line
gethShell geth = do
  pwPath <- using $ fileContaining $ select $ textToLines $ gethPassword geth
  inshellWithJoinedErr (gethCommand geth $
                                    format ("--unlock 0 --password "%fp) pwPath)
                       empty

tee :: FilePath -> Shell Line -> Shell Line
tee filepath lines = do
  handle <- using $ writeonly filepath
  liftIO $ hSetBuffering handle LineBuffering
  line <- lines
  liftIO $ T.hPutStrLn handle $ lineToText line
  return line

data Transitioned
  = PreTransition
  | PostTransition

observingTransition :: (a -> Bool) -> Shell (a, b) -> Shell (a, (Transitioned, b))
observingTransition test lines = do
  mvar <- liftIO newEmptyMVar
  (line, b) <- lines
  isEmpty <- liftIO $ isEmptyMVar mvar

  let thisIsTheLine = test line
  when (isEmpty && thisIsTheLine) $ liftIO $ putMVar mvar ()

  let isPost = not isEmpty || thisIsTheLine
  return (line, (if isPost then PostTransition else PreTransition, b))

data NodeOnline = NodeOnline -- IPC is up; ready for us to start raft
data NodeTerminated = NodeTerminated deriving Eq

-- All http connections for this node are established
data AllConnected = AllConnected

-- TODO: move from tuple to first-class data type
-- TODO: once we have >1 data type (e.g. regular vs partition testing), we can
--       use a typeclass ReportsOnline/ReportsBooted/HasBooted to access the
--       (Maybe NodeOnline) field.
observingBoot :: Shell (Line, a) -> Shell (Line, (Maybe NodeOnline, a))
observingBoot lines = (second.first) isOnline <$> observingTransition ipcOpened lines
  where
    ipcOpened line = "IPC endpoint opened:" `isInfixOf` (lineToText line)
    isOnline = \case
      PreTransition -> Nothing
      PostTransition -> Just NodeOnline

observingLastBlock :: Shell (Line, a)
                   -> Shell (Line, (Last Block, a))
observingLastBlock incoming = do
    st <- liftIO $ newMVar (mempty :: Last Block)
    (line, a) <- incoming
    case match blockPattern (lineToText line) of
      [latest] -> liftIO $ modifyMVar_ st (\prev -> pure (prev <> pure latest))
      _       -> pure ()
    lastBlock <- liftIO (readMVar st)
    return (line, (lastBlock, a))

  where
    blockPattern :: Pattern Block
    blockPattern = has $
      Block . pack <$> ("Successfully extended chain: " *> count 64 hexDigit)

observingRaftStatus :: Shell (Line, a)
                    -> Shell (Line, (Last RaftStatus, a))
observingRaftStatus incoming = do
    st <- liftIO $ newMVar (mempty :: Last RaftStatus)
    (line, a) <- incoming
    case match statusPattern (lineToText line) of
      [raftStatus] -> liftIO $ modifyMVar_ st (const $ pure $ pure raftStatus)
      _            -> pure ()
    lastRaftStatus <- liftIO (readMVar st)
    return (line, (lastRaftStatus, a))

  where
    statusPattern :: Pattern RaftStatus
    statusPattern = has $ RaftStatus <$> (text " became "  *> fmap toRole (plus lower))
                                     <*> (text " at term " *> decimal)

    toRole :: Text -> RaftRole
    toRole "follower"  = Follower
    toRole "candidate" = Candidate
    toRole "leader"    = Leader
    toRole unknown = error $ "failed to parse unknown raft role: " ++ T.unpack unknown

startObserving :: Shell Line -> Shell (Line, ())
startObserving incoming = (, ()) <$> incoming

uniqueMatch :: Pattern a -> Text -> Maybe a
uniqueMatch pat txt = case match pat txt of
  [result] -> Just result
  _ -> Nothing

observingActivation :: Shell (Line, a)
                    -> Shell (Line, (Set GethId, a))
observingActivation incoming = do
  connections <- liftIO $ newMVar (mempty :: Set GethId)
  (line, a) <- incoming
  let pat = patternForCheckpoint PeerConnected
        <|> patternForCheckpoint PeerDisconnected
      delta = uniqueMatch pat (lineToText line)
  result <- liftIO $ pureModifyMVar connections (applyDelta delta)

  return (line, (result, a))

instrumentedGethShell :: Geth
                      -> Shell (Line,
                               (Last RaftStatus,
                               (Last Block,
                               (Maybe NodeOnline,
                               (Set GethId,
                               ())))))
instrumentedGethShell geth = gethShell geth
                           & tee logPath
                           & startObserving
                           & observingActivation
                           & observingBoot
                           & observingLastBlock
                           & observingRaftStatus
  where
    logPath = fromText $ format ("geth"%d%".out") $ gId . gethId $ geth

runNode :: forall m. (MonadManaged m)
        => Int
        -> Geth
        -> m ( Async NodeOnline
             , Async NodeTerminated
             , MVar (Last Block)
             , MVar (Last RaftStatus)
             , Async AllConnected )
runNode numNodes geth = do
  (onlineMvar, lastBlockMvar, lastRaftMvar, allConnectedMVar) <- liftIO $
    (,,,) <$> newEmptyMVar <*> newMVar mempty <*> newMVar mempty <*> newEmptyMVar

  -- TODO: take as arg:
  let instrumentedLines = instrumentedGethShell geth

  -- expect one connection to each of the other peers
      expectedPeerCount = numNodes - 1

      started :: m (Async NodeOnline)
      started = awaitMVar onlineMvar

      terminated :: m (Async NodeTerminated)
      terminated = fmap ($> NodeTerminated) processor

      connected :: m (Async AllConnected)
      connected = awaitMVar allConnectedMVar

      processor :: m (Async ())
      processor = fork $ foldIO instrumentedLines $ Fold.mapM_ $
        \(_line, (lastRaftStatus, (lastBlock, (mOnline, (connSet, ()))))) -> do
          void $ swapMVar lastBlockMvar lastBlock
          void $ swapMVar lastRaftMvar lastRaftStatus

          when (Set.size connSet == expectedPeerCount) $
            ensureMVarTransition allConnectedMVar AllConnected

          when (isJust mOnline) $ ensureMVarTransition onlineMvar NodeOnline

  ( , ,lastBlockMvar,lastRaftMvar,) <$> started <*> terminated <*> connected

sendJs :: MonadIO m => Geth -> Text -> m ()
sendJs geth js = shells (gethCommand geth subcmd) empty
  where
    subcmd = sendJsSubcommand (dataDirPath $ gethDataDir geth) js

startRaft :: MonadIO m => Geth -> m ()
startRaft geth = sendJs geth "raft.startNode();"

runNodesIndefinitely :: MonadManaged m => [Geth] -> m ()
runNodesIndefinitely geths = do
  let numNodes = length geths
  (readyAsyncs, terminatedAsyncs, _lastBlocks, _lastRafts, _establishedConnections) <-
    unzip5 <$> traverse (runNode numNodes) geths

  awaitAll readyAsyncs

  void $ liftIO $ forConcurrently geths startRaft

  awaitAll terminatedAsyncs

txRpcBody :: Geth -> Value
txRpcBody geth = object
  [ "id"      .= (1 :: Int)
  , "jsonrpc" .= t "2.0"
  , "method"  .= t "eth_sendTransaction"
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
spamTransactions gethList =
  let spamInfinite (geth:geths) = sendTx geth >> spamInfinite geths
      spamInfinite _ = error "spamTransactions: list must have at least one element"
  in spamInfinite (cycle gethList)

bench :: MonadIO m => Geth -> Seconds -> m ()
bench geth (Seconds seconds) = view benchShell
  where
    luaEscapeSingleQuotes = jsEscapeSingleQuotes
    lua = format ("wrk.method = 'POST'\n"  %
                  "wrk.body   = '"%s%"'\n" %
                  "wrk.headers['Content-Type'] = 'application/json'")
                 (luaEscapeSingleQuotes $ textEncode $ txRpcBody geth)

    benchShell = do
      luaPath <- using $ fileContaining $ select $ textToLines lua
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
