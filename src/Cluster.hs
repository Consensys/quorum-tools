{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Cluster where

import           Control.Arrow              ((>>>))
import           Control.Concurrent.Async   (Async, forConcurrently)
import           Control.Concurrent.MVar    (isEmptyMVar, modifyMVar_,
                                             newEmptyMVar, newMVar, putMVar,
                                             readMVar, swapMVar, tryTakeMVar)
import qualified Control.Foldl              as Fold
import           Control.Lens               (at, view, (^.))
import           Control.Monad              (replicateM)
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Data.Aeson                 (FromJSON, ToJSON, Value, decode,
                                             encode, object, withObject, (.:),
                                             (.=))
import           Data.Aeson.Types           (parseMaybe)
import qualified Data.Aeson.Types           as Aeson
import           Data.Bifunctor             (first, second)
import           Data.Functor               (($>))
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Monoid                (Last, (<>))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text, isInfixOf, pack, replace)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Text.Lazy             (fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding    as LT
import           Prelude                    hiding (FilePath, lines)
import           Safe                       (atMay, headMay)
import           System.IO                  (BufferMode (..), hClose,
                                             hSetBuffering)
import           Turtle                     hiding (view, env)

import           Checkpoint
import           Cluster.Types
import           Control

emptyClusterEnv :: ClusterEnv
emptyClusterEnv = ClusterEnv
  { _clusterPassword           = "abcd"
  , _clusterNetworkId          = 1418
  , _clusterBaseHttpPort       = 30400
  , _clusterBaseRpcPort        = 40400
  , _clusterVerbosity          = 3
  , _clusterGenesisJson        = "gdata" </> "genesis.json"
  , _clusterIps                = Map.empty
  , _clusterDataDirs           = Map.empty
  , _clusterConstellationConfs = Map.empty
  }

mkClusterEnv :: (GethId -> Ip) -> (GethId -> DataDir) -> [GethId] -> ClusterEnv
mkClusterEnv mkIp mkDataDir gids = emptyClusterEnv
    { _clusterIps      = Map.fromList [(gid, mkIp gid)      | gid <- gids]
    , _clusterDataDirs = Map.fromList [(gid, mkDataDir gid) | gid <- gids]
    }

mkLocalEnv :: Int -> ClusterEnv
mkLocalEnv size = mkClusterEnv mkIp mkDataDir gids
  where
    mkIp = const $ Ip "127.0.0.1"
    mkDataDir gid = DataDir $ "gdata" </> fromText (nodeName gid)
    gids = clusterGids size

nodeName :: GethId -> Text
nodeName gid = format ("geth"%d) (gId gid)

pureGidDataDir :: GethId -> ClusterEnv -> DataDir
pureGidDataDir gid env = force $ env ^. clusterDataDirs . at gid
  where
    force = fromMaybe $ error $ "no data dir found for " <> show gid

gidDataDir :: HasEnv m => GethId -> m DataDir
gidDataDir gid = pureGidDataDir gid <$> ask

httpPort :: HasEnv m => GethId -> m Port
httpPort (GethId gid) = (fromIntegral gid +) <$> view clusterBaseHttpPort

rpcPort :: HasEnv m => GethId -> m Port
rpcPort (GethId gid) = (fromIntegral gid +) <$> view clusterBaseRpcPort

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

initNode :: (MonadIO m, HasEnv m) => FilePath -> GethId -> m ()
initNode genesisJsonPath gid = do
  cmd <- setupCommand gid <*> pure (format ("init "%fp) genesisJsonPath)
  void $ sh $ inshellWithErr cmd empty

readAccountKey :: MonadIO m => DataDir -> AccountId -> m (Maybe AccountKey)
readAccountKey dir acctId@(AccountId aid) = do
    let keystoreDir = dataDirPath dir </> "keystore"
    paths <- fold (ls keystoreDir) Fold.list
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
    pw <- view clusterPassword
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

gidIp :: (HasEnv m) => GethId -> m Ip
gidIp gid = force . Map.lookup gid <$> view clusterIps
  where
    force = fromMaybe $ error $ "no IP found for " <> show gid

-- | We need to use the RPC interface to get the EnodeId if we haven't yet
-- started up geth.
requestEnodeId :: (MonadIO m, HasEnv m) => GethId -> m EnodeId
requestEnodeId gid = do
  mkCmd <- setupCommand gid
  (Ip ip) <- gidIp gid

  let enodeIdShell = do
                       jsPath <- using $ fileContaining jsPayload
                       let cmd = mkCmd $ format ("js "%fp) jsPath
                       inshellWithJoinedErr cmd empty
                   & grep (begins "enode")
                   & sed (fmap (\a b -> a <> ip <> b) chars
                           <*  text "[::]"
                           <*> chars)

  EnodeId . lineToText . forceNodeId <$> fold enodeIdShell Fold.head

  where
    jsPayload = return "console.log(admin.nodeInfo.enode)"
    forceNodeId = fromMaybe $ error "unable to extract enode ID"

mkGeth :: (MonadIO m, HasEnv m) => GethId -> EnodeId -> AccountId -> m Geth
mkGeth gid eid aid = do
  rpcPort' <- rpcPort gid
  ip <- gidIp gid

  Geth <$> pure gid
       <*> pure eid
       <*> httpPort gid
       <*> pure rpcPort'
       <*> pure aid
       <*> view clusterPassword
       <*> view clusterNetworkId
       <*> view clusterVerbosity
       <*> gidDataDir gid
       <*> gidIp gid
       <*> pure (format ("http://"%s%":"%d) (getIp ip) rpcPort')
       <*> (Map.lookup gid <$> view clusterConstellationConfs)

installAccountKey :: (MonadIO m, HasEnv m) => GethId -> AccountKey -> m ()
installAccountKey gid acctKey = do
  dir <- gidDataDir gid
  let keystoreDir = dataDirPath dir </> "keystore"
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
  eid <- requestEnodeId gid
  mkGeth gid eid (akAccountId acctKey)

shellEscapeSingleQuotes :: Text -> Text
shellEscapeSingleQuotes = replace "'" "'\"'\"'" -- see http://bit.ly/2eKRS6W

jsEscapeSingleQuotes :: Text -> Text
jsEscapeSingleQuotes = replace "'" "\\'"

gethIpcPath :: DataDir -> FilePath
gethIpcPath datadir = dataDirPath datadir </> "geth.ipc"

-- TODO: switch to a more efficient version
textEncode :: ToJSON a => a -> Text
textEncode = toStrict . LT.decodeUtf8 . encode

-- TODO: switch to a more efficient version
textDecode :: FromJSON a => Text -> Maybe a
textDecode = decode . LT.encodeUtf8 . fromStrict

writeStaticNodes :: MonadIO m => [Geth] -> Geth -> m ()
writeStaticNodes sibs geth = output jsonPath contents
  where
    jsonPath = dataDirPath (gethDataDir geth) </> "static-nodes.json"
    contents = select $ textToLines $ textEncode $ gethEnodeId <$> sibs

readStaticNodes :: MonadIO m => DataDir -> m [EnodeId]
readStaticNodes (DataDir ddPath) = force . textDecode <$> strict (input path)
  where
    path = ddPath </> "static-nodes.json"
    force = fromMaybe $ error "failed to load enodes from static-nodes.json"

-- | If we've already started up geth in the past, we don't need to use RPC
-- interface to get the EnodeId; we can read it directly from the datadir.
readEnodeId :: (HasEnv m, MonadIO m) => GethId -> m EnodeId
readEnodeId gid = do
    nodeDataDir <- gidDataDir gid
    enodeIds <- readStaticNodes nodeDataDir
    let nodeIdx = gId gid - 1
    return $ forceEnodeId $ enodeIds `atMay` nodeIdx

  where
    forceEnodeId = fromMaybe $ error $
      "enode ID not found in list for " <> show gid

readAccountId :: (HasEnv m, MonadIO m) => GethId -> m AccountId
readAccountId gid = do
    (DataDir ddPath) <- gidDataDir gid
    let keyPath = ddPath </> "keystore" </> fromText (nodeName gid)
    keyContents <- strict $ input keyPath
    pure $ force $ parseMaybe aidParser =<< textDecode keyContents

  where
    aidParser :: Value -> Aeson.Parser AccountId
    aidParser = withObject "AccountId" $ \obj ->
      fmap AccountId $ obj .: "address"

    force = fromMaybe $ error "failed to load account ID from keystore"

generateAccountKeys :: (MonadIO m, HasEnv m) => Int -> m [AccountKey]
generateAccountKeys numAccts = do
  clusterEnv <- ask
  liftIO $ with (DataDir <$> mktempdir "/tmp" "geth") $ \tmpDataDir ->
    runReaderT (replicateM numAccts $ createAccount tmpDataDir) clusterEnv

createGenesisJson :: (MonadIO m, HasEnv m) => [AccountId] -> m FilePath
createGenesisJson acctIds = do
    jsonPath <- view clusterGenesisJson
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
  acctKeys <- generateAccountKeys (length gids)
  genesisJsonPath <- createGenesisJson $ akAccountId <$> acctKeys

  clusterEnv <- ask
  geths <- liftIO $ forConcurrently (zip gids acctKeys) $ \(gid, acctKey) ->
    runReaderT (createNode genesisJsonPath gid acctKey) clusterEnv

  void $ liftIO $ forConcurrently geths $ writeStaticNodes geths

  pure geths

wipeLocalClusterRoot :: (MonadIO m) => FilePath -> m ()
wipeLocalClusterRoot rootDir = do
  dirExists <- testdir rootDir
  when dirExists $ rmtree rootDir
  mktree rootDir

wipeAndSetupNodes :: (MonadIO m, HasEnv m) => FilePath -> [GethId] -> m [Geth]
wipeAndSetupNodes rootDir gids = do
  wipeLocalClusterRoot rootDir
  setupNodes gids

gethShell :: Geth -> Shell Line
gethShell geth = do
  pwPath <- using $ fileContaining $ select $ textToLines $ gethPassword geth

  case gethConstellationConfig geth of
    Just conf -> export "PRIVATE_CONFIG" (format fp conf)
    Nothing -> pure ()

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

observingTransition :: (a -> Bool) -> Shell (a, b) -> Shell (a, (Transitioned, b))
observingTransition test lines = do
  mvar <- liftIO newEmptyMVar
  (line, b) <- lines
  isEmpty <- liftIO $ isEmptyMVar mvar

  let thisIsTheLine = test line
  when (isEmpty && thisIsTheLine) $ liftIO $ putMVar mvar ()

  let isPost = not isEmpty || thisIsTheLine
  return (line, (if isPost then PostTransition else PreTransition, b))

-- TODO: move from tuple to first-class data type
-- TODO: once we have >1 data type (e.g. regular vs partition testing), we can
--       use a typeclass ReportsOnline/ReportsBooted/HasBooted to access the
--       (Maybe NodeOnline) field.
observingBoot :: Shell (Line, a) -> Shell (Line, (Maybe NodeOnline, a))
observingBoot lines = (second.first) isOnline <$> observingTransition ipcOpened lines
  where
    ipcOpened line = "IPC endpoint opened:" `isInfixOf` lineToText line
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

-- | Helper for the most common (only) use case for matchCheckpoint.
matchCheckpoint' :: Checkpoint a -> Line -> (a -> IO ()) -> Shell ()
matchCheckpoint' cpt line cb = case matchCheckpoint cpt line of
  Just a  -> liftIO (cb a)
  Nothing -> pure ()

observingTxes
  :: Shell (Line, a)
  -> Shell (Line, ((OutstandingTxes, TxAddrs), a))
observingTxes incoming = do
    outstanding <- liftIO $ newMVar mempty
    addrs <- liftIO $ newMVar mempty
    (line, a) <- incoming

    matchCheckpoint' TxCreated line $ \(tx, addr) -> do
      pureModifyMVar_ outstanding (Set.insert tx)
      pureModifyMVar_ addrs (Map.insert tx addr)

    matchCheckpoint' TxAccepted line $ \tx ->
      pureModifyMVar_ outstanding (Set.delete tx)

    txResult <- liftIO $ OutstandingTxes <$> readMVar outstanding
    addrsResult <- liftIO $ TxAddrs <$> readMVar addrs

    return (line, ((txResult, addrsResult), a))

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

observingRoles
  :: Shell (Line, a)
  -> Shell (Line, (Maybe AssumedRole, a))
observingRoles incoming = do
  roleMV <- liftIO newEmptyMVar
  (line, a) <- incoming

  matchCheckpoint' BecameMinter line $ \() ->
    void $ ensureMVarTransition roleMV AssumedRole

  matchCheckpoint' BecameVerifier line $ \() ->
    void $ ensureMVarTransition roleMV AssumedRole

  status <- liftIO $ tryTakeMVar roleMV
  return (line, (status, a))

startObserving :: Shell Line -> Shell (Line, ())
startObserving incoming = (, ()) <$> incoming

observingActivation :: Shell (Line, a)
                    -> Shell (Line, (Set GethId, a))
observingActivation incoming = do
  connections <- liftIO $ newMVar (mempty :: Set GethId)
  (line, a) <- incoming

  matchCheckpoint' PeerConnected line $ \(PeerJoined joined) ->
    pureModifyMVar_ connections (Set.insert joined)

  matchCheckpoint' PeerDisconnected line $ \(PeerLeft left) ->
    pureModifyMVar_ connections (Set.delete left)

  result <- liftIO $ readMVar connections

  return (line, (result, a))

instrumentedGethShell :: Geth
                      -> Shell (Line,
                               ((OutstandingTxes, TxAddrs),
                               (Last RaftStatus,
                               (Last Block,
                               (Maybe NodeOnline,
                               (Set GethId,
                               (Maybe AssumedRole,
                               ())))))))
instrumentedGethShell geth
  = gethShell geth
  & tee logPath
  & startObserving
  & observingRoles
  & observingActivation
  & observingBoot
  & observingLastBlock
  & observingRaftStatus
  & observingTxes
  where
    logPath = fromText $ nodeName (gethId geth) <> ".out"

runNode :: forall m. (MonadManaged m)
        => Int
        -> Geth
        -> m NodeInstrumentation
runNode numNodes geth = do
  (onlineMvar, lastBlockMvar, lastRaftMvar, outstandingTxesMVar, txAddrsMVar, allConnectedMVar, assumedRoleMVar)
    <- liftIO $ (,,,,,,)
      <$> newEmptyMVar
      <*> newMVar mempty
      <*> newMVar mempty
      <*> newMVar mempty
      <*> newMVar mempty
      <*> newEmptyMVar
      <*> newEmptyMVar

  -- TODO: take as arg:
  let instrumentedLines = instrumentedGethShell geth

      -- with the HTTP transport, each node actually even connects to itself
      expectedPeerCount = numNodes

      started :: m (Async NodeOnline)
      started = awaitMVar onlineMvar

      terminated :: m (Async NodeTerminated)
      terminated = fmap ($> NodeTerminated) processor

      connected :: m (Async AllConnected)
      connected = awaitMVar allConnectedMVar

      assumedRole :: m (Async AssumedRole)
      assumedRole = awaitMVar assumedRoleMVar

      processor :: m (Async ())
      processor = fork $ foldIO instrumentedLines $ Fold.mapM_ $
        \(_line,
         ((outstandingTxes, txAddrs),
         (lastRaftStatus,
         (lastBlock,
         (mOnline,
         (connSet,
         (mAssumedRole, ()))))))) -> do
          void $ swapMVar lastBlockMvar lastBlock
          void $ swapMVar lastRaftMvar lastRaftStatus
          void $ swapMVar outstandingTxesMVar outstandingTxes
          void $ swapMVar txAddrsMVar txAddrs

          when (Set.size connSet == expectedPeerCount) $
            ensureMVarTransition allConnectedMVar AllConnected

          when (isJust mOnline) $ ensureMVarTransition onlineMvar NodeOnline

          -- TODO fix a lot of duplication between the two places that produce
          -- an AssumedRole in an MVar
          when (isJust mAssumedRole) $
            ensureMVarTransition assumedRoleMVar AssumedRole

  NodeInstrumentation
    <$> started
    <*> terminated
    <*> pure lastBlockMvar
    <*> pure lastRaftMvar
    <*> pure outstandingTxesMVar
    <*> pure txAddrsMVar
    <*> connected
    <*> assumedRole

runNodesIndefinitely :: MonadManaged m => [Geth] -> m ()
runNodesIndefinitely geths = do
  let numNodes = length geths
      extractInstruments NodeInstrumentation {nodeOnline, nodeTerminated} =
        (nodeOnline, nodeTerminated)
  instruments <- traverse (runNode numNodes) geths
  let (_, terminatedAsyncs) = unzip $ extractInstruments <$> instruments

  awaitAll terminatedAsyncs
