{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RecordWildCards     #-}

module Cluster where

import           Control.Arrow              ((>>>))
import           Control.Concurrent.Async   (forConcurrently)
import qualified Control.Foldl              as Fold
import           Control.Lens               (at, view, (^.))
import           Control.Monad              (replicateM)
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Data.Aeson                 (Value, withObject, (.:))
import           Data.Aeson.Types           (parseMaybe)
import qualified Data.Aeson.Types           as Aeson
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Set                   as Set
import           Data.Text                  (Text, replace)
import           Prelude                    hiding (FilePath, lines)
import           Safe                       (atMay, headMay)
import           System.IO                  (hClose)
import           Turtle                     hiding (env, view)

import           Cluster.Genesis            (createGenesisJson)
import           Cluster.Observing
import           Cluster.Types
import           Cluster.Util               (textDecode, textEncode,
                                             bytes20P, matchOnce,
                                             HexPrefix(..), printHex, tee, inshellWithJoinedErr)
import           Constellation              (constellationConfPath,
                                             setupConstellationNode)
import           Cluster.Control

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
  , _clusterConsensus          = Raft
  , _clusterPrivacySupport     = PrivacyDisabled
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

constellationPort :: HasEnv m => GethId -> m Port
constellationPort (GethId gid) = pure $ (fromIntegral gid +) 9000 -- TODO: CONVERT THIS TO USE BASE PORT

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
                               " "%s%
                               " "%s)
                          (dataDirPath (gethDataDir geth))
                          (gethHttpPort geth)
                          (gethRpcPort geth)
                          (gethNetworkId geth)
                          (gethVerbosity geth)
                          (consensusOptions (gethConsensusPeer geth))
  where
    consensusOptions :: ConsensusPeer -> Text
    consensusOptions RaftPeer = "--raft"
    consensusOptions (QuorumChainPeer mVoterAcct mBlockMakerAcct) =
      let voteStr = case mVoterAcct of
            Just acctId -> format
              ("--voteaccount \""%s%"\" --votepassword \"\"")
              (accountIdToText acctId)
            Nothing -> ""
          makerStr = case mBlockMakerAcct of
            Just acctId -> format
              ("--blockmakeraccount \""%s%"\" --blockmakerpassword \"\"")
              (accountIdToText acctId)
            Nothing -> ""
      in format (s%" "%s) voteStr makerStr

initNode :: (MonadIO m, HasEnv m) => FilePath -> GethId -> m ()
initNode genesisJsonPath gid = do
  cmd <- setupCommand gid <*> pure (format ("init "%fp) genesisJsonPath)
  void $ sh $ inshellWithErr cmd empty

readAccountKey :: MonadIO m => DataDir -> AccountId -> m (Maybe AccountKey)
readAccountKey dir acctId = do
    let keystoreDir = dataDirPath dir </> "keystore"
    paths <- fold (ls keystoreDir) Fold.list
    let acctIdText = printHex WithoutPrefix (unAddr (accountId acctId))
    let hasAccountId = format fp
          >>> match (contains $ text acctIdText)
          >>> null
          >>> not
    let mPath = headMay $ filter hasAccountId paths

    fmap (AccountKey acctId) <$> sequence (strict . input <$> mPath)

createAccount :: (MonadIO m, HasEnv m) => DataDir -> m AccountKey
createAccount dir = do
    let cmd = rawCommand dir "account new"
    pw <- view clusterPassword
    -- Enter pw twice in response to "Passphrase:" and "Repeat passphrase:"
    let acctShell = inshellWithJoinedErr cmd (select $ textToLines pw
                                                    <> textToLines pw)
                  & grep (begins "Address: ")
                  & sed (chars *> between (char '{') (char '}') chars)
    let mkAccountId = forceAcctId -- force head
          >>> lineToText
          -- expect this line to be 20 hex bytes
          >>> matchOnce (bytes20P WithoutPrefix) >>> forceAcctBytes
          -- an account id is an Addr, is 20 bytes
          >>> Addr >>> AccountId
    aid <- mkAccountId <$> fold acctShell Fold.head
    mKey <- readAccountKey dir aid
    return $ forceKey mKey

  where
    forceAcctId = fromMaybe $ error "unable to extract account ID"
    forceAcctBytes = fromMaybe $ error "unable to convert account ID to bytes"
    forceKey = fromMaybe $ error "unable to find key in keystore"

fileContaining :: Shell Line -> Managed FilePath
fileContaining contents = do
  dir <- using $ mktempdir "/tmp" "geth"
  (path, handle) <- using $ mktemp dir "geth"
  liftIO $ do
    outhandle handle contents
    hClose handle
  return path

gidIp :: HasEnv m => GethId -> m Ip
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

mkConsensusPeer :: GethId -> Consensus -> ConsensusPeer
mkConsensusPeer _   Raft = RaftPeer
mkConsensusPeer gid (QuorumChain (bmGid, bmAid) voterAccts)
  | gid == bmGid = QuorumChainPeer mVoterAcct (Just bmAid)
  | otherwise    = QuorumChainPeer mVoterAcct Nothing
  where
    mVoterAcct = Map.lookup gid voterAccts

mkGeth
  :: (MonadIO m, HasEnv m)
  => GethId -> EnodeId -> AccountId -> m Geth
mkGeth gid eid aid = do
  rpcPort' <- rpcPort gid
  ip <- gidIp gid
  datadir <- gidDataDir gid

  Geth <$> pure gid
       <*> pure eid
       <*> httpPort gid
       <*> pure rpcPort'
       <*> pure aid
       <*> view clusterPassword
       <*> view clusterNetworkId
       <*> view clusterVerbosity
       <*> pure datadir
       <*> fmap (mkConsensusPeer gid) (view clusterConsensus)
       <*> gidIp gid
       <*> pure (format ("http://"%s%":"%d) (getIp ip) rpcPort')
       <*> fmap (\case
                   PrivacyEnabled -> Just $ constellationConfPath datadir
                   PrivacyDisabled -> Nothing)
                (view clusterPrivacySupport)

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
      AccountId . Addr <$> obj .: "address"

    force = fromMaybe $ error "failed to load account ID from keystore"

generateAccountKeys :: (MonadIO m, HasEnv m) => Int -> m [AccountKey]
generateAccountKeys numAccts = do
  clusterEnv <- ask
  liftIO $ with (DataDir <$> mktempdir "/tmp" "geth") $ \tmpDataDir ->
    runReaderT (replicateM numAccts $ createAccount tmpDataDir) clusterEnv

-- TODO: probably refactor this to take a Geth, not GethId?
mkConstellationConfig :: HasEnv m => GethId -> m ConstellationConfig
mkConstellationConfig thisGid = do
    -- Everyone connects to all the nodes spun up before them
    let priorPeers :: [GethId]
        priorPeers = enumFromTo 1 (pred thisGid)

    ConstellationConfig <$> constellationUrl thisGid
                        <*> gidDataDir thisGid
                        <*> pure thisGid
                        <*> traverse constellationUrl priorPeers
  where
    constellationUrl :: HasEnv m => GethId -> m Text
    constellationUrl gid = do
      ip <- gidIp gid
      port <- constellationPort gid
      pure $ format ("http://"%s%":"%d%"/") (getIp ip) port

setupNodes :: (MonadIO m, HasEnv m) => Maybe DataDir -> [GethId] -> m [Geth]
setupNodes deployDatadir gids = do
  acctKeys <- generateAccountKeys (length gids)
  genesisJsonPath <- createGenesisJson $ akAccountId <$> acctKeys

  clusterEnv <- ask
  geths <- liftIO $ forConcurrently (zip gids acctKeys) $ \(gid, acctKey) ->
    runReaderT (createNode genesisJsonPath gid acctKey) clusterEnv

  void $ liftIO $ forConcurrently geths $ writeStaticNodes geths

  privacySuport <- view clusterPrivacySupport
  when (privacySuport == PrivacyEnabled) $
    void $ liftIO $ forConcurrently gids $ \gid -> do
      constConf <- runReaderT (mkConstellationConfig gid) clusterEnv
      setupConstellationNode deployDatadir constConf

  pure geths

wipeLocalClusterRoot :: (MonadIO m) => FilePath -> m ()
wipeLocalClusterRoot rootDir = do
  dirExists <- testdir rootDir
  when dirExists $ rmtree rootDir
  mktree rootDir

wipeAndSetupNodes :: (MonadIO m, HasEnv m) => Maybe DataDir -> FilePath -> [GethId] -> m [Geth]
wipeAndSetupNodes deployDatadir rootDir gids = do
  wipeLocalClusterRoot rootDir
  setupNodes deployDatadir gids

gethShell :: Geth -> Shell Line
gethShell geth = do
  pwPath <- using $ fileContaining $ select $ textToLines $ gethPassword geth

  case gethConstellationConfig geth of
    Just conf -> export "PRIVATE_CONFIG" (format fp conf)
    Nothing   -> pure ()

  inshellWithJoinedErr (gethCommand geth $
                                    format ("--unlock 0 --password "%fp) pwPath)
                       empty

runNode :: forall m. (MonadManaged m)
        => Int
        -> Geth
        -> m NodeInstrumentation
runNode numNodes geth = do
  -- allocate events and behaviors
  (nodeOnline,      triggerStarted)     <- eventVar NodeOnline
  (allConnected,    triggerConnected)   <- eventVar AllConnected
  (assumedRole,     triggerAssumedRole) <- eventVar AssumedRole
  (lastBlock,       updateLastBlock)    <- behaviorVar
  (lastRaftStatus,  updateRaftStatus)   <- behaviorVar
  (outstandingTxes, updateOutstanding)  <- behaviorVar
  (txAddrs,         updateAddrs)        <- behaviorVar
  (membershipMVar,  updateMembership)   <- behaviorVar

  let predicate connSet =
      -- with the HTTP transport, each node actually even connects to itself
        if Set.size connSet == numNodes then Just () else Nothing
  async <- behaviorToEvent membershipMVar predicate

  let logPath = fromText $ nodeName (gethId geth) <> ".out"
      instrumentedLines
        = gethShell geth
        & tee logPath
        & observingRoles triggerAssumedRole
        & observingActivation updateMembership
        & observingBoot triggerStarted
        & observingLastBlock updateLastBlock
        & observingRaftStatus updateRaftStatus
        & observingTxes updateOutstanding updateAddrs

  _ <- fork $ wait async >> triggerConnected
  nodeTerminated <- fork $ NodeTerminated <$ sh instrumentedLines

  pure $ NodeInstrumentation {..}

runNodesIndefinitely :: MonadManaged m => [Geth] -> m ()
runNodesIndefinitely geths = do
  let numNodes = length geths
      extractInstruments NodeInstrumentation {nodeOnline, nodeTerminated} =
        (nodeOnline, nodeTerminated)
  instruments <- traverse (runNode numNodes) geths
  let (_, terminatedAsyncs) = unzip $ extractInstruments <$> instruments

  awaitAll terminatedAsyncs
