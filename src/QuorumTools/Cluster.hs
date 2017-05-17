{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module QuorumTools.Cluster where

import           Control.Arrow              ((>>>))
import           Control.Concurrent.Async   (forConcurrently)
import qualified Control.Foldl              as Fold
import           Control.Lens               (at, has, ix, view, (^.), (^?))
import           Control.Monad              (replicateM)
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Data.Aeson                 (Value, withObject, (.:))
import           Data.Aeson.Types           (parseMaybe)
import qualified Data.Aeson.Types           as Aeson
import           Data.Bool                  (bool)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             ((<>))
import           Data.Set                   (member)
import qualified Data.Set                   as Set
import           Data.Text                  (Text, replace)
import           Prelude                    hiding (FilePath, lines)
import           Safe                       (atMay, headMay)
import           System.IO                  (hClose)
import           Turtle                     hiding (env, has, view, (<>))

import           QuorumTools.Constellation  (constellationConfPath,
                                             setupConstellationNode)
import           QuorumTools.Control
import           QuorumTools.Genesis        (createGenesisJson)
import           QuorumTools.Observing
import           QuorumTools.Types
import           QuorumTools.Util           (HexPrefix (..), bytes20P,
                                             inshellWithJoinedErr, matchOnce,
                                             printHex, tee, textDecode,
                                             textEncode)

emptyClusterEnv :: ClusterEnv
emptyClusterEnv = ClusterEnv
  { _clusterPassword           = CleartextPassword "abcd"
  , _clusterNetworkId          = 1418
  , _clusterBaseHttpPort       = 30400
  , _clusterBaseRpcPort        = 40400
  , _clusterVerbosity          = 3
  , _clusterGenesisJson        = "gdata" </> "genesis.json"
  , _clusterIps                = Map.empty
  , _clusterDataDirs           = Map.empty
  , _clusterConstellationConfs = Map.empty
  , _clusterAccountKeys        = Map.empty
  , _clusterInitialMembers     = Set.empty
  , _clusterConsensus          = Raft
  , _clusterPrivacySupport     = PrivacyDisabled
  }

mkClusterEnv :: (GethId -> Ip)
             -> (GethId -> DataDir)
             -> Map GethId AccountKey
             -> ClusterEnv
mkClusterEnv mkIp mkDataDir keys = emptyClusterEnv
    { _clusterIps            = Map.fromList [(gid, mkIp gid)      | gid <- gids]
    , _clusterDataDirs       = Map.fromList [(gid, mkDataDir gid) | gid <- gids]
    , _clusterAccountKeys    = keys
    , _clusterInitialMembers = Set.fromList gids
    }
  where
    gids = Map.keys keys

mkLocalEnv :: Map GethId AccountKey -> ClusterEnv
mkLocalEnv = mkClusterEnv mkIp mkDataDir
  where
    mkIp = const $ Ip "127.0.0.1"
    mkDataDir gid = DataDir $ "gdata" </> fromText (nodeName gid)

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

bootnodeCommand :: Text
bootnodeCommand = "bootnode --nodekeyhex 77bd02ffa26e3fb8f324bda24ae588066f1873d95680104de5bc2db9e7b2e510 --addr='127.0.0.1:33445'"


bootnodeEnode :: EnodeId
bootnodeEnode = EnodeId "enode://61077a284f5ba7607ab04f33cfde2750d659ad9af962516e159cf6ce708646066cd927a900944ce393b98b95c914e4d6c54b099f568342647a1cd4a262cc0423@[127.0.0.1]:33445"

gethCommand :: Geth -> Text -> Text
gethCommand geth more = format (s%" geth --datadir "%fp                    %
                                       " --port "%d                        %
                                       " --rpcport "%d                     %
                                       " --networkid "%d                   %
                                       " --verbosity "%d                   %
                                       " --nodiscover"                     %
                                       " --rpc"                            %
                                       " --rpccorsdomain '*'"              %
                                       " --rpcaddr localhost"              %
                                       " --rpcapi eth,net,web3,raft,admin" %
                                       " --emitcheckpoints"                %
                                       " --unlock 0"                       %
                                       " "%s%
                                       " "%s)
                          envVar
                          (dataDirPath (gethDataDir geth))
                          (gethHttpPort geth)
                          (gethRpcPort geth)
                          (gethNetworkId geth)
                          (gethVerbosity geth)
                          consensusOptions
                          more
  where
    envVar :: Text
    envVar = case gethConstellationConfig geth of
      Just conf -> "PRIVATE_CONFIG=" <> format fp conf
      Nothing   -> ""

    consensusOptions :: Text
    consensusOptions = case gethConsensusPeer geth of
      RaftPeer -> case gethJoinMode geth of
        JoinExisting   -> format ("--raft --raftjoinexisting "%d) (gId $ gethId geth)
        JoinNewCluster -> "--raft"
      QuorumChainPeer (EnodeId bootnode) acctId mRole ->
        let roleSpecificText = case mRole of
              Nothing -> ""
              Just BlockMaker -> format
                ("--blockmakeraccount '"%s%"' --blockmakerpassword 'abcd'"
                %" --voteaccount '"%s%"' --votepassword 'abcd'"
                )
                (accountIdToText acctId)
                (accountIdToText acctId)
              Just Voter -> format
                ("--voteaccount '"%s%"' --votepassword 'abcd'")
                (accountIdToText acctId)

        in format ("--bootnodes '"%s%"' --networkid 84234 "%s) bootnode roleSpecificText

initNode :: (MonadIO m, HasEnv m) => FilePath -> GethId -> m ()
initNode genesisJsonPath gid = do
  cmd <- setupCommand gid <*> pure (format ("init "%fp) genesisJsonPath)
  void $ sh $ inshellWithErr cmd empty

generateClusterKeys :: MonadIO m => [GethId] -> Password -> m (Map GethId AccountKey)
generateClusterKeys gids pw = liftIO $ with mkDataDirs $ \dirs ->
    Map.fromList . zip gids <$> forConcurrently dirs (createAccount pw)

  where
    mkDataDirs :: Managed [DataDir]
    mkDataDirs = replicateM (length gids) $ DataDir <$> mktempdir "/tmp" "geth"

findAccountKey :: MonadIO m => DataDir -> AccountId -> m (Maybe AccountKey)
findAccountKey dir acctId = do
    let keystoreDir = dataDirPath dir </> "keystore"
    paths <- fold (ls keystoreDir) Fold.list
    let acctIdText = printHex WithoutPrefix (unAddr (accountId acctId))
        hasAccountId = format fp
          >>> match (contains $ text acctIdText)
          >>> null
          >>> not
        mPath = headMay $ filter hasAccountId paths

    fmap (AccountKey acctId) <$> sequence (strict . input <$> mPath)

readAccountKey :: MonadIO m => DataDir -> GethId -> m AccountKey
readAccountKey (DataDir ddPath) gid = do
    keyContents <- strict $ input keyPath
    let acctId = forceAcctId $ parseMaybe aidParser =<< textDecode keyContents
    pure $ AccountKey acctId keyContents

  where
    keyPath = ddPath </> "keystore" </> fromText (nodeName gid)

    aidParser :: Value -> Aeson.Parser AccountId
    aidParser = withObject "AccountId" $ \obj ->
      AccountId . Addr <$> obj .: "address"

    forceAcctId = fromMaybe $ error "failed to load account ID from keystore"

createAccount :: MonadIO m => Password -> DataDir -> m AccountKey
createAccount (CleartextPassword pw) dir = do
    let cmd = rawCommand dir "account new"
    -- Enter pw twice in response to "Passphrase:" and "Repeat passphrase:"
    let acctShell = inshell cmd (select $ textToLines pw <> textToLines pw)
                  & grep (begins "Address: ")
                  & sed (chars *> between (char '{') (char '}') chars)
    let mkAccountId = forceAcctId -- force head
          >>> lineToText
          -- expect this line to be 20 hex bytes
          >>> matchOnce (bytes20P WithoutPrefix) >>> forceAcctBytes
          -- an account id is an Addr, is 20 bytes
          >>> Addr >>> AccountId
    aid <- mkAccountId <$> fold acctShell Fold.head
    mKey <- findAccountKey dir aid
    return $ forceKey mKey

  where
    forceAcctId    = fromMaybe $ error "unable to extract account ID"
    forceAcctBytes = fromMaybe $ error "unable to convert account ID to bytes"
    forceKey       = fromMaybe $ error "unable to find key in keystore"

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
--
-- TODO: see if we can do this through HTTP instead, now that we've whitelisted
--       access to the admin API via RPC.
--
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

mkConsensusPeer :: GethId -> AccountId -> Consensus -> ConsensusPeer
mkConsensusPeer _   _   Raft = RaftPeer
mkConsensusPeer gid aid (QuorumChain bootnode bmGid voterGids) =
  QuorumChainPeer bootnode aid $ if | gid == bmGid ->           Just BlockMaker
                                    | gid `member` voterGids -> Just Voter
                                    | otherwise ->              Nothing

mkGeth :: (MonadIO m, HasEnv m) => GethId -> EnodeId -> m Geth
mkGeth gid eid = do
  rpcPort' <- rpcPort gid
  ip <- gidIp gid
  datadir <- gidDataDir gid
  cEnv <- ask

  let isInitialMember = has (clusterInitialMembers . ix gid) cEnv
      aid = fromMaybe (error $ "missing key in env for " <> show gid)
                      (cEnv ^? clusterAccountKeys . ix gid . akAccountId)

  Geth <$> pure gid
       <*> pure eid
       <*> httpPort gid
       <*> pure rpcPort'
       <*> pure aid
       <*> view clusterPassword
       <*> view clusterNetworkId
       <*> view clusterVerbosity
       <*> pure datadir
       <*> fmap (mkConsensusPeer gid aid) (view clusterConsensus)
       <*> pure (bool JoinExisting JoinNewCluster isInitialMember)
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
  output jsonPath (select $ textToLines $ _akKey acctKey)

createNode :: (MonadIO m, HasEnv m)
           => FilePath
           -> GethId
           -> m Geth
createNode genesisJsonPath gid = do
    initNode genesisJsonPath gid
    cEnv <- ask
    let acctKey = forceAcctKey $ cEnv ^? clusterAccountKeys . ix gid
    installAccountKey gid acctKey
    eid <- requestEnodeId gid
    mkGeth gid eid

  where
    forceAcctKey :: Maybe AccountKey -> AccountKey
    forceAcctKey = fromMaybe $ error $ "key missing in env for " <> show gid

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
  acctKeys <- view clusterAccountKeys
  genesisJsonPath <- createGenesisJson $ _akAccountId <$> Map.elems acctKeys

  clusterEnv <- ask
  geths <- liftIO $ forConcurrently gids $ \gid ->
    runReaderT (createNode genesisJsonPath gid) clusterEnv

  let initialGeths = filter ((JoinNewCluster ==) . gethJoinMode) geths

  void $ liftIO $ forConcurrently geths $ writeStaticNodes initialGeths

  privacySupport <- view clusterPrivacySupport
  when (privacySupport == PrivacyEnabled) $
    void $ liftIO $ forConcurrently gids $ \gid -> do
      constConf <- runReaderT (mkConstellationConfig gid) clusterEnv
      setupConstellationNode deployDatadir constConf

  pure geths

wipeLocalClusterRoot :: (MonadIO m) => FilePath -> m ()
wipeLocalClusterRoot rootDir = do
  dirExists <- testdir rootDir
  when dirExists $ rmtree rootDir
  mktree rootDir

wipeAndSetupNodes
  :: (MonadIO m, HasEnv m)
  => Maybe DataDir
  -> FilePath
  -> [GethId]
  -> m [Geth]
wipeAndSetupNodes deployDatadir rootDir gids = do
  wipeLocalClusterRoot rootDir
  setupNodes deployDatadir gids

gethShell :: Geth -> Shell Line
gethShell geth = do
  pwPath <- using $ fileContaining $ select $ textToLines $
    pwCleartext $ gethPassword geth

  inshellWithJoinedErr (gethCommand geth $ format ("--password "%fp) pwPath)
                       empty

runNode :: forall m. (MonadManaged m)
        => Int
        -> Geth
        -> m NodeInstrumentation
runNode numInitialNodes geth = do
  -- allocate events and behaviors
  (nodeOnline,   triggerStarted)     <- event NodeOnline
  (allConnected, triggerConnected)   <- event AllConnected
  (assumedRole,  triggerAssumedRole) <- event AssumedRole
  lastBlock                          <- behavior
  lastRaftStatus                     <- behavior
  outstandingTxes                    <- behavior
  txAddrs                            <- behavior
  membershipChanges                  <- behavior

  clusterIsFull <- watch membershipChanges $ \peers ->
    -- with the HTTP transport, each node actually even connects to itself
    if Set.size peers == numInitialNodes then Just () else Nothing

  let logPath = fromText $ nodeName (gethId geth) <> ".out"
      instrumentedLines
        = gethShell geth
        & tee logPath
        & observingRoles      triggerAssumedRole
        & observingActivation (transition membershipChanges)
        & observingBoot       triggerStarted
        & observingLastBlock  (transition lastBlock)
        & observingRaftStatus (transition lastRaftStatus)
        & observingTxes       (transition outstandingTxes) (transition txAddrs)

  _ <- fork $ wait clusterIsFull >> triggerConnected
  nodeTerminated <- fork $ NodeTerminated <$ sh instrumentedLines

  pure NodeInstrumentation {..}

runNodesIndefinitely :: MonadManaged m => [Geth] -> m ()
runNodesIndefinitely geths = do
  let numInitialNodes = length geths
  instruments <- traverse (runNode numInitialNodes) geths
  let terminatedAsyncs = nodeTerminated <$> instruments

  awaitAll terminatedAsyncs
