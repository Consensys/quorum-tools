{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module QuorumTools.Types where

import           Control.Concurrent.Async (Async)
import           Control.Lens             (makeLenses)
import           Control.Monad.Reader     (MonadReader)
import           Data.Aeson               (FromJSON (parseJSON),
                                           ToJSON (toJSON), Value (String))
import           Data.Aeson.Types         (typeMismatch)
import           Data.Map.Strict          (Map)
import           Data.Set                 (Set)
import           Data.String
import           Data.Text                (Text)
import           Prelude                  hiding (FilePath)
import           Turtle                   (ExitCode, FilePath)

import           QuorumTools.Control      (Behavior)
import           QuorumTools.Util

-- Constellation

data ConstellationConfig = ConstellationConfig
  { ccUrl        :: Text
  , ccDataDir    :: DataDir -- TODO: probably pull this out
  , ccPort       :: Port
  , ccOtherNodes :: [Text]
  } deriving (Eq, Show)

-- Geth / Cluster

newtype GethId = GethId { gId :: Int }
  deriving (Show, Eq, Num, Ord, Enum)

clusterGids :: Int -> [GethId]
clusterGids size = GethId <$> [1..size]

newtype Verbosity = Verbosity Int
  deriving (Eq, Show, Num, Enum, Ord, Real, Integral)

--
-- TODO: replace these two with Data.Time.Units
--
newtype Millis = Millis Int deriving Num
newtype Seconds = Seconds Int deriving Num

newtype Port = Port { getPort :: Int }
  deriving (Eq, Show, Enum, Ord, Num, Real, Integral)

newtype Ip = Ip { getIp :: Text }
  deriving (Eq, Show)

data DataDir = DataDir { dataDirPath :: FilePath }
  deriving (Show, Eq)

data AccountId = AccountId { accountId :: Addr }
  deriving (Show, Eq)

data AccountKey = AccountKey { _akAccountId :: AccountId
                             , _akKey       :: Text
                             }
  deriving (Show, Eq)

data Consensus
  = Raft        { _raftBasePort :: Port }
  | QuorumChain { _qcBlockMaker    :: GethId
                , _qcVoterAccounts :: Set GethId }
  deriving (Eq, Show)

data QuorumChainRole
  = BlockMaker
  | Voter
  deriving (Eq, Show)

data ConsensusPeer
  = RaftPeer Port
  | QuorumChainPeer AccountId (Maybe QuorumChainRole)
  deriving (Eq, Show)

data PrivacySupport
  = PrivacyEnabled
  | PrivacyDisabled
  deriving (Eq, Show)

data JoinMode
  = JoinExisting
  | JoinNewCluster
  deriving (Eq, Show)

type HasEnv = MonadReader ClusterEnv

data EnodeId = EnodeId Text
  deriving (Show, Eq)

instance ToJSON EnodeId where
  toJSON (EnodeId eid) = String eid

instance FromJSON EnodeId where
  parseJSON str@(String _) = EnodeId <$> parseJSON str
  parseJSON invalid        = typeMismatch "EnodeId" invalid

newtype Password
  = CleartextPassword { pwCleartext :: Text }
  deriving (Eq)

instance Show Password where
  show _ = "Password \"[REDACTED]\""

data ProvisionError
  = GethInitFailed ExitCode Text

data Geth =
  Geth { gethId                  :: GethId
       , gethEnodeId             :: EnodeId
       , gethHttpPort            :: Port
       , gethRpcPort             :: Port
       , gethAccountId           :: AccountId
       , gethPassword            :: Password
       , gethNetworkId           :: Int
       , gethVerbosity           :: Verbosity
       , gethDataDir             :: DataDir
       , gethConsensusPeer       :: ConsensusPeer
       , gethJoinMode            :: JoinMode
       , gethIp                  :: Ip
       , gethUrl                 :: Text
       , gethConstellationConfig :: Maybe FilePath
       }
  deriving (Show, Eq)

-- Addresses, transactions and blocks

newtype Addr = Addr { unAddr :: Bytes20 }
  deriving (Show, Eq)

addrToText :: Addr -> Text
addrToText = hexPrefixed . unAddr

accountIdToText :: AccountId -> Text
accountIdToText = addrToText . accountId

showGethAccountId :: Geth -> Text
showGethAccountId = accountIdToText . gethAccountId

data CallArgs = CallArgs
  { callTo     :: Bytes20
  , callMethod :: UnencodedMethod
  }

data TxSync
  = Sync
  | Async

data Tx = Tx
  -- (txFrom is implicitly eth.accounts[0])
  { txTo      :: Maybe Bytes20
  , txMethod  :: UnencodedMethod
  , txPrivacy :: Privacy
  , txSync    :: TxSync
  }

-- Contract constructor arguments
-- * The contract to create
-- * The initial value: could easily be generalized from 32 bytes
-- * Synchronous or asynchronous
data CreateArgs = CreateArgs Contract Bytes32 TxSync

data SpamMode
  = BenchEmptyTx
  | SendTx Tx

newtype UnencodedMethod = UnencodedMethod Text deriving IsString

newtype TxId = TxId { txId :: Bytes32 }
  deriving (Show, Eq, Ord)

newtype Block = Block Text
  deriving (Eq, Show)

newtype OutstandingTxes = OutstandingTxes { unOutstandingTxes :: Set TxId }
  deriving (Monoid)

newtype TxAddrs = TxAddrs { unTxAddrs :: Map TxId Addr }
  deriving (Monoid, Eq)

-- Raft

data RaftRole
  = Leader
  | Candidate
  | Follower
  deriving (Eq, Show)

data RaftStatus = RaftStatus { raftRole :: RaftRole
                             , raftTerm :: Int }
  deriving Show

-- Node instrumentation

data Transitioned
  = PreTransition
  | PostTransition

data AssumedRole = AssumedRole
data NodeOnline = NodeOnline -- IPC is up; ready for us to start raft
data NodeTerminated = NodeTerminated deriving Eq

-- All http connections for this node are established
data AllConnected = AllConnected

data NodeInstrumentation = NodeInstrumentation
  { nodeOnline      :: Async NodeOnline
  , nodeTerminated  :: Async NodeTerminated
  , lastBlock       :: Behavior Block
  , lastRaftStatus  :: Behavior RaftStatus
  , outstandingTxes :: Behavior OutstandingTxes
  , txAddrs         :: Behavior TxAddrs
  , allConnected    :: Async AllConnected
  , assumedRole     :: Async AssumedRole
  , killNode        :: IO ()
  }

-- Checkpointing

newtype PeerJoined = PeerJoined GethId deriving Show
newtype PeerLeft = PeerLeft GethId deriving Show

-- | Some checkpoint in the execution of the program.
data Checkpoint result where
  PeerConnected    :: Checkpoint PeerJoined
  PeerDisconnected :: Checkpoint PeerLeft

  BecameMinter     :: Checkpoint ()
  BecameVerifier   :: Checkpoint ()

  TxCreated        :: Checkpoint (TxId, Addr)
  TxAccepted       :: Checkpoint TxId

-- Packet filtering

newtype PfToken = PfToken Text

newtype Pid = Pid Int

-- Contracts

-- Holding a base-64 encoded public key
newtype Secp256k1 = Secp256k1 { unSecp256k1 :: Text }

-- | A contract may be visible to everyone or only to a list of public keys
data Privacy
  = Public
  | PrivateFor [Secp256k1]

-- A contract is:
-- * its privacy
-- * bytecode
-- * abi
data Contract = Contract Privacy [UnencodedMethod] Text Text

data ClusterEnv
  = ClusterEnv { _clusterPassword              :: Password
               , _clusterNetworkId             :: Int
               , _clusterBaseHttpPort          :: Port
               , _clusterBaseRpcPort           :: Port
               , _clusterBaseConstellationPort :: Port
               , _clusterVerbosity             :: Verbosity
               , _clusterGenesisJson           :: FilePath
               , _clusterIps                   :: Map GethId Ip
               , _clusterDataDirs              :: Map GethId DataDir
               --
               -- TODO: remove this? seems to be unused:
               --
               , _clusterConstellationConfs    :: Map GethId FilePath
               , _clusterAccountKeys           :: Map GethId AccountKey
               , _clusterInitialMembers        :: Set GethId
               , _clusterConsensus             :: Consensus
               , _clusterPrivacySupport        :: PrivacySupport
               }
  deriving (Eq, Show)

-- Lenses

makeLenses ''AccountKey
makeLenses ''ClusterEnv
makeLenses ''Consensus
