{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cluster.Types where

import           Control.Concurrent.Async (Async)
import           Control.Concurrent.MVar  (MVar)
import           Control.Lens             (makeLenses)
import           Control.Monad.Reader     (MonadReader)
import           Data.Aeson               (FromJSON (parseJSON),
                                           ToJSON (toJSON), Value (String))
import           Data.Aeson.Types         (typeMismatch)
import           Data.Map.Strict          (Map)
import           Data.Monoid              (Last)
import           Data.Set                 (Set)
import           Data.Text                (Text)
import           Prelude                  hiding (FilePath)
import           Turtle                   (FilePath)

-- Constellation

data ConstellationConfig = ConstellationConfig
  { ccUrl        :: Text
  , ccDatadir    :: DataDir
  , ccGethId     :: GethId
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

data ClusterEnv
  = ClusterEnv { _clusterPassword           :: Text
               , _clusterNetworkId          :: Int
               , _clusterBaseHttpPort       :: Port
               , _clusterBaseRpcPort        :: Port
               , _clusterVerbosity          :: Verbosity
               , _clusterGenesisJson        :: FilePath
               , _clusterIps                :: Map GethId Ip
               , _clusterDataDirs           :: Map GethId DataDir
               , _clusterConstellationConfs :: Map GethId FilePath
               }
  deriving (Eq, Show)

makeLenses ''ClusterEnv

type HasEnv = MonadReader ClusterEnv

data EnodeId = EnodeId Text
  deriving (Show, Eq)

instance ToJSON EnodeId where
  toJSON (EnodeId eid) = String eid

instance FromJSON EnodeId where
  parseJSON str@(String _) = EnodeId <$> parseJSON str
  parseJSON invalid        = typeMismatch "EnodeId" invalid

data AccountId = AccountId { accountId :: Text }
  deriving (Show, Eq)

data AccountKey = AccountKey { akAccountId :: AccountId
                             , akKey       :: Text
                             }
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
       , gethIp        :: Ip
       , gethUrl       :: Text
       , gethConstellationConfig :: Maybe FilePath
       }
  deriving (Show, Eq)

-- Addresses, transactions and blocks

newtype Addr = Addr { unAddr :: Text }
  deriving (Show, Eq)

newtype TxId = TxId { txId :: Text }
  deriving (Show, Eq, Ord)

newtype Block = Block Text
  deriving (Eq, Show)

newtype OutstandingTxes = OutstandingTxes { unOutstandingTxes :: Set TxId }
  deriving (Monoid)

newtype TxAddrs = TxAddrs { unTxAddrs :: Map TxId Addr }
  deriving (Monoid, Eq)

-- Raft. TODO: see whether we need this anymore.

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
  , lastBlock       :: MVar (Last Block)
  , lastRaftStatus  :: MVar (Last RaftStatus)
  , outstandingTxes :: MVar OutstandingTxes
  , txAddrs         :: MVar TxAddrs
  , allConnected    :: Async AllConnected
  , assumedRole     :: Async AssumedRole
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

-- | A contract may be visible to everyone or only to a list of public keys
data ContractPrivacy
  = Public
  | PrivateFor [Text]

-- A contract is:
-- * its privacy
-- * bytecode
-- * abi
data Contract = Contract ContractPrivacy Text Text

-- AWS support

data AwsClusterType
  = SingleRegion
  | MultiRegion
