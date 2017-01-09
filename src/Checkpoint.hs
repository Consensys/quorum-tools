{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Checkpoint where

import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Turtle

raftSentinel :: Text
raftSentinel = "RAFT-CHECKPOINT"

sentinelForCheckpoint :: Checkpoint a -> Text
sentinelForCheckpoint PeerConnected    = "PEER-CONNECTED"
sentinelForCheckpoint PeerDisconnected = "PEER-DISCONNECTED"

newtype GethId = GethId { gId :: Int }
  deriving (Show, Eq, Num, Ord, Enum)

-- | Signifies a peer join or exit.
data ConnectionDelta = ConnectionDelta ConnectionActivation GethId deriving Show
data ConnectionActivation = BecameActive | BecameInactive deriving Show

-- | Add or remove a peer from the set of known peers for a node.
applyDelta :: Maybe ConnectionDelta -> Set GethId -> Set GethId
applyDelta Nothing set = set
applyDelta (Just (ConnectionDelta dir gid)) set = case dir of
  BecameActive -> Set.insert gid set
  BecameInactive -> Set.delete gid set

-- | Some checkpoint in the execution of the program.
data Checkpoint result where
  PeerConnected :: Checkpoint ConnectionDelta
  PeerDisconnected :: Checkpoint ConnectionDelta

-- Note we use @suffix@ because the content is preceded by a timestamp.
patternForCheckpoint :: Checkpoint a -> Pattern a
patternForCheckpoint cpt = suffix $
     text raftSentinel
  >> space
  >> text (sentinelForCheckpoint cpt)
  >> space
  >> mkCheckpointPattern cpt

mkCheckpointPattern :: Checkpoint a -> Pattern a
mkCheckpointPattern PeerConnected =
  ConnectionDelta BecameActive . GethId <$> decimal
mkCheckpointPattern PeerDisconnected =
  ConnectionDelta BecameInactive . GethId <$> decimal

