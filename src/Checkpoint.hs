{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Checkpoint where

import           Turtle

import           Cluster.Types
import           Cluster.Util (bytes20P, bytes32P, HexPrefix(..))

raftSentinel :: Text
raftSentinel = "RAFT-CHECKPOINT"

-- Note we use @suffix@ because the content is preceded by a timestamp.
patternForCheckpoint :: Checkpoint a -> Pattern a
patternForCheckpoint cpt = suffix $
     text raftSentinel
  >> space
  >> text (sentinelForCheckpoint cpt)
  >> space
  >> mkCheckpointPattern cpt

sentinelForCheckpoint :: Checkpoint a -> Text
sentinelForCheckpoint PeerConnected    = "PEER-CONNECTED"
sentinelForCheckpoint PeerDisconnected = "PEER-DISCONNECTED"
sentinelForCheckpoint BecameMinter     = "BECAME-MINTER"
sentinelForCheckpoint BecameVerifier   = "BECAME-VERIFIER"
sentinelForCheckpoint TxCreated        = "TX-CREATED"
sentinelForCheckpoint TxAccepted       = "TX-ACCEPTED"

mkCheckpointPattern :: Checkpoint a -> Pattern a
mkCheckpointPattern PeerConnected = PeerJoined . GethId <$> decimal
mkCheckpointPattern PeerDisconnected = PeerLeft . GethId <$> decimal
mkCheckpointPattern BecameMinter = pure ()
mkCheckpointPattern BecameVerifier = pure ()
mkCheckpointPattern TxCreated = do
  _ <- "("
  transactionId <- bytes32P WithPrefix
  _ <- ", "
  addr <- bytes20P WithPrefix
  _ <- ")"
  return (TxId transactionId, Addr addr)
mkCheckpointPattern TxAccepted = TxId <$> bytes32P WithPrefix

matchCheckpoint :: Checkpoint a -> Line -> Maybe a
matchCheckpoint cpt line =
  case match (patternForCheckpoint cpt) (lineToText line) of
    [result] -> Just result
    _        -> Nothing
