{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Checkpoint where

import           Turtle

import           Cluster.Types

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
  transactionId <- "0x" >> plus hexDigit
  _ <- ", "
  addr <- "0x" >> plus hexDigit
  _ <- ")"
  return (TxId transactionId, Addr addr)
mkCheckpointPattern TxAccepted = "0x" >> TxId <$> plus hexDigit

matchCheckpoint :: Checkpoint a -> Line -> Maybe a
matchCheckpoint cpt line =
  case match (patternForCheckpoint cpt) (lineToText line) of
    [result] -> Just result
    _        -> Nothing
