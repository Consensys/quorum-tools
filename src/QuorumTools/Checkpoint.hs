{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module QuorumTools.Checkpoint where

import           Turtle

import           QuorumTools.Types
import           QuorumTools.Util (bytes20P, bytes32P, HexPrefix(..))

quorumSentinel :: Text
quorumSentinel = "QUORUM-CHECKPOINT"

-- Note we use @suffix@ because the content is preceded by a timestamp.
patternForCheckpoint :: Checkpoint a -> Pattern a
patternForCheckpoint cpt =
     "INFO "
  >> bracketed (count 14 dot) -- timestamp
  >> spaces
  >> text quorumSentinel
  >> spaces1
  >> "name="
  >> text (sentinelForCheckpoint cpt)
  >> mkCheckpointPattern cpt

sentinelForCheckpoint :: Checkpoint a -> Text
sentinelForCheckpoint PeerConnected      = "PEER-CONNECTED"
sentinelForCheckpoint PeerDisconnected   = "PEER-DISCONNECTED"
sentinelForCheckpoint BecameMinter       = "BECAME-MINTER"
sentinelForCheckpoint BecameVerifier     = "BECAME-VERIFIER"
sentinelForCheckpoint BlockVotingStarted = "BLOCK-VOTING-STARTED"
sentinelForCheckpoint TxCreated          = "TX-CREATED"
sentinelForCheckpoint TxAccepted         = "TX-ACCEPTED"
sentinelForCheckpoint BlockCreated       = "BLOCK-CREATED"

mkCheckpointPattern :: Checkpoint a -> Pattern a
mkCheckpointPattern PeerConnected
  = spaces >> "peer=" >> PeerJoined . GethId <$> decimal
mkCheckpointPattern PeerDisconnected
  = spaces >> "peer=" >> PeerLeft . GethId <$> decimal
mkCheckpointPattern BecameMinter       = pure ()
mkCheckpointPattern BecameVerifier     = pure ()
mkCheckpointPattern BlockVotingStarted = pure ()
mkCheckpointPattern TxCreated = do
  _ <- spaces
  transactionId <- "tx=" >> bytes32P WithPrefix
  _ <- spaces
  addr <- "to=" >> bytes20P WithPrefix
  return (TxId transactionId, Addr addr)
mkCheckpointPattern TxAccepted
  = spaces >> "tx=" >> TxId <$> bytes32P WithPrefix
mkCheckpointPattern BlockCreated
  = spaces >> "block=" >> Block <$> bytes32P WithoutPrefix

bracketed :: Pattern a -> Pattern a
bracketed pat = "[" *> pat <* "]"

quoted :: Pattern a -> Pattern a
quoted pat = "\"" *> pat <* "\""

matchCheckpoint :: Checkpoint a -> Text -> Maybe a
matchCheckpoint cpt line =
  case match (patternForCheckpoint cpt) line of
    [result] -> Just result
    _        -> Nothing
