{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module QuorumTools.Checkpoint where

import           Data.Text (pack)
import           Turtle

import           QuorumTools.Types
import           QuorumTools.Util (bytes20P, bytes32P, HexPrefix(..))

quorumSentinel :: Text
quorumSentinel = "QUORUM-CHECKPOINT"

-- Note we use @suffix@ because the content is preceded by a timestamp.
patternForCheckpoint :: Checkpoint a -> Pattern a
patternForCheckpoint cpt = suffix $
     text quorumSentinel
  >> space
  >> text (sentinelForCheckpoint cpt)
  >> space
  >> bracketed (mkCheckpointPattern cpt)

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
mkCheckpointPattern PeerConnected      = PeerJoined . GethId <$> decimal
mkCheckpointPattern PeerDisconnected   = PeerLeft   . GethId <$> decimal
mkCheckpointPattern BecameMinter       = pure ()
mkCheckpointPattern BecameVerifier     = pure ()
mkCheckpointPattern BlockVotingStarted = pure ()
mkCheckpointPattern TxCreated          = do
  transactionId <- bytes32P WithPrefix
  _ <- " "
  addr <- bytes20P WithPrefix
  return (TxId transactionId, Addr addr)
mkCheckpointPattern TxAccepted         = TxId <$> bytes32P WithPrefix
mkCheckpointPattern BlockCreated       = Block . pack <$> count 64 hexDigit

bracketed :: Pattern a -> Pattern a
bracketed pat = "[" *> pat <* "]"

matchCheckpoint :: Checkpoint a -> Text -> Maybe a
matchCheckpoint cpt line =
  case match (patternForCheckpoint cpt) line of
    [result] -> Just result
    _        -> Nothing
