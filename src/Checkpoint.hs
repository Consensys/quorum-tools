{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Checkpoint where

import           Turtle

raftSentinel :: Text
raftSentinel = "RAFT-CHECKPOINT"

newtype GethId = GethId { gId :: Int }
  deriving (Show, Eq, Num, Ord, Enum)

newtype TxId = TxId { txId :: Text }
  deriving (Show, Eq, Ord)

newtype Addr = Addr { unAddr :: Text }
  deriving Eq

newtype PeerJoined = PeerJoined GethId deriving Show
newtype PeerLeft = PeerLeft GethId deriving Show

-- | Some checkpoint in the execution of the program.
data Checkpoint result where
  PeerConnected :: Checkpoint PeerJoined
  PeerDisconnected :: Checkpoint PeerLeft

  BecameMinter :: Checkpoint ()
  BecameVerifier :: Checkpoint ()

  TxCreated :: Checkpoint (TxId, Addr)
  TxAccepted :: Checkpoint TxId

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
  txId <- "0x" >> plus hexDigit
  _ <- space
  addr <- "0x" >> plus hexDigit
  _ <- ")"
  return (TxId txId, Addr addr)
mkCheckpointPattern TxAccepted = "0x" >> TxId <$> plus hexDigit

matchCheckpoint :: Checkpoint a -> Line -> Maybe a
matchCheckpoint cpt line =
  case match (patternForCheckpoint cpt) (lineToText line) of
    [result] -> Just result
    _ -> Nothing
