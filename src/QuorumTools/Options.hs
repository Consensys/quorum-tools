{-# LANGUAGE OverloadedStrings #-}

module QuorumTools.Options where

import           Turtle                    hiding (view)

import           QuorumTools.Types

consensusParser :: Parser Consensus
consensusParser = opt parse "consensus" 'c' msg <|> pure Raft
  where
    msg = "The consensus mechanism. One of [raft clique pow]. Default: raft"

    parse :: Text -> Maybe Consensus
    parse "raft"   = Just Raft
    parse "clique" = Just Clique
    parse "pow"    = Just ProofOfWork
    parse _        = Nothing
