{-# LANGUAGE OverloadedStrings #-}

-- Partitioning leader from the cluster
module QuorumTools.Test.Raft.LeaderPartitionTest where

import           QuorumTools.Test.Outline
import           QuorumTools.Types

import Turtle (liftIO)

leaderPartitionTestMain :: IO ()
leaderPartitionTestMain = testNTimes 12 PrivacyDisabled (NumNodes 3) $
  \iNodes -> do
    let (dropNode, _):_stableNodes = iNodes

    timestampedMessage "starting test with a pause"
    td 2

    timestampedMessage "starting spammer"
    withSpammer [dropNode] $ do
      -- run with all three nodes for a second, partition 1 for ten seconds,
      -- run with all three for a while
      td 5
      timestampedMessage "partitioning"
      partition "gdata" (10 * 1000) (gethId dropNode)
      timestampedMessage "unpartitioning"
      td 5

    timestampedMessage "ending test and waiting"
    -- _ <- liftIO getLine
    -- pure ()
