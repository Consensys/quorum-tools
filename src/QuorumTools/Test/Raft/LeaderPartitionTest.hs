{-# LANGUAGE OverloadedStrings #-}

-- Partitioning leader from the cluster
module QuorumTools.Test.Raft.LeaderPartitionTest where

import           QuorumTools.Test.Outline
import           QuorumTools.Types
import           QuorumTools.Util         (timestampedMessage)

leaderPartitionTestMain :: IO ()
leaderPartitionTestMain = testNTimes 1 PrivacyDisabled Raft (NumNodes 3) $
  \iNodes -> do
    let (dropNode, _):_stableNodes = iNodes

    timestampedMessage "starting test with a pause"
    td 2

    timestampedMessage "starting spammer"
    withSpammer [dropNode] $ do
      -- run with all three nodes for a second, partition 1 for ten seconds,
      -- run with all three for a while
      td 1
      timestampedMessage "partitioning"
      partition "gdata" (10 * 1000) (gethId dropNode)
      timestampedMessage "unpartitioning"
      td 5

    timestampedMessage "ending test"
