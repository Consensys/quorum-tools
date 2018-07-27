{-# LANGUAGE OverloadedStrings #-}

-- Regression test for #428. This is designed to partition the (node which
-- still believes it's) leader while the other node takes over and starts
-- producing blocks.
module QuorumTools.Test.Raft.Regression428 where

import           QuorumTools.Test.Outline
import           QuorumTools.Types
import           QuorumTools.Util         (timestampedMessage)

regression428TestMain :: IO ()
regression428TestMain = testNTimes 10 PrivacyDisabled Raft (NumNodes 2) $
  \iNodes -> do
    let [(node1, _), (node2, _)] = iNodes

    timestampedMessage "starting test with a pause"
    td 2

    timestampedMessage "starting spammer"
    withSpammer (fst <$> iNodes) $ do
      -- run with both nodes for a second, then partition each independently
      td 1
      timestampedMessage "partitioning"
      partition "gdata" (2 * 1000) (gethId node1)
      timestampedMessage "unpartitioning / partitioning"
      td 2
      partition "gdata" (2 * 1000) (gethId node2)
      timestampedMessage "unpartitioning / partitioning"
      td 2
      partition "gdata" (2 * 1000) (gethId node1)
      timestampedMessage "unpartitioning"
      td 2

    timestampedMessage "ending test"
