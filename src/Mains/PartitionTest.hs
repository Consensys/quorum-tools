{-# LANGUAGE OverloadedStrings #-}

-- One-second partition
module Mains.PartitionTest where

import Cluster.Types
import TestOutline

partitionTestMain :: IO ()
partitionTestMain = testNTimes 100 PrivacyDisabled (NumNodes 3) $ \iNodes -> do
  let dropNode:stableNodes = fst <$> iNodes

  timestampedMessage "starting test with a pause"
  td 2

  timestampedMessage "starting spammer"
  withSpammer stableNodes $ do
    -- run with all three nodes for a second, partition 1 for a second, run with
    -- all three for another second
    td 1
    timestampedMessage "partitioning"
    partition "gdata" 1000 (gethId dropNode)
    timestampedMessage "unpartitioning"
    td 5

  timestampedMessage "ending test"
