{-# LANGUAGE OverloadedStrings #-}
-- Leader partition
module Main where

import Cluster
import TestOutline

exitP :: TestPredicate
exitP (TestNum 9) _             = DoTerminateSuccess
exitP _           (Falsified _) = DoTerminateFailure
exitP _           _             = DontTerminate

main :: IO ()
main = tester exitP (NumNodes 3) $ \nodes -> do
  let dropNode:_stableNodes = nodes

  timestampedMessage "starting test with a pause"
  td 2

  timestampedMessage "starting spammer"
  withSpammer [dropNode] $ do
    -- run with all three nodes for a second, partition 1 for ten seconds, run with
    -- all three for a while
    td 1
    timestampedMessage "partitioning"
    partition "gdata" (10 * 1000) (gethId dropNode)
    timestampedMessage "unpartitioning"
    td 5

  timestampedMessage "ending test"
