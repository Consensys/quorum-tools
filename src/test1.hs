{-# LANGUAGE OverloadedStrings #-}
-- One-second partition
module Main where

import Cluster
import TestOutline

exitP :: TestPredicate
exitP (TestNum 99) _             = DoTerminateSuccess
exitP _            (Falsified _) = DoTerminateFailure
exitP _            _             = DontTerminate

main :: IO ()
main = tester exitP (NumNodes 3) $ \nodes -> do
  let dropNode:stableNodes = nodes

  timestampedMessage "starting test with a pause"
  td 2

  timestampedMessage "starting spammer"
  withSpammer stableNodes $ do
    -- run with all three nodes for a second, partition 1 for a second, run with
    -- all three for another second
    td 1
    timestampedMessage "partitioning"
    partition 1000 (gethId dropNode)
    timestampedMessage "unpartitioning"
    td 5

  timestampedMessage "ending test"
