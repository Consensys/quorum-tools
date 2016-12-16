-- One-second partition
module Main where

import Cluster
import TestOutline

exitP :: TestPredicate
exitP (TestNum 99) _                        = DoTerminateSuccess
exitP _            (Falsified NoBlockFound) = DoTerminateFailure
exitP _            _                        = DontTerminate

main :: IO ()
main = tester exitP (NumNodes 3) $ \nodes -> do
  let dropNode:stableNodes = nodes

  withSpammer stableNodes $ do
    -- run with all three nodes for a second, partition 1 for a second, run with
    -- all three for another second
    td 1
    partition 1000 (gethId dropNode)
    td 2
