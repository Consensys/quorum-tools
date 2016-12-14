-- One-second partition
module Main where

import Cluster
import TestOutline

main :: IO ()
main = repeatTester (Repeat 100) (NumNodes 3) $ \nodes -> do
  let dropNode:stableNodes = nodes

  withSpammer stableNodes $ do
    -- run with all three nodes for a second, partition 1 for a second, run with
    -- all three for another second
    td 1
    partition 1000 (gethId dropNode)
    td 2
