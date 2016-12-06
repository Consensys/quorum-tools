-- One-second partition
module Main where

import Cluster
import TestOutline

main :: IO ()
main = tester 3 $ \nodes -> do
  startAndUnlock nodes
  let dropNode:stableNodes = nodes

  withSpammer stableNodes $ do
    -- run with all three nodes for a second, partition 1 for a second, run with
    -- all three for another second
    td 1
    partition 1000 (gethId dropNode)
    td 1
