-- Late-joining node
module Main where

import Turtle

import Cluster
import ClusterAsync
import TestOutline

main :: IO ()
main = testOnce (NumNodes 3) $ \nodes -> do
  -- have the first one join three seconds after the others
  let late:early = nodes

  -- XXX this is out of date
  startRaftAcross early

  -- while sending transactions to the started nodes, wait three seconds before
  -- starting the third
  withSpammer early $ do
    void $ clusterAsync $ do
      td 3
      startRaft late

    -- run it for ten seconds
    td 10
