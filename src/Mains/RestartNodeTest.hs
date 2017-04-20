{-# LANGUAGE OverloadedStrings #-}

-- Run a cluster, stop and restart one node
module Mains.RestartNodeTest where

import Control.Concurrent.Async (Concurrently(..))
import Data.Monoid              (Last)
import Turtle

import Cluster
import Cluster.Control
import Cluster.Types
import TestOutline

numNodes :: Int
numNodes = 3

cEnv :: ClusterEnv
cEnv = mkLocalEnv numNodes

waitForElection :: MonadIO m => NodeInstrumentation -> m ()
waitForElection instruments = do
  timestampedMessage "awaiting a successful raft election"
  _ <- wait (assumedRole instruments)
  timestampedMessage "initial election succeeded"

type NodeInfo = (Last Block, OutstandingTxes)

refine :: Either FailureReason a -> IO a
refine (Left failure) = print failure >> exit failedTestCode
refine (Right a) = pure a

readNodeInfo :: Either FailureReason NodeInstrumentation -> IO NodeInfo
readNodeInfo = refine >=> \instruments -> (,)
  <$> observe (lastBlock instruments)
  <*> observe (outstandingTxes instruments)

--   seconds  |   spammer    |    node 1    |    nodes 2 / 3
-- ---------------------------------------------------------
--      1     |      ^       |       ^      |         ^
--      2     |      |       |       |      |         |
--      3     |      |       |       |      |         |
--      4     |      v       |       |      |         |
--      5     |              |       v      |         |
--      6     |              |              |         |
--      7     |              |       ^      |         |
--      8     |      ^       |       |      |         |
--      9     |      |       |       |      |         |
--      10    |      |       |       |      |         |
--      11    |      v       |       |      |         |
--      12    |              |       |      |         |
--      13    |              |       |      |         |
--      14    |              |       v      |         v

node1Plan :: Geth -> IO NodeInfo
node1Plan geth = do
  _ <- run cEnv $ do
    instruments <- runNode numNodes JoinNewCluster geth
    waitForElection instruments
    td 5

  td 1

  readNodeInfo <=< run cEnv $ do
    instruments <- runNode numNodes JoinNewCluster geth
    td 8
    pure instruments

nodes23Plan :: Geth -> IO NodeInfo
nodes23Plan geth =
  readNodeInfo <=< run cEnv $ do
    instruments <- runNode numNodes JoinNewCluster geth
    waitForElection instruments
    withSpammer [geth] $ td 4
    td 3
    withSpammer [geth] $ td 4
    td 3
    pure instruments


restartNodeTestMain :: IO ()
restartNodeTestMain = do
  let gethIds = [1..GethId numNodes]

  nodes <- run cEnv $ do
    nodes <- wipeAndSetupNodes Nothing "gdata" gethIds
    pure nodes

  g1:g2g3 <- refine nodes

  instruments <- runConcurrently $ sequenceA $ map Concurrently $
    node1Plan g1 : map nodes23Plan g2g3

  let (lastBlocks, outstandingTxes_) = unzip instruments
      result =
        verifyLastBlocks lastBlocks <> verifyOutstandingTxes outstandingTxes_

  case result of
    Falsified reason -> do
      putStrLn "falsified"
      print reason
      exit failedTestCode
    _  -> putStrLn "all successful!"
