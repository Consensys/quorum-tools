{-# LANGUAGE OverloadedStrings #-}

-- Run a cluster, stop and restart one node
module Mains.RestartNodeTest where

import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.MVar  (readMVar)
import Data.Monoid              (Last)
import Turtle

import Cluster
import Cluster.Types
import TestOutline

numNodes :: Int
numNodes = 3

waitForElection :: MonadIO m => NodeInstrumentation -> m ()
waitForElection instruments = do
  timestampedMessage "awaiting a successful raft election"
  _ <- wait (assumedRole instruments)
  timestampedMessage "initial election succeeded"

type NodeInfo = (Last Block, OutstandingTxes)

readNodeInfo :: NodeInstrumentation -> IO NodeInfo
readNodeInfo instruments = (,)
  <$> readMVar (lastBlock instruments)
  <*> readMVar (outstandingTxes instruments)

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
  run numNodes $ do
    instruments <- runNode numNodes geth
    waitForElection instruments
    td 5

  td 1

  readNodeInfo <=< run numNodes $ do
    instruments <- runNode numNodes geth
    td 8
    pure instruments

nodes23Plan :: Geth -> IO NodeInfo
nodes23Plan geth =
  readNodeInfo <=< run numNodes $ do
    instruments <- runNode numNodes geth
    waitForElection instruments
    withSpammer [geth] $ td 4
    td 3
    withSpammer [geth] $ td 4
    td 3
    pure instruments


restartNodeTestMain :: IO ()
restartNodeTestMain = do
  let gethIds = [1..GethId numNodes]

  nodes <- run numNodes $ do
    nodes <- wipeAndSetupNodes Nothing "gdata" gethIds
    pure nodes

  let g1:g2g3 = nodes

  instruments <- runConcurrently $ sequenceA $ map Concurrently $
    node1Plan g1 : map nodes23Plan g2g3

  let (lastBlocks, outstandingTxes_) = unzip instruments
      result =
        verifyLastBlocks lastBlocks <> verifyOutstandingTxes outstandingTxes_

  case result of
    Falsified reason -> do
      putStrLn $ "falsified"
      print reason
      exit failedTestCode
    _           -> putStrLn "all successful!"
