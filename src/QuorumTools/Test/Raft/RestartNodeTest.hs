{-# LANGUAGE OverloadedStrings #-}

-- Run a cluster, stop and restart one node
module QuorumTools.Test.Raft.RestartNodeTest where

import           Control.Concurrent.Async (Concurrently (..))
import           Control.Lens             ((.~))
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              (Last, getLast)
import           Turtle

import           QuorumTools.Cluster
import           QuorumTools.Control
import           QuorumTools.Test.Outline
import           QuorumTools.Types

numNodes :: Int
numNodes = 3

waitForElection :: MonadIO m => NodeInstrumentation -> m ()
waitForElection instruments = do
  timestampedMessage "awaiting a successful raft election"
  _ <- wait (assumedRole instruments)
  timestampedMessage "initial election succeeded"

type NodeInfo = (Last Block, OutstandingTxes)

refine :: Either FailureReason a -> IO a
refine (Left failure) = print failure >> exit failedTestCode
refine (Right a)      = pure a

readNodeInfo :: Either FailureReason NodeInstrumentation -> IO NodeInfo
readNodeInfo = refine >=> \instruments -> (,)
  <$> observe (lastBlock instruments)
  <*> fmap (fromMaybe mempty . getLast) (observe (outstandingTxes instruments))

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

node1Plan :: ClusterEnv -> Geth -> IO NodeInfo
node1Plan cEnv geth = do
  _ <- runTestM cEnv $ do
    instruments <- runNode numNodes geth
    waitForElection instruments
    td 5

  td 1

  readNodeInfo <=< runTestM cEnv $ do
    instruments <- runNode numNodes geth
    td 8
    pure instruments

nodes23Plan :: ClusterEnv -> Geth -> IO NodeInfo
nodes23Plan cEnv geth =
  readNodeInfo <=< runTestM cEnv $ do
    instruments <- runNode numNodes geth
    waitForElection instruments
    withSpammer [geth] $ td 4
    td 3
    withSpammer [geth] $ td 4
    td 3
    pure instruments


restartNodeTestMain :: IO ()
restartNodeTestMain = do
  let gids = [1..GethId numNodes]
      password = CleartextPassword "abcd"

  keys <- generateClusterKeys gids password

  let cEnv = mkLocalEnv keys
           & clusterPassword .~ password

  nodes <- runTestM cEnv $ wipeAndSetupNodes Nothing "gdata" gids

  g1:g2g3 <- refine nodes

  instruments <- runConcurrently $ sequenceA $ map Concurrently $
    node1Plan cEnv g1 : map (nodes23Plan cEnv) g2g3

  let (lastBlocks, outstandingTxes_) = unzip instruments
      result =
        verifyLastBlocks lastBlocks <> verifyOutstandingTxes outstandingTxes_

  case result of
    Falsified reason -> do
      putStrLn "falsified"
      print reason
      exit failedTestCode
    _  -> putStrLn "all successful!"
