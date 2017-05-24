{-# LANGUAGE OverloadedStrings #-}

-- Test removing nodes from the cluster and adding new ones.
--
-- Cycle through nodes until we've completely replaced the originals.
module Mains.CycleTest where

import           Control.Lens
import           Data.Set             as Set
import           Turtle               (liftIO)

import           Cluster
import           Cluster.Client
import           Cluster.Control
import           Cluster.Types
import           TestOutline

cycleTestMain :: IO ()
cycleTestMain = do
  let gids = [1..6] :: [GethId]
      password = CleartextPassword "abcd"

  keys <- generateClusterKeys (length gids) password

  let cEnv = mkLocalEnv keys
           & clusterPrivacySupport .~ PrivacyDisabled
           & clusterInitialMembers .~ Set.fromList (take 3 gids)
           & clusterPassword       .~ password

  result <- run cEnv $ do
    [g1, g2, g3, g4, g5, g6] <- wipeAndSetupNodes Nothing "gdata" gids

    startInstrs <- traverse (runNode 3) [g1, g2, g3]
    endInstrs <- traverse (runNode 3) [g4, g5, g6]

    timestampedMessage "waiting for initial nodes to assume raft roles"

    awaitAll (assumedRole <$> startInstrs)

    timestampedMessage "starting test with a pause"

    td 2
    sendEmptyTx g1

    g2 `addsNode` g4

    g3 `removesNode` g1

    td 2
    sendEmptyTx g2

    g3 `addsNode` g5

    g4 `removesNode` g2

    td 2
    sendEmptyTx g3

    g4 `addsNode` g6

    g5 `removesNode` g3

    td 2
    withSpammer [g4, g5, g6] $ td 2

    td 2
    liftIO $ verify
      (lastBlock <$> endInstrs)
      (outstandingTxes <$> endInstrs)
      (nodeTerminated <$> endInstrs)

  print result
