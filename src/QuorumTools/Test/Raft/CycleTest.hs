{-# LANGUAGE OverloadedStrings #-}

-- Test removing nodes from the cluster and adding new ones.
--
-- Cycle through nodes until we've completely replaced the originals.
module QuorumTools.Test.Raft.CycleTest where

import           Control.Lens
import qualified Data.Set                 as Set

import           QuorumTools.Client
import           QuorumTools.Cluster
import           QuorumTools.Control
import           QuorumTools.Test.Outline
import           QuorumTools.Types
import           QuorumTools.Util         (timestampedMessage)

cycleTestMain :: IO ()
cycleTestMain = do
  let gids = [1..6] :: [GethId]
      password = CleartextPassword "abcd"

  keys <- generateClusterKeys gids password

  let cEnv = mkLocalEnv keys Raft
           & clusterPrivacySupport .~ PrivacyDisabled
           & clusterInitialMembers .~ Set.fromList (take 3 gids)
           & clusterPassword       .~ password

  result <- runTestM cEnv $ do
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
    verify (lastBlock <$> endInstrs)
           (outstandingTxes <$> endInstrs)
           (nodeTerminated <$> endInstrs)

  print result
