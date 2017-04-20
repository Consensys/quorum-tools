{-# LANGUAGE OverloadedStrings #-}

-- Test removing nodes from the cluster and adding new ones.
--
-- Cycle through nodes until we've completely replaced the originals.
module Mains.CycleTest where

import           Control.Lens
import Turtle.Shell

import Cluster
import Cluster.Client
import Cluster.Control
import Cluster.Types
import TestOutline

cycleTestMain :: IO ()
cycleTestMain = do
  let gethNums = [1..6]
      cEnv = mkLocalEnv 6
           & clusterPrivacySupport .~ PrivacyDisabled

  result <- run cEnv $ do
    [g1, g2, g3, g4, g5, g6] <- wipeAndSetupNodes Nothing "gdata" gethNums

    startInstrs <- traverse (runNode 6 JoinNewCluster) [g1, g2, g3]
    endInstrs <- traverse (uncurry (runNode 6))
      [ (JoinExisting 4, g4)
      , (JoinExisting 5, g5)
      , (JoinExisting 6, g6)
      ]

    awaitAll (assumedRole <$> startInstrs)

    timestampedMessage "starting test with a pause"
    td 2

    sendEmptyTx g1

    -- remove g1, add g4
    membershipChange g2 (RemoveNode g1)
    membershipChange g3 (AddNode g4)

    sendEmptyTx g2

    -- TODO: maybe wait for checkpoints before proceeding?
    membershipChange g3 (RemoveNode g2)
    membershipChange g4 (AddNode g5)

    sendEmptyTx g3

    membershipChange g4 (RemoveNode g3)
    membershipChange g5 (AddNode g6)

    -- send txes for few seconds
    withSpammer [g3, g4, g5] $ td 2

    liftIO $ verify
      (lastBlock <$> endInstrs)
      (outstandingTxes <$> endInstrs)
      (nodeTerminated <$> endInstrs)

  print result
