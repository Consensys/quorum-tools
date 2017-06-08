{-# LANGUAGE OverloadedStrings #-}

-- Test removing a node from the cluster and adding it back with a new raft ID
module QuorumTools.Test.Raft.LeaveJoinTest where

import           Control.Concurrent.Async (wait)
import           Control.Lens
import           Control.Monad            (void)
import           Data.Monoid              ((<>))
import           Turtle                   (liftIO)

import           QuorumTools.Cluster
import           QuorumTools.Control
import           QuorumTools.Test.Outline
import           QuorumTools.Types

leaveJoinTestMain :: IO ()
leaveJoinTestMain = do
  let gids = [1..3] :: [GethId]
      clusterSize = length gids
      password = CleartextPassword "abcd"

  keys <- generateClusterKeys gids password
  let cEnv = mkLocalEnv keys
           & clusterPrivacySupport .~ PrivacyDisabled
           & clusterPassword       .~ password

  result <- runTestM cEnv $ do
    [g1, g2, g3] <- wipeAndSetupNodes Nothing "gdata" gids

    instruments <- traverse (runNode clusterSize) [g1, g2, g3]

    awaitAll (assumedRole <$> instruments)

    timestampedMessage "starting test with a pause"
    td 2

    withSpammer [g1, g2, g3] $ td 1

    -- remove g1, pause, add it back as g4, with the same blockchain data.
    g2 `removesNode` g1

    withSpammer [g2, g3] $ td 1

    let g4 = g1 { gethId = 4
                , gethJoinMode = JoinExisting
                }

    g3 `addsNode` g4

    g4i <- runNode clusterSize g4

    void $ liftIO $ wait $ assumedRole g4i

    withSpammer [g2, g3, g4] $ td 1
    td 1

    let allInstruments     = instruments <> [g4i]
        runningInstruments = drop 1 allInstruments

    verify (lastBlock       <$> runningInstruments)
           (outstandingTxes <$> allInstruments)
           (nodeTerminated  <$> runningInstruments)

  print result
