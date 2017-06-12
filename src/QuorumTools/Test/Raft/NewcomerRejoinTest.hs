{-# LANGUAGE OverloadedStrings #-}

-- Test removing a newcomer node from the cluster and re-adding it with a new
-- raft ID
module QuorumTools.Test.Raft.NewcomerRejoinTest where

import           Control.Concurrent.Async (wait)
import           Control.Lens
import           Control.Monad            (void)
import qualified Data.Set as Set
import           Turtle                   (liftIO)

import           QuorumTools.Cluster
import           QuorumTools.Control
import           QuorumTools.Test.Outline
import           QuorumTools.Types

newcomerRejoinTestMain :: IO ()
newcomerRejoinTestMain = do
  let gids = [1..3] :: [GethId]
      clusterSize = length gids
      password = CleartextPassword "abcd"

  keys <- generateClusterKeys gids password
  let cEnv = mkLocalEnv keys
           & clusterPrivacySupport .~ PrivacyDisabled
           & clusterInitialMembers .~ Set.fromList (take 2 gids)
           & clusterPassword       .~ password

  result <- runTestM cEnv $ do
    [g1, g2, g3] <- wipeAndSetupNodes Nothing "gdata" gids

    [g1i, g2i, g3i] <- traverse (runNode clusterSize) [g1, g2, g3]

    awaitAll (assumedRole <$> [g1i, g2i])

    timestampedMessage "starting test with a pause"
    td 2

    withSpammer [g1, g2] $ td 1

    g2 `addsNode` g3
    td 2

    withSpammer [g1, g2, g3] $ td 1
    td 1

    -- remove g3, pause, add it back as g4, with the same blockchain data.
    g2 `removesNode` g3

    withSpammer [g1, g2] $ td 1

    _ <- liftIO $ killNode g3i

    td 1

    let g4 = g3 { gethId = 4
                , gethJoinMode = JoinExisting
                }

    g2 `addsNode` g4

    g4i <- runNode clusterSize g4

    void $ liftIO $ wait $ assumedRole g4i

    withSpammer [g1, g2, g4] $ td 1
    td 1

    let instruments' = [g1i, g2i, g4i]

    verify (lastBlock       <$> instruments')
           (outstandingTxes <$> instruments')
           (nodeTerminated  <$> instruments')

  print result
