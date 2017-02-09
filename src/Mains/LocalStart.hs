{-# LANGUAGE OverloadedStrings #-}

-- | Starts an existing cluster.
module Mains.LocalStart where

import           Control.Monad.Reader (runReaderT)
import           Turtle

import           Cluster              (mkLocalEnv, runNodesIndefinitely)
import           Cluster.Client       (loadLocalNode)
import           Cluster.Types

localStartMain :: IO ()
localStartMain = sh $ flip runReaderT (mkLocalEnv clusterSize) $ do
    geths <- traverse loadLocalNode $ clusterGids clusterSize
    runNodesIndefinitely geths

  where
    clusterSize = 3
