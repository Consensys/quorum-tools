{-# LANGUAGE OverloadedStrings #-}

-- | Starts an existing cluster.
module Main where

import           Control.Monad.Reader (runReaderT)
import           Turtle

import           Cluster              (clusterGids, mkLocalEnv,
                                       runNodesIndefinitely)
import           Cluster.Client       (loadLocalNode)

main :: IO ()
main = sh $ flip runReaderT (mkLocalEnv clusterSize) $ do
    geths <- traverse loadLocalNode $ clusterGids clusterSize
    runNodesIndefinitely geths

  where
    clusterSize = 3
