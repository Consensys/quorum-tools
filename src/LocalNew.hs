{-# LANGUAGE OverloadedStrings #-}

-- | Creates and starts a new cluster, destroying old datadirs in the process.
module Main where

import           Control.Monad.Reader (runReaderT)
import           Turtle

import           Cluster              (clusterGids, mkLocalEnv,
                                       runNodesIndefinitely, wipeAndSetupNodes)

main :: IO ()
main = sh $ flip runReaderT (mkLocalEnv clusterSize) $ do
    geths <- wipeAndSetupNodes "gdata" (clusterGids clusterSize)
    runNodesIndefinitely geths

  where
    clusterSize = 3
