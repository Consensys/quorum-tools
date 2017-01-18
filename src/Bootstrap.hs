{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader (runReaderT)
import qualified Data.Map.Strict      as Map
import           Prelude              hiding (FilePath)
import           Turtle

import           Cluster

main :: IO ()
main = sh $ flip runReaderT clusterEnv $
    void $ wipeAndSetupNodes clusterDataRoot [1, 2, 3]

  where
    clusterDataRoot :: FilePath
    clusterDataRoot = "cluster-data"

    clusterEnv = defaultClusterEnv
      { _clusterGenesisJson = clusterDataRoot </> "genesis.json"
         --
         -- TODO: build this from cluster.tf.json:
         --
      , _clusterIps         = Map.fromList
          [ (1, Ip "10.0.1.104")
          , (2, Ip "10.0.2.222")
          , (3, Ip "10.0.3.49") ]
      , _clusterDataDirs    = Map.fromList
          [ (1, DataDir $ clusterDataRoot </> "geth1")
          , (2, DataDir $ clusterDataRoot </> "geth2")
          , (3, DataDir $ clusterDataRoot </> "geth3") ]
      }
