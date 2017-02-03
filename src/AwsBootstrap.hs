{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Monad.Reader (runReaderT)
import           Prelude              hiding (FilePath)
import           Turtle

import           Checkpoint
import           Cluster
import           Cluster.Aws          (awsIp)

data AwsConfig
  = AwsConfig { numNodes   :: Int
              , numSubnets :: Int
              , rootDir    :: FilePath
              }

cliParser :: Parser AwsConfig
cliParser = AwsConfig
  <$> optInt  "nodes"   'n' "Number of nodes in the cluster"
  <*> optInt  "subnets" 's' "Number of subnets in the cluster"
  <*> optPath "path"    'p' "Output path"

mkBootstrapEnv :: AwsConfig -> ClusterEnv
mkBootstrapEnv config = (mkClusterEnv (awsIp size subnets) mkDataDir size)
    { _clusterGenesisJson = dataRoot </> "genesis.json"
    }
  where
    dataRoot = rootDir config
    size     = numNodes config
    subnets  = numSubnets config

    mkDataDir (GethId gid) = DataDir $
      dataRoot </> fromText (format ("geth"%d) gid)

main :: IO ()
main = do
    config <- options "AWS cluster bootstrapping script" cliParser

    sh $ flip runReaderT (mkBootstrapEnv config) $
      wipeAndSetupNodes (rootDir config) (clusterGids $ numNodes config)
