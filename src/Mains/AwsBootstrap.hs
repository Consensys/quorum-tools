{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Mains.AwsBootstrap where

import           Control.Monad.Reader (runReaderT)
import           Prelude              hiding (FilePath)
import           Turtle

import           Cluster
import           Cluster.Types
import           Cluster.Aws          (internalAwsIp)

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
mkBootstrapEnv config = (mkClusterEnv (internalAwsIp size subnets)
                                      mkDataDir
                                      size)
    { _clusterGenesisJson = dataRoot </> "genesis.json"
    }
  where
    dataRoot = rootDir config
    size     = numNodes config
    subnets  = numSubnets config

    mkDataDir (GethId gid) = DataDir $
      dataRoot </> fromText (format ("geth"%d) gid)

awsBootstrapMain :: IO ()
awsBootstrapMain = do
    config <- options "AWS cluster bootstrapping script" cliParser

    sh $ flip runReaderT (mkBootstrapEnv config) $
      wipeAndSetupNodes (rootDir config) (clusterGids $ numNodes config)
