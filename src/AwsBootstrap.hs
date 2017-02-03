{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Monad.Reader (runReaderT)
import           Prelude              hiding (FilePath)
import           Turtle

import           Checkpoint
import           Cluster

--
-- TODO: add a separate binary (aws-spam) that sends txes to the cluster.
--       it could wait until the local node says that the cluster has a leader.
--
--       if we wanted to do more realistic workload simulation, we could sample
--         tx sends from a poisson distribution.
--

data AwsConfig
  = AwsConfig { numNodes   :: Int
              , numSubnets :: Int
              , rootDir    :: FilePath }

cliParser :: Parser AwsConfig
cliParser = AwsConfig
  <$> optInt  "nodes"   'n' "Number of nodes in the cluster"
  <*> optInt  "subnets" 's' "Number of subnets in the cluster"
  <*> optPath "path"    'p' "Output path"

mkAwsEnv :: AwsConfig -> ClusterEnv
mkAwsEnv config = (mkClusterEnv mkIp mkDataDir size)
    { _clusterGenesisJson = dataRoot </> "genesis.json"
    }
  where
    dataRoot = rootDir config
    size     = numNodes config
    subnets  = numSubnets config

    mkIp (GethId gid) = Ip $ format ("10.0."%d%"."%d) subnet lastOctet
      where
        idx = gid - 1 -- Zero-indexed geth id
        subnet    = 1 + (idx `mod` size)
        lastOctet = 101 + (idx `div` subnets)

    mkDataDir (GethId gid) = DataDir $
      dataRoot </> fromText (format ("geth"%d) gid)

main :: IO ()
main = do
    config <- options "AWS cluster bootstrapping script" cliParser

    sh $ flip runReaderT (mkAwsEnv config) $
      wipeAndSetupNodes (rootDir config) (clusterGids $ numNodes config)
