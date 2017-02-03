{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Monad.Reader (runReaderT)
import qualified Data.Map.Strict      as Map
import           Prelude              hiding (FilePath)
import           Turtle

import           Checkpoint
import           Cluster

--
-- TODO: add a separate binary (aws-transactor) that sends txes to the cluster,
--       perhaps per a poisson distribution. such a binary could wait until the
--       local node says that the cluster has a leader.
--

data Config = Config { numNodes :: Int, numSubnets :: Int, rootDir :: FilePath }

cliParser :: Parser Config
cliParser = Config <$> optInt  "nodes"   'n' "Number of nodes in the cluster"
                   <*> optInt  "subnets" 's' "Number of subnets in the cluster"
                   <*> optPath "path"    'p' "Output path"

main :: IO ()
main = do
    config <- options "AWS cluster bootstrapping script" cliParser
    let gids = gethIds config

    sh $ flip runReaderT (clusterEnv config gids) $
      wipeAndSetupNodes (rootDir config) gids

  where
    gethIds config = GethId <$> [1..(numNodes config)]

    -- Here we follow the convention used in our Terraform scripts.
    ipAddr :: Config -> GethId -> Ip
    ipAddr config (GethId gid) = Ip $ format ("10.0."%d%"."%d) subnet lastOctet
      where
        idx = gid - 1 -- Zero-indexed geth id
        subnet    = 1 + (idx `mod` numNodes config)
        lastOctet = 101 + (idx `div` numSubnets config)

    dataDir :: Config -> GethId -> DataDir
    dataDir config (GethId gid) = DataDir $
      rootDir config </> fromText (format ("geth"%d) gid)

    clusterEnv :: Config -> [GethId] -> ClusterEnv
    clusterEnv config gids = defaultClusterEnv
      { _clusterGenesisJson = rootDir config </> "genesis.json"
         --
         -- NOTE: could get this information from a .tf.json file:
         --
      , _clusterIps      = Map.fromList [(gid, ipAddr config gid)  | gid <- gids]
      , _clusterDataDirs = Map.fromList [(gid, dataDir config gid) | gid <- gids]
      }
