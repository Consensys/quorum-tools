{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Bootstraps a cluster for a single AWS region.
module Mains.AwsBootstrap where

import           Control.Monad.Reader (runReaderT)
import           Data.Bool            (bool)
import           Prelude              hiding (FilePath)
import           Turtle

import           Cluster
import           Cluster.Aws          (internalAwsIp)
import           Cluster.Types

data AwsConfig
  = AwsConfig { numNodes    :: Int
              , numSubnets  :: Int
              , rootDir     :: FilePath
              , clusterType :: AwsClusterType
              , firstGid    :: GethId
              }

cliParser :: Parser AwsConfig
cliParser = AwsConfig
  <$> optInt  "nodes"         'n' "Number of nodes in the region"
  <*> optInt  "subnets"       's' "Number of subnets in the region"
  <*> optPath "path"          'p' "Output path"
  <*> fmap (bool SingleRegion MultiRegion)
           (switch  "multi-region" 'g' "Whether the cluster is multi-region")
  <*> fmap GethId
           (optInt  "first-geth-id" 'i' "First geth ID in the region")

mkBootstrapEnv :: AwsConfig -> [GethId] -> ClusterEnv
mkBootstrapEnv config gids = (mkClusterEnv mkIp mkDataDir gids)
    { _clusterGenesisJson = dataRoot </> "genesis.json"
    }
  where
    dataRoot = rootDir config
    size     = numNodes config
    subnets  = numSubnets config

    mkDataDir (GethId gid) = DataDir $
      dataRoot </> fromText (format ("geth"%d) gid)

    -- In the multi-region setting, since we are connecting to other nodes over
    -- the open internet, we do so through local SSH tunnels.
    mkIp = case clusterType config of
      SingleRegion -> internalAwsIp size subnets
      MultiRegion  -> const $ Ip "127.0.0.1"

awsBootstrapMain :: IO ()
awsBootstrapMain = do
  config <- options "Bootstraps a cluster for a single AWS region" cliParser
  let gids = take (numNodes config) (enumFrom $ firstGid config)

  sh $ flip runReaderT (mkBootstrapEnv config gids) $
    wipeAndSetupNodes (rootDir config) gids
