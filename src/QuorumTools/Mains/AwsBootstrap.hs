{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Bootstraps an AWS cluster
module QuorumTools.Mains.AwsBootstrap where

import           Control.Lens         ((.~))
import           Control.Monad.Reader (runReaderT)
import           Data.Bool            (bool)
import           Data.Map.Strict      (Map)
import           Prelude              hiding (FilePath)
import           Turtle

import           QuorumTools.Aws      (dockerHostIp, internalAwsIp)
import           QuorumTools.Cluster
import           QuorumTools.Types

data AwsConfig
  = AwsConfig { numSubnets  :: Int
              , rootDir     :: FilePath
              , clusterType :: AwsClusterType
              , clusterSize :: Int
              }

cliParser :: Parser AwsConfig
cliParser = AwsConfig
  <$> optInt  "subnets"       's' "Number of subnets in the region"
  <*> optPath "path"          'p' "Output path"
  <*> fmap (bool SingleRegion MultiRegion)
           (switch  "multi-region" 'm' "Whether the cluster is multi-region")
  <*> optInt "cluster-size"   'n' "Total cluster size across all regions"

mkBootstrapEnv :: AwsConfig -> Password -> Map GethId AccountKey -> ClusterEnv
mkBootstrapEnv config password keys = mkClusterEnv mkIp mkDataDir keys
    & clusterGenesisJson    .~ dataRoot </> "genesis.json"
    & clusterPrivacySupport .~ PrivacyEnabled
    & clusterPassword       .~ password

  where
    dataRoot = rootDir config
    size     = clusterSize config
    subnets  = numSubnets config

    mkDataDir (GethId gid) = DataDir $
      dataRoot </> fromText (format ("geth"%d) gid)

    -- In the multi-region setting, since we are connecting to other nodes over
    -- the open internet, we do so through local SSH tunnels.
    mkIp = case clusterType config of
      SingleRegion -> internalAwsIp size subnets
      MultiRegion  -> const dockerHostIp

awsBootstrapMain :: IO ()
awsBootstrapMain = awsBootstrap =<< parseConfig
  where
    parseConfig = options "Bootstraps an AWS cluster" cliParser

awsBootstrap :: AwsConfig -> IO ()
awsBootstrap config = do
    keys <- generateClusterKeys gids password

    sh $ flip runReaderT (mkBootstrapEnv config password keys) $
      wipeAndSetupNodes (Just remoteDataDir) (rootDir config) gids

  where
    gids          = [1..GethId (clusterSize config)]
    remoteDataDir = DataDir "/datadir"
    password      = CleartextPassword "abcd"
