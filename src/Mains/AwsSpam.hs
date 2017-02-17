{-# LANGUAGE OverloadedStrings #-}

module Mains.AwsSpam where

import           Control.Monad.Reader (runReaderT)
import           Control.RateLimit    (RateLimit)
import           Data.Time.Units      (Millisecond)
import           Turtle

import           Cluster
import           Cluster.Types
import           Cluster.Aws          (internalAwsIp)
import           Cluster.Client       (loadLocalNode, perSecond, spamGeth)

--
-- NOTE: this could wait until the local node says that the cluster has a leader
--
--       if we wanted to do more realistic workload simulation, we could sample
--         tx sends from a poisson distribution.
--

cliParser :: Parser (RateLimit Millisecond)
cliParser = perSecond <$>
  optInteger "rps"  'r' "The number of requests per second"

cEnv :: GethId -> ClusterEnv
cEnv gid = mkClusterEnv (internalAwsIp maxClusterSize numSubnets)
                        mkDataDir
                        [gid]
  where
    -- We don't need the exact cluster size here; just something higher than the
    -- geth ID we want to spam:
    maxClusterSize = 10
    numSubnets     = 3
    mkDataDir = const $ DataDir "/datadir"

readGidFromHomedir :: IO GethId
readGidFromHomedir = GethId . read <$> readFile "/home/ubuntu/node-id"

awsSpamMain :: IO ()
awsSpamMain = do
  rateLimit <- options "Spams the local node with public transactions" cliParser
  gid <- readGidFromHomedir
  geth <- runReaderT (loadLocalNode gid) (cEnv gid)
  spamGeth geth rateLimit
