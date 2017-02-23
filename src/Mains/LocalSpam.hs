{-# LANGUAGE OverloadedStrings #-}

module Mains.LocalSpam where

import           Control.Monad.Reader (runReaderT)
import           Control.RateLimit    (RateLimit)
import           Data.Time.Units      (Millisecond)
import           Turtle

import           Cluster              (mkLocalEnv)
import           Cluster.Types
import           Cluster.Client       (loadLocalNode, spamGeth,
                                       perSecond)
import           Cluster.SpamArgs

cliParser :: Parser (GethId, RateLimit Millisecond, Maybe Text, Maybe Text)
cliParser = (,,,)
  <$> gethIdP
  <*> rateLimitP
  <*> optional contractP
  <*> optional privateForP
  where
    gethIdP = GethId <$>
      optInt     "geth" 'g' "The Geth ID of the local node to spam"
    rateLimitP = perSecond <$>
      optInteger "rps"  'r' "The number of requests per second"

localSpamMain :: IO ()
localSpamMain = do
    (gid, rateLimit, contractM, privateForM) <-
      options "Local geth spammer" cliParser
    let benchTx = processContractArgs contractM privateForM
    geth <- runReaderT (loadLocalNode gid) (mkLocalEnv maxClusterSize)
    spamGeth benchTx geth rateLimit

  where
    -- We don't need the exact cluster size here; just something higher than the
    -- geth ID we want to spam.
    maxClusterSize = 10
