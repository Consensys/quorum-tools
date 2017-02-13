{-# LANGUAGE OverloadedStrings #-}

module Mains.LocalSpam where

import           Control.Monad.Reader (runReaderT)
import           Control.RateLimit    (RateLimit)
import qualified Data.Text            as T
import           Data.Time.Units      (Millisecond)
import           Turtle

import           Cluster              (mkLocalEnv)
import           Cluster.Types
import           Cluster.Client       (BenchType(..), loadLocalNode, spamGeth,
                                       perSecond)
import           Cluster.Util         (Bytes20, bytes20P)
import           SharedPartitioning   (matchOnce)

hexLike :: Pattern a -> Pattern a
hexLike = (optional "0x" >>)

-- TODO: restrict more than chars1 -- figure out what characters are allowed in
-- a signature
contractPattern :: Pattern (Bytes20, UnencodedMethod)
contractPattern = hexLike $ (,) <$> bytes20P <*> fmap UnencodedMethod chars1

addrP :: Pattern Addr
addrP = hexLike $ Addr . T.pack <$> count 40 hexDigit

privateForPattern :: Pattern [Addr]
privateForPattern = addrP `sepBy` ","

processContractArgs :: Maybe Text -> Maybe Text -> BenchType
processContractArgs contractT privateForT = maybe BenchEmptyTx BenchTx $ do
  rawContract <- contractT
  rawPrivateFor <- privateForT

  let privacy = case matchOnce privateForPattern rawPrivateFor of
        Nothing -> Public
        Just addrs -> PrivateFor addrs

  (addr, method) <- matchOnce contractPattern rawContract
  pure $ Tx (Just addr) method privacy SendTransactionAsync

cliParser :: Parser (GethId, RateLimit Millisecond, Maybe Text, Maybe Text)
cliParser = (,,,) <$> gethIdP <*> rateLimitP <*> optional contractP <*> optional privateForP
  where
    gethIdP = GethId <$>
      optInt     "geth" 'g' "The Geth ID of the local node to spam"
    rateLimitP = perSecond <$>
      optInteger "rps"  'r' "The number of requests per second"
    contractP = optText "contract" 'c'
      "Contract address and method <addr>:increment()"
    privateForP = optText "privatefor" 'p'
      "Comma-separated addresses with access to this transaction"

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
