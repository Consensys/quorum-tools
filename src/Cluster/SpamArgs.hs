{-# LANGUAGE OverloadedStrings #-}

module Cluster.SpamArgs where

import qualified Data.Text            as T
import           Turtle

import           Cluster.Types
import           Cluster.Client       (BenchType(..))
import           Cluster.Util         (Bytes20, bytes20P)
import           SharedPartitioning   (matchOnce)

hexLike :: Pattern a -> Pattern a
hexLike = (optional "0x" >>)

-- TODO: restrict more than chars1 -- figure out what characters are allowed in
-- a signature
contractPattern :: Pattern (Bytes20, UnencodedMethod)
contractPattern = hexLike $ (,) <$> bytes20P <*> (":" >> fmap UnencodedMethod chars1)

addrP :: Pattern Addr
addrP = hexLike $ Addr . T.pack <$> many (noneOf [','])

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

contractP :: Parser Text
contractP = optText "contract" 'c'
  "Contract address and method <addr>:increment()"

privateForP :: Parser Text
privateForP = optText "privatefor" 'p'
  "Comma-separated addresses with access to this transaction"
