{-# LANGUAGE OverloadedStrings #-}

module Cluster.SpamArgs where

import           Turtle               hiding (char)

import           Cluster.Types
import           Cluster.Client       (SpamMode(..))
import           Cluster.Util         (Bytes20, bytes20P, HexPrefix(..),
                                       matchOnce)
import           Data.Char            (isHexDigit)
import qualified Data.Text            as T

-- TODO: restrict more than chars1 -- figure out what characters are allowed in
-- a signature
contractPattern :: Pattern (Bytes20, UnencodedMethod)
contractPattern = (,)
  <$> bytes20P WithPrefix
  <*> (":" >> fmap UnencodedMethod chars1)

b64Digit :: Pattern Char
b64Digit = satisfy isB64Digit
  where isB64Digit char = isHexDigit char || char `elem` ['/', '+', '=']

-- A (compressed) Secp256k1 public key is 32 bytes (* 11 / 8) = 44 base-64
-- characters (64 = 2^11).
pubKeyP :: Pattern Secp256k1
pubKeyP = Secp256k1 . T.pack <$> count 44 b64Digit

privateForPattern :: Pattern [Secp256k1]
privateForPattern = pubKeyP `sepBy` ","

processContractArgs :: Maybe Text -> Maybe Text -> SpamMode
processContractArgs contractT privateForT = maybe BenchEmptyTx SendTx $ do
  rawContract <- contractT
  rawPrivateFor <- privateForT

  let privacy = case matchOnce privateForPattern rawPrivateFor of
        Nothing -> Public
        Just addrs -> PrivateFor addrs

  (addr, method) <- matchOnce contractPattern rawContract
  pure $ Tx (Just addr) method privacy Async

contractP :: Parser Text
contractP = optText "contract" 'c'
  "Contract address and method <addr>:increment()"

privateForP :: Parser Text
privateForP = optText "privatefor" 'p'
  "Comma-separated addresses with access to this transaction"
