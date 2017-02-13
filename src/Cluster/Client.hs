{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cluster.Client
  ( BenchType(..)
  , spamGeth
  , sendTx
  , bench
  , loadLocalNode
  , perSecond
  ) where

import           Control.RateLimit       (RateLimit (PerExecution), dontCombine,
                                          generateRateLimitedFunction)
import           Data.Aeson              (Value(Array, String), object, (.=))
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Time.Units
import qualified Data.Vector             as V
import           Network.HTTP.Client     (defaultManagerSettings)
import           Network.Wreq            (post)
import           Network.Wreq.Session    (Session)
import qualified Network.Wreq.Session    as Sess
import           Prelude                 hiding (FilePath, lines)
import           Turtle

import           Cluster
import           Cluster.Types
import           Cluster.Util

t :: Text -> Text
t = id

encodeMethod :: UnencodedMethod -> Bytes32
encodeMethod (UnencodedMethod signature)
  = sha3Bytes (T.encodeUtf8 signature)

bytesText :: Bytes32 -> Text
bytesText (Bytes32 bs) = T.decodeUtf8 bs

emptyTxRpcBody :: Geth -> Value
emptyTxRpcBody geth = object
    [ "id"      .= (1 :: Int)
    , "jsonrpc" .= t "2.0"
    , "method"  .= t "eth_sendTransactionAsync"
    , "params"  .=
      [ object
        [ "from" .= (accountId . gethAccountId $ geth)
        , "to"   .= t "0000000000000000000000000000000000000000"
        ]
      ]
    ]

txRpcBody :: Tx -> Geth -> Value
txRpcBody (Tx maybeTo method privacy op) geth = object
    [ "id"      .= (1 :: Int)
    , "jsonrpc" .= t "2.0"
    , "method"  .= opName
    , "params"  .= [ object params'' ]
    ] where
        params =
          [ "from" .= ("0x" <> accountId (gethAccountId geth))
          , "data" .= ("0x" <> bytesText (encodeMethod method))
          ]

        params' = case privacy of
          Public -> params
          PrivateFor addrs ->
            let addrsVal = Array (String . unAddr <$> V.fromList addrs)
            in params <> ["privateFor" .= addrsVal]

        params'' = case maybeTo of
          Nothing -> params'
          Just toBytes -> params' <> ["to" .= hexPrefixed toBytes]

        opName :: Text
        opName = case op of
          Call                 -> "eth_call"
          SendTransaction      -> "eth_sendTransaction"
          SendTransactionAsync -> "eth_sendTransactionAsync"

sendTx :: MonadIO io => Geth -> Tx -> io ()
sendTx geth tx
  = liftIO $ void $ post (T.unpack $ gethUrl geth) (txRpcBody tx geth)
  -- TODO: temporarily disabling parsing of result, though we'll want this when
  -- we port getStorage to rpc
  --
  -- where
  --   parse :: Response LSB.ByteString -> Either Text TxId
  --   parse r = fromMaybe parseFailure mParsed
  --     where
  --       parseFailure = Left $ toStrict $
  --         "failed to parse RPC response: " <> LT.decodeUtf8 (r^.responseBody)
  --       mParsed :: Maybe (Either Text TxId)
  --       mParsed = (r^?responseBody.key "result"._String.to (Right . TxId))
  --             <|> (r^?responseBody.key "error".key "message"._String.to Left)

data BenchType
  = BenchEmptyTx
  | BenchTx Tx

benchTxBody :: BenchType -> Geth -> Value
benchTxBody = \case
  BenchEmptyTx -> emptyTxRpcBody
  BenchTx tx -> txRpcBody tx

bench :: MonadIO m => BenchType -> Geth -> Seconds -> m ()
bench benchTy geth (Seconds seconds) = view benchShell
  where
    luaEscapeSingleQuotes = jsEscapeSingleQuotes
    lua = format ("wrk.method = 'POST'\n"  %
                  "wrk.body   = '"%s%"'\n" %
                  "wrk.headers['Content-Type'] = 'application/json'")
                 (luaEscapeSingleQuotes $ textEncode $ benchTxBody benchTy geth)

    benchShell = do
      luaPath <- using $ fileContaining $ select $ textToLines lua
      let cmd = format ("wrk -s "%fp%" -c 1 -d "%d%"s -t 1 "%s)
                       luaPath
                       seconds
                       (gethUrl geth)
      inshell cmd empty

--
-- NOTE: this only works for the *local* node. the account ID is obtained from
--       the local data dir.
--
loadLocalNode :: (MonadIO m, HasEnv m) => GethId -> m Geth
loadLocalNode gid = do
  eid <- readEnodeId gid
  aid <- readAccountId gid
  mkGeth gid eid aid

every :: TimeUnit a => a -> RateLimit a
every = PerExecution

perSecond :: Integer -> RateLimit Millisecond
perSecond times = every $
  fromMicroseconds $ toMicroseconds (1 :: Second) `div` times

spam :: (MonadIO m, TimeUnit a) => BenchType -> Session -> RateLimit a -> Geth -> m ()
spam benchTy session rateLimit geth = do
  let gUrl = T.unpack $ gethUrl geth
      postBody = Sess.post session gUrl
      txBody = benchTxBody benchTy geth
  waitThenPost <- liftIO $
    generateRateLimitedFunction rateLimit postBody dontCombine
  forever $ liftIO $ waitThenPost txBody

spamGeth :: (MonadIO m, TimeUnit a) => BenchType -> Geth -> RateLimit a -> m ()
spamGeth benchTy geth rateLimit =
    liftIO $ Sess.withSessionControl Nothing mgrSettings $ \session ->
      spam benchTy session rateLimit geth

  where
    mgrSettings = defaultManagerSettings
