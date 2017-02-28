{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cluster.Client
  ( SpamMode(..)
  , spamGeth
  , sendTransaction
  , call
  , bench
  , loadLocalNode
  , perSecond
  ) where

import           Control.Lens            (to, (^.), (^?))
import           Control.RateLimit       (RateLimit (PerExecution), dontCombine,
                                          generateRateLimitedFunction)
import           Data.Aeson              (Value(Array, String), object, (.=))
import           Data.Aeson.Lens         (key, _String)
import qualified Data.ByteString.Lazy    as LSB
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Text.Lazy          (toStrict)
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Time.Units
import qualified Data.Vector             as V
import           Network.HTTP.Client     (defaultManagerSettings)
import           Network.Wreq            (Response, post, responseBody)
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
encodeMethod (UnencodedMethod signature) = sha3Bytes (T.encodeUtf8 signature)

emptyTxRpcBody :: Geth -> Value
emptyTxRpcBody geth = object
    [ "id"      .= (1 :: Int)
    , "jsonrpc" .= t "2.0"
    , "method"  .= t "eth_sendTransaction"
    , "params"  .=
      [ object
        [ "from" .= showGethAccountId geth
        , "to"   .= hexPrefixed (intToHexBS 0)
        ]
      ]
    ]

sendBody :: Tx -> Geth -> Value
sendBody (Tx maybeTo method privacy op) geth = object
    [ "id"      .= (1 :: Int)
    , "jsonrpc" .= t "2.0"
    , "method"  .= opName
    , "params"  .= [ object params'' ]
    ] where
        params =
          [ "from" .= showGethAccountId geth
          , "data" .= hexPrefixed (encodeMethod method)
          ]

        params' = case privacy of
          Public -> params
          PrivateFor addrs ->
            let addrsVal = Array (String . unSecp256k1 <$> V.fromList addrs)
            in params <> ["privateFor" .= addrsVal]

        params'' = case maybeTo of
          Nothing -> params'
          Just toBytes -> params' <> ["to" .= hexPrefixed toBytes]

        opName :: Text
        opName = case op of
          Sync  -> "eth_sendTransaction"
          Async -> "eth_sendTransactionAsync"

callBody :: CallArgs -> Geth -> Value
callBody (CallArgs toBytes method) geth = object
  [ "id"      .= (1 :: Int)
  , "jsonrpc" .= t "2.0"
  , "method"  .= t "eth_call"
  , "params"  .=
    [ object
      [ "from" .= showGethAccountId geth
      , "data" .= hexPrefixed (encodeMethod method)
      , "to"   .= hexPrefixed toBytes
      ]
    , "latest" -- (block number)
    ]
  ]

call :: MonadIO io => Geth -> CallArgs -> io (Either Text Text)
call geth args
  = liftIO $ parse <$> post (T.unpack (gethUrl geth)) (callBody args geth)
  where
    parse :: Response LSB.ByteString -> Either Text Text
    parse r = fromMaybe parseFailure mParsed
      where
        parseFailure = Left $ toStrict $
          "failed to parse RPC response: " <> LT.decodeUtf8 (r^.responseBody)
        mParsed :: Maybe (Either Text Text)
        mParsed = (r^?responseBody.key "result"._String.to Right)
              <|> (r^?responseBody.key "error".key "message"._String.to Left)

sendTransaction :: MonadIO io => Geth -> Tx -> io ()
sendTransaction geth args
  = liftIO $ void $ post (T.unpack (gethUrl geth)) (sendBody args geth)

spamBody :: SpamMode -> Geth -> Value
spamBody = \case
  BenchEmptyTx -> emptyTxRpcBody
  SendTx args -> sendBody args

bench :: MonadIO m => SpamMode -> Geth -> Seconds -> m ()
bench spamMode geth (Seconds seconds) = view benchShell
  where
    luaEscapeSingleQuotes = jsEscapeSingleQuotes
    lua = format ("wrk.method = 'POST'\n"  %
                  "wrk.body   = '"%s%"'\n" %
                  "wrk.headers['Content-Type'] = 'application/json'")
                 (luaEscapeSingleQuotes $ textEncode $ spamBody spamMode geth)

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

spam :: (MonadIO m, TimeUnit a) => SpamMode -> Session -> RateLimit a -> Geth -> m ()
spam spamMode session rateLimit geth = do
  let gUrl = T.unpack $ gethUrl geth
      postBody = Sess.post session gUrl
      txBody = spamBody spamMode geth
  waitThenPost <- liftIO $
    generateRateLimitedFunction rateLimit postBody dontCombine
  forever $ liftIO $ waitThenPost txBody

spamGeth :: (MonadIO m, TimeUnit a) => SpamMode -> Geth -> RateLimit a -> m ()
spamGeth spamMode geth rateLimit =
    liftIO $ Sess.withSessionControl Nothing mgrSettings $ \session ->
      spam spamMode session rateLimit geth

  where
    mgrSettings = defaultManagerSettings
