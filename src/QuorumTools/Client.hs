{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module QuorumTools.Client
  ( SpamMode(..)
  , spamGeth
  , sendTransaction
  , call
  , create
  , sendEmptyTx
  , bench
  , loadNode
  , perSecond
  , addNode
  , removeNode
  ) where

import           Control.Lens            (Fold, to, (^.), (^?))
import           Control.RateLimit       (RateLimit (PerExecution), dontCombine,
                                          generateRateLimitedFunction)
import           Data.Aeson              (Value (Array, String), object, toJSON,
                                          (.=))
import           Data.Aeson.Lens         (key, _Integral, _Null, _String)
import           Data.Aeson.Types        (Pair)
import qualified Data.ByteString         as BS
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
import qualified Network.Wreq.Session    as Sess
import           Prelude                 hiding (FilePath, lines)
import           Turtle                  hiding (Fold)

import           QuorumTools.Cluster
import           QuorumTools.Types
import           QuorumTools.Util

data TxResult
  = TxAck TxId -- for sync
  | TxSent     -- for async
  deriving (Show)

t :: Text -> Text
t = id

i :: Int -> Int
i = id

encodeMethod :: UnencodedMethod -> Bytes32
encodeMethod (UnencodedMethod signature) = sha3Bytes (T.encodeUtf8 signature)

opName :: TxSync -> Text
opName = \case
  Sync  -> "eth_sendTransaction"
  Async -> "eth_sendTransactionAsync"

setPrivateFor :: Privacy -> [Pair] -> [Pair]
setPrivateFor privacy params = case privacy of
  Public -> params
  PrivateFor addrs ->
    let addrsVal = Array (String . unSecp256k1 <$> V.fromList addrs)
    in params <> ["privateFor" .= addrsVal]

emptyTxRpcBody :: Geth -> Value
emptyTxRpcBody geth = object
    [ "id"      .= i 1
    , "jsonrpc" .= t "2.0"
    , "method"  .= t "eth_sendTransaction"
    , "params"  .=
      [ object
        [ "from" .= showGethAccountId geth
        , "to"   .= hexPrefixed (intToBytes20 0)
        ]
      ]
    ]

sendBody :: Tx -> Geth -> Value
sendBody (Tx maybeTo method privacy sync) geth = object
    [ "id"      .= i 1
    , "jsonrpc" .= t "2.0"
    , "method"  .= opName sync
    , "params"  .= [ object params' ]
    ] where
        params = setPrivateFor privacy
          [ "from" .= showGethAccountId geth
          , "data" .= hexPrefixed (encodeMethod method)
          ]

        params' = case maybeTo of
          Nothing -> params
          Just toBytes -> params <> ["to" .= hexPrefixed toBytes]

createBody :: CreateArgs -> Geth -> Value
createBody
  (CreateArgs (Contract privacy _methods bytecode _abi) initVal sync)
  geth         = object
  [ "id"      .= i 1
  , "jsonrpc" .= t "2.0"
  , "method"  .= opName sync
  , "params"  .=
    [ object $ setPrivateFor privacy
      [ "from"  .= showGethAccountId geth
      , "data"  .= ("0x" <> bytecode <> printHex WithoutPrefix initVal)
      , "gas"   .= t "0x47B760"
      ]
    ]
  ]

callBody :: CallArgs -> Geth -> Value
callBody (CallArgs toBytes method) geth = object
  [ "id"      .= i 1
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

extractResult :: Fold Value a -> Response LSB.ByteString -> Either Text a
extractResult subfield r = fromMaybe parseFailure mParsed
  where
    parseFailure = Left $ toStrict $
      "failed to parse RPC response: " <> LT.decodeUtf8 (r^.responseBody)
    mParsed = r^?responseBody.key "result".subfield.to Right
          <|> r^?responseBody.key "error".key "message"._String.to Left

call :: MonadIO io => Geth -> CallArgs -> io (Either Text BS.ByteString)
call geth args =
    liftIO $ extract <$> post (T.unpack (gethUrl geth)) (callBody args geth)
  where
    extract = extractResult $ _String.to textToBytes.traverse

extractTxResult :: TxSync -> Response LSB.ByteString -> Either Text TxResult
extractTxResult mode resp =
  case mode of
    Sync -> extractResult
              (_String . to textToBytes32 . traverse . to (TxAck . TxId))
              resp
    Async -> extractResult (_Null . to (const TxSent)) resp

sendTransaction :: MonadIO m => Geth -> Tx -> m (Either Text TxResult)
sendTransaction geth args = liftIO $ extractTxResult (txSync args) <$>
  post (T.unpack (gethUrl geth)) (sendBody args geth)

create :: MonadIO m => Geth -> CreateArgs -> m (Either Text TxResult)
create geth args@(CreateArgs _ _ mode) = liftIO $ extractTxResult mode <$>
  post (T.unpack (gethUrl geth)) (createBody args geth)

addNode :: MonadIO m => Geth -> EnodeId -> m (Either Text GethId)
addNode geth (EnodeId eid) = liftIO $
    extractResult (_Integral . to GethId) <$> post url body
  where
    url = T.unpack (gethUrl geth)

    body :: Value
    body = object
      [ "id"      .= i 1
      , "jsonrpc" .= t "2.0"
      , "method"  .= t "raft_addPeer"
      , "params"  .= [String eid]
      ]

removeNode :: MonadIO m => Geth -> GethId -> m (Either Text ())
removeNode geth gid = liftIO $ extractResult (to $ const ()) <$> post url body
  where
    url = T.unpack (gethUrl geth)

    body :: Value
    body = object
      [ "id"      .= i 1
      , "jsonrpc" .= t "2.0"
      , "method"  .= t "raft_removePeer"
      , "params"  .= [toJSON (gId gid)]
      ]

sendEmptyTx :: MonadIO io => Geth -> io ()
sendEmptyTx geth = liftIO $ void $
  post (T.unpack (gethUrl geth)) (emptyTxRpcBody geth)

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

loadNode :: (MonadIO m, HasEnv m) => GethId -> m Geth
loadNode gid = mkGeth gid =<< readEnodeId gid

every :: TimeUnit a => a -> RateLimit a
every = PerExecution

perSecond :: Integer -> RateLimit Millisecond
perSecond times = every $
  fromMicroseconds $ toMicroseconds (1 :: Second) `div` times

spamGeth :: (MonadIO m, TimeUnit a) => SpamMode -> RateLimit a -> Geth -> m ()
spamGeth spamMode rateLimit geth =
  liftIO $ Sess.withSessionControl Nothing defaultManagerSettings $ \sess -> do
    let gUrl = T.unpack $ gethUrl geth
        -- TODO: take timestamp before each post, or maybe use ekg
        postBody = Sess.post sess gUrl
        txBody = spamBody spamMode geth
    waitThenPost <- liftIO $
      generateRateLimitedFunction rateLimit postBody dontCombine
    forever $ liftIO $ waitThenPost txBody
