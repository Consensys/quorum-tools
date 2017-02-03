{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Cluster.Client
  ( sendTx
  , spamTransactions
  , spamGeth
  , bench
  , loadLocalNode
  , perSecond
  ) where

import qualified Control.Foldl             as Fold
import           Control.Lens              (to, (^.), (^?))
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Control.RateLimit         (RateLimit (PerExecution),
                                            dontCombine,
                                            generateRateLimitedFunction)
import           Data.Aeson                (Value, object, (.=))
import           Data.Aeson.Lens           (key, _String)
import qualified Data.ByteString.Lazy      as LSB
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import           Data.Text.Lazy            (toStrict)
import qualified Data.Text.Lazy.Encoding   as LT
import           Data.Time.Units
import           Network.HTTP.Client       (defaultManagerSettings)
import           Network.Wreq              (Response, post, responseBody)
import           Network.Wreq.Session      (Session)
import qualified Network.Wreq.Session      as Sess
import           Prelude                   hiding (FilePath, lines)
import           Safe                      (headMay)
import           Turtle

import           Checkpoint
import           Cluster

txRpcBody :: Geth -> Value
txRpcBody geth = object
    [ "id"      .= (1 :: Int)
    , "jsonrpc" .= t "2.0"
    , "method"  .= t "eth_sendTransaction"
    , "params"  .=
      [ object
        [ "from" .= (accountId . gethAccountId $ geth)
        , "to"   .= t "0000000000000000000000000000000000000000"
        ]
      ]
    ]

  where
    t :: Text -> Text
    t = id

sendTx :: MonadIO io => Geth -> io (Either Text TxId)
sendTx geth = liftIO $ parse <$> post (T.unpack $ gethUrl geth) (txRpcBody geth)
  where
    parse :: Response LSB.ByteString -> Either Text TxId
    parse r = fromMaybe parseFailure mParsed
      where
        parseFailure = Left $ toStrict $
          "failed to parse RPC response: " <> LT.decodeUtf8 (r^.responseBody)
        mParsed :: Maybe (Either Text TxId)
        mParsed = (r^?responseBody.key "result"._String.to (Right . TxId))
              <|> (r^?responseBody.key "error".key "message"._String.to Left)

bench :: MonadIO m => Geth -> Seconds -> m ()
bench geth (Seconds seconds) = view benchShell
  where
    luaEscapeSingleQuotes = jsEscapeSingleQuotes
    lua = format ("wrk.method = 'POST'\n"  %
                  "wrk.body   = '"%s%"'\n" %
                  "wrk.headers['Content-Type'] = 'application/json'")
                 (luaEscapeSingleQuotes $ textEncode $ txRpcBody geth)

    benchShell = do
      luaPath <- using $ fileContaining $ select $ textToLines lua
      let cmd = format ("wrk -s "%fp%" -c 1 -d "%d%"s -t 1 "%s)
                       luaPath
                       seconds
                       (gethUrl geth)
      inshell cmd empty

-- | Continuously send transaction requests in a round-robin order. This runs
--   indefinitely. Assumes that the list has at least one element.
--
-- TODO: this should be converted to use 'spam' below, re-using pooled
-- connections.
--
spamTransactions :: MonadIO m => [Geth] -> m ()
spamTransactions geths = go geths
  where
    go (geth:rest) = sendTx geth >> go rest
    go []          = go geths

--
-- NOTE: this only works for the *local* node. the account ID is obtained from
--       the local IPC connection. the enode ID is too, but we could start
--       reading those from `readStaticNodes`.
--
loadLocalNode :: (MonadIO m, HasEnv m) => GethId -> m Geth
loadLocalNode gid = do
  nodeDataDir <- gidDataDir gid
  let js = "console.log(eth.accounts[0] + '!' + admin.nodeInfo.enode)"
  cmd <- setupCommand gid <*> pure (sendJsSubcommand (dataDirPath nodeDataDir) js)

  let pat :: Pattern (AccountId, EnodeId)
      pat = pure (,) <*> fmap (AccountId . T.pack) ("0x" *> count 40 hexDigit)
                     <*> fmap EnodeId              ("!" *> begins "enode")

  (aid, eid) <- fmap forceMaybe $ runMaybeT $ do
    line <- MaybeT $ fold (inshell cmd empty) Fold.head
    MaybeT $ return $ headMay $ match pat (lineToText line)

  mkGeth gid eid aid

  where
    forceMaybe = fromMaybe $ error "unable to extract account and enode ID"

every :: TimeUnit a => a -> RateLimit a
every = PerExecution

perSecond :: Integer -> RateLimit Millisecond
perSecond times = every $
  fromMicroseconds $ toMicroseconds (1 :: Second) `div` times

spam :: (MonadIO m, TimeUnit a) => Session -> RateLimit a -> Geth -> m ()
spam session rateLimit geth = do
  let url = T.unpack $ gethUrl geth
      postBody = Sess.post session url
  waitThenPost <- liftIO $
    generateRateLimitedFunction rateLimit postBody dontCombine
  forever $ liftIO $ waitThenPost (txRpcBody geth)

spamGeth :: (MonadIO m, TimeUnit a) => Geth -> RateLimit a -> m ()
spamGeth geth rateLimit =
    liftIO $ Sess.withSessionControl Nothing mgrSettings $ \session ->
      spam session rateLimit geth

  where
    mgrSettings = defaultManagerSettings
