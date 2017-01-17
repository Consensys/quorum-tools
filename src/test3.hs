{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- Test public / private state consistency
module Main where

import           Control.Concurrent.MVar  (readMVar)
import           Control.Lens               (to, (^.), (^?))
import Control.Monad (forM, forM_)
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Data.Aeson                 (ToJSON (toJSON), Value, object,
                                             (.=))
import           Data.Aeson.Lens            (key, _String)
import qualified Data.ByteString.Lazy       as LSB
import           Data.List                  (unzip7)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import Data.Monoid.Same
import Data.Monoid (All(..))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Text.Lazy             (toStrict)
import qualified Data.Text.Lazy.Encoding    as LT
import GHC.Generics hiding (to)
import           Network.Wreq               (Response, post, responseBody)

import Turtle
import           Checkpoint
import Cluster hiding (txRpcBody, sendTx)
import           Control
import TestOutline hiding (verify)

main :: IO ()
main = sh $ flip runReaderT defaultClusterEnv $ do
  let numNodes = 3
  let gids = [1..GethId numNodes]

  geths <- setupNodes gids
  (readyAsyncs,
   _terminatedAsyncs,
   _lastBlockMs,
   _lastRafts,
   _outstandingTxesMs,
   txAddrsMs,
   allConnected)
   <- unzip7 <$> traverse (runNode numNodes) geths

  -- wait for geth to launch, then start raft and run the test body
  timestampedMessage "awaiting all ready"
  awaitAll readyAsyncs -- "IPC endpoint opened"
  timestampedMessage "got all ready"

  startRaftAcross geths

  timestampedMessage "awaiting all TCP connections"
  awaitAll allConnected -- "peer * became active"
  timestampedMessage "got all TCP connections"

  submittedTxs <- forM txList $ \(preTx@(PreTransaction _data _)) -> do
    txIds <- forM geths $ \node -> do
      result <- sendTx node preTx
      case result of
        Left txErr -> liftIO $ T.putStrLn txErr >> exit failedTestCode
        Right txId -> return txId

    case allSame txIds of
      Same resultingTxId -> return (SubmittedTransaction {_data, privateInfo = Nothing, resultingTxId})
      _ -> error "not same (1)"

  txAddrs <- liftIO $ traverse readMVar txAddrsMs
  txAddrs' <- case allSame txAddrs of
    Same txAddrs' -> return txAddrs'
    _ -> error "not same (2)"

  forM_ geths $ \geth -> liftIO $ do
    okay <- verify geth txAddrs' submittedTxs
    unless okay (exit failedTestCode)

  liftIO $ putStrLn "all successful!"

data Contract = Contract Text Text

{-
pragma solidity ^0.4.0;

contract SimpleStorage {
    uint storedData;

    function increment() {
        storedData = storedData + 1;
    }

    function get() constant returns (uint) {
        return storedData;
    }
}
 -}

simpleStorage :: Contract
simpleStorage = Contract
  "606060405234610000575b60b8806100186000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680636d4ce63c146046578063d09de08a146066575b6000565b3460005760506072565b6040518082815260200191505060405180910390f35b346000576070607d565b005b600060005490505b90565b6001600054016000819055505b5600a165627a7a7230582075ddbf72b28ea6bb283ba9153afa8abd0a2a1f6a627aa8c47db363629936097e0029"
  "[{\"constant\":true,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"increment\",\"outputs\":[],\"payable\":false,\"type\":\"function\"}]"

-- | For every submitted transaction, find the address it's located at, look up
-- that address on each geth, and make sure it's what we expect
--
-- verify:
-- * from
-- * to
-- * privateFrom
-- * privateFor
-- * data
-- * v
verify :: Geth -> TxAddrs -> [SubmittedTransaction] -> IO Bool
verify geth (TxAddrs addrs) txes = do
  eachTxAsExpected <- forM txes $ \submittedTx -> case Map.lookup (resultingTxId submittedTx) addrs of
    Just addr -> hasExpectedXXX geth addr
    Nothing -> return False
  return $ getAll (foldMap All eachTxAsExpected)

-- | Check that this geth has the expected
hasExpected
  where hasExpectedXXX = error "TODO(joel)"

type Memory = Map.Map Text Text

data EthState = EthState
  { party :: Party
  , privState :: Memory
  , pubState :: Memory
  }

-- TODO maybe use secp256k1 package
data Party = Party
  { pubKey :: Text
  , privKey :: Text
  } deriving (Eq, Generic)

instance ToJSON Party where

data PrivateInfo = PrivateInfo
  { privateFor :: [Party]
  , privateFrom :: Party
  } deriving Generic

instance ToJSON PrivateInfo where

data PreTransaction = PreTransaction
  { _data :: Text
  , privateInfo :: Maybe PrivateInfo
  }

data SubmittedTransaction = SubmittedTransaction
  { _data :: Text
  , privateInfo :: Maybe PrivateInfo
  , resultingTxId :: TxId
  }

party1, party2, party3 :: Party
party1 = undefined
party2 = undefined
party3 = undefined

serializeTx :: PreTransaction -> Value
serializeTx (PreTransaction {_data, privateInfo}) = case privateInfo of
  Nothing -> object [ "data" .= _data ]
  Just (PrivateInfo {privateFor, privateFrom}) -> object
    [ "data" .= _data
    , "privateFor" .= toJSON privateFor
    , "privateFrom" .= toJSON privateFrom
    ]

-- transition :: SubmittedTransaction -> EthState -> EthState
-- transition (SubmittedTransaction {_data, privateInfo}) (EthState {party, privState, pubState}) =
--   case privateInfo of
--     Nothing -> EthState {party, privState, pubState = transition' _data pubState}
--     Just (PrivateInfo {privateFor}) ->
--       -- let newPrivState =
--       --       if party `elem` privateFor
--       --       then transition' _data privState
--       --       else privState
--       EthState {party, privState, pubState}

pattern PubTx :: Text -> PreTransaction
pattern PubTx _data = PreTransaction { _data, privateInfo = Nothing }

pattern PrivTx :: Text -> Party -> [Party] -> PreTransaction
pattern PrivTx _data privateFrom privateFor = PreTransaction
  { _data
  , privateInfo = Just (PrivateInfo
    { privateFrom
    , privateFor
    })
  }

txList :: [PreTransaction]
txList =
  [ PubTx "Nasdaq feed 1"
  , PrivTx "private transfer 1 -> 2" party1 [party2]
  , PubTx "Nasdaq feed 2"
  , PrivTx "information share 3 -> [1, 2]" party3 [party1, party2]
  , PubTx "Nasdaq feed 3"
  ]

txRpcBody :: PreTransaction -> Value
txRpcBody tx = object
  [ "id"      .= (1 :: Int)
  , "jsonrpc" .= t "2.0"
  , "method"  .= t "eth_sendTransaction"
  , "params"  .= serializeTx tx
  ]

  where
    t :: Text -> Text
    t = id

-- TODO figure out how to send private txes
sendTx :: MonadIO io => Geth -> PreTransaction -> io (Either Text TxId)
sendTx geth tx = liftIO $ parse <$> post (T.unpack $ gethUrl geth) (txRpcBody tx)
  where
    parse :: Response LSB.ByteString -> Either Text TxId
    parse r = fromMaybe parseFailure mParsed
      where
        parseFailure = Left $ toStrict $
          "failed to parse RPC response: " <> LT.decodeUtf8 (r^.responseBody)
        mParsed :: Maybe (Either Text TxId)
        mParsed = (r^?responseBody.key "result"._String.to (Right . TxId))
              <|> (r^?responseBody.key "error".key "message"._String.to Left)

-- Example result:
-- {
--   blockHash: "0x0000000000000000000000000000000000000000000000000000000000000000",
--   blockNumber: null,
--   from: "0xb382cb1b938278b85265f29f5cf587eaf2523962",
--   gas: 90000,
--   gasPrice: 0,
--   hash: "0x07ff1bd59eee81d95dbff4860237129d73ce7174dcd9d3bd12c5d1ac808ce174",
--   input: "0x1234",
--   nonce: 2,
--   r: "0x78d92baf095f62fd60760700dd46c2d0bf886b7eef3f7ab6a47379ef6af42d2f",
--   s: "0x2a932772b2748f5cd24964b714f84c58b7341dfe4eaede419fa5f531adf27bf7",
--   to: null,
--   transactionIndex: null,
--   v: "0x1b",
--   value: 0
-- }

data AddressContents = AddressContents
  { from ::
  } deriving (Show, Eq)

gethUrl' :: Geth -> String
gethUrl' = T.unpack . gethUrl

post' :: MonadIO io => Geth -> Value -> io (Response ByteString)
post' geth value = liftIO $ post (T.unpack $ gethUrl geth) value

getAddress :: MonadIO io => Geth -> TxId -> io (Either Text AddressContents)
getAddress geth tx = do
  parse <$> post' geth (get
