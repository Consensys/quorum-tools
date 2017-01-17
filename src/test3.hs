{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- Test public / private state consistency
module Main where

import           Control.Monad              (forM_)
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Data.List                  (unzip7)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Text.Read                  (readMaybe)

import Turtle
import Checkpoint
import Cluster hiding (txRpcBody, sendTx)
import Control
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
   _txAddrsMs,
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

  storageAddr <- createContract (head geths) simpleStorage

  let sendTo = cycle geths

  let expectEq :: MonadIO io => Either Text Int -> Int -> io ()
      expectEq val expected = liftIO $
        if val == Right expected
        then pure ()
        else do
          putStrLn $ "Got wrong value: " <> show val <> " instead of " <> show expected
          exit failedTestCode

  forM_ (zip [1..5000] sendTo) $ \(no, geth) -> do
    incrementStorage geth simpleStorage storageAddr
    i <- getStorage geth simpleStorage storageAddr
    expectEq i no

  liftIO $ putStrLn "all successful!"

data Contract = Contract Text Text

createContract :: MonadIO io => Geth -> Contract -> io Addr
createContract geth contract@(Contract mem _abi) =
  let fromAddr = accountId $ gethAccountId geth
      cmd = T.unlines
        [ "var contractInstance = contract.new({"
        , "  from: '0x" <> fromAddr <> "',"
        , "  data: '" <> mem <> "',"
        , "  gas: '4700000'"
        , "}, function(e, contract) { console.log(e, contract.address); });"
        , "admin.sleepBlocks(1);"
        , "console.log(contractInstance.address);"
        , "contractInstance"
        ]
      -- look for TX-CREATED result addr
      step mAddr line = case matchCheckpoint TxCreated line of
        Just (_txid, addr) -> Just addr
        Nothing -> mAddr
      forceAddr mAddr = case mAddr of
        Just addr -> addr
        Nothing -> error "unable to extract addr from contract creation"
      -- TODO: switch to `find`
      consumer :: Fold Line Addr
      consumer = Fold step Nothing forceAddr
  in contractCmd geth contract cmd consumer

incrementStorage :: MonadIO io => Geth -> Contract -> Addr -> io ()
incrementStorage geth contract addr =
  let fromAddr = accountId $ gethAccountId geth
      cmd = "contract.at(" <> unAddr addr <> ").increment({ from: " <> fromAddr <> "})"
      consumer = pure () -- ignore the output
  in contractCmd geth contract cmd consumer

getStorage :: MonadIO io => Geth -> Contract -> Addr -> io (Either Text Int)
getStorage geth contract addr =
  let cmd = "contract.at(" <> unAddr addr <> ").get()"
      -- just parse only line
      step _ line = case readMaybe (T.unpack $ lineToText line) of
        Just num -> Right num
        Nothing -> Left (lineToText line)
      consumer = Fold step (Left "no lines sent to getStorage") id
  in contractCmd geth contract cmd consumer

contractCmd :: MonadIO io => Geth -> Contract -> Text -> Fold Line a -> io a
contractCmd geth (Contract _mem abi) cmd consumer =
  let pw = gethPassword geth
      unlockLine = "personal.unlockAccount(eth.accounts[0], '" <> pw <> "');"
      contractLine = "var contract = web3.eth.contract(" <> abi <> ");"
      dataDir = dataDirPath $ gethDataDir geth
      cmd' = T.unlines [unlockLine, contractLine, cmd]

      -- almost sendJs, but output-aware
      outputLines = inshellWithJoinedErr
        (gethCommand geth (sendJsSubcommand dataDir cmd'))
        empty
  in do
        liftIO $ T.putStrLn cmd'
        fold outputLines consumer


-- pragma solidity ^0.4.0;
--
-- contract SimpleStorage {
--     uint storedData;
--
--     function increment() {
--         storedData = storedData + 1;
--     }
--
--     function get() constant returns (uint) {
--         return storedData;
--     }
-- }

simpleStorage :: Contract
simpleStorage = Contract
  "606060405234610000575b60b8806100186000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680636d4ce63c146046578063d09de08a146066575b6000565b3460005760506072565b6040518082815260200191505060405180910390f35b346000576070607d565b005b600060005490505b90565b6001600054016000819055505b5600a165627a7a7230582075ddbf72b28ea6bb283ba9153afa8abd0a2a1f6a627aa8c47db363629936097e0029"
  "[{\"constant\":true,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"increment\",\"outputs\":[],\"payable\":false,\"type\":\"function\"}]"
