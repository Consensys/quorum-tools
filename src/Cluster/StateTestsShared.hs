{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- Utilities shared between the public and private state tests
module Cluster.StateTestsShared where

import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Text.Read                  (readMaybe)

import Prelude hiding (FilePath)
import Turtle hiding (match)
import Checkpoint
import Cluster
import Cluster.Client (sendTx)
import Cluster.Types
import Cluster.Util (Bytes20(..))
import Control
import TestOutline hiding (verify)


clusterSize :: Int
clusterSize = 3

startEnv :: ClusterEnv
startEnv = mkLocalEnv clusterSize

expectEq :: MonadIO io => Either Text Int -> Int -> io ()
expectEq val expected = liftIO $
  when (val /= Right expected) $ do
    putStrLn $
      "Got wrong value: " <> show val <> " instead of " <> show expected
    exit failedTestCode

createContract :: MonadIO io => Geth -> Contract -> io Addr
createContract geth contract@(Contract privacy _ops mem _abi) =
  let fromAddr = accountId $ gethAccountId geth
      cmd = T.unlines
        [ "var contractInstance = contract.new(42, {"
        , "  from: '0x" <> fromAddr <> "',"
        , "  data: '" <> mem <> "',"
        , "  gas: '4700000',"
        , privacyLine privacy
        , "}, function(e, contract) {"
        -- BIG HACK
        , "  console.log('RAFT-CHECKPOINT TX-CREATED (0x0000000000000000000000000000000000000000000000000000000000000000, ' + contract.address + ')');"
        , "});"
        , "admin.sleepBlocks(1);"
        ]

      -- look for TX-CREATED result addr
      force = fromMaybe (error "unable to extract addr from contract creation")

      consumer :: Fold Line Addr
      consumer = snd . force <$> find' (matchCheckpoint TxCreated)

  in contractCmd geth contract cmd consumer

privacyLine :: Privacy -> Text
privacyLine = \case
  Public -> ""
  PrivateFor addrs ->
    "privateFor: [" <> T.intercalate ", " (quote <$> addrs) <> "],"
  where
    quote :: Addr -> Text
    quote (Addr t) = "\"" <> t <> "\""

sleepBlock :: MonadIO io => Geth -> io ()
sleepBlock geth = fold (sendJs geth "admin.sleepBlocks(1);") (pure ())

addrToBytes :: Addr -> Bytes20
addrToBytes = Bytes20 . T.encodeUtf8 . unAddr

incrementStorage :: MonadIO io => Geth -> Contract -> Addr -> io ()
incrementStorage geth (Contract privacy _ _ _) addr =
  -- TODO: think about making addr a bytestring (or Bytes20) newtype
  let addr' = addrToBytes addr
  -- TODO: remove "increment()" duplication
  in sendTx geth (Tx (Just addr') "increment()" privacy SendTransaction)

getStorage :: MonadIO io => Geth -> Contract -> Addr -> io (Either Text Int)
getStorage geth contract addr =
  let cmd = "contract.at('" <> unAddr addr <> "').get()"
      -- just parse only line
      step _ line = case readMaybe (T.unpack $ lineToText line) of
        Just num -> Right num
        Nothing -> Left (lineToText line)
      consumer = Fold step (Left "no lines sent to getStorage") id
  in contractCmd geth contract cmd consumer
  -- TODO: convert to rpc form:
  -- let addr' = addrToBytes addr
  -- in sendTx geth (Tx (Just addr') "get()" privacy Call)

contractCmd :: MonadIO io => Geth -> Contract -> Text -> Fold Line a -> io a
contractCmd geth (Contract _privacy _ops _mem abi) cmd consumer =
  let pw = gethPassword geth
      unlockLine = "personal.unlockAccount(eth.accounts[0], '" <> pw <> "');"
      contractLine = "var contract = web3.eth.contract(" <> abi <> ");"
      cmd' = T.unlines [unlockLine, contractLine, cmd]
      outputLines = sendJs geth cmd'
  in fold outputLines consumer

sendJs :: Geth -> Text -> Shell Line
sendJs geth js = inshellWithJoinedErr (gethCommand geth subcmd) empty
  where
    subcmd = sendJsSubcommand (gethDataDir geth) js

sendJsSubcommand :: DataDir -> Text -> Text
sendJsSubcommand gDataDir js = format ("--exec '"%s%"' attach "%s)
                                      (shellEscapeSingleQuotes js)
                                      ipcEndpoint
  where
    ipcEndpoint :: Text
    ipcEndpoint = format ("ipc:"%fp) $ gethIpcPath gDataDir

-- pragma solidity ^0.4.0;
--
-- contract SimpleStorage {
--     uint storedData;
--
--     function SimpleStorage(uint initVal) {
--         storedData = initVal;
--     }
--
--     function increment() {
--         storedData = storedData + 1;
--     }
--
--     function get() constant returns (uint) {
--         return storedData;
--     }
-- }

simpleStorage :: Privacy -> Contract
simpleStorage privacy = Contract
  privacy
  ["increment()", "get()"]
  "6060604052341561000c57fe5b6040516020806100fd833981016040528080519060200190919050505b806000819055505b505b60bc806100416000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680636d4ce63c146044578063d09de08a146067575bfe5b3415604b57fe5b60516076565b6040518082815260200191505060405180910390f35b3415606e57fe5b60746081565b005b600060005490505b90565b6001600054016000819055505b5600a165627a7a72305820ce68eb4fb5f27717950dfc3d9e95e23c6ba3815af890ad8705a3a68af19c1ac20029"
  "[{\"constant\":true,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"increment\",\"outputs\":[],\"payable\":false,\"type\":\"function\"},{\"inputs\":[{\"name\":\"initVal\",\"type\":\"uint256\"}],\"payable\":false,\"type\":\"constructor\"}]"
