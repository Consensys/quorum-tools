{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- Utilities shared between the public and private state tests
module Cluster.StateTestsShared where

import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T

import Prelude hiding (FilePath)
import Turtle
import Checkpoint
import Cluster
import Cluster.Client (call, sendTransaction)
import Cluster.Control
import Cluster.Types
import Cluster.Util (bytes32P, toInt, HexPrefix(..), printHex, inshellWithJoinedErr)
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
createContract geth (Contract privacy _ops mem abi) =
  let pw = gethPassword geth
      cmd = T.unlines
        [ "personal.unlockAccount(eth.accounts[0], '" <> pw <> "');"
        , "var contract = eth.contract(" <> abi <> ");"
        , "var contractInstance = contract.new(42, {"
        , "  from: '" <> showGethAccountId geth <> "',"
        , "  data: '" <> mem <> "',"
        , "  gas: '4700000',"
        , privacyLine privacy
        , "}, function(e, contract) {"
        -- BIG HACK:
        -- Why do we do it this way? The (correct) checkpoint will be output to
        -- the geth log after all...
        --
        -- Because it's much simpler to block on the main thread until the
        -- `sleepBlocks` finishes than it is to coordinate threads (the main
        -- thread and the log listener).
        , "  console.log('RAFT-CHECKPOINT TX-CREATED (0x0000000000000000000000000000000000000000000000000000000000000000, ' + contract.address + ')');"
        , "});"
        , "admin.sleepBlocks(1);"
        ]

      -- look for TX-CREATED result addr
      force = fromMaybe (error "unable to extract addr from contract creation")

      consumer :: Fold Text Addr
      consumer = snd . force <$> find' (matchCheckpoint TxCreated)

  in fold (lineToText <$> sendJs geth cmd) consumer

privacyLine :: Privacy -> Text
privacyLine = \case
  Public -> ""
  PrivateFor addrs ->
    "privateFor: [" <> T.intercalate ", " (quote <$> addrs) <> "],"
  where
    quote :: Secp256k1 -> Text
    quote (Secp256k1 t) = "\"" <> t <> "\""

incrementStorage :: MonadIO io => Geth -> Contract -> Addr -> io ()
incrementStorage geth (Contract privacy _ _ _) (Addr addrBytes) =
  -- TODO: remove "increment()" duplication
  sendTransaction geth (Tx (Just addrBytes) "increment()" privacy Sync)

getStorage :: MonadIO io => Geth -> Contract -> Addr -> io (Either Text Int)
getStorage geth _contract addr = do
  resp <- call geth (CallArgs (unAddr addr) "get()")
  pure $ case resp of
    Left msg -> Left msg
    -- we get this back when not party to a transaction. bug?
    Right "0x" -> Right 0
    Right resp' -> case match (bytes32P WithPrefix) resp' of
      [b32] -> case toInt b32 of
        Nothing -> Left ("couldn't coerce to int: " <> printHex WithPrefix b32)
        Just result -> Right result
      _ -> Left ("failed to find hex string: " <> resp')

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
