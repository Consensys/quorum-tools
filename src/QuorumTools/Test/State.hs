{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Utilities shared between the public and private state tests
module QuorumTools.Test.State where

import           Control.Monad.Except  (MonadError(..))
import           Control.Monad.Managed (MonadManaged)
import qualified Data.Map              as Map
import           Prelude               hiding (FilePath)
import           Turtle

import qualified QuorumTools.Client       as Client
import           QuorumTools.Control
import           QuorumTools.Types
import           QuorumTools.Test.Outline hiding (verify)
import           QuorumTools.Util                (toInt, HexPrefix(..),
                                                  printHex, intToBytes32)

expectEq
  :: MonadError FailureReason m
  => [(GethId, Int, Either Text Int)]
  -> m ()
expectEq vals =
  let passes (_, val, Right expected) = val == expected
      passes (_,   _,         Left _) = False
  in when (not $ all passes vals) (throwError (WrongValue vals))

-- TODO: ideally this would wait for the particular tx that corresponds to the
-- contract creation.
createContract :: (MonadManaged m, MonadError FailureReason m)
               => Geth -> Contract -> Behavior TxAddrs -> m Addr
createContract geth contract addrs = do
  let initVal = intToBytes32 42
  creationTxEvt <- watch addrs $ \(TxAddrs addrMap) ->
    if not (Map.null addrMap)
    then Just (head (Map.elems addrMap))
    else Nothing
  eTxId <- Client.create geth (CreateArgs contract initVal Sync)
  case eTxId of
    Left msg -> throwError $ RpcFailure msg
    Right _txResult -> return ()

  wait creationTxEvt

incrementStorage :: (MonadIO m, MonadError FailureReason m)
                 => Geth -> TxSync -> Contract -> Addr -> m ()
incrementStorage geth mode (Contract privacy _ _ _) (Addr addrBytes) = do
  let txArgs = Tx (Just addrBytes) "increment()" privacy mode
  eAddr <- Client.sendTransaction geth txArgs
  case eAddr of
    Left msg -> throwError $ RpcFailure msg
    Right _txResult -> return ()

getStorage :: MonadIO io => Contract -> Addr -> Geth -> io (Either Text Int)
getStorage _contract addr geth = do
  resp <- Client.call geth (CallArgs (unAddr addr) "get()")
  pure $ case resp of
    Left msg -> Left msg
    Right bs -> case toInt bs of
      Nothing -> Left $ "couldn't coerce to int: " <> printHex WithPrefix bs
      Just i -> Right i

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
