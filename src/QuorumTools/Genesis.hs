{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}

module QuorumTools.Genesis where

import           Control.Lens      (view)
import           Data.Aeson
import           Data.Default      (def)
import qualified Data.Map.Strict   as Map
import           Data.Map.Strict   (Map)
import qualified Data.Text         as T
import           Turtle            hiding (view)
import           Prelude           hiding (FilePath)

import           QuorumTools.Types
import           QuorumTools.Util

createGenesisJson :: (MonadIO m, HasEnv m) => m FilePath
createGenesisJson = do
    consensus <- view clusterConsensus
    jsonPath <- view clusterGenesisJson
    balances <- view clusterInitialBalances
    mode <- view clusterMode
    output jsonPath (contents balances consensus mode)
    return jsonPath

  where
    contents :: Map AccountId Integer -> Consensus -> ClusterMode -> Shell Line
    contents bals consensus mode = select $ textToLines $ textEncode $ object
      [ "alloc"      .= (object $
        map (\(ai, bal) ->
              (accountIdToText ai) .= object ["balance" .= T.pack (show bal)])
            (Map.toList bals) :: Value)
      , "coinbase"   .= addrToText def
      , "config"     .= object
        ([ "homesteadBlock" .= i 100000000
         , "chainId"        .= i 1
         , "eip155Block"    .= i 100000000
         , "eip158Block"    .= i 100000000
         , "isQuorum"       .= (mode == QuorumMode)
         ] <> case consensus of
                Raft _ -> []
                Clique _ -> [ "clique" .= object
                              [ "period" .= i 1
                              , "epoch"  .= i 30000
                              ]
                            ]
                ProofOfWork -> [])
      , "difficulty" .= t "0x0"
      , "extraData"  .=
        case consensus of
          Raft _ -> empty32
          Clique addrs ->
            t $ "0x48616c6c6f2077656c7400000000000000000000000000000000000000000000"
              <> foldMap (printHex WithoutPrefix . unAddr . accountId) addrs
              <> T.replicate (65 * 2) "0"
          ProofOfWork -> empty32
      , "gasLimit"   .= t "0xE0000000"
      , "mixhash"    .= empty32
      , "nonce"      .= t "0x0"
      , "parentHash" .= empty32
      , "timestamp"  .= t "0x00"
      ]

    t = id :: Text -> Text
    i = id :: Int -> Int

    empty32 :: Text
    empty32 = hexPrefixed (def :: Bytes32)
