{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}

module QuorumTools.Genesis where

import           Control.Lens      (view)
import           Data.Aeson
import           Data.Default      (def)
import qualified Data.Text         as T
import           Turtle            hiding (view)
import           Prelude           hiding (FilePath)

import           QuorumTools.Types
import           QuorumTools.Util

createGenesisJson :: (MonadIO m, HasEnv m) => m FilePath
createGenesisJson = do
    consensus <- view clusterConsensus
    jsonPath <- view clusterGenesisJson
    output jsonPath (contents consensus)
    return jsonPath

  where
    contents :: Consensus -> Shell Line
    contents consensus = select $ textToLines $ textEncode $ object
      [ "alloc"      .= object []
      , "coinbase"   .= addrToText def
      , "config"     .= object
        ([ "homesteadBlock" .= i 100000000
         , "chainId"        .= i 1
         , "eip155Block"    .= i 100000000
         , "eip158Block"    .= i 100000000
         , "isQuorum"       .= True
         ] <> case consensus of
                Raft _ -> []
                Clique _ -> [ "clique" .= object
                              [ "period" .= i 1
                              , "epoch"  .= i 30000
                              ]
                            ])
      , "difficulty" .= t "0x0"
      , "extraData"  .=
        case consensus of
          Raft _ -> empty32
          Clique addrs ->
            t $ "0x48616c6c6f2077656c7400000000000000000000000000000000000000000000"
              <> foldMap (printHex WithoutPrefix . unAddr . accountId) addrs
              <> T.replicate (65 * 2) "0"
      , "gasLimit"   .= t "0xE0000000"
      , "mixhash"    .=
        case consensus of
          Raft _ -> t "0x00000000000000000000000000000000000000647572616c65787365646c6578"
          Clique _ -> empty32
      , "nonce"      .= t "0x0"
      , "parentHash" .= empty32
      , "timestamp"  .= t "0x00"
      ]

    t = id :: Text -> Text
    i = id :: Int -> Int

    empty32 :: Text
    empty32 = hexPrefixed (def :: Bytes32)
