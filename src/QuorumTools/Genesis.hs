{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}

module QuorumTools.Genesis where

import           Control.Lens      (view)
import           Data.Aeson
import           Data.Default      (def)
import           Turtle            hiding (view)
import           Prelude           hiding (FilePath)

import           QuorumTools.Types
import           QuorumTools.Util

createGenesisJson :: (MonadIO m, HasEnv m) => m FilePath
createGenesisJson = do
    jsonPath <- view clusterGenesisJson
    output jsonPath contents
    return jsonPath

  where
    contents :: Shell Line
    contents = select $ textToLines $ textEncode $ object
      [ "alloc"      .= object []
      , "coinbase"   .= addrToText def
      , "config"     .= object
        [ "homesteadBlock" .= i 100000000
        , "chainId"        .= i 1
        , "eip155Block"    .= i 100000000
        , "eip158Block"    .= i 100000000
        , "isQuorum"       .= True
        ]
      , "difficulty" .= t "0x0"
      , "extraData"  .= t "0x0000000000000000000000000000000000000000000000000000000000000000"
      , "gasLimit"   .= t "0xE0000000"
      , "mixhash"    .= t "0x00000000000000000000000000000000000000647572616c65787365646c6578"
      , "nonce"      .= t "0x0"
      , "parentHash" .= t "0x0000000000000000000000000000000000000000000000000000000000000000"
      , "timestamp"  .= t "0x00"
      ]

    t = id :: Text -> Text
    i = id :: Int -> Int
