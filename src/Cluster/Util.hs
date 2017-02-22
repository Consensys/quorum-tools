{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cluster.Util where

import           Crypto.Hash
import           Data.Aeson
import qualified Data.ByteArray          as BA
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as B16
import qualified Data.ByteString.Char8   as B8
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Text.Lazy          (fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding as LT
import           Numeric                 (showHex)

-- TODO: switch to a more efficient version
textEncode :: ToJSON a => a -> Text
textEncode = toStrict . LT.decodeUtf8 . encode

-- TODO: switch to a more efficient version
textDecode :: FromJSON a => Text -> Maybe a
textDecode = decode . LT.encodeUtf8 . fromStrict

-- holds 20 bytes / 40 chars
newtype Bytes20 = Bytes20 { unBytes20 :: ByteString }

-- holds 32 bytes / 64 chars
newtype Bytes32 = Bytes32 { unBytes32 :: ByteString }

instance Show Bytes20 where
  show = T.unpack . hexPrefixed

instance Show Bytes32 where
  show = T.unpack . hexPrefixed

class Hex a where
  toHex :: ByteString -> a
  fromHex :: a -> ByteString
  hexPrefixed :: a -> Text

instance Hex Bytes20 where
  hexPrefixed = hexPrefixed . unBytes20
  toHex bs = let len = BS.length bs in
    if | len == 40 -> Bytes20 bs
       | len < 40 -> Bytes20 (B8.replicate (40 - len) '0' <> bs)
       | otherwise -> error "too many bytes for Bytes20"
  fromHex (Bytes20 bs) = bs

instance Hex Bytes32 where
  hexPrefixed = hexPrefixed . unBytes32
  toHex bs = let len = BS.length bs in
    if | len == 64 -> Bytes32 bs
       | len < 64 -> Bytes32 (B8.replicate (64 - len) '0' <> bs)
       | otherwise -> error "too many bytes for Bytes32"
  fromHex (Bytes32 bs) = bs

instance Hex ByteString where
  hexPrefixed = T.decodeUtf8 . BS.append "0x"
  toHex = id
  fromHex = id

sha3Bytes :: ByteString -> Bytes32
sha3Bytes = toHex . B16.encode . BS.pack . BA.unpack . hashKeccak
  where hashKeccak :: ByteString -> Digest Keccak_256
        hashKeccak = hash

padAddress :: Bytes20 -> Bytes32
padAddress = toHex . fromHex

padIndex :: Int -> Bytes32
padIndex = toHex . hexInt

hexInt :: Int -> ByteString
hexInt i = B8.pack (showHex i "")

