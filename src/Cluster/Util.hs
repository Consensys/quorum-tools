{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO rename this to Cluster.Types.Hex?
module Cluster.Util where

import           Crypto.Hash
import           Data.Aeson
import           Data.Aeson.Types        (typeMismatch)
import qualified Data.ByteArray          as BA
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as B16
import qualified Data.ByteString.Char8   as B8
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.IO            as T
import           System.IO               (BufferMode (..), hSetBuffering)
import           Data.Text.Lazy          (fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding as LT
import           Numeric                 (showHex, readHex)
import           Prelude                 hiding (FilePath, lines)
import           Turtle                  hiding (bytes, prefix, text)
import           Turtle.Pattern          (Pattern, count, hexDigit, skip,
                                          match)

inshellWithJoinedErr :: Text -> Shell Line -> Shell Line
inshellWithJoinedErr cmd inputShell = do
  line <- inshellWithErr cmd inputShell
  case line of
    Left txt  -> return txt
    Right txt -> return txt

tee :: FilePath -> Shell Line -> Shell Line
tee filepath lines = do
  handle <- using $ writeonly filepath
  liftIO $ hSetBuffering handle LineBuffering
  line <- lines
  liftIO $ T.hPutStrLn handle $ lineToText line
  return line

matchOnce :: Pattern a -> Text -> Maybe a
matchOnce pat line = case match pat line of
  [result] -> Just result
  _        -> Nothing

-- TODO: switch to a more efficient version
textEncode :: ToJSON a => a -> Text
textEncode = toStrict . LT.decodeUtf8 . encode

-- TODO: switch to a more efficient version
textDecode :: FromJSON a => Text -> Maybe a
textDecode = decode . LT.encodeUtf8 . fromStrict

-- holds 20 bytes / 40 chars
newtype Bytes20 = Bytes20 { unBytes20 :: ByteString }
  deriving (Eq, Ord)

-- holds 32 bytes / 64 chars
newtype Bytes32 = Bytes32 { unBytes32 :: ByteString }
  deriving (Eq, Ord)

data HexPrefix
  = WithPrefix
  | WithoutPrefix

prefixP :: HexPrefix -> Pattern ()
prefixP WithPrefix    = skip "0x"
prefixP WithoutPrefix = pure ()

bytes20P :: HexPrefix -> Pattern Bytes20
bytes20P parsePre = prefixP parsePre >> (Bytes20 . B8.pack <$> count 40 hexDigit)

bytes32P :: HexPrefix -> Pattern Bytes32
bytes32P parsePre = prefixP parsePre >> (Bytes32 . B8.pack <$> count 64 hexDigit)

instance Show Bytes20 where
  show = T.unpack . hexPrefixed

textToBytes20 :: Text -> Maybe Bytes20
textToBytes20 = matchOnce (bytes20P WithPrefix)

instance FromJSON Bytes20 where
  parseJSON jsonVal
    | String text <- jsonVal
    , Just bytes <- textToBytes20 text
    = pure bytes
    | otherwise = typeMismatch "Bytes20" jsonVal

instance Show Bytes32 where
  show = T.unpack . hexPrefixed

textToBytes32 :: Text -> Maybe Bytes32
textToBytes32 = matchOnce (bytes32P WithPrefix)

instance FromJSON Bytes32 where
  parseJSON jsonVal
    | String text <- jsonVal
    , Just bytes <- textToBytes32 text
    = pure bytes
    | otherwise = typeMismatch "Bytes32" jsonVal

class Hex a where
  toHex :: ByteString -> a
  fromHex :: a -> ByteString
  printHex :: HexPrefix -> a -> Text

instance Hex Bytes20 where
  printHex prefix = printHex prefix . unBytes20
  toHex bs = let len = BS.length bs in
    if | len == 40 -> Bytes20 bs
       | len < 40 -> Bytes20 (B8.replicate (40 - len) '0' <> bs)
       | otherwise -> error "too many bytes for Bytes20"
  fromHex (Bytes20 bs) = bs

instance Hex Bytes32 where
  printHex prefix = printHex prefix . unBytes32
  toHex bs = let len = BS.length bs in
    if | len == 64 -> Bytes32 bs
       | len < 64 -> Bytes32 (B8.replicate (64 - len) '0' <> bs)
       | otherwise -> error "too many bytes for Bytes32"
  fromHex (Bytes32 bs) = bs

instance Hex ByteString where
  printHex prefix = T.decodeUtf8 . maybeAppend
    where maybeAppend = case prefix of
            WithPrefix -> BS.append "0x"
            WithoutPrefix -> id
  toHex = id
  fromHex = id

sha3Bytes :: ByteString -> Bytes32
sha3Bytes = toHex . B16.encode . BS.pack . BA.unpack . hashKeccak
  where hashKeccak :: ByteString -> Digest Keccak_256
        hashKeccak = hash

padAddress :: Bytes20 -> Bytes32
padAddress = toHex . fromHex

intToBytes32 :: Int -> Bytes32
intToBytes32 = toHex . intToHexBS where
  intToHexBS :: Int -> ByteString
  intToHexBS i = B8.pack (showHex i "")

intToBytes20 :: Int -> Bytes20
intToBytes20 = toHex . intToHexBS where
  intToHexBS :: Int -> ByteString
  intToHexBS i = B8.pack (showHex i "")

toInt :: Hex a => a -> Maybe Int
toInt h = case readHex (B8.unpack (fromHex h)) of
  [(i, "")] -> Just i
  _ -> Nothing

hexPrefixed :: Hex a => a -> Text
hexPrefixed = printHex WithPrefix
