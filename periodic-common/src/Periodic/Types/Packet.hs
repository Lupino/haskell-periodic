{-# LANGUAGE RecordWildCards #-}
module Periodic.Types.Packet
  (
    PacketHdr (..)
  , Packet (..)
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Digest.CRC32       (CRC32 (..))
import           Periodic.Types.Internal

data PacketHdr = PacketHdr
  { packetMagic :: ByteString
  , packetSize  :: Int
  , packetCRC   :: CRC32
  }

instance Binary PacketHdr where
  get = do
    packetMagic <- getByteString 4
    packetSize <- fromIntegral <$> getWord32be
    packetCRC <- CRC32 <$> getWord32be
    return PacketHdr{..}
  put PacketHdr{..} = do
    putByteString packetMagic
    putWord32be $ fromIntegral packetSize
    putWord32be $ crc32 packetCRC

instance Byteable PacketHdr where
  toBytes = toStrict . encode

instance Parser PacketHdr where
  runParser = parseBinary

data Packet = Packet
  { packetHdr  :: PacketHdr
  , packetData :: ByteString
  }

instance Binary Packet where
    get = do
        hdr <- get
        dat <- getByteString $ fromIntegral (packetSize hdr)
        return $ Packet hdr dat

    put (Packet hdr dat) = do
        put hdr
        putByteString dat

instance Byteable Packet where
  toBytes = toStrict . encode

instance Parser Packet where
  runParser = parseBinary
