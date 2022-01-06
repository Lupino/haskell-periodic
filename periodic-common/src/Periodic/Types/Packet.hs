{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Periodic.Types.Packet
  ( Magic (..)
  , Packet
  , getPacketData
  , getPacketMagic
  , packetREQ
  , packetRES
  , getResult
  , recvRawPacket

  , RegPacket
  , regPacketREQ
  , regPacketRES
  , getClientType
  , recvRegPacket
  ) where

import           Data.Binary             (Binary (..), decode, decodeOrFail,
                                          encode)
import           Data.Binary.Get         (getByteString, getWord32be)
import           Data.Binary.Put         (putByteString, putWord32be)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B (drop, empty, length)
import           Data.ByteString.Builder (toLazyByteString, word32BE)
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import           Metro.Class             (GetPacketId (..), RecvPacket (..),
                                          SendPacket (..), SetPacketId (..))
import           Periodic.CRC32          (CRC32 (..), digest)
import           Periodic.Types.Error    (Error (..))
import           Periodic.Types.Internal (Msgid (..))
import           UnliftIO                (MonadIO, throwIO)

data Magic = REQ
    | RES
    deriving (Show, Eq)

instance Binary Magic where
  get = do
    bs <- getByteString 4
    case bs of
      "\x00REQ" -> pure REQ
      "\x00RES" -> pure RES
      _         -> fail $ "No such magic " ++ show bs

  put REQ = putByteString "\x00REQ"
  put RES = putByteString "\x00RES"

magicLength :: Int
magicLength = 4

discoverMagic :: Monad m => ByteString -> (Int -> m ByteString) -> m (Magic, ByteString)
discoverMagic "\0REQ" _ = return (REQ, "\0REQ")
discoverMagic "\0RES" _ = return (REQ, "\0RES")
discoverMagic prev recv = do
  bs <- (prev <>) <$> recv 1
  if B.length bs > magicLength then discoverMagic (B.drop (B.length bs - magicLength) bs) recv
                               else discoverMagic bs recv

newtype PacketLength = PacketLength Int
  deriving (Show, Eq)

instance Binary PacketLength where
  get = PacketLength . fromIntegral <$> getWord32be
  put (PacketLength l) = putWord32be $ fromIntegral l

instance Binary CRC32 where
  get = CRC32 <$> getWord32be
  put (CRC32 l) = putWord32be l

putBS bs = do
  put $ PacketLength $ B.length bs
  put $ digest bs
  putByteString bs

getHead = do
  magic <- get
  PacketLength _ <- get
  crc <- get
  return (magic, crc)

data Packet a = Packet
    { packetMagic :: Magic
    , packetCRC   :: CRC32
    , packetId    :: Msgid
    , packetData  :: a
    }
    deriving (Show, Eq)

instance Binary a => Binary (Packet a) where
  get = do
    (magic, crc) <- getHead
    pid <- getWord32be
    Packet magic crc (Msgid pid) <$> get
  put (Packet magic _ (Msgid pid) body) = do
    put magic
    putBS $ enc pid <> toStrict (encode body)
    where enc = toStrict . toLazyByteString . word32BE

commonRecvPacket :: (MonadIO m, Binary pkt) => (pkt -> CRC32) -> (ByteString -> m ()) -> (Int -> m ByteString) -> m pkt
commonRecvPacket f putBack recv = do
    (_, magicbs) <- discoverMagic B.empty recv
    hbs <- recv 4
    crcbs <- recv 4
    case decode (fromStrict hbs) of
      PacketLength len -> do
        bs <- recv len
        let putBackAndThrow e = do
              putBack $ hbs <> crcbs <> bs
              throwIO e

        case decodeOrFail (fromStrict $ magicbs <> hbs <> crcbs <> bs) of
          Left (_, _, e1)   -> putBackAndThrow $ PacketDecodeError $ "Packet: " <> e1
          Right (_, _, pkt) ->
            if digest bs == f pkt then return pkt
                                  else putBackAndThrow CRCNotMatch

recvRawPacket :: (MonadIO m, Binary a) => (ByteString -> m ()) -> (Int -> m ByteString) -> m (Packet a)
recvRawPacket = commonRecvPacket packetCRC

instance Binary a => RecvPacket () (Packet a) where
  recvPacket _ = recvRawPacket

instance Binary a => SendPacket (Packet a) where


instance GetPacketId Msgid (Packet a) where
  getPacketId = packetId

instance SetPacketId Msgid (Packet a) where
  setPacketId k pkt = pkt { packetId = k }


getPacketData :: Packet a -> a
getPacketData = packetData

getPacketMagic :: Packet a -> Magic
getPacketMagic = packetMagic

packetREQ :: a -> Packet a
packetREQ = Packet REQ (CRC32 0) (Msgid 0)

packetRES :: a -> Packet a
packetRES = Packet RES (CRC32 0) (Msgid 0)

getResult :: a -> (b -> a) -> Maybe (Packet b) -> a
getResult defv _ Nothing  = defv
getResult _ f (Just rpkt) = f (getPacketData rpkt)

data RegPacket a = RegPacket
    { regMagic :: Magic
    , regCRC   :: CRC32
    , regType  :: a
    }
    deriving (Show, Eq)

instance Binary a => Binary (RegPacket a) where
  get = do
    (magic, crc) <- getHead
    RegPacket magic crc <$> get
  put (RegPacket magic _ body) = do
    put magic
    putBS $ toStrict (encode body)

recvRegPacket :: (MonadIO m, Binary a) => (ByteString -> m ()) -> (Int -> m ByteString) -> m (RegPacket a)
recvRegPacket = commonRecvPacket regCRC

instance Binary a => RecvPacket () (RegPacket a) where
  recvPacket _ = recvRegPacket

instance Binary a => SendPacket (RegPacket a) where

regPacketREQ :: a -> RegPacket a
regPacketREQ = RegPacket REQ (CRC32 0)

regPacketRES :: a -> RegPacket a
regPacketRES = RegPacket RES (CRC32 0)

getClientType :: RegPacket a -> a
getClientType = regType
