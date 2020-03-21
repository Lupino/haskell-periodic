{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Periodic.Types.Packet
  ( Magic (..)
  , Msgid (..)
  , msgidLength
  , Packet
  , getPacketData
  , getPacketMagic
  , packetREQ
  , packetRES

  , RegPacket
  , regPacket
  , getClientType
  ) where

import           Data.Binary               (Binary (..), decode, decodeOrFail,
                                            encode)
import           Data.Binary.Get           (getByteString, getWord32be)
import           Data.Binary.Put           (putByteString, putWord32be)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (drop, empty, length)
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           Metro.Class               (GetPacketId (..), RecvPacket (..),
                                            SendPacket (..), SetPacketId (..))
import           Periodic.CRC32            (CRC32 (..), digest)
import           Periodic.Types.ClientType (ClientType (..))
import           Periodic.Types.Error      (Error (..))
import           UnliftIO                  (throwIO)

newtype Msgid = Msgid ByteString
  deriving (Show, Eq)

msgidLength :: Int
msgidLength = 4

data Magic = REQ
    | RES
    deriving (Show, Eq)

instance Binary Magic where
  get = do
    bs <- getByteString 4
    case bs of
      "\0REQ" -> pure REQ
      "\0RES" -> pure RES
      _       -> fail $ "No such magic " ++ show bs

  put REQ = putByteString "\0REQ"
  put RES = putByteString "\0RES"

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
    pid <- getByteString 4
    Packet magic crc (Msgid pid) <$> get
  put (Packet magic _ (Msgid pid) body) = do
    put magic
    putBS $ pid <> toStrict (encode body)

commonRecvPacket f recv = do
    (_, magicbs) <- discoverMagic B.empty recv
    hbs <- recv 4
    crcbs <- recv 4
    case decode (fromStrict hbs) of
      PacketLength len -> do
        bs <- recv len
        case decodeOrFail (fromStrict $ magicbs <> hbs <> crcbs <> bs) of
          Left (_, _, e1)   -> throwIO $ PacketDecodeError $ "Packet: " <> e1
          Right (_, _, pkt) ->
            if digest bs == f pkt then return pkt
                                  else throwIO CRCNotMatch

instance Binary a => RecvPacket (Packet a) where
  recvPacket = commonRecvPacket packetCRC

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
packetREQ = Packet REQ (CRC32 0) (Msgid "0000")

packetRES :: a -> Packet a
packetRES = Packet RES (CRC32 0) (Msgid "0000")

data RegPacket = RegPacket
    { regMagic :: Magic
    , regCRC   :: CRC32
    , regType  :: ClientType
    }
    deriving (Show, Eq)

instance Binary RegPacket where
  get = do
    (magic, crc) <- getHead
    RegPacket magic crc <$> get
  put (RegPacket magic _ body) = do
    put magic
    putBS $ toStrict (encode body)

instance RecvPacket RegPacket where
  recvPacket = commonRecvPacket regCRC

instance SendPacket RegPacket where

regPacket :: ClientType -> RegPacket
regPacket = RegPacket REQ (CRC32 0)

getClientType :: RegPacket -> ClientType
getClientType = regType
