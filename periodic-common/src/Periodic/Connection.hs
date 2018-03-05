{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Connection
  (
    Connection
  , magicREQ
  , magicRES
  , newConn
  , newServerConn
  , newClientConn
  , receive
  , send
  , close
  , connected
  , connid
  ) where

import           Data.Byteable               (toBytes)
import qualified Data.ByteString             as B
import qualified Periodic.Lock               as L (Lock, new, with)
import           Periodic.Transport          (Transport (recvData, sendData))
import qualified Periodic.Transport          as T (Transport (close))

import           Control.Concurrent.STM.TVar
import           Control.Exception           (throwIO)
import           Control.Monad               (when)
import           Control.Monad.STM           (atomically)
import           Periodic.Types              (Error (..), runParser)
import           Periodic.Types.Packet
import           Periodic.Utils              (maxLength)
import           System.Entropy              (getEntropy)
import           System.Timeout              (timeout)

data Connection = Connection { transport     :: Transport
                             , requestMagic  :: B.ByteString
                             , responseMagic :: B.ByteString
                             , readLock      :: L.Lock
                             , writeLock     :: L.Lock
                             , status        :: TVar Bool
                             , connid        :: B.ByteString
                             , buffer        :: TVar B.ByteString
                             }

magicREQ :: B.ByteString
magicREQ = "\x00REQ"

magicRES :: B.ByteString
magicRES = "\x00RES"

newConn :: Transport -> B.ByteString -> B.ByteString -> IO Connection
newConn transport requestMagic responseMagic = do
  readLock <- L.new
  writeLock <- L.new
  status <- newTVarIO True
  connid <- getEntropy 4
  buffer <- newTVarIO B.empty
  return Connection {..}

newServerConn :: Transport -> IO Connection
newServerConn transport = newConn transport magicREQ magicRES

newClientConn :: Transport -> IO Connection
newClientConn transport = newConn transport magicRES magicREQ

receive :: Connection -> IO B.ByteString
receive c@Connection{..} =
  L.with readLock $ do
    hdr <- recv' c 8
    when (B.null hdr || B.length hdr < 8) $ throwIO TransportClosed
    case runParser hdr of
      Left _ -> throwIO MagicNotMatch
      Right PacketHdr{..} -> do
        if packetMagic == requestMagic then do
          ret <- timeout 100000000 $ recv' c packetSize
          case ret of
            Nothing -> throwIO TransportTimeout
            Just bs -> return bs
        else throwIO MagicNotMatch

recv' :: Connection -> Int -> IO B.ByteString
recv' Connection{..} nbytes = do
  buf <- atomically $ do
    bf <- readTVar buffer
    writeTVar buffer $! B.drop nbytes bf
    return $ B.take nbytes bf
  if B.length buf == nbytes then return buf
                            else do
                              otherBuf <- readBuf (nbytes - B.length buf)
                              let out = B.concat [ buf, otherBuf ]
                              atomically . writeTVar buffer $! B.drop nbytes out
                              return $ B.take nbytes out

  where readBuf :: Int -> IO B.ByteString
        readBuf 0  = return B.empty
        readBuf nb = do
          buf <- recvData transport 1024
          when (B.null buf) $ throwIO TransportClosed
          if B.length buf >= nb then return buf
                                else do
                                  otherBuf <- readBuf (nb - B.length buf)
                                  return $ B.concat [ buf, otherBuf ]



send :: Connection -> B.ByteString -> IO ()
send Connection{..} dat =
  L.with writeLock $ do
    when (B.length dat > maxLength) $ throwIO DataTooLarge
    sendData transport $ toBytes Packet
      { packetHdr = PacketHdr
        { packetMagic = responseMagic
        , packetSize = B.length dat
        }
      , packetData = dat
      }

connected :: Connection -> IO Bool
connected Connection{..} = readTVarIO status

close :: Connection -> IO ()
close Connection{..} = do
  atomically $ writeTVar status False
  T.close transport
