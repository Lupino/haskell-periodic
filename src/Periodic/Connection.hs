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
  ) where

import qualified Control.Concurrent.Lock   as L (Lock, new, with)
import qualified Data.ByteString.Char8     as B (ByteString, length)
import           Network.Socket            (Socket)
import qualified Network.Socket            as Socket (close)
import           Network.Socket.ByteString (recv, sendAll)

import           Periodic.Utils            (makeHeader, parseHeader)

data Connection = Connection { sock          :: Socket
                             , requestMagic  :: B.ByteString
                             , responseMagic :: B.ByteString
                             , readLock      :: L.Lock
                             , writeLock     :: L.Lock
                             }

magicREQ :: B.ByteString
magicREQ = "\x00REQ"

magicRES :: B.ByteString
magicRES = "\x00RES"

errorMagicNotMatch :: B.ByteString
errorMagicNotMatch = "Magic not match"

newConn :: Socket -> B.ByteString -> B.ByteString -> IO Connection
newConn sock requestMagic responseMagic = do
  readLock <- L.new
  writeLock <- L.new
  return Connection {..}

newServerConn :: Socket -> IO Connection
newServerConn sock = newConn sock magicREQ magicRES

newClientConn :: Socket -> IO Connection
newClientConn sock = newConn sock magicRES magicREQ

receive :: Connection -> IO (Either B.ByteString B.ByteString)
receive (Connection {..}) = do
  L.with readLock $ do
    magic <- recv sock 4
    case magic == requestMagic of
      False -> return $ Left errorMagicNotMatch
      True -> do
        header <- recv sock 4
        dat <- recv sock (parseHeader header)
        return $ Right dat

send :: Connection -> B.ByteString -> IO ()
send (Connection {..}) dat = do
  L.with writeLock $ do
    sendAll sock responseMagic
    sendAll sock (makeHeader $ B.length dat)
    sendAll sock dat

close :: Connection -> IO ()
close (Connection { .. }) = Socket.close sock
