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
import qualified Data.ByteString.Char8     as B (ByteString, length, null)
import           Network.Socket            (Socket)
import qualified Network.Socket            as Socket (close)
import           Network.Socket.ByteString (recv, sendAll)

import           Control.Exception         (throwIO)
import           Control.Monad             (when)
import           Periodic.Types            (Error (..))
import           Periodic.Utils            (makeHeader, maxLength, parseHeader)

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

newConn :: Socket -> B.ByteString -> B.ByteString -> IO Connection
newConn sock requestMagic responseMagic = do
  readLock <- L.new
  writeLock <- L.new
  return Connection {..}

newServerConn :: Socket -> IO Connection
newServerConn sock = newConn sock magicREQ magicRES

newClientConn :: Socket -> IO Connection
newClientConn sock = newConn sock magicRES magicREQ

receive :: Connection -> IO B.ByteString
receive (Connection {..}) = do
  L.with readLock $ do
    magic <- recv sock 4
    case magic == requestMagic of
      False -> if B.null magic then throwIO SocketClosed
                               else throwIO MagicNotMatch
      True -> do
        header <- recv sock 4
        recv sock (parseHeader header)

send :: Connection -> B.ByteString -> IO ()
send (Connection {..}) dat = do
  L.with writeLock $ do
    when (B.length dat > maxLength) $ throwIO DataTooLarge
    sendAll sock responseMagic
    sendAll sock (makeHeader $ B.length dat)
    sendAll sock dat

close :: Connection -> IO ()
close (Connection { .. }) = Socket.close sock
