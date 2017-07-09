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

import qualified Data.ByteString    as B
import qualified Periodic.Lock      as L (Lock, new, with)
import           Periodic.Transport (Transport (recvData, sendData))
import qualified Periodic.Transport as T (Transport (close))

import           Control.Exception  (throwIO)
import           Control.Monad      (when)
import           Data.IORef         (IORef, atomicModifyIORef', newIORef)
import           Periodic.Types     (Error (..))
import           Periodic.Utils     (makeHeader, maxLength, parseHeader)
import           System.Entropy     (getEntropy)
import           System.Timeout     (timeout)

data Connection = Connection { transport     :: Transport
                             , requestMagic  :: B.ByteString
                             , responseMagic :: B.ByteString
                             , readLock      :: L.Lock
                             , writeLock     :: L.Lock
                             , status        :: IORef Bool
                             , connid        :: B.ByteString
                             , buffer        :: IORef B.ByteString
                             }

magicREQ :: B.ByteString
magicREQ = "\x00REQ"

magicRES :: B.ByteString
magicRES = "\x00RES"

newConn :: Transport -> B.ByteString -> B.ByteString -> IO Connection
newConn transport requestMagic responseMagic = do
  readLock <- L.new
  writeLock <- L.new
  status <- newIORef True
  connid <- getEntropy 4
  buffer <- newIORef B.empty
  return Connection {..}

newServerConn :: Transport -> IO Connection
newServerConn transport = newConn transport magicREQ magicRES

newClientConn :: Transport -> IO Connection
newClientConn transport = newConn transport magicRES magicREQ

receive :: Connection -> IO B.ByteString
receive c@(Connection {..}) = do
  L.with readLock $ do
    magic <- recv' c 4
    case magic == requestMagic of
      False -> do
        setStatus c False
        if B.null magic then throwIO TransportClosed
                        else throwIO MagicNotMatch
      True -> do
        header <- recv' c 4
        ret <- timeout 100000000 $ recv' c (parseHeader header)
        case ret of
          Nothing -> throwIO TransportTimeout
          Just bs -> return bs

recv' :: Connection -> Int -> IO B.ByteString
recv' (Connection {..}) nbytes = do
  buf <- atomicModifyIORef' buffer $ \v -> (B.drop nbytes v, B.take nbytes v)
  if B.length buf == nbytes then return buf
                            else do
                              otherBuf <- readBuf (nbytes - B.length buf)
                              let out = B.concat [ buf, otherBuf ]
                              atomicModifyIORef' buffer $
                                \_ -> (B.drop nbytes out, B.take nbytes out)

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
send (Connection {..}) dat = do
  L.with writeLock $ do
    when (B.length dat > maxLength) $ throwIO DataTooLarge
    sendData transport $ B.concat [ responseMagic, makeHeader $ B.length dat, dat ]

connected :: Connection -> IO Bool
connected (Connection {..}) = atomicModifyIORef' status $ \v -> (v, v)

setStatus :: Connection -> Bool -> IO ()
setStatus (Connection {..}) v' = atomicModifyIORef' status $ \_ -> (v', ())

close :: Connection -> IO ()
close c@(Connection { .. }) = do
  setStatus c False
  T.close transport
