{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Periodic.Connection
  (
    ConnEnv
  , ConnectionT
  , runConnectionT
  , initServerConnEnv
  , initClientConnEnv
  , initConnEnv
  , receive
  , send
  , close
  , connid
  , connid'
  , statusTVar
  ) where

import           Control.Concurrent.STM      (TVar, newTVarIO, readTVar,
                                              writeTVar)
import           Control.Exception           (throwIO)
import           Control.Monad               (when)
import           Control.Monad.Base
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Reader.Class  (MonadReader (ask), asks)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import           Data.Byteable               (toBytes)
import qualified Data.ByteString             as B
import           Periodic.CRC32              as CRC (digest)
import qualified Periodic.Lock               as L (Lock, new, with)
import           Periodic.Transport          (Transport (recvData, sendData))
import qualified Periodic.Transport          as T (Transport (close))
import           Periodic.Types              (Error (..), runParser)
import           Periodic.Types.Packet
import           Periodic.Utils              (maxLength)
import           System.Entropy              (getEntropy)
import           System.Timeout              (timeout)

magicREQ :: B.ByteString
magicREQ = "\x00REQ"

magicRES :: B.ByteString
magicRES = "\x00RES"

data ConnEnv = ConnEnv
  { transport     :: Transport
  , requestMagic  :: B.ByteString
  , responseMagic :: B.ByteString
  , connectionid  :: B.ByteString
  , readLock      :: L.Lock
  , writeLock     :: L.Lock
  , buffer        :: TVar B.ByteString
  , status        :: TVar Bool
  }

newtype ConnectionT m a = ConnectionT { unConnectionT :: ReaderT ConnEnv m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadReader ConnEnv
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

deriving instance MonadBase IO m => MonadBase IO (ConnectionT m)

instance MonadTransControl ConnectionT where
  type StT ConnectionT a = StT (ReaderT ConnEnv) a
  liftWith = defaultLiftWith ConnectionT unConnectionT
  restoreT = defaultRestoreT ConnectionT

instance MonadBaseControl IO m => MonadBaseControl IO (ConnectionT m) where
  type StM (ConnectionT m) a = ComposeSt ConnectionT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

runConnectionT :: ConnEnv -> ConnectionT m a -> m a
runConnectionT connEnv = flip runReaderT connEnv . unConnectionT

initConnEnv :: Transport -> B.ByteString -> B.ByteString -> IO ConnEnv
initConnEnv transport requestMagic responseMagic = do
  connectionid <- getEntropy 4
  readLock <- L.new
  writeLock <- L.new
  status <- newTVarIO True
  buffer <- newTVarIO B.empty
  return ConnEnv{..}


initServerConnEnv :: Transport -> IO ConnEnv
initServerConnEnv transport = initConnEnv transport magicREQ magicRES

initClientConnEnv :: Transport -> IO ConnEnv
initClientConnEnv transport = initConnEnv transport magicRES magicREQ

receive :: MonadIO m => ConnectionT m B.ByteString
receive = do
  connEnv@ConnEnv{..} <- ask
  liftIO $ L.with readLock $ do
    hdr <- recv' connEnv 12
    when (B.null hdr || B.length hdr < 12) $ throwIO TransportClosed
    case runParser hdr of
      Left _ -> throwIO MagicNotMatch
      Right PacketHdr{..} ->
        if packetMagic == requestMagic then do
          ret <- timeout 100000000 $ recv' connEnv packetSize
          case ret of
            Nothing -> throwIO TransportTimeout
            Just bs ->
              if CRC.digest bs == packetCRC then return bs
                                            else throwIO CRCNotMatch
        else throwIO MagicNotMatch

recv' :: ConnEnv -> Int -> IO B.ByteString
recv' ConnEnv{..} nbytes = do
  buf <- atomically $ do
    bf <- readTVar buffer
    writeTVar buffer $! B.drop nbytes bf
    return $! B.take nbytes bf
  if B.length buf == nbytes then return buf
                            else do
                              otherBuf <- readBuf (nbytes - B.length buf)
                              let out = B.concat [ buf, otherBuf ]
                              atomically . writeTVar buffer $! B.drop nbytes out
                              return $! B.take nbytes out

  where readBuf :: Int -> IO B.ByteString
        readBuf 0  = return B.empty
        readBuf nb = do
          buf <- recvData transport 1024
          when (B.null buf) $ throwIO TransportClosed
          if B.length buf >= nb then return buf
                                else do
                                  otherBuf <- readBuf (nb - B.length buf)
                                  return $! B.concat [ buf, otherBuf ]

send :: MonadIO m => B.ByteString -> ConnectionT m ()
send dat = do
  ConnEnv{..} <- ask
  liftIO $ L.with writeLock $ do
    when (B.length dat > maxLength) $ throwIO DataTooLarge
    sendData transport $ toBytes Packet
      { packetHdr = PacketHdr
        { packetMagic = responseMagic
        , packetSize = B.length dat
        , packetCRC  = CRC.digest dat
        }
      , packetData = dat
      }

connid :: Monad m => ConnectionT m B.ByteString
connid = asks connectionid

connid' :: ConnEnv -> B.ByteString
connid' = connectionid

close :: MonadIO m => ConnectionT m ()
close = do
  ConnEnv{..} <- ask
  liftIO . atomically $ writeTVar status False
  liftIO $ T.close transport

statusTVar :: Monad m => ConnectionT m (TVar Bool)
statusTVar = status <$> ask
