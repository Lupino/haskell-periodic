{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Periodic.Connection
  ( ConnEnv
  , ConnectionT
  , FromConn (..)
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

import           Control.Monad              (when)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.Byteable              (toBytes)
import qualified Data.ByteString            as B
import           Data.Int                   (Int32)
import           Periodic.CRC32             as CRC (digest)
import qualified Periodic.Lock              as L (Lock, new, with)
import           Periodic.Transport         (Transport (recvData, sendData))
import qualified Periodic.Transport         as T (Transport (close))
import           Periodic.Types             (Error (..), runParser)
import           Periodic.Types.Packet
import           System.Entropy             (getEntropy)
import           UnliftIO

maxLength :: Int
maxLength = fromIntegral (maxBound :: Int32)

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
    )

instance MonadUnliftIO m => MonadUnliftIO (ConnectionT m) where
  askUnliftIO = ConnectionT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runConnectionT r))
  withRunInIO inner = ConnectionT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runConnectionT r)

class FromConn n m where
  fromConn :: ConnectionT n a -> m n a

runConnectionT :: ConnEnv -> ConnectionT m a -> m a
runConnectionT connEnv = flip runReaderT connEnv . unConnectionT

initConnEnv :: MonadIO m => Transport -> B.ByteString -> B.ByteString -> m ConnEnv
initConnEnv transport requestMagic responseMagic = do
  connectionid <- liftIO $ getEntropy 4
  readLock <- L.new
  writeLock <- L.new
  status <- newTVarIO True
  buffer <- newTVarIO B.empty
  return ConnEnv{..}


initServerConnEnv :: MonadIO m => Transport -> m ConnEnv
initServerConnEnv transport = initConnEnv transport magicREQ magicRES

initClientConnEnv :: MonadIO m => Transport -> m ConnEnv
initClientConnEnv transport = initConnEnv transport magicRES magicREQ

receive :: MonadUnliftIO m => ConnectionT m B.ByteString
receive = do
  connEnv@ConnEnv{..} <- ask
  L.with readLock $ do
    hdr <- recv' connEnv 12
    when (B.null hdr || B.length hdr < 12) $ throwIO TransportClosed
    case runParser hdr of
      Left _ -> throwIO MagicNotMatch
      Right PacketHdr{..} ->
        if packetMagic == requestMagic then do
          ret <- timeout 100000000 $ recv' connEnv $ fromIntegral packetSize
          case ret of
            Nothing -> throwIO TransportTimeout
            Just bs ->
              if CRC.digest bs == packetCRC then return bs
                                            else throwIO CRCNotMatch
        else throwIO MagicNotMatch

recv' :: MonadIO m => ConnEnv -> Int -> m B.ByteString
recv' ConnEnv{..} nbytes = do
  buf <- atomically $ do
    bf <- readTVar buffer
    writeTVar buffer $! B.drop nbytes bf
    return $! B.take nbytes bf
  if B.length buf == nbytes then return buf
                            else do
                              otherBuf <- liftIO $ readBuf (nbytes - B.length buf)
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

send :: MonadUnliftIO m => B.ByteString -> ConnectionT m ()
send dat = do
  ConnEnv{..} <- ask
  L.with writeLock $ do
    when (B.length dat > maxLength) $ throwIO DataTooLarge
    liftIO $ sendData transport $ toBytes Packet
      { packetHdr = PacketHdr
        { packetMagic = responseMagic
        , packetSize = fromIntegral $ B.length dat
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
  atomically $ writeTVar status False
  liftIO $ T.close transport

statusTVar :: Monad m => ConnectionT m (TVar Bool)
statusTVar = status <$> ask
