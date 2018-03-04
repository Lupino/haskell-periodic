{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Connection
  (
    ConnectionState
  , ConnectionConfig
  , ConnectionT
  , runConnectionT
  , initServerConnectionConfig
  , initClientConnectionConfig
  , initConnectionConfig
  , initConnectionState
  , receive
  , send
  , close
  , connected
  , connid
  , connid'
  ) where

import qualified Data.ByteString            as B
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import qualified Periodic.Lock              as L (Lock, new, with)
import           Periodic.Transport         (Transport (recvData, sendData))
import qualified Periodic.Transport         as T (Transport (close))

import           Control.Concurrent.STM
import           Control.Exception          (throwIO)
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.STM          (atomically)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State  (StateT, evalStateT, get, gets)
import           Data.Binary                (Binary)
import qualified Data.Binary                as Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Periodic.Types             (Error (..))
import           Periodic.Utils             (maxLength)
import           System.Entropy             (getEntropy)
import           System.Timeout             (timeout)

data Header = Header { _magic :: B.ByteString
                     , _size  :: Int
                     }

instance Binary Header where
  get = do
    _magic <- getByteString 4
    _size <- fromIntegral <$> getWord32be
    return Header{..}
  put Header{..} = do
    putByteString _magic
    putWord32be $ fromIntegral _size

magicREQ :: B.ByteString
magicREQ = "\x00REQ"

magicRES :: B.ByteString
magicRES = "\x00RES"

data ConnectionConfig = ConnectionConfig
  { transport     :: Transport
  , requestMagic  :: B.ByteString
  , responseMagic :: B.ByteString
  , connectionid  :: B.ByteString
  , readLock      :: L.Lock
  , writeLock     :: L.Lock
  }

data ConnectionState = ConnectionState
  { buffer :: TVar B.ByteString
  , status :: TVar Bool
  }

type ConnectionT m = StateT ConnectionState (ReaderT ConnectionConfig m)

runConnectionT :: Monad m => ConnectionState -> ConnectionConfig -> ConnectionT m a -> m a
runConnectionT connectionState connectionConfig =
  flip runReaderT connectionConfig . flip evalStateT connectionState

initConnectionConfig :: Transport -> B.ByteString -> B.ByteString -> IO ConnectionConfig
initConnectionConfig transport requestMagic responseMagic = do
  connid <- getEntropy 4
  readLock <- L.new
  writeLock <- L.new
  return ConnectionConfig{..}

initConnectionState :: IO ConnectionState
initConnectionState = do
  status <- newTVarIO True
  buffer <- newTVarIO B.empty
  return ConnectionState{..}

initServerConnectionConfig :: Transport -> IO ConnectionConfig
initServerConnectionConfig transport = initConnectionConfig transport magicREQ magicRES

initClientConnectionConfig :: Transport -> IO ConnectionConfig
initClientConnectionConfig transport = initConnectionConfig transport magicRES magicREQ

receive :: MonadIO m => ConnectionT m B.ByteString
receive = do
  state <- get
  config <- lift $ ask
  liftIO $ L.with (readLock config) $ do
    header <- recv' state config 8
    when (B.null header || B.length header < 8) $ throwIO TransportClosed
    let Header {..} = Binary.decode $ fromStrict header
    if _magic == requestMagic config then do
      ret <- timeout 100000000 $ recv' state config _size
      case ret of
        Nothing -> throwIO TransportTimeout
        Just bs -> return bs
    else throwIO MagicNotMatch

recv' :: ConnectionState -> ConnectionConfig -> Int -> IO B.ByteString
recv' ConnectionState{..} ConnectionConfig{..} nbytes = do
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

send :: MonadIO m => B.ByteString -> ConnectionT m ()
send dat = do
  ConnectionConfig{..} <- lift $ ask
  liftIO $ L.with writeLock $ do
    when (B.length dat > maxLength) $ throwIO DataTooLarge
    sendData transport $ B.concat
      [ toStrict . Binary.encode . Header responseMagic $ B.length dat, dat ]

connected :: MonadIO m => ConnectionT m Bool
connected = liftIO . readTVarIO =<< gets status

connid :: Monad m => ConnectionT m B.ByteString
connid = lift $ asks connectionid

connid' :: ConnectionConfig -> B.ByteString
connid' = connectionid

close :: MonadIO m => ConnectionT m ()
close = do
  ConnectionState{..} <- get
  ConnectionConfig{..} <- lift ask
  liftIO . atomically $ writeTVar status False
  liftIO $ T.close transport
