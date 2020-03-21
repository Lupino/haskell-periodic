{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Periodic.Transport.XOR
  ( xorBS
  , XOR
  , xorConfig
  ) where

import           Data.Bits            (xor)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import           Metro.Class          (Transport (..))
import qualified Metro.Lock           as L
import           UnliftIO

data XOR tp = XOR
    { transport :: tp
    , sn        :: TVar LB.ByteString
    , rn        :: TVar LB.ByteString
    , sl        :: L.Lock
    , rl        :: L.Lock
    }

instance Transport tp => Transport (XOR tp) where
  data TransportConfig (XOR tp) = XORConfig FilePath (TransportConfig tp)
  newTransport (XORConfig fn config) = do
    transport <- newTransport config
    key <- LB.readFile fn
    sn <- newTVarIO $ LB.cycle key
    rn <- newTVarIO $ LB.cycle key
    sl <- L.new
    rl <- L.new
    return XOR {..}

  recvData XOR {..} nbytes = L.with rl $ xorBS rn =<< recvData transport nbytes
  sendData XOR {..} bs = L.with sl $ xorBS sn bs >>= sendData transport
  closeTransport XOR {..} = closeTransport transport

xorBS :: TVar LB.ByteString -> B.ByteString -> IO B.ByteString
xorBS ref bs = atomically $ do
  buf <- readTVar ref
  writeTVar ref $! LB.drop len buf
  return . xor' $! LB.take len buf

 where  bs' = LB.fromStrict bs
        len = LB.length bs'
        xor' = B.pack . LB.zipWith xor bs'

xorConfig :: FilePath -> TransportConfig tp -> TransportConfig (XOR tp)
xorConfig = XORConfig
