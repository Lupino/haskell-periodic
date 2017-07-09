module Periodic.Transport.XOR
  (
    xorBS
  , makeXORTransport
  ) where

import           Data.Bits            (xor)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import           Data.IORef           (IORef, atomicModifyIORef', newIORef)
import qualified Periodic.Lock        as L
import           Periodic.Transport   (Transport (..))

xorBS :: IORef LB.ByteString -> B.ByteString -> IO B.ByteString
xorBS ref bs = do
  xor' <$> atomicModifyIORef' ref (\v -> (LB.drop len v, LB.take len v))

 where  bs' = LB.fromStrict bs
        len = LB.length bs'
        xor' = B.pack . LB.zipWith xor bs'

makeXORTransport :: LB.ByteString -> Transport -> IO Transport
makeXORTransport key transport = do
  sn <- newIORef $ LB.cycle key
  rn <- newIORef $ LB.cycle key
  sl <- L.new
  rl <- L.new
  return Transport { recvData = \nbytes -> L.with rl $
                                    xorBS rn =<< recvData transport nbytes
                   , sendData = \bs -> L.with sl $
                                    xorBS sn bs >>= sendData transport
                   , close    = close transport
                   }
