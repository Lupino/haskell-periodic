module Periodic.Transport.XOR
  (
    xorBS
  , makeXORTransport
  ) where

import           Control.Arrow               ((&&&))
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM           (atomically)
import           Data.Bits                   (xor)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as LB
import qualified Periodic.Lock               as L
import           Periodic.Transport          (Transport (..))

xorBS :: TVar LB.ByteString -> B.ByteString -> IO B.ByteString
xorBS ref bs = atomically $ do
  buf <- readTVar ref
  writeTVar ref $ LB.drop len buf
  return . xor' $ LB.take len buf

 where  bs' = LB.fromStrict bs
        len = LB.length bs'
        xor' = B.pack . LB.zipWith xor bs'

makeXORTransport :: LB.ByteString -> Transport -> IO Transport
makeXORTransport key transport = do
  sn <- newTVarIO $ LB.cycle key
  rn <- newTVarIO $ LB.cycle key
  sl <- L.new
  rl <- L.new
  return Transport { recvData = \nbytes -> L.with rl $
                                    xorBS rn =<< recvData transport nbytes
                   , sendData = \bs -> L.with sl $
                                    xorBS sn bs >>= sendData transport
                   , close    = close transport
                   }
