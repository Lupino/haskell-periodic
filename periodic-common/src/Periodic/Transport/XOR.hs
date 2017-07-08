module Periodic.Transport.XOR
  (
    xorBS
  , makeXORTransport
  ) where

import           Data.Bits          (xor)
import qualified Data.ByteString    as B
import           Periodic.Transport (Transport (..))

xorBS :: B.ByteString -> B.ByteString -> B.ByteString
xorBS key bs | B.null bs = B.empty
             | otherwise = B.concat [ xor' headBS, xorBS key tailBS ]

  where len = B.length key
        headBS = B.take len bs
        tailBS = B.drop len bs
        xor' = B.pack . B.zipWith xor key

makeXORTransport :: B.ByteString -> Transport -> IO Transport
makeXORTransport key transport = do
  return Transport { recvData = \nbytes -> xorBS key <$> recvData transport nbytes
                   , sendData = \bs -> sendData transport $ xorBS key bs
                   , close    = close transport
                   }
