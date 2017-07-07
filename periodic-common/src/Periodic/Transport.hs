module Periodic.Transport
  (
    Transport (..)
  , makeSocketTransport
  ) where

import           Data.ByteString           (ByteString)
import           Network.Socket            (Socket)
import qualified Network.Socket            as Socket (close)
import           Network.Socket.ByteString (recv, sendAll)

data Transport = Transport { recvData :: Int -> IO ByteString
                           , sendData :: ByteString -> IO ()
                           , close    :: IO ()
                           }

makeSocketTransport :: Socket -> IO Transport
makeSocketTransport sock =
  return Transport { recvData = recv sock
                   , sendData = sendAll sock
                   , close    = Socket.close sock
                   }
