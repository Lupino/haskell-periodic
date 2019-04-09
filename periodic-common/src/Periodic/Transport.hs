{-# LANGUAGE TypeFamilies #-}
module Periodic.Transport
  ( Transport (..)
  ) where

import           Data.ByteString (ByteString)

class Transport transport where
  data TransportConfig transport
  newTransport   :: TransportConfig transport -> IO transport
  recvData       :: transport -> Int -> IO ByteString
  sendData       :: transport -> ByteString -> IO ()
  closeTransport :: transport -> IO ()
