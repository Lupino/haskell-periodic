{-# LANGUAGE TypeFamilies #-}
module Periodic.Transport.Socket
  ( Socket
  , rawSocket
  , socketUri
  ) where

import           Network.Socket            (close)
import qualified Network.Socket            as S (Socket)
import           Network.Socket.ByteString (recv, sendAll)
import           Periodic.Socket           (connect)
import           Periodic.Transport

newtype Socket = Socket S.Socket

instance Transport Socket where
  data TransportConfig Socket =
      RawSocket S.Socket
    | SocketUri String
  newTransport (RawSocket soc) = pure $ Socket soc
  newTransport (SocketUri h)   = Socket <$> connect h
  recvData (Socket soc) = recv soc
  sendData (Socket soc) = sendAll soc
  closeTransport (Socket soc) = close soc

rawSocket :: S.Socket -> TransportConfig Socket
rawSocket = RawSocket

socketUri :: String -> TransportConfig Socket
socketUri = SocketUri
