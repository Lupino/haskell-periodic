{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE TypeFamilies              #-}
module Periodic.Transport.WebSockets
  ( WebSocket
  , serverConfig
  , clientConfig
  ) where

import           Data.ByteString           (ByteString, empty)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as BL
import           Network.WebSockets        as WS
import qualified Network.WebSockets.Stream as WS
import           Periodic.Transport

mkStream :: Transport tp => tp -> IO WS.Stream
mkStream transport =
  WS.makeStream
    (do
        bs <- recvData transport 8192
        return $ if BC.null bs then Nothing else Just bs)
    (\case
        Nothing -> return ()
        Just bl -> sendData transport $ BL.toStrict bl)

wsRecvData :: WS.Connection -> Int -> IO ByteString
wsRecvData conn _ = do
  msg <- WS.receiveDataMessage conn
  case msg of
    WS.Binary bs -> pure $ BL.toStrict bs
    _            -> pure empty

wsSendData :: WS.Connection -> ByteString -> IO ()
wsSendData conn bs =
  WS.sendDataMessage conn . WS.Binary $ BL.fromStrict bs

newtype WebSocket = WS WS.Connection

instance Transport WebSocket where
  data TransportConfig WebSocket =
      forall tp. (Transport tp) => WSServer (TransportConfig tp)
    | forall tp. (Transport tp) => WSClient (TransportConfig tp) String String
  newTransport (WSServer config) = do
    transport <- newTransport config
    stream <- mkStream transport
    pendingConn <- WS.makePendingConnectionFromStream stream WS.defaultConnectionOptions
    WS <$> WS.acceptRequest pendingConn
  newTransport (WSClient config host port) = do
    transport <- newTransport config
    stream <- mkStream transport
    WS <$> WS.newClientConnection stream host port WS.defaultConnectionOptions []


serverConfig :: Transport tp => TransportConfig tp -> TransportConfig WebSocket
serverConfig = WSServer

clientConfig :: Transport tp => TransportConfig tp -> String -> String -> TransportConfig WebSocket
clientConfig = WSClient
