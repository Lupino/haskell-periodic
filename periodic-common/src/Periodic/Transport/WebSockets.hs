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

data WebSocket tp = WS WS.Connection tp

instance Transport tp => Transport (WebSocket tp) where
  data TransportConfig (WebSocket tp) =
      WSServer (TransportConfig tp)
    | WSClient (TransportConfig tp) String String
  newTransport (WSServer config) = do
    transport <- newTransport config
    stream <- mkStream transport
    pendingConn <- WS.makePendingConnectionFromStream stream WS.defaultConnectionOptions
    flip WS transport <$> WS.acceptRequest pendingConn
  newTransport (WSClient config host port) = do
    transport <- newTransport config
    stream <- mkStream transport
    flip WS transport <$> WS.newClientConnection stream host port WS.defaultConnectionOptions []

  recvData (WS conn _) = wsRecvData conn
  sendData (WS conn _) = wsSendData conn

  closeTransport (WS _ tp) = closeTransport tp


serverConfig :: Transport tp => TransportConfig tp -> TransportConfig (WebSocket tp)
serverConfig = WSServer

clientConfig :: Transport tp => TransportConfig tp -> String -> String -> TransportConfig (WebSocket tp)
clientConfig = WSClient
