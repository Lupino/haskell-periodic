module Periodic.Transport.WebSockets
  ( makeServerTransport
  , makeClientTransport
  ) where

import           Data.ByteString           (ByteString, empty)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as BL
import           Network.WebSockets        as WS
import qualified Network.WebSockets.Stream as WS
import           Periodic.Transport        (Transport (..))

mkStream :: Transport -> IO WS.Stream
mkStream transport =
  WS.makeStream
    (do
        bs <- recvData transport 8192
        return $ if BC.null bs then Nothing else Just bs)
    (\mbBl -> case mbBl of
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

makeServerTransport :: Transport -> IO Transport
makeServerTransport transport = do
  stream <- mkStream transport
  pendingConn <- WS.makePendingConnectionFromStream stream WS.defaultConnectionOptions
  conn <- WS.acceptRequest pendingConn
  return Transport
    { recvData = wsRecvData conn
    , sendData = wsSendData conn
    , close = close transport
    }

makeClientTransport :: String -> String -> Transport -> IO Transport
makeClientTransport host port transport = do
  stream <- mkStream transport
  conn <- WS.newClientConnection stream host port WS.defaultConnectionOptions []
  return Transport
    { recvData = wsRecvData conn
    , sendData = wsSendData conn
    , close = close transport
    }
