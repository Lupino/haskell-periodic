{-# LANGUAGE OverloadedStrings #-}

module Periodic.Server
  (
    startServer
  ) where

import           Control.Monad             (forever, void)
import           Network.BSD               (getProtocolNumber)
import           Network.Socket            hiding (close)
import qualified Network.Socket            as Socket

-- server
import           Control.Exception         (bracketOnError, try)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (head)
import           Periodic.Connection       (close, newServerConn, receive)
import           Periodic.Server.Client    (newClient)
import           Periodic.Server.Scheduler
import           Periodic.Server.Worker    (newWorker)
import           Periodic.Types            (ClientType (..), Error (..))

listenOn :: Maybe String -> String -> IO Socket
listenOn host port = do
  proto <- getProtocolNumber "tcp"
  -- We should probably specify addrFamily = AF_INET6 and the filter
  -- code below should be removed. AI_ADDRCONFIG is probably not
  -- necessary. But this code is well-tested. So, let's keep it.
  let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]
                           , addrSocketType = Stream
                           , addrProtocol = proto }
  addrs <- getAddrInfo (Just hints) host (Just port)
  -- Choose an IPv6 socket if exists.  This ensures the socket can
  -- handle both IPv4 and IPv6 if v6only is false.
  let addrs' = filter (\x -> addrFamily x == AF_INET6) addrs
      addr = if null addrs' then head addrs else head addrs'
  bracketOnError
      (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
      (Socket.close)
      (\sock -> do
          setSocketOption sock ReuseAddr 1
          bind sock (addrAddress addr)
          listen sock maxListenQueue
          return sock
      )

startServer :: Maybe String -> String -> IO ()
startServer host port = do
  sock <- listenOn host port
  sched <- newScheduler

  forever $ mainLoop sock sched

mainLoop :: Socket -> Scheduler -> IO ()
mainLoop sock sched = do
  (sock', _) <- accept sock
  conn <- newServerConn sock'
  e <- try $ receive conn
  case e of
    Left SocketClosed  -> close conn
    Left MagicNotMatch -> close conn
    Right pl           ->
      case tp pl of
        Nothing         -> close conn
        Just TypeClient -> void $ newClient conn sched
        Just TypeWorker -> void $ newWorker conn sched

  where tp :: ByteString -> Maybe ClientType
        tp bs = if v < minBound || v > maxBound then Nothing
                                                else Just (toEnum v)
          where v = fromEnum $ B.head bs
