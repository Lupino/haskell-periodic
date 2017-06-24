{-# LANGUAGE OverloadedStrings #-}

module Periodic.Server
  (
    startServer
  ) where

import           Control.Monad             (forever, void)
import           Network                   (PortID, listenOn)
import           Network.Socket            (Socket, accept)

-- server
import           Control.Exception         (try)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (head)
import           Periodic.Connection       (close, newServerConn, receive)
import           Periodic.Server.Client    (newClient)
import           Periodic.Server.Scheduler
import           Periodic.Server.Worker    (newWorker)
import           Periodic.Types            (ClientType (..), Error (..))


startServer :: PortID -> IO ()
startServer portID = do
  sock <- listenOn portID
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
