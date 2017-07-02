{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server
  (
    startServer
  ) where

import           Control.Monad             (forever, void)
import           Network.Socket            (Socket, accept)
import qualified Network.Socket            as Socket (close)

-- process
import           Control.Concurrent        (forkIO, killThread)
import           Control.Concurrent.MVar   (MVar, newEmptyMVar, putMVar,
                                            takeMVar)
import           System.Posix.Signals      (Handler (Catch), installHandler,
                                            sigINT, sigTERM)

-- server
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (head, null)
import           Periodic.Connection       (Connection, close, connid,
                                            newServerConn, receive, send)
import           Periodic.Server.Client    (newClient)
import           Periodic.Server.Scheduler
import           Periodic.Server.Store     (newStore)
import           Periodic.Server.Worker    (newWorker)
import           Periodic.Types            (ClientType (..))
import           Periodic.Utils            (tryIO)

handleExit :: MVar Bool -> IO ()
handleExit mv = putMVar mv True

startServer :: FilePath -> Socket -> IO ()
startServer storePath sock = do
  store <- newStore storePath
  sched <- newScheduler store
  -- Handle dying
  bye <- newEmptyMVar
  void $ installHandler sigTERM (Catch $ handleExit bye) Nothing
  void $ installHandler sigINT (Catch $ handleExit bye) Nothing

  thread <- forkIO $ forever $ do
    -- if accept failed exit
    e <- tryIO $ mainLoop sock sched
    case e of
      Right _ -> return ()
      Left e'  -> do
        print e'
        handleExit bye
  void $ takeMVar bye
  killThread thread
  shutdown sched
  Socket.close sock

mainLoop :: Socket -> Scheduler -> IO ()
mainLoop sock sched = do
  (sock', _) <- accept sock
  void $ forkIO $ handleConnection sock' sched

handleConnection :: Socket -> Scheduler -> IO ()
handleConnection sock sched = do
  conn <- newServerConn sock
  receiveThen conn $ \pl ->
    sendThen conn $ do
      case tp pl of
        Nothing         -> close conn
        Just TypeClient -> void $ newClient conn sched 300
        Just TypeWorker -> void $ newWorker conn sched 300

  where tp :: ByteString -> Maybe ClientType
        tp bs | B.null bs = Nothing
              | v == 1    = Just TypeClient
              | v == 2    = Just TypeWorker
              | otherwise = Nothing
          where v = fromEnum $ B.head bs

        receiveThen :: Connection -> (ByteString -> IO ()) -> IO ()
        receiveThen conn next = do
          e <- tryIO $ receive conn
          case e of
            Left _   -> close conn
            Right pl -> next pl

        sendThen :: Connection -> IO () -> IO ()
        sendThen conn next = do
          e <- tryIO $ send conn (connid conn)
          case e of
            Left _  -> close conn
            Right _ -> next
