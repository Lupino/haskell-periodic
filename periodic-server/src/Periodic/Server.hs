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
import           Control.Concurrent.MVar   (MVar, newEmptyMVar, takeMVar,
                                            tryPutMVar)
import           System.Posix.Signals      (Handler (Catch), installHandler,
                                            sigINT, sigTERM)

-- server
import           Control.Exception         (SomeException, try)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (head, null)
import           Periodic.Connection       (Connection, close, connid,
                                            newServerConn, receive, send)
import           Periodic.Server.Client    (newClient)
import           Periodic.Server.Scheduler
import           Periodic.Server.Worker    (newWorker)
import           Periodic.Transport        (Transport)
import           Periodic.Types            (ClientType (..))
import           Periodic.Utils            (tryIO)

handleExit :: MVar () -> IO ()
handleExit mv = void $ tryPutMVar mv ()

startServer :: (Socket -> IO Transport) -> FilePath -> Socket -> IO ()
startServer makeTransport storePath sock = do
  -- Handle dying
  bye <- newEmptyMVar
  void $ installHandler sigTERM (Catch $ handleExit bye) Nothing
  void $ installHandler sigINT (Catch $ handleExit bye) Nothing

  sched <- newScheduler storePath $ handleExit bye

  thread <- forkIO $ forever $ do
    -- if accept failed exit
    e <- tryIO $ mainLoop makeTransport sock sched
    case e of
      Right _ -> return ()
      Left e'  -> do
        print e'
        handleExit bye

  takeMVar bye
  killThread thread
  shutdown sched
  Socket.close sock

mainLoop :: (Socket -> IO Transport) -> Socket -> Scheduler -> IO ()
mainLoop makeTransport sock sched = do
  (sock', _) <- accept sock
  void $ forkIO $ handleConnection sched =<< makeTransport sock'

handleConnection :: Scheduler -> Transport -> IO ()
handleConnection sched transport = do
  conn <- newServerConn transport
  receiveThen conn $ \pl ->
    sendThen conn $
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
          e <- try $ receive conn
          case e of
            Left (_ :: SomeException) -> close conn
            Right pl                  -> next pl

        sendThen :: Connection -> IO () -> IO ()
        sendThen conn next = do
          e <- try $ send conn (connid conn)
          case e of
            Left (_ :: SomeException) -> close conn
            Right _                   -> next
