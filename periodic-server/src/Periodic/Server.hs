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
import           Control.Exception         (SomeException, try)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (head, null)
import           Periodic.Connection       (close, newServerConn, receive)
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
  Socket.close sock

mainLoop :: Socket -> Scheduler -> IO ()
mainLoop sock sched = do
  (sock', _) <- accept sock
  conn <- newServerConn sock'
  e <- try $ receive conn
  case e of
    Left (_::SomeException) -> close conn
    Right pl                ->
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
