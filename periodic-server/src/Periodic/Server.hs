{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server
  (
    startServer
  ) where

import           Control.Monad             (forever, void, when)
import           Network.Socket            (Socket, accept)
import qualified Network.Socket            as Socket (close)

-- process
import           Control.Concurrent        (forkIO, killThread, threadDelay)
import           Control.Concurrent.MVar   (MVar, newEmptyMVar, takeMVar,
                                            tryPutMVar)
import           System.Posix.Signals      (Handler (Catch), installHandler,
                                            sigINT, sigTERM)

-- server
import           Control.Exception         (SomeException, try)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (head, null)
import           Data.Int                  (Int64)
import           Periodic.Connection       (Connection, close, connid,
                                            newServerConn, receive, send)
import           Periodic.IOHashMap        (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap        as HM
import           Periodic.Server.Client    (newClient, runClient, startClient)
import qualified Periodic.Server.Client    as Client
import           Periodic.Server.Scheduler
import           Periodic.Server.Worker    (newWorker, runWorker, startWorker)
import qualified Periodic.Server.Worker    as Worker
import           Periodic.Transport        (Transport)
import           Periodic.Types            (ClientType (..))
import           Periodic.Utils            (getEpochTime, tryIO)
import           System.Log.Logger         (errorM)

type ClientList = IOHashMap ByteString Client.Connection
type WorkerList = IOHashMap ByteString Worker.Connection

handleExit :: MVar () -> IO ()
handleExit mv = void $ tryPutMVar mv ()

startServer :: (Socket -> IO Transport) -> FilePath -> Socket -> IO ()
startServer makeTransport storePath sock = do
  -- Handle dying
  bye <- newEmptyMVar
  void $ installHandler sigTERM (Catch $ handleExit bye) Nothing
  void $ installHandler sigINT (Catch $ handleExit bye) Nothing

  sched <- newScheduler storePath $ handleExit bye

  clientList <- newIOHashMap
  workerList <- newIOHashMap

  runCheckClientState clientList 100
  runCheckWorkerState workerList 100

  thread <- forkIO $ forever $ do
    -- if accept failed exit
    e <- tryIO $ mainLoop makeTransport sock sched clientList workerList
    case e of
      Right _ -> return ()
      Left e'  -> do
        print e'
        handleExit bye

  takeMVar bye
  killThread thread
  shutdown sched
  Socket.close sock

mainLoop :: (Socket -> IO Transport) -> Socket -> Scheduler -> ClientList -> WorkerList -> IO ()
mainLoop makeTransport sock sched clientList workerList = do
  (sock', _) <- accept sock
  void $ forkIO $ handleConnection sched clientList workerList =<< makeTransport sock'

handleConnection :: Scheduler -> ClientList -> WorkerList -> Transport -> IO ()
handleConnection sched clientList workerList transport = do
  conn <- newServerConn transport
  receiveThen conn $ \pl ->
    sendThen conn $
      case tp pl of
        Nothing         -> close conn
        Just TypeClient -> do
          client <- newClient conn sched
          HM.insert clientList (connid conn) client
          startClient client $ HM.delete clientList (connid conn)
        Just TypeWorker -> do
          worker <- newWorker conn sched
          HM.insert workerList (connid conn) worker
          startWorker worker $ HM.delete workerList (connid conn)

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

runCheckWorkerState :: WorkerList -> Int64 -> IO ()
runCheckWorkerState ref alive = runCheckState "Worker" ref (checkWorkerState ref alive) alive

checkWorkerState :: WorkerList -> Int64 -> Worker.Connection -> IO ()
checkWorkerState ref alive w = do
  expiredAt <- (alive +) <$> runWorker w Worker.getLastVist
  now <- getEpochTime
  when (now > expiredAt) $ do
    runWorker w Worker.close
    wid <- runWorker w Worker.workerId
    HM.delete ref wid

runCheckClientState :: ClientList -> Int64 -> IO ()
runCheckClientState ref alive = runCheckState "Client" ref (checkClientState ref alive) alive

checkClientState :: ClientList -> Int64 -> Client.Connection -> IO ()
checkClientState ref alive c = do
  expiredAt <- (alive +) <$> runClient c Client.getLastVist
  now <- getEpochTime
  when (now > expiredAt) $ do
    runClient c Client.close
    cid <- runClient c Client.clientId
    HM.delete ref cid

runCheckState :: String -> IOHashMap a b -> (b -> IO ()) -> Int64 -> IO ()
runCheckState var ref checkAlive alive = void . forkIO . forever $ do
  threadDelay $ fromIntegral alive * 1000 * 1000
  mapM_ checkAlive =<< HM.elems ref
  size <- HM.size ref
  errorM "Periodic.Server" $ "Total " ++ var ++ ": " ++ show size
