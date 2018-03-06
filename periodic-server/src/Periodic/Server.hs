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
import           Control.Exception         (SomeException)
import           Control.Monad.Catch       (try)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString           (ByteString)
import           Data.Int                  (Int64)
import           Periodic.Connection
import qualified Periodic.Connection       as Conn
import           Periodic.IOHashMap        (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap        as HM
import           Periodic.Server.Client
import qualified Periodic.Server.Client    as Client
import           Periodic.Server.Scheduler
import           Periodic.Server.Worker
import qualified Periodic.Server.Worker    as Worker
import           Periodic.Transport        (Transport)
import           Periodic.Types            (ClientType (..), runParser)
import           Periodic.Utils            (getEpochTime, tryIO)
import           System.Log.Logger         (errorM)

type ClientList = IOHashMap ByteString (ClientEnv IO)
type WorkerList = IOHashMap ByteString (WorkerEnv IO)

handleExit :: MVar () -> IO ()
handleExit mv = void $ tryPutMVar mv ()

startServer :: (Socket -> IO Transport) -> FilePath -> Socket -> IO ()
startServer makeTransport storePath sock = do
  -- Handle dying
  bye <- newEmptyMVar
  void $ installHandler sigTERM (Catch $ handleExit bye) Nothing
  void $ installHandler sigINT (Catch $ handleExit bye) Nothing

  schedConfig <- initSchedConfig storePath $ handleExit bye
  schedState <- initSchedState

  runSchedT schedState schedConfig startSchedT

  clientList <- newIOHashMap
  workerList <- newIOHashMap

  runCheckClientState clientList 100
  runCheckWorkerState workerList 100

  thread <- forkIO $ forever $ do
    -- if accept failed exit
    e <- tryIO $ mainLoop makeTransport sock schedState schedConfig clientList workerList
    case e of
      Right _ -> return ()
      Left e'  -> do
        print e'
        handleExit bye

  takeMVar bye
  killThread thread
  runSchedT schedState schedConfig shutdown
  Socket.close sock

mainLoop
  :: (Socket -> IO Transport)
  -> Socket
  -> SchedState IO -> SchedConfig
  -> ClientList -> WorkerList -> IO ()
mainLoop makeTransport sock schedState schedConfig clientList workerList = do
  (sock', _) <- accept sock
  void $ forkIO $ handleConnection schedState schedConfig clientList workerList =<< makeTransport sock'

handleConnection
  :: SchedState IO -> SchedConfig
  -> ClientList -> WorkerList -> Transport -> IO ()
handleConnection schedState schedConfig clientList workerList transport = do
  connectionConfig <- initServerConnectionConfig transport
  connectionState <- initConnectionState

  runConnectionT connectionState connectionConfig $
   receiveThen $ \pl ->
     sendThen $
       case runParser pl of
         Left _           -> Conn.close
         Right TypeClient -> do
           cid <- connid
           liftIO $ do
             clientEnv <- initClientEnv connectionState connectionConfig schedState schedConfig
             HM.insert clientList cid clientEnv
             startClientT clientEnv
             HM.delete clientList cid
         Right TypeWorker -> do
           cid <- connid
           liftIO $ do
             workerEnv <- initWorkerEnv connectionState connectionConfig schedState schedConfig
             HM.insert workerList cid workerEnv
             startWorkerT workerEnv
             HM.delete workerList cid

  where receiveThen :: (ByteString -> ConnectionT IO ()) -> ConnectionT IO ()
        receiveThen next = do
          e <- try receive
          case e of
            Left (_ :: SomeException) -> Conn.close
            Right pl                  -> next pl

        sendThen :: ConnectionT IO () -> ConnectionT IO ()
        sendThen next = do
          e <- try $ send =<< connid
          case e of
            Left (_ :: SomeException) -> Conn.close
            Right _                   -> next

runCheckWorkerState :: WorkerList -> Int64 -> IO ()
runCheckWorkerState ref alive = runCheckState "Worker" ref (checkWorkerState ref alive) alive

checkWorkerState :: WorkerList -> Int64 -> WorkerEnv IO -> IO ()
checkWorkerState ref alive env0 = runWorkerT env0 $ do
  expiredAt <- (alive +) <$> Worker.getLastVist
  now <- liftIO getEpochTime
  when (now > expiredAt) $ do
    Worker.close
    wid <- lift $ lift connid
    liftIO $ HM.delete ref wid

runCheckClientState :: ClientList -> Int64 -> IO ()
runCheckClientState ref alive = runCheckState "Client" ref (checkClientState ref alive) alive

checkClientState :: ClientList -> Int64 -> ClientEnv IO -> IO ()
checkClientState ref alive env0 = runClientT env0 $ do
  expiredAt <- (alive +) <$> Client.getLastVist
  now <- liftIO getEpochTime
  when (now > expiredAt) $ do
    Client.close
    cid <- lift $ lift connid
    liftIO $ HM.delete ref cid

runCheckState :: String -> IOHashMap a b -> (b -> IO ()) -> Int64 -> IO ()
runCheckState var ref checkAlive alive = void . forkIO . forever $ do
  threadDelay $ fromIntegral alive * 1000 * 1000
  mapM_ checkAlive =<< HM.elems ref
  size <- HM.size ref
  errorM "Periodic.Server" $ "Total " ++ var ++ ": " ++ show size
