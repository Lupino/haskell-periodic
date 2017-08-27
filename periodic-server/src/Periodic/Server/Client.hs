{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server.Client
  (
    Client
  , newClient
  , cClose
  ) where

import           Control.Concurrent        (forkIO)
import           Control.Exception         (SomeException, try)
import           Control.Monad             (forever, void, when)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as B (concat, empty, intercalate,
                                                 pack)
import           Data.Foldable             (forM_)
import           Periodic.Connection       (Connection, close, receive)
import           Periodic.TM

import           Periodic.Agent            (Agent, newAgent, send, send_)
import           Periodic.Server.FuncStat  (FuncStat (..))
import           Periodic.Server.Scheduler (Scheduler, dropFunc, dumpJob,
                                            pushJob, removeJob, shutdown,
                                            status)
import           Periodic.Types.Job        (Job, decodeJob, encodeJob)

import           Periodic.Timer
import           Periodic.Types            (Command (..), Payload (..))
import           Periodic.Utils            (getEpochTime, parsePayload)

import           Data.Int                  (Int64)
import           Data.IORef                (IORef, atomicModifyIORef', newIORef)

data Client = Client { cConn      :: Connection
                     , cSched     :: Scheduler
                     , cRunner    :: ThreadManager
                     , cKeepAlive :: Int64
                     , cTimer     :: Timer
                     , cLastVist  :: IORef Int64
                     }

newClient :: Connection -> Scheduler -> Int64 -> IO Client
newClient cConn cSched cKeepAlive = do
  cRunner <- newThreadManager
  cLastVist <- newIORef =<< getEpochTime
  cTimer <- newTimer
  let c = Client {..}

  runner <- forkIO $ forever $ mainLoop c
  initTimer cTimer $ checkAlive c

  setThreadId cRunner runner

  repeatTimer' cTimer (fromIntegral cKeepAlive)
  return c

mainLoop :: Client -> IO ()
mainLoop c@Client{..} = do
  e <- try $ receive cConn
  setLastVistTime c =<< getEpochTime
  case e of
    Left (_::SomeException) -> cClose c
    Right pl                -> do
      e' <- try $ handlePayload c (parsePayload pl)
      case e' of
        Left (_::SomeException) -> cClose c
        Right _                 -> return ()

setLastVistTime :: Client -> Int64 -> IO ()
setLastVistTime Client{..} v = atomicModifyIORef' cLastVist (const (v, ()))

getLastVistTime :: Client -> IO Int64
getLastVistTime Client{..} = atomicModifyIORef' cLastVist (\v -> (v, v))

checkAlive :: Client -> IO ()
checkAlive c@Client{..} = do
  expiredAt <- (cKeepAlive +) <$> getLastVistTime c
  now <- getEpochTime
  when (now > expiredAt) $ cClose c

handlePayload :: Client -> Payload -> IO ()
handlePayload c Payload{..} = go payloadCMD
  where go :: Command -> IO ()
        go SubmitJob = handleSubmitJob sched agent payloadData
        go Status    = handleStatus sched agent
        go Ping      = send agent Pong B.empty
        go DropFunc  = handleDropFunc sched agent payloadData
        go RemoveJob = handleRemoveJob sched agent payloadData
        go Dump      = handleDump sched agent
        go Load      = handleLoad sched payloadData
        go Shutdown  = handleShutdown sched
        go _         = send agent Unknown B.empty

        agent = newAgent payloadID $ cConn c
        sched = cSched c

handleSubmitJob :: Scheduler -> Agent -> ByteString -> IO ()
handleSubmitJob sc ag pl =
  case decodeJob pl of
    Nothing -> send ag Noop B.empty
    Just job -> do
      pushJob sc job
      send ag Success B.empty

handleStatus :: Scheduler -> Agent -> IO ()
handleStatus sc ag = do
  stats <- map go <$> status sc
  send_ ag $ B.intercalate "\n" stats

  where go :: FuncStat -> ByteString
        go FuncStat{..} = B.concat [ sFuncName
                                      , ","
                                      , B.pack $ show sWorker
                                      , ","
                                      , B.pack $ show sJob
                                      , ","
                                      , B.pack $ show sProcess
                                      , ","
                                      , B.pack $ show sSchedAt
                                      ]

handleDropFunc :: Scheduler -> Agent -> ByteString -> IO ()
handleDropFunc sc ag pl = do
  dropFunc sc pl
  send ag Success B.empty

handleRemoveJob :: Scheduler -> Agent -> ByteString -> IO ()
handleRemoveJob sc ag pl =
  case decodeJob pl of
    Nothing -> send ag Noop B.empty
    Just job -> do
      removeJob sc job
      send ag Success B.empty

handleDump :: Scheduler -> Agent -> IO ()
handleDump sc ag = do
  jobs <- dumpJob sc
  mapM_ doSend jobs
  send_ ag "EOF"

  where doSend :: Job -> IO ()
        doSend = send_ ag . encodeJob

handleLoad :: Scheduler -> ByteString -> IO ()
handleLoad sc pl = forM_ (decodeJob pl) (pushJob sc)

handleShutdown :: Scheduler -> IO ()
handleShutdown = shutdown

cClose :: Client -> IO ()
cClose Client{ .. } = void $ forkIO $ do
  killThread cRunner
  clearTimer cTimer
  close cConn
