{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server.Client
  (
    Client
  , newClient
  , cClose
  ) where

import           Control.Concurrent           (forkIO)
import           Control.Exception            (SomeException, try)
import           Control.Monad                (forever, void, when)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B (concat, intercalate, pack)
import           Data.Foldable                (forM_)
import           Periodic.Connection          (Connection, close)
import qualified Periodic.Connection          as Conn (receive)
import           Periodic.TM

import           Periodic.Agent               (Agent, newAgent, receive, send,
                                               send_)
import           Periodic.Server.FuncStat     (FuncStat (..))
import           Periodic.Server.Scheduler    (Scheduler, dropFunc, dumpJob,
                                               pushJob, removeJob, shutdown,
                                               status)
import           Periodic.Types.ClientCommand
import           Periodic.Types.Job           (Job, decodeJob, encodeJob)
import           Periodic.Types.ServerCommand

import           Periodic.Timer
import           Periodic.Utils               (getEpochTime)

import           Data.Int                     (Int64)
import           Data.IORef                   (IORef, atomicModifyIORef',
                                               newIORef)

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
  e <- try $ Conn.receive cConn
  setLastVistTime c =<< getEpochTime
  case e of
    Left (_::SomeException) -> cClose c
    Right pl                -> do
      e' <- try $ handlePayload c pl
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

handlePayload :: Client -> ByteString -> IO ()
handlePayload c pl = do
  agent <- newAgent pl $ cConn c
  cmd <- receive agent :: IO (Either String ClientCommand)
  case cmd of
    Left e     -> cClose c
    Right cmd' -> go agent cmd'
  where go :: Agent -> ClientCommand -> IO ()
        go agent (SubmitJob job) = handleSubmitJob sched agent job
        go agent Status          = handleStatus sched agent
        go agent Ping            = send agent Pong
        go agent (DropFunc fn)   = handleDropFunc sched agent fn
        go agent (RemoveJob job) = handleRemoveJob sched agent job
        go agent Dump            = handleDump sched agent
        go _ (Load dat)          = handleLoad sched dat
        go _ Shutdown            = handleShutdown sched
        go agent _               = send agent Unknown

        sched = cSched c

handleSubmitJob :: Scheduler -> Agent -> Job -> IO ()
handleSubmitJob sc ag j = do
  pushJob sc j
  send ag Success


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
  send ag Success

handleRemoveJob :: Scheduler -> Agent -> Job -> IO ()
handleRemoveJob sc ag job = do
  removeJob sc job
  send ag Success

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
