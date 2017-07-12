{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server.Worker
  (
    Worker
  , newWorker
  , wClose
  ) where

import           Control.Concurrent        (forkIO)
import           Control.Exception         (SomeException, try)
import           Control.Monad             (forever, void, when)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (empty)
import           Data.Int                  (Int64)
import           Periodic.Connection       (Connection, close, receive)
import qualified Periodic.Lock             as L (Lock, new, with)
import           Periodic.TM

import           Periodic.Agent            (Agent, newAgent, send)
import           Periodic.IOList           (IOList, delete, elem, insert,
                                            newIOList, toList)
import           Periodic.Server.Scheduler
import           Prelude                   hiding (elem)

import           Periodic.Timer
import           Periodic.Types            (Command (..), Payload (..))
import           Periodic.Types.Job        (FuncName, JobHandle)
import           Periodic.Utils            (breakBS, getEpochTime, parsePayload,
                                            readBS)

import           Data.IORef                (IORef, atomicModifyIORef', newIORef)

data Worker = Worker { wConn      :: Connection
                     , wSched     :: Scheduler
                     , wRunner    :: ThreadManager
                     , wTimer     :: Timer
                     , wFuncList  :: IOList FuncName
                     , wJobQueue  :: IOList JobHandle
                     , wKeepAlive :: Int64
                     , wLastVist  :: IORef Int64
                     , wClosed    :: IORef Bool
                     , wLocker    :: L.Lock
                     }

newWorker :: Connection -> Scheduler -> Int64 -> IO Worker
newWorker wConn wSched wKeepAlive = do
  wFuncList <- newIOList
  wJobQueue <- newIOList
  wRunner <- newThreadManager
  wTimer <- newTimer
  wClosed   <- newIORef False
  wLocker   <- L.new
  wLastVist <- newIORef =<< getEpochTime

  let w = Worker { .. }

  runner <- forkIO $ forever $ mainLoop w
  setThreadId wRunner runner

  initTimer wTimer $ checkAlive w
  repeatTimer' wTimer $ fromIntegral wKeepAlive
  return w

mainLoop :: Worker -> IO ()
mainLoop w@(Worker {..}) = do
  e <- try $ receive wConn
  setLastVistTime w =<< getEpochTime
  case e of
    Left (_::SomeException) -> wClose w
    Right pl                -> do
      e' <- try $ handlePayload w (parsePayload pl)
      case e' of
        Left (_::SomeException) -> wClose w
        Right _                 -> return ()

setLastVistTime :: Worker -> Int64 -> IO ()
setLastVistTime (Worker {..}) v = atomicModifyIORef' wLastVist (\_ -> (v, ()))

getLastVistTime :: Worker -> IO Int64
getLastVistTime (Worker {..}) = atomicModifyIORef' wLastVist (\v -> (v, v))

checkAlive :: Worker -> IO ()
checkAlive w@(Worker {..}) = do
  expiredAt <- (wKeepAlive +) <$> getLastVistTime w
  now <- getEpochTime
  if now > expiredAt then wClose w
                     else return ()

handlePayload :: Worker -> Payload -> IO ()
handlePayload w (Payload {..}) = go payloadCMD
  where go :: Command -> IO ()
        go GrabJob    = handleGrabJob sched funcList jobQueue agent
        go WorkDone   = handleWorkDone sched jobQueue payloadData
        go WorkFail   = handleWorkFail sched jobQueue payloadData
        go SchedLater = handleSchedLater sched jobQueue payloadData
        go Sleep      = send agent Noop B.empty
        go Ping       = send agent Pong B.empty
        go CanDo      = handleCanDo sched funcList payloadData
        go CantDo     = handleCantDo sched funcList payloadData
        go _          = send agent Unknown B.empty

        agent = newAgent payloadID $ wConn w
        sched = wSched w
        funcList = wFuncList w
        jobQueue = wJobQueue w

handleGrabJob :: Scheduler -> IOList FuncName -> IOList JobHandle -> Agent -> IO ()
handleGrabJob = pushGrab

handleWorkDone :: Scheduler -> IOList JobHandle -> ByteString -> IO ()
handleWorkDone sched jq jh = do
  doneJob sched jh
  delete jq jh

handleWorkFail :: Scheduler -> IOList JobHandle -> ByteString -> IO ()
handleWorkFail sched jq jh = do
  failJob sched jh
  delete jq jh

handleSchedLater :: Scheduler -> IOList JobHandle -> ByteString -> IO ()
handleSchedLater sched jq pl = do
  let (jh, later, step) = parse (breakBS 3 pl)

  schedLaterJob sched jh later step
  delete jq jh

  where parse :: [ByteString] -> (ByteString, Int64, Int)
        parse (a:b:c:_) = (a, readBS b, readBS c)
        parse (a:b:[])  = (a, readBS b, 0)
        parse (a:[])    = (a, 0, 0)
        parse []        = (B.empty, 0, 0)

handleCanDo :: Scheduler -> IOList FuncName -> ByteString -> IO ()
handleCanDo sched fl fn = do
  has <- elem fl fn
  when (not has) $ do
    addFunc sched fn
    insert fl fn

handleCantDo :: Scheduler -> IOList FuncName -> ByteString -> IO ()
handleCantDo sched fl fn = do
  has <- elem fl fn
  when has $ do
    removeFunc sched fn
    delete fl fn

wClose :: Worker -> IO ()
wClose (Worker { .. }) = void $ forkIO $ L.with wLocker $ do
  closed <- atomicModifyIORef' wClosed (\v -> (True, v))
  killThread wRunner

  clearTimer wTimer

  when (not closed) $ do
    mapM_ (failJob wSched) =<< toList wJobQueue
    mapM_ (removeFunc wSched) =<< toList wFuncList
    close wConn
