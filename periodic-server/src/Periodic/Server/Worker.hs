{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server.Worker
  (
    Worker
  , newWorker
  , wClose
  ) where

import           Control.Concurrent        (ThreadId, forkIO, killThread,
                                            threadDelay)
import           Control.Exception         (SomeException, try)
import           Control.Monad             (forever, void, when)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (empty)
import           Data.Int                  (Int64)
import           Data.Maybe                (fromJust, isJust)
import           Periodic.Connection       (Connection, close, receive)

import           Periodic.Server.Agent     (Agent, newAgent, send)
import           Periodic.Server.FuncList  (FuncList, newFuncList)
import qualified Periodic.Server.FuncList  as FL
import           Periodic.Server.Scheduler

import           Periodic.Types            (Command (..), Payload (..))
import           Periodic.Utils            (breakBS, getEpochTime, parsePayload,
                                            readBS)

import           Data.IORef                (IORef, atomicModifyIORef', newIORef)

data Worker = Worker { wConn      :: Connection
                     , wSched     :: Scheduler
                     , wThreadID  :: IORef (Maybe ThreadId)
                     , wThreadID1 :: IORef (Maybe ThreadId)
                     , wFuncList  :: FuncList Bool
                     , wJobQueue  :: FuncList Bool
                     , wKeepAlive :: Int64
                     , wLastVist  :: IORef Int64
                     , wClosed    :: IORef Bool
                     }

newWorker :: Connection -> Scheduler -> Int64 -> IO Worker
newWorker wConn wSched wKeepAlive = do
  wFuncList <- newFuncList
  wJobQueue <- newFuncList
  wThreadID <- newIORef Nothing
  wThreadID1 <- newIORef Nothing
  wClosed   <- newIORef False
  wLastVist <- newIORef =<< getEpochTime

  let w = Worker { .. }

  threadID <- forkIO $ forever $ mainLoop w
  threadID1 <- forkIO $ forever $ checkAlive w
  atomicModifyIORef' wThreadID (\_ -> (Just threadID, ()))
  atomicModifyIORef' wThreadID1 (\_ -> (Just threadID1, ()))
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
                     else threadDelay . fromIntegral $ wKeepAlive * 1000000

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

handleGrabJob :: Scheduler -> FuncList Bool -> FuncList Bool -> Agent -> IO ()
handleGrabJob = pushGrab

handleWorkDone :: Scheduler -> FuncList Bool -> ByteString -> IO ()
handleWorkDone sched jq jh = do
  doneJob sched jh
  FL.delete jq jh

handleWorkFail :: Scheduler -> FuncList Bool -> ByteString -> IO ()
handleWorkFail sched jq jh = do
  failJob sched jh
  FL.delete jq jh

handleSchedLater :: Scheduler -> FuncList Bool -> ByteString -> IO ()
handleSchedLater sched jq pl = do
  let (jh, later, step) = parse (breakBS 3 pl)

  schedLaterJob sched jh later step
  FL.delete jq jh

  where parse :: [ByteString] -> (ByteString, Int64, Int)
        parse (a:b:c:_) = (a, readBS b, readBS c)
        parse (a:b:[])  = (a, readBS b, 0)
        parse (a:[])    = (a, 0, 0)
        parse []        = (B.empty, 0, 0)

handleCanDo :: Scheduler -> FuncList Bool -> ByteString -> IO ()
handleCanDo sched fl fn = do
  addFunc sched fn
  FL.insert fl fn True

handleCantDo :: Scheduler -> FuncList Bool -> ByteString -> IO ()
handleCantDo sched fl fn = do
  removeFunc sched fn
  FL.delete fl fn

wClose :: Worker -> IO ()
wClose (Worker { .. }) = void $ forkIO $ do
  closed <- atomicModifyIORef' wClosed (\v -> (True, v))
  when (not closed) $ do
    threadID <- atomicModifyIORef' wThreadID (\v -> (v, v))
    when (isJust threadID) $ killThread (fromJust threadID)
    threadID1 <- atomicModifyIORef' wThreadID1 (\v -> (v, v))
    when (isJust threadID1) $ killThread (fromJust threadID1)

    mapM_ (failJob wSched) =<< FL.keys wJobQueue
    mapM_ (removeFunc wSched) =<< FL.keys wFuncList
    close wConn
