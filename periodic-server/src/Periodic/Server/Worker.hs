{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.Worker
  (
    Worker
  , newWorker
  , wClose
  ) where

import           Control.Concurrent        (ThreadId, forkIO, killThread,
                                            threadDelay)
import           Control.Exception         (try)
import           Control.Monad             (forever, void, when)
import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Char8     as B (breakSubstring, drop, empty,
                                                 null, unpack)
import           Data.Int                  (Int64)
import           Data.Maybe                (fromJust, isJust)
import           Periodic.Connection       (Connection, close, receive)
import           Text.Read                 (readMaybe)

import           Periodic.Server.Agent     (Agent, newAgent, send)
import           Periodic.Server.FuncList  (FuncList, newFuncList)
import qualified Periodic.Server.FuncList  as FL
import           Periodic.Server.Scheduler

import           Periodic.Types            (Command (..), Error (..),
                                            Payload (..), nullChar,
                                            nullCharLength)
import           Periodic.Utils            (getEpochTime, parsePayload)

import           Data.IORef                (IORef, atomicModifyIORef', newIORef)

data Worker = Worker { wConn      :: Connection
                     , wSched     :: Scheduler
                     , wThreadID  :: IORef (Maybe ThreadId)
                     , wThreadID1 :: IORef (Maybe ThreadId)
                     , wFuncList  :: FuncList Bool
                     , wJobQueue  :: FuncList Bool
                     , wKeepAlive :: Int64
                     , wLastVist  :: IORef Int64
                     }

newWorker :: Connection -> Scheduler -> Int64 -> IO Worker
newWorker wConn wSched wKeepAlive = do
  wFuncList <- newFuncList
  wJobQueue <- newFuncList
  wThreadID <- newIORef Nothing
  wThreadID1 <- newIORef Nothing
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
    Left SocketClosed -> wClose w
    Left _            -> wClose w
    Right pl          -> handlePayload w (parsePayload pl)

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
  let (jh, later, step) = parse (breakBS pl)

  schedLaterJob sched jh later step
  FL.delete jq jh

  where breakBS = B.breakSubstring nullChar
        readBS :: (Num a, Read a) => ByteString -> a
        readBS = maybe 0 id . readMaybe . B.unpack
        parse :: (ByteString, ByteString) -> (ByteString, Int64, Int)
        parse (jh, xs) | B.null xs = (jh, 0, 0)
                       | otherwise = parse1 jh (breakBS $ B.drop nullCharLength xs)

        parse1 :: ByteString -> (ByteString, ByteString) -> (ByteString, Int64, Int)
        parse1 jh (later, step) | B.null step = (jh, readBS later, 0)
                                | otherwise = (jh, readBS later, readBS $ B.drop nullCharLength step)

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
  threadID <- atomicModifyIORef' wThreadID (\v -> (v, v))
  when (isJust threadID) $ killThread (fromJust threadID)
  threadID1 <- atomicModifyIORef' wThreadID1 (\v -> (v, v))
  when (isJust threadID1) $ killThread (fromJust threadID1)

  mapM_ (failJob wSched) =<< FL.keys wJobQueue
  mapM_ (removeFunc wSched) =<< FL.keys wFuncList
  close wConn
