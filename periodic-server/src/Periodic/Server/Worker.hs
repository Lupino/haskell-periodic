{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server.Worker
  (
    Worker
  , newWorker
  , wClose
  ) where

import           Control.Concurrent           (forkIO)
import           Control.Exception            (SomeException, try)
import           Control.Monad                (forever, unless, void, when)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B (empty)
import           Data.Int                     (Int64)
import           Periodic.Connection          (Connection, close)
import qualified Periodic.Connection          as Conn (receive)
import qualified Periodic.Lock                as L (Lock, new, with)
import           Periodic.TM

import           Periodic.Agent               (Agent, newAgent, receive, send)
import           Periodic.IOList              (IOList, delete, elem, insert,
                                               newIOList, toList)
import           Periodic.Server.Scheduler
import           Prelude                      hiding (elem)

import           Periodic.Timer
import           Periodic.Types.Job           (FuncName, JobHandle)
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           Periodic.Utils               (breakBS, getEpochTime, readBS)

import           Data.IORef                   (IORef, atomicModifyIORef',
                                               newIORef)

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
mainLoop w@Worker{..} = do
  e <- try $ Conn.receive wConn
  setLastVistTime w =<< getEpochTime
  case e of
    Left (_::SomeException) -> wClose w
    Right pl                -> do
      e' <- try $ handlePayload w pl
      case e' of
        Left (_::SomeException) -> wClose w
        Right _                 -> return ()

setLastVistTime :: Worker -> Int64 -> IO ()
setLastVistTime Worker{..} v = atomicModifyIORef' wLastVist (const (v, ()))

getLastVistTime :: Worker -> IO Int64
getLastVistTime Worker{..} = atomicModifyIORef' wLastVist (\v -> (v, v))

checkAlive :: Worker -> IO ()
checkAlive w@Worker{..} = do
  expiredAt <- (wKeepAlive +) <$> getLastVistTime w
  now <- getEpochTime
  when (now > expiredAt) $ wClose w

handlePayload :: Worker -> ByteString -> IO ()
handlePayload w pl = do
  agent <- newAgent pl $ wConn w
  cmd <- receive agent :: IO (Either String WorkerCommand)
  case cmd of
    Left e     -> wClose w
    Right cmd' -> go agent cmd'
  where go :: Agent -> WorkerCommand -> IO ()
        go ag GrabJob            = pushGrab sc fl jq ag
        go _ (WorkDone jh)       = doneJob sc jh >> delete jq jh
        go _ (WorkFail jh)       = failJob sc jh >> delete jq jh
        go _ (SchedLater jh l s) = schedLaterJob sc jh l s >> delete jq jh
        go ag Sleep              = send ag Noop
        go ag Ping               = send ag Pong
        go _ (CanDo fn)          = handleCanDo sc fl fn
        go _ (CantDo fn)         = handleCantDo sc fl fn
        go _ (Broadcast fn)      = handleBroadcast sc fl fn
        go ag _                  = send ag Unknown

        sc = wSched w
        fl = wFuncList w
        jq = wJobQueue w

handleCanDo :: Scheduler -> IOList FuncName -> ByteString -> IO ()
handleCanDo sched fl fn = do
  has <- elem fl fn
  unless has $ do
    addFunc sched fn
    insert fl fn

handleCantDo :: Scheduler -> IOList FuncName -> ByteString -> IO ()
handleCantDo sched fl fn = do
  has <- elem fl fn
  when has $ do
    removeFunc sched fn
    delete fl fn

handleBroadcast :: Scheduler -> IOList FuncName -> ByteString -> IO ()
handleBroadcast sched fl fn = do
  has <- elem fl fn
  unless has $ do
    broadcastFunc sched fn True
    insert fl fn

wClose :: Worker -> IO ()
wClose Worker{..} = void $ forkIO $ L.with wLocker $ do
  closed <- atomicModifyIORef' wClosed (\v -> (True, v))
  killThread wRunner

  clearTimer wTimer

  unless closed $ do
    mapM_ (failJob wSched) =<< toList wJobQueue
    mapM_ (removeFunc wSched) =<< toList wFuncList
    close wConn
