{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.Scheduler
  (
    Scheduler
  , newScheduler
  , pushJob
  , pushGrab
  , failJob
  , doneJob
  , schedLaterJob
  , addFunc
  , removeFunc
  , dropFunc
  , removeJob
  , dumpJob
  , status
  ) where

import qualified Control.Concurrent.Lock   as L (Lock, new, with)
import           Control.Monad             (void, when)
import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Char8     as B (concat)
import           Data.Int                  (Int64)
import           Data.UnixTime
import           Periodic.Server.Agent     (Agent, aAlive, send)
import           Periodic.Server.FuncList  (FuncList, FuncName, newFuncList)
import qualified Periodic.Server.FuncList  as FL
import           Periodic.Server.FuncStat
import           Periodic.Server.GrabQueue
import           Periodic.Server.Job       (Job (..), JobHandle, jHandle,
                                            unparseJob)
import           Periodic.Server.JobQueue  (JobQueue)
import qualified Periodic.Server.JobQueue  as JQ
import           Periodic.Types            (Command (JobAssign), nullChar)

import           Control.Concurrent        (ThreadId, forkIO, killThread,
                                            threadDelay)
import           Data.IORef                (IORef, atomicModifyIORef', newIORef)
import           Data.Maybe                (fromJust, isJust)

data Scheduler = Scheduler { sFuncStatList  :: FuncStatList
                           , sGrabQueue     :: GrabQueue
                           , sJobQueue      :: JobQueue
                           , sProcessJob    :: FuncList Job
                           , sMainTimer     :: IORef (Maybe ThreadId)
                           , sMainTimerLock :: L.Lock
                           , sMainTimerWait :: IORef Bool
                           }

newScheduler :: IO Scheduler
newScheduler = do
  sFuncStatList  <- newFuncList
  sGrabQueue     <- newFuncList
  sJobQueue      <- newFuncList
  sProcessJob    <- newFuncList
  sMainTimer     <- newIORef Nothing
  sMainTimerWait <- newIORef False
  sMainTimerLock <- L.new
  let sched = Scheduler {..}

  nextMainTimer sched 1
  return sched

pushJob :: Scheduler -> Job -> IO ()
pushJob sched@(Scheduler {..}) job = do
  JQ.pushJob sJobQueue job
  FL.alter sFuncStatList updateFuncStat n
  nextMainTimer sched 1

  where n = jFuncName job
        updateFuncStat :: Maybe FuncStat -> Maybe FuncStat
        updateFuncStat Nothing = Just ((funcStat n) { sJob = 1, sSchedAt = jSchedAt job })
        updateFuncStat (Just fs) = Just (fs { sJob = sJob fs + 1
                                            , sSchedAt = if sSchedAt fs > jSchedAt job  then jSchedAt job
                                                                                        else sSchedAt fs
                                            })

removeJob :: Scheduler -> Job -> IO ()
removeJob sched@(Scheduler {..}) job = do
  has <- JQ.memberJob sJobQueue (jFuncName job) (jName job)
  when has $ do
    JQ.removeJob sJobQueue (jFuncName job) (jName job)
    minJob <- JQ.findMinJob sJobQueue (jFuncName job)
    let update v = v { sJob = sJob v - 1
                     , sSchedAt = case minJob of
                                    Nothing -> sSchedAt v
                                    Just mj -> jSchedAt mj
                     }

    FL.adjust sFuncStatList update (jFuncName job)

  isProc <- FL.member sProcessJob (jHandle job)
  when isProc $ do
    FL.delete sProcessJob (jHandle job)
    let update v = v { sJob     = max (sJob v - 1) 0
                     , sProcess = max (sProcess v - 1) 0
                     }

    FL.adjust sFuncStatList update (jFuncName job)

  nextMainTimer sched 1

dumpJob :: Scheduler -> IO [Job]
dumpJob (Scheduler {..}) = do
  jobs <- JQ.dumpJob sJobQueue
  jobs' <- FL.elems sProcessJob
  return (jobs ++ jobs')

addFunc :: Scheduler -> FuncName -> IO ()
addFunc sched@(Scheduler {..}) n = do
  FL.alter sFuncStatList updateFuncStat n
  nextMainTimer sched 1

  where updateFuncStat :: Maybe FuncStat -> Maybe FuncStat
        updateFuncStat Nothing   = Just ((funcStat n) { sWorker = 1 })
        updateFuncStat (Just fs) = Just (fs { sWorker = sWorker fs + 1 })

removeFunc :: Scheduler -> FuncName -> IO ()
removeFunc sched@(Scheduler {..}) n = do
  FL.alter sFuncStatList updateFuncStat n
  nextMainTimer sched 1

  where updateFuncStat :: Maybe FuncStat -> Maybe FuncStat
        updateFuncStat Nothing = Just (funcStat n)
        updateFuncStat (Just fs) = Just (fs { sWorker = max (sWorker fs - 1) 0
                                            })

dropFunc :: Scheduler -> FuncName -> IO ()
dropFunc sched@(Scheduler {..}) n = do
  st <- FL.lookup sFuncStatList n
  case st of
    Nothing -> return ()
    Just st' -> do
      if sWorker st' > 0 then return ()
                         else do
                           FL.delete sFuncStatList n
                           FL.delete sJobQueue n

pushGrab :: Scheduler -> FuncList Bool -> FuncList Bool -> Agent -> IO ()
pushGrab sched@(Scheduler {..}) fl jh ag = do
  pushAgent sGrabQueue fl jh ag
  nextMainTimer sched 1

processJob :: Scheduler -> IO ()
processJob sched@(Scheduler {..}) = do
  st <- getFirstSched sFuncStatList sGrabQueue
  case st of
    Nothing -> nextMainTimer sched 10000
    Just st' -> do
      now <- read . show . toEpochTime <$> getUnixTime
      if now < (sSchedAt st') then nextMainTimer sched (sSchedAt st' - now)
                              else submitJob st'

  where revertJob :: Job -> IO ()
        revertJob job = do
          JQ.pushJob sJobQueue job
          nextMainTimer sched 1

        submitJob :: FuncStat -> IO ()
        submitJob st = do
          job <- JQ.popJob sJobQueue (sFuncName st)
          case job of
            Nothing   -> nextMainTimer sched 1
            Just job' -> do
              agent <- popMaybeAgent sGrabQueue (sFuncName st)
              case agent of
                Nothing -> revertJob job'
                Just (jq, agent') -> do
                  alive <- aAlive agent'
                  case alive of
                    False -> revertJob job'
                    True -> do
                      let jh = jHandle job'
                      assignJob agent' job'
                      FL.insert sProcessJob jh job'
                      FL.insert jq jh True
                      minJob <- JQ.findMinJob sJobQueue (jFuncName job')
                      let update v = v { sProcess = sProcess v + 1
                                       , sSchedAt = case minJob of
                                                      Nothing -> sSchedAt v
                                                      Just mj -> jSchedAt mj
                                       }
                      FL.adjust sFuncStatList update (jFuncName job')

                      nextMainTimer sched 1

assignJob :: Agent -> Job -> IO ()
assignJob agent job = send agent JobAssign $ B.concat [ jHandle job
                                                      , nullChar
                                                      , unparseJob job
                                                      ]

nextMainTimer :: Scheduler -> Int64 -> IO ()
nextMainTimer sched@(Scheduler {..}) t = L.with sMainTimerLock $ do
  threadID <- atomicModifyIORef' sMainTimer (\v -> (v, v))
  wait <- atomicModifyIORef' sMainTimerWait (\v -> (v, v))
  threadID' <- forkIO $ do
    atomicModifyIORef' sMainTimerWait (\v -> (True, ()))
    threadDelay $ fromIntegral t * 1000
    atomicModifyIORef' sMainTimerWait (\v -> (False, ()))
    processJob sched

  atomicModifyIORef' sMainTimer (\v -> (Just threadID', ()))
  when (isJust threadID && wait) $ killThread (fromJust threadID)

failJob :: Scheduler -> JobHandle -> IO ()
failJob sched@(Scheduler {..}) jh = do
  job <- FL.lookup sProcessJob jh
  case job of
    Nothing -> return ()
    Just job' -> do
      JQ.pushJob sJobQueue job'
      FL.delete sProcessJob jh
      let update v = v { sProcess = max (sProcess v - 1) 0
                       , sSchedAt = if sSchedAt v > jSchedAt job' then jSchedAt job'
                                                                  else sSchedAt v
                       }
      FL.adjust sFuncStatList update (jFuncName job')
      nextMainTimer sched 1

doneJob :: Scheduler -> JobHandle -> IO ()
doneJob (Scheduler {..}) jh = do
  job <- FL.lookup sProcessJob jh
  case job of
    Nothing -> return ()
    Just job' -> do
      FL.delete sProcessJob jh
      let update v = v { sProcess = max (sProcess v - 1) 0
                       , sJob = max (sJob v - 1) 0
                       }
      FL.adjust sFuncStatList update (jFuncName job')

schedLaterJob :: Scheduler -> JobHandle -> Int64 -> Int -> IO ()
schedLaterJob sched@(Scheduler {..}) jh later step = do
  job <- FL.lookup sProcessJob jh
  case job of
    Nothing -> return ()
    Just job' -> do
      nextSchedAt <- (+) later . read . show . toEpochTime <$> getUnixTime
      JQ.pushJob sJobQueue (job' { jSchedAt = nextSchedAt, jCount = jCount job' + step })
      FL.delete sProcessJob jh
      let update v = v { sProcess = max (sProcess v - 1) 0
                       , sSchedAt = if sSchedAt v > nextSchedAt then nextSchedAt
                                                                else sSchedAt v
                       }
      FL.adjust sFuncStatList update (jFuncName job')
      nextMainTimer sched 1

status :: Scheduler -> IO [FuncStat]
status (Scheduler {..}) = FL.elems sFuncStatList
