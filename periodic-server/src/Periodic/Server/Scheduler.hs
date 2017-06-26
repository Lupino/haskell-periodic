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

import           Periodic.Server.Store     (Store)
import qualified Periodic.Server.Store     as Store

data Scheduler = Scheduler { sFuncStatList  :: FuncStatList
                           , sGrabQueue     :: GrabQueue
                           , sJobQueue      :: JobQueue
                           , sProcessJob    :: FuncList Job
                           , sMainTimer     :: IORef (Maybe ThreadId)
                           , sMainTimerLock :: L.Lock
                           , sMainTimerWait :: IORef Bool
                           , sStore         :: Store
                           }

newScheduler :: Store -> IO Scheduler
newScheduler sStore = do
  sFuncStatList  <- newFuncList
  sGrabQueue     <- newFuncList
  sJobQueue      <- newFuncList
  sProcessJob    <- newFuncList
  sMainTimer     <- newIORef Nothing
  sMainTimerWait <- newIORef False
  sMainTimerLock <- L.new
  let sched = Scheduler {..}

  jobs <- Store.dumpJob sStore
  mapM_ (restoreJob sched) jobs

  nextMainTimer sched 0
  return sched

pushJob :: Scheduler -> Job -> IO ()
pushJob sched@(Scheduler {..}) job = do
  exists <- Store.existsJob sStore jh
  if exists then do
    has <- JQ.memberJob sJobQueue fn jn
    when has $ do
      Store.insertJob sStore job
      JQ.pushJob sJobQueue job
      FL.adjust sFuncStatList adjustStat fn
  else do
    Store.insertJob sStore job
    JQ.pushJob sJobQueue job
    size <- JQ.sizeJob sJobQueue fn
    FL.alter sFuncStatList (updateStat size) fn

  nextMainTimer sched 0

  where fn = jFuncName job
        jh = jHandle job
        jn = jName job
        updateStat :: Int -> Maybe FuncStat -> Maybe FuncStat
        updateStat s Nothing = Just ((funcStat fn) { sJob = 1, sSchedAt = jSchedAt job })
        updateStat s (Just fs) = Just (fs { sJob = fromIntegral s
                                          , sSchedAt = min (sSchedAt fs) (jSchedAt job)
                                          })

        adjustStat :: FuncStat -> FuncStat
        adjustStat v = v { sSchedAt = min (jSchedAt job) (sSchedAt v) }

restoreJob :: Scheduler -> Job -> IO ()
restoreJob sched@(Scheduler {..}) job = do
  JQ.pushJob sJobQueue job
  size <- JQ.sizeJob sJobQueue fn
  FL.alter sFuncStatList (updateStat size) fn

  where fn = jFuncName job
        updateStat :: Int -> Maybe FuncStat -> Maybe FuncStat
        updateStat s Nothing = Just ((funcStat fn) { sJob = 1, sSchedAt = jSchedAt job })
        updateStat s (Just fs) = Just (fs { sJob = fromIntegral s
                                          , sSchedAt = min (sSchedAt fs) (jSchedAt job)
                                          })


removeJob :: Scheduler -> Job -> IO ()
removeJob sched@(Scheduler {..}) job = do
  has <- JQ.memberJob sJobQueue (jFuncName job) (jName job)
  when has $ do
    JQ.removeJob sJobQueue (jFuncName job) (jName job)
    minJob <- JQ.findMinJob sJobQueue (jFuncName job)
    let update v = v { sJob = min (sJob v - 1) 0
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

  Store.deleteJob sStore (jHandle job)

  nextMainTimer sched 0

dumpJob :: Scheduler -> IO [Job]
dumpJob (Scheduler {..}) = Store.dumpJob sStore

addFunc :: Scheduler -> FuncName -> IO ()
addFunc sched@(Scheduler {..}) n = do
  FL.alter sFuncStatList updateFuncStat n
  nextMainTimer sched 0

  where updateFuncStat :: Maybe FuncStat -> Maybe FuncStat
        updateFuncStat Nothing   = Just ((funcStat n) { sWorker = 1 })
        updateFuncStat (Just fs) = Just (fs { sWorker = sWorker fs + 1 })

removeFunc :: Scheduler -> FuncName -> IO ()
removeFunc sched@(Scheduler {..}) n = do
  FL.alter sFuncStatList updateFuncStat n
  nextMainTimer sched 0

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
                           jhs <- map jHandle <$> JQ.dumpJobByFuncName sJobQueue n
                           FL.delete sJobQueue n
                           mapM_ (Store.deleteJob sStore) jhs

pushGrab :: Scheduler -> FuncList Bool -> FuncList Bool -> Agent -> IO ()
pushGrab sched@(Scheduler {..}) fl jh ag = do
  pushAgent sGrabQueue fl jh ag
  nextMainTimer sched 0

processJob :: Scheduler -> IO ()
processJob sched@(Scheduler {..}) = do
  st <- getFirstSched sFuncStatList sGrabQueue
  case st of
    Nothing -> nextMainTimer sched 10
    Just st' -> do
      now <- read . show . toEpochTime <$> getUnixTime
      if now < (sSchedAt st') then nextMainTimer sched (sSchedAt st' - now)
                              else submitJob now st'

  where revertJob :: Job -> IO ()
        revertJob job = do
          JQ.pushJob sJobQueue job
          nextMainTimer sched 0

        popJobThen :: Int64 -> FuncStat -> (Job -> IO ()) -> IO ()
        popJobThen now st done = do
          job <- JQ.popJob sJobQueue (sFuncName st)
          case job of
            Nothing -> do
              size <- JQ.sizeJob sJobQueue (sFuncName st)
              let update v = v { sJob = fromIntegral size }
              FL.adjust sFuncStatList update (sFuncName st)
              nextMainTimer sched 0
            Just job' -> if jSchedAt job' > now then revertJob job'
                                                else done job'

        popMaybeAgentThen :: FuncStat -> (Agent -> Job -> IO ()) -> Job -> IO ()
        popMaybeAgentThen st done job = do
          agent <- popMaybeAgent sGrabQueue (sFuncName st)
          case agent of
            Nothing           -> revertJob job
            Just (jq, agent') -> do
              alive <- aAlive agent'
              if alive then FL.insert jq (jHandle job) True >> done agent' job
                       else revertJob job

        doneSubmitJob :: Agent -> Job -> IO ()
        doneSubmitJob agent job = do
          let jh = jHandle job
          assignJob agent job
          FL.insert sProcessJob jh job
          minJob <- JQ.findMinJob sJobQueue (jFuncName job)
          let update v = v { sProcess = min (sProcess v + 1) (sJob v)
                           , sSchedAt = case minJob of
                                          Nothing -> sSchedAt v
                                          Just mj -> jSchedAt mj
                           }
          FL.adjust sFuncStatList update (jFuncName job)

          nextMainTimer sched 0

        submitJob :: Int64 -> FuncStat -> IO ()
        submitJob now st = popJobThen now st $ popMaybeAgentThen st doneSubmitJob

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
    when (t > 0) $ do
      atomicModifyIORef' sMainTimerWait (\v -> (True, ()))
      threadDelay $ fromIntegral t * 1000000
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
                       , sSchedAt = min (sSchedAt v) (jSchedAt job')
                       }
      FL.adjust sFuncStatList update (jFuncName job')
      nextMainTimer sched 0

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
      Store.deleteJob sStore jh

schedLaterJob :: Scheduler -> JobHandle -> Int64 -> Int -> IO ()
schedLaterJob sched@(Scheduler {..}) jh later step = do
  job <- FL.lookup sProcessJob jh
  case job of
    Nothing -> return ()
    Just job' -> do
      nextSchedAt <- (+) later . read . show . toEpochTime <$> getUnixTime
      let nextJob = job' { jSchedAt = nextSchedAt, jCount = jCount job' + step }

      JQ.pushJob sJobQueue nextJob
      Store.insertJob sStore nextJob
      FL.delete sProcessJob jh

      let update v = v { sProcess = max (sProcess v - 1) 0
                       , sSchedAt = min (sSchedAt v) nextSchedAt
                       }

      FL.adjust sFuncStatList update (jFuncName job')
      nextMainTimer sched 0

status :: Scheduler -> IO [FuncStat]
status (Scheduler {..}) = FL.elems sFuncStatList
