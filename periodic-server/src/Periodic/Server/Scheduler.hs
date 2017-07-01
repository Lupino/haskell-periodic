{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import qualified Control.Concurrent.Lock      as L (Lock, new, with)
import           Control.Exception            (SomeException, try)
import           Control.Monad                (void, when)
import qualified Data.ByteString.Char8        as B (concat)
import           Data.Int                     (Int64)
import           Periodic.Server.Agent        (Agent, aAlive, send)
import           Periodic.Server.FuncList     (FuncList, FuncName, newFuncList)
import qualified Periodic.Server.FuncList     as FL
import           Periodic.Server.FuncStat
import           Periodic.Server.GrabQueue
import           Periodic.Server.JobQueue     (JobQueue)
import qualified Periodic.Server.JobQueue     as JQ
import           Periodic.Server.ProcessQueue (ProcessQueue)
import qualified Periodic.Server.ProcessQueue as PQ
import           Periodic.Types               (Command (JobAssign), Job (..),
                                               JobHandle, jHandle, nullChar,
                                               unHandle, unparseJob)
import           Periodic.Utils               (getEpochTime, tryIO)

import           Control.Concurrent           (ThreadId, forkIO, killThread,
                                               threadDelay)
import           Data.IORef                   (IORef, atomicModifyIORef',
                                               newIORef)
import           Data.Maybe                   (fromJust, isJust)

import           Periodic.Server.Store        (Store)
import qualified Periodic.Server.Store        as Store

data Scheduler = Scheduler { sFuncStatList  :: FuncStatList
                           , sGrabQueue     :: GrabQueue
                           , sJobQueue      :: JobQueue
                           , sProcessJob    :: ProcessQueue
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

  mapM_ (restoreJob sched) =<< Store.dumpJob sStore
  mapM_ (adjustFuncStat sched) =<< (FL.keys sJobQueue)

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
  else do
    Store.insertJob sStore job
    JQ.pushJob sJobQueue job

  adjustFuncStat sched fn
  nextMainTimer sched 0

  where fn = jFuncName job
        jh = jHandle job
        jn = jName job

adjustFuncStat :: Scheduler -> FuncName -> IO ()
adjustFuncStat (Scheduler {..}) fn = do
  size <- fromIntegral <$> JQ.sizeJob sJobQueue fn
  sizePQ <- fromIntegral <$> PQ.sizeJob sProcessJob fn
  minJob <- JQ.findMinJob sJobQueue fn
  FL.alter sFuncStatList (update (size + sizePQ) sizePQ minJob) fn

  where update :: Int64 -> Int64 -> Maybe Job -> Maybe FuncStat -> Maybe FuncStat
        update size sizePQ Nothing Nothing = Just ((funcStat fn) { sJob = size
                                                                 , sProcess = sizePQ
                                                                 })
        update size sizePQ (Just job) Nothing = Just ((funcStat fn) { sJob = size
                                                                    , sProcess = sizePQ
                                                                    , sSchedAt = jSchedAt job
                                                                    })
        update size sizePQ Nothing (Just stat) = Just (stat { sJob = size
                                                            , sProcess = sizePQ
                                                            })
        update size sizePQ (Just job) (Just stat) = Just (stat { sJob = size
                                                               , sProcess = sizePQ
                                                               , sSchedAt = jSchedAt job
                                                               })

restoreJob :: Scheduler -> Job -> IO ()
restoreJob (Scheduler {..}) job = do
  JQ.pushJob sJobQueue job

removeJob :: Scheduler -> Job -> IO ()
removeJob sched@(Scheduler {..}) job = do
  has <- JQ.memberJob sJobQueue (jFuncName job) (jName job)
  when has $ JQ.removeJob sJobQueue (jFuncName job) (jName job)

  isProc <- FL.member sProcessJob (jHandle job)
  when isProc $ PQ.removeJob sProcessJob (jFuncName job) (jName job)
  Store.deleteJob sStore (jHandle job)
  adjustFuncStat sched (jFuncName job)

  nextMainTimer sched 0

dumpJob :: Scheduler -> IO [Job]
dumpJob (Scheduler {..}) = Store.dumpJob sStore

addFunc :: Scheduler -> FuncName -> IO ()
addFunc sched@(Scheduler {..}) n = do
  FL.alter sFuncStatList updateStat n
  nextMainTimer sched 0

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just ((funcStat n) { sWorker = 1 })
        updateStat (Just fs) = Just (fs { sWorker = sWorker fs + 1 })

removeFunc :: Scheduler -> FuncName -> IO ()
removeFunc sched@(Scheduler {..}) n = do
  FL.alter sFuncStatList updateStat n
  nextMainTimer sched 0

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just (funcStat n)
        updateStat (Just fs) = Just (fs { sWorker = max (sWorker fs - 1) 0 })

dropFunc :: Scheduler -> FuncName -> IO ()
dropFunc (Scheduler {..}) n = do
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
      now <- getEpochTime
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
              adjustFuncStat sched (sFuncName st)
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
          e <- try $ assignJob agent job
          case e of
            Left (_::SomeException) -> revertJob job
            Right _ -> do
              PQ.insertJob sProcessJob job
              adjustFuncStat sched (jFuncName job)
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
      atomicModifyIORef' sMainTimerWait (\_ -> (True, ()))
      void . tryIO . threadDelay $ fromIntegral t * 1000000
    atomicModifyIORef' sMainTimerWait (\_ -> (False, ()))
    processJob sched

  atomicModifyIORef' sMainTimer (\_ -> (Just threadID', ()))
  when (isJust threadID && wait) $ killThread (fromJust threadID)

failJob :: Scheduler -> JobHandle -> IO ()
failJob sched@(Scheduler {..}) jh = do
  job <- PQ.lookupJob sProcessJob fn jn
  case job of
    Nothing -> return ()
    Just job' -> do
      JQ.pushJob sJobQueue job'
      PQ.removeJob sProcessJob fn jn
      adjustFuncStat sched fn
      nextMainTimer sched 0

  where (fn, jn) = unHandle jh

doneJob :: Scheduler -> JobHandle -> IO ()
doneJob sched@(Scheduler {..}) jh = do
  job <- PQ.lookupJob sProcessJob fn jn
  case job of
    Nothing -> return ()
    Just _ -> do
      PQ.removeJob sProcessJob fn jn
      adjustFuncStat sched fn
      Store.deleteJob sStore jh

  where (fn, jn) = unHandle jh

schedLaterJob :: Scheduler -> JobHandle -> Int64 -> Int -> IO ()
schedLaterJob sched@(Scheduler {..}) jh later step = do
  job <- PQ.lookupJob sProcessJob fn jn
  case job of
    Nothing -> return ()
    Just job' -> do
      nextSchedAt <- (+) later <$> getEpochTime
      let nextJob = job' { jSchedAt = nextSchedAt, jCount = jCount job' + step }

      JQ.pushJob sJobQueue nextJob
      Store.insertJob sStore nextJob

      PQ.removeJob sProcessJob fn jn
      adjustFuncStat sched fn
      nextMainTimer sched 0

  where (fn, jn) = unHandle jh

status :: Scheduler -> IO [FuncStat]
status (Scheduler {..}) = FL.elems sFuncStatList
