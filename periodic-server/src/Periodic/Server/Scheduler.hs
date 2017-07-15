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
  , shutdown
  ) where

import           Control.Exception            (SomeException, try)
import           Control.Monad                (unless, void, when)
import qualified Data.ByteString              as B (concat, readFile, writeFile)
import           Data.Int                     (Int64)
import           Periodic.Agent               (Agent, aAlive, send)
import           Periodic.IOHashMap           (newIOHashMap)
import qualified Periodic.IOHashMap           as FL
import           Periodic.IOList              (IOList)
import qualified Periodic.IOList              as IL
import qualified Periodic.Lock                as L (Lock, new, with)
import           Periodic.Server.FuncStat
import           Periodic.Server.GrabQueue
import           Periodic.Server.JobQueue     (JobQueue)
import qualified Periodic.Server.JobQueue     as JQ
import           Periodic.Server.ProcessQueue (ProcessQueue)
import qualified Periodic.Server.ProcessQueue as PQ
import           Periodic.Timer
import           Periodic.Types               (Command (JobAssign), nullChar)
import           Periodic.Types.Job
import           Periodic.Utils               (getEpochTime)

import qualified Data.Store                   as DS
import           System.Directory             (createDirectoryIfMissing,
                                               doesFileExist)
import           System.FilePath              ((</>))

import           Control.Concurrent           (forkIO)
import           Data.IORef                   (IORef, atomicModifyIORef',
                                               newIORef)


data Scheduler = Scheduler { sFuncStatList :: FuncStatList
                           , sLocker       :: L.Lock
                           , sGrabQueue    :: GrabQueue
                           , sJobQueue     :: JobQueue
                           , sProcessJob   :: ProcessQueue
                           , sMainTimer    :: Timer
                           , sRevertTimer  :: Timer
                           , sStoreTimer   :: Timer
                           , sStorePath    :: FilePath
                           , sCleanup      :: IO ()
                           , sAlive        :: IORef Bool
                           }

newScheduler :: FilePath -> IO () -> IO Scheduler
newScheduler path sCleanup = do
  sFuncStatList  <- newIOHashMap
  sLocker        <- L.new
  sGrabQueue     <- newGrabQueue
  sJobQueue      <- newIOHashMap
  sProcessJob    <- newIOHashMap
  sMainTimer     <- newTimer
  sRevertTimer   <- newTimer
  sStoreTimer    <- newTimer
  sAlive         <- newIORef True
  let sStorePath = path </> "dump.db"
      sched = Scheduler {..}

  createDirectoryIfMissing True path
  exists <- doesFileExist sStorePath
  when exists $ mapM_ (restoreJob sched) =<< loadJob sched
  mapM_ (adjustFuncStat sched) =<< (FL.keys sJobQueue)

  initTimer sMainTimer   $ processJob sched
  initTimer sRevertTimer $ revertProcessQueue sched
  initTimer sStoreTimer  $ saveJob sched

  startTimer sMainTimer 0
  repeatTimer' sRevertTimer 300
  repeatTimer' sStoreTimer  300

  return sched

pushJob :: Scheduler -> Job -> IO ()
pushJob sched@(Scheduler {..}) job = do
  exists <- JQ.memberJob sJobQueue fn jn
  isProc <- FL.member sProcessJob jh
  if exists then JQ.pushJob sJobQueue job
            else unless isProc $ JQ.pushJob sJobQueue job

  adjustFuncStat sched fn
  startTimer sMainTimer 0

  where fn = jFuncName job
        jh = jHandle job
        jn = jName job

adjustFuncStat :: Scheduler -> FuncName -> IO ()
adjustFuncStat (Scheduler {..}) fn = L.with sLocker $ do
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
  adjustFuncStat sched (jFuncName job)

  startTimer sMainTimer 0

dumpJob :: Scheduler -> IO [Job]
dumpJob (Scheduler {..}) = do
  js <- JQ.dumpJob sJobQueue
  js' <- PQ.dumpJob sProcessJob
  return $ concat [js, js']

saveJob :: Scheduler -> IO ()
saveJob sched@(Scheduler {..}) =
  dumpJob sched >>= B.writeFile sStorePath . DS.encode

loadJob :: Scheduler -> IO [Job]
loadJob (Scheduler {..}) = DS.decodeIO =<< B.readFile sStorePath

addFunc :: Scheduler -> FuncName -> IO ()
addFunc (Scheduler {..}) n = L.with sLocker $ do
  FL.alter sFuncStatList updateStat n
  startTimer sMainTimer 0

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just ((funcStat n) { sWorker = 1 })
        updateStat (Just fs) = Just (fs { sWorker = sWorker fs + 1 })

removeFunc :: Scheduler -> FuncName -> IO ()
removeFunc (Scheduler {..}) n = L.with sLocker $ do
  FL.alter sFuncStatList updateStat n
  startTimer sMainTimer 0

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just (funcStat n)
        updateStat (Just fs) = Just (fs { sWorker = max (sWorker fs - 1) 0 })

dropFunc :: Scheduler -> FuncName -> IO ()
dropFunc (Scheduler {..}) n = L.with sLocker $ do
  st <- FL.lookup sFuncStatList n
  case st of
    Nothing -> return ()
    Just st' -> do
      if sWorker st' > 0 then return ()
                         else do
                           FL.delete sFuncStatList n
                           FL.delete sJobQueue n

pushGrab :: Scheduler -> IOList FuncName -> IOList JobHandle -> Agent -> IO ()
pushGrab (Scheduler {..}) fl jh ag = do
  pushAgent sGrabQueue fl jh ag
  startTimer sMainTimer 0

processJob :: Scheduler -> IO ()
processJob sched@(Scheduler {..}) = do
  st <- getFirstSched sFuncStatList sGrabQueue
  case st of
    Nothing -> startTimer' sMainTimer 100
    Just st' -> do
      now <- getEpochTime
      if now < (sSchedAt st') then startTimer' sMainTimer (fromIntegral $ sSchedAt st' - now)
                              else submitJob now st'

  where revertJob :: Job -> IO ()
        revertJob job = do
          JQ.pushJob sJobQueue job
          startTimer sMainTimer 0

        popJobThen :: Int64 -> FuncStat -> (Job -> IO ()) -> IO ()
        popJobThen now st done = do
          job <- JQ.popJob sJobQueue (sFuncName st)
          case job of
            Nothing -> do
              adjustFuncStat sched (sFuncName st)
              startTimer sMainTimer 0
            Just job' -> if jSchedAt job' > now then revertJob job'
                                                else done job'

        popMaybeAgentThen :: FuncStat -> (Agent -> Job -> IO ()) -> Job -> IO ()
        popMaybeAgentThen st done job = do
          agent <- popMaybeAgent sGrabQueue (sFuncName st)
          case agent of
            Nothing           -> revertJob job
            Just (jq, agent') -> do
              alive <- aAlive agent'
              if alive then IL.insert jq (jHandle job) >> done agent' job
                       else revertJob job

        doneSubmitJob :: Agent -> Job -> IO ()
        doneSubmitJob agent job = do
          e <- try $ assignJob agent job
          case e of
            Left (_::SomeException) -> revertJob job
            Right _ -> do
              nextSchedAt <- getEpochTime
              PQ.insertJob sProcessJob job { jSchedAt = nextSchedAt }
              adjustFuncStat sched (jFuncName job)
              startTimer sMainTimer 0

        submitJob :: Int64 -> FuncStat -> IO ()
        submitJob now st = popJobThen now st $ popMaybeAgentThen st doneSubmitJob

assignJob :: Agent -> Job -> IO ()
assignJob agent job = send agent JobAssign $ B.concat [ jHandle job
                                                      , nullChar
                                                      , encodeJob job
                                                      ]

failJob :: Scheduler -> JobHandle -> IO ()
failJob sched@(Scheduler {..}) jh = do
  job <- PQ.lookupJob sProcessJob fn jn
  case job of
    Nothing -> return ()
    Just job' -> do
      nextSchedAt <- getEpochTime
      let nextJob = job' { jSchedAt = nextSchedAt }

      JQ.pushJob sJobQueue nextJob
      PQ.removeJob sProcessJob fn jn
      adjustFuncStat sched fn
      startTimer sMainTimer 0

  where (fn, jn) = unHandle jh

doneJob :: Scheduler -> JobHandle -> IO ()
doneJob sched@(Scheduler {..}) jh = do
  job <- PQ.lookupJob sProcessJob fn jn
  case job of
    Nothing -> return ()
    Just _ -> do
      PQ.removeJob sProcessJob fn jn
      adjustFuncStat sched fn

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
      PQ.removeJob sProcessJob fn jn
      adjustFuncStat sched fn
      startTimer sMainTimer 0

  where (fn, jn) = unHandle jh

status :: Scheduler -> IO [FuncStat]
status (Scheduler {..}) = FL.elems sFuncStatList

revertProcessQueue :: Scheduler -> IO ()
revertProcessQueue sched@(Scheduler {..}) = do
  now <- getEpochTime
  mapM_ (failJob sched . jHandle) =<< filter (isTimeout now) <$> PQ.dumpJob sProcessJob
  where isTimeout :: Int64 -> Job -> Bool
        isTimeout t1 (Job { jSchedAt = t }) = (t + 600) < t1

shutdown :: Scheduler -> IO ()
shutdown sched@(Scheduler {..}) = L.with sLocker $ do
  alive <- atomicModifyIORef' sAlive $ \v -> (False, v)
  when alive $ do
    clearTimer sMainTimer
    clearTimer sRevertTimer
    clearTimer sStoreTimer
    saveJob sched
    void $ forkIO $ sCleanup
