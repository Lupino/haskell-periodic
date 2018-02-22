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
  , broadcastFunc
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
import           Data.Maybe                   (fromJust, fromMaybe, isJust)
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
import           Periodic.Types.Internal      (nullChar)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand (ServerCommand (JobAssign))
import           Periodic.Utils               (getEpochTime)

import qualified Data.Store                   as DS
import           System.Directory             (createDirectoryIfMissing,
                                               doesFileExist)
import           System.FilePath              ((</>))

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM            (atomically)


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
                           , sAlive        :: TVar Bool
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
  sAlive         <- newTVarIO True
  let sStorePath = path </> "dump.db"
      sched = Scheduler {..}

  createDirectoryIfMissing True path
  exists <- doesFileExist sStorePath
  when exists $ mapM_ (restoreJob sched) =<< loadJob sched
  mapM_ (adjustFuncStat sched) =<< FL.keys sJobQueue

  startScheduler sched 0
  repeatTimer' sRevertTimer 300 $ revertProcessQueue sched
  repeatTimer' sStoreTimer  300 $ saveJob sched

  return sched

startScheduler :: Scheduler -> Int -> IO ()
startScheduler sched@Scheduler{..} delay =
  startTimer' sMainTimer delay $ processJob sched

pushJob :: Scheduler -> Job -> IO ()
pushJob sched@Scheduler{..} job = do
  exists <- JQ.memberJob sJobQueue fn jn
  isProc <- PQ.memberJob sProcessJob fn (hashJobName jn)
  if exists then JQ.pushJob sJobQueue job
            else unless isProc $ JQ.pushJob sJobQueue job

  adjustFuncStat sched fn
  startScheduler sched 0

  where fn = jFuncName job
        jh = jHandle job
        jn = jName job

adjustFuncStat :: Scheduler -> FuncName -> IO ()
adjustFuncStat Scheduler{..} fn = L.with sLocker $ do
  size <- fromIntegral <$> JQ.sizeJob sJobQueue fn
  sizePQ <- fromIntegral <$> PQ.sizeJob sProcessJob fn
  schedAt <- do
    minJob <- JQ.findMinJob sJobQueue fn
    case minJob of
      Nothing  -> getEpochTime
      Just job -> return $ jSchedAt job
  FL.alter sFuncStatList (update (size + sizePQ) sizePQ schedAt) fn

  where update :: Int64 -> Int64 -> Int64 -> Maybe FuncStat -> Maybe FuncStat
        update size sizePQ schedAt st =
          Just ((fromMaybe (funcStat fn) st) { sJob = size
                                             , sProcess = sizePQ
                                             , sSchedAt = schedAt
                                             })

restoreJob :: Scheduler -> Job -> IO ()
restoreJob Scheduler{..} = JQ.pushJob sJobQueue

removeJob :: Scheduler -> Job -> IO ()
removeJob sched@Scheduler{..} job = do
  has <- JQ.memberJob sJobQueue (jFuncName job) (jName job)
  when has $ JQ.removeJob sJobQueue (jFuncName job) (jName job)

  isProc <- PQ.memberJob sProcessJob (jFuncName job) (hashJobName $ jName job)
  when isProc $ PQ.removeJob sProcessJob (jFuncName job) (hashJobName $ jName job)
  adjustFuncStat sched (jFuncName job)

  startScheduler sched 0

dumpJob :: Scheduler -> IO [Job]
dumpJob Scheduler{..} = do
  js <- JQ.dumpJob sJobQueue
  js' <- PQ.dumpJob sProcessJob
  return $ js ++ js'

saveJob :: Scheduler -> IO ()
saveJob sched@Scheduler{..} =
  dumpJob sched >>= B.writeFile sStorePath . DS.encode

loadJob :: Scheduler -> IO [Job]
loadJob Scheduler{..} = DS.decodeIO =<< B.readFile sStorePath

addFunc :: Scheduler -> FuncName -> IO ()
addFunc sched n = broadcastFunc sched n False

broadcastFunc :: Scheduler -> FuncName -> Bool -> IO ()
broadcastFunc sched@Scheduler{..} n cast = L.with sLocker $ do
  FL.alter sFuncStatList updateStat n
  startScheduler sched 0

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just ((funcStat n) {sWorker = 1, sBroadcast = cast})
        updateStat (Just fs) = Just (fs { sWorker = sWorker fs + 1, sBroadcast = cast })

removeFunc :: Scheduler -> FuncName -> IO ()
removeFunc sched@Scheduler{..} n = L.with sLocker $ do
  FL.alter sFuncStatList updateStat n
  startScheduler sched 0

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just (funcStat n)
        updateStat (Just fs) = Just (fs { sWorker = max (sWorker fs - 1) 0 })

dropFunc :: Scheduler -> FuncName -> IO ()
dropFunc Scheduler{..} n = L.with sLocker $ do
  st <- FL.lookup sFuncStatList n
  when (isJust st) $
    when (sWorker (fromJust st) == 0) $ do
      FL.delete sFuncStatList n
      FL.delete sJobQueue n

pushGrab :: Scheduler -> IOList FuncName -> IOList JobHandle -> Agent -> IO ()
pushGrab sched@Scheduler{..} fl jh ag = do
  pushAgent sGrabQueue fl jh ag
  startScheduler sched 0

processJob :: Scheduler -> IO ()
processJob sched@Scheduler{..} = do
  st <- getFirstSched sFuncStatList sGrabQueue
  case st of
    Nothing -> startScheduler sched 100
    Just st' -> do
      now <- getEpochTime
      if now < sSchedAt st' then startScheduler sched (fromIntegral $ sSchedAt st' - now)
                            else submitJob now st'

  where revertJob :: Job -> IO ()
        revertJob job = do
          JQ.pushJob sJobQueue job
          startScheduler sched 0

        popJobThen :: Int64 -> FuncStat -> (Job -> IO ()) -> IO ()
        popJobThen now st done = do
          job <- JQ.popJob sJobQueue (sFuncName st)
          case job of
            Nothing -> do
              adjustFuncStat sched (sFuncName st)
              startScheduler sched 0
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

        popAgentListThen :: FuncStat -> (Agent -> Job -> IO ()) -> Job -> IO ()
        popAgentListThen st done job = do
          agents <- popAgentList sGrabQueue (sFuncName st)
          when (null agents) $ revertJob job
          mapM_ (flip done job . snd) agents

        doneSubmitJob :: Bool -> Agent -> Job -> IO ()
        doneSubmitJob cast agent job = do
          e <- try $ assignJob agent job
          case e of
            Left (_::SomeException) -> unless cast $ revertJob job
            Right _ -> do
              unless cast $ do
                nextSchedAt <- getEpochTime
                PQ.insertJob sProcessJob job { jSchedAt = nextSchedAt }

              adjustFuncStat sched (jFuncName job)
              startScheduler sched 0

        prepareAgent :: FuncStat -> (Bool -> Agent -> Job -> IO ()) -> Job -> IO ()
        prepareAgent st@FuncStat{sBroadcast=True} done  = popAgentListThen st (done True)
        prepareAgent st@FuncStat{sBroadcast=False} done = popMaybeAgentThen st (done False)

        submitJob :: Int64 -> FuncStat -> IO ()
        submitJob now st = popJobThen now st $ prepareAgent st doneSubmitJob

assignJob :: Agent -> Job -> IO ()
assignJob agent job = send agent (JobAssign (jHandle job) job)

failJob :: Scheduler -> JobHandle -> IO ()
failJob sched@Scheduler{..} jh = do
  job <- PQ.lookupJob sProcessJob fn jn
  when (isJust job) $ do
    nextSchedAt <- getEpochTime
    retryJob sched ((fromJust job) {jSchedAt = nextSchedAt})

  where (fn, jn) = unHandle jh

retryJob :: Scheduler -> Job -> IO ()
retryJob sched@Scheduler{..} job = do
  JQ.pushJob sJobQueue job
  PQ.removeJob sProcessJob fn (hashJobName jn)
  adjustFuncStat sched fn
  startScheduler sched 0

  where  fn = jFuncName job
         jn = jName job


doneJob :: Scheduler -> JobHandle -> IO ()
doneJob sched@Scheduler{..} jh = do
  job <- PQ.lookupJob sProcessJob fn jn
  when (isJust job) $ do
    PQ.removeJob sProcessJob fn jn
    adjustFuncStat sched fn

  where (fn, jn) = unHandle jh

schedLaterJob :: Scheduler -> JobHandle -> Int64 -> Int -> IO ()
schedLaterJob sched@Scheduler{..} jh later step = do
  job <- PQ.lookupJob sProcessJob fn jn
  when (isJust job) $ do
    let job' = fromJust job
    nextSchedAt <- (+) later <$> getEpochTime
    retryJob sched job' {jSchedAt = nextSchedAt , jCount = jCount job' + step}

  where (fn, jn) = unHandle jh

status :: Scheduler -> IO [FuncStat]
status Scheduler{..} = FL.elems sFuncStatList

revertProcessQueue :: Scheduler -> IO ()
revertProcessQueue sched@Scheduler{..} = do
  now <- getEpochTime
  mapM_ (failJob sched . jHandle) =<< filter (isTimeout now) <$> PQ.dumpJob sProcessJob
  where isTimeout :: Int64 -> Job -> Bool
        isTimeout t1 Job{jSchedAt = t} = (t + 600) < t1

shutdown :: Scheduler -> IO ()
shutdown sched@Scheduler{..} = L.with sLocker $ do
  alive <- atomically $ do
    t <- readTVar sAlive
    writeTVar sAlive False
    return t
  when alive $ do
    clearTimer sMainTimer
    clearTimer sRevertTimer
    clearTimer sStoreTimer
    saveJob sched
    void $ forkIO sCleanup
