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
import           Control.Monad                (forever, unless, void, when)
import qualified Data.ByteString              as B (concat, readFile, writeFile)
import           Data.Int                     (Int64)
import           Data.Maybe                   (fromJust, fromMaybe, isJust)
import           Periodic.Agent               (Agent, aAlive, send)
import           Periodic.IOHashMap           (IOHashMap, newIOHashMap)
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

import           Control.Concurrent           (forkIO, killThread, myThreadId,
                                               threadDelay)
import           Control.Concurrent.Async     (Async, async, cancel)
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM            (atomically, retry)


data Scheduler = Scheduler { sFuncStatList :: FuncStatList
                           , sLocker       :: L.Lock
                           , sGrabQueue    :: GrabQueue
                           , sJobQueue     :: JobQueue
                           , sProcessJob   :: ProcessQueue
                           , sRevertTimer  :: Timer
                           , sStoreTimer   :: Timer
                           , sStorePath    :: FilePath
                           , sCleanup      :: IO ()
                           , sAlive        :: TVar Bool
                           , sAsyncJob     :: IOHashMap JobHandle (Async ())
                           }

newScheduler :: FilePath -> IO () -> IO Scheduler
newScheduler path sCleanup = do
  sFuncStatList  <- newIOHashMap
  sLocker        <- L.new
  sGrabQueue     <- newGrabQueue
  sJobQueue      <- newIOHashMap
  sProcessJob    <- newIOHashMap
  sAsyncJob      <- newIOHashMap
  sRevertTimer   <- newTimer
  sStoreTimer    <- newTimer
  sAlive         <- newTVarIO True
  let sStorePath = path </> "dump.db"
      sched = Scheduler {..}

  createDirectoryIfMissing True path
  exists <- doesFileExist sStorePath
  when exists $ mapM_ (pushJob sched) =<< loadJob sched
  mapM_ (adjustFuncStat sched) =<< FL.keys sJobQueue

  repeatTimer' sRevertTimer 300 $ revertProcessQueue sched
  repeatTimer' sStoreTimer  300 $ saveJob sched

  return sched

pushJob :: Scheduler -> Job -> IO ()
pushJob sched@Scheduler{..} job = do
  exists <- JQ.memberJob sJobQueue fn jn
  isProc <- PQ.memberJob sProcessJob fn (hashJobName jn)
  if exists then doPushJob
            else unless isProc doPushJob

  adjustFuncStat sched fn

  where fn = jFuncName job
        jh = jHandle job
        jn = jName job

        doPushJob :: IO ()
        doPushJob = do
          w <- FL.lookup sAsyncJob (jHandle job)
          when (isJust w) $ do
            cancel (fromJust w)
            FL.delete sAsyncJob (jHandle job)

          w' <- schedJob sched job
          FL.insert sAsyncJob (jHandle job) w'

          JQ.pushJob sJobQueue job


schedJob :: Scheduler -> Job -> IO (Async ())
schedJob sched@Scheduler{..} job@Job{..} = async . forever $ do
  now <- getEpochTime
  when (jSchedAt > now) . threadDelay . fromIntegral $ (jSchedAt - now) * 1000000
  FuncStat{..} <- atomically $ do
    st <- FL.lookupSTM sFuncStatList jFuncName
    case st of
      Nothing                  -> retry
      Just FuncStat{sWorker=0} -> retry
      Just st'                 -> pure st'
  if sBroadcast then popAgentListThen $ doneSubmitJob True
                else popAgentThen $ doneSubmitJob False

  where popAgentThen :: (Agent -> IO ()) -> IO ()
        popAgentThen done = do
          (jq, agent) <- atomically $ popAgentSTM sGrabQueue jFuncName
          alive <- aAlive agent
          when alive $ do
            IL.insert jq (jHandle job)
            done agent

        popAgentListThen :: (Agent -> IO ()) -> IO ()
        popAgentListThen done = do
          agents <- popAgentList sGrabQueue jFuncName
          mapM_ (done . snd) agents
          unless (null agents) doneJob

        doneSubmitJob :: Bool -> Agent -> IO ()
        doneSubmitJob cast agent = do
          e <- try $ assignJob agent job
          case e of
            Left (_::SomeException) -> pure () -- retry
            Right _ -> do
              unless cast $ do
                nextSchedAt <- getEpochTime
                PQ.insertJob sProcessJob job { jSchedAt = nextSchedAt }

              adjustFuncStat sched jFuncName

              doneJob

        doneJob :: IO ()
        doneJob = do
          JQ.removeJob sJobQueue jFuncName jName
          FL.delete sAsyncJob (jHandle job)
          killThread =<< myThreadId

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

removeJob :: Scheduler -> Job -> IO ()
removeJob sched@Scheduler{..} job = do
  has <- JQ.memberJob sJobQueue (jFuncName job) (jName job)
  when has $ JQ.removeJob sJobQueue (jFuncName job) (jName job)

  isProc <- PQ.memberJob sProcessJob (jFuncName job) (hashJobName $ jName job)
  when isProc $ PQ.removeJob sProcessJob (jFuncName job) (hashJobName $ jName job)
  adjustFuncStat sched (jFuncName job)

  w <- FL.lookup sAsyncJob (jHandle job)
  when (isJust w) $ do
    cancel (fromJust w)
    FL.delete sAsyncJob (jHandle job)

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
broadcastFunc sched@Scheduler{..} n cast = L.with sLocker $
  FL.alter sFuncStatList updateStat n

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just ((funcStat n) {sWorker = 1, sBroadcast = cast})
        updateStat (Just fs) = Just (fs { sWorker = sWorker fs + 1, sBroadcast = cast })

removeFunc :: Scheduler -> FuncName -> IO ()
removeFunc sched@Scheduler{..} n = L.with sLocker $
  FL.alter sFuncStatList updateStat n

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
pushGrab sched@Scheduler{..} = pushAgent sGrabQueue

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

  w <- schedJob sched job
  FL.insert sAsyncJob (jHandle job) w

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
    clearTimer sRevertTimer
    clearTimer sStoreTimer
    mapM_ cancel =<< FL.elems sAsyncJob
    saveJob sched
    void $ forkIO sCleanup
