module Periodic.Server.Hook
  ( HookEvent (..)
  , HookName (..)
  , Hook (..)
  , emptyHook
  , socketHook
  , genHook
  , hookName
  , eventPushJob
  , eventFailJob
  , eventDoneJob
  , eventSchedLaterJob
  , eventAcquireLock
  , eventReleaseLock
  , eventRemoveJob
  , runHook
  , GetHookName
  ) where


import           Control.Monad            (forM_, forever, void)
import qualified Data.ByteString.Char8   as B (pack, unpack)
import           Metro.Class             (Transport (..))
import qualified Metro.Lock              as L (Lock, new, with)
import           Metro.TP.Socket         (socket)
import           Periodic.Server.Persist (Persist (..))
import           Periodic.Types          (FuncName (..), Job, JobHandle,
                                          LockName (..), getFuncName, unHandle)
import           System.Environment      (lookupEnv)
import           System.Log.Logger       (errorM, infoM)
import           Text.Read               (readMaybe)
import           UnliftIO                (MonadIO (..), STM, TBQueue, TVar, async,
                                          atomically,
                                          isFullTBQueue, newTBQueueIO,
                                          newTVarIO, readTVar, readTVarIO,
                                          timeout, tryAny, tryReadTBQueue,
                                          writeTBQueue, writeTVar, readTBQueue)

newtype HookEvent = HookEvent String
  deriving (Show)

newtype HookName = HookName String
  deriving (Show)

newtype Hook db = Hook
  { runHook_ :: db -> HookEvent -> HookName -> Double -> IO ()
  }

eventPushJob       = HookEvent "pushJob"
eventFailJob       = HookEvent "failJob"
eventDoneJob       = HookEvent "doneJob"
eventSchedLaterJob = HookEvent "schedLaterJob"
eventAcquireLock   = HookEvent "acquireLock"
eventReleaseLock   = HookEvent "releaseLock"
eventRemoveJob     = HookEvent "removeJob"

runHook :: (MonadIO m, GetHookName a) => Hook db -> db -> HookEvent -> a -> Double -> m ()
runHook hook db evt n c = liftIO $ runHook_ hook db evt (hookName n) c

emptyHook :: Hook db
emptyHook = Hook $ \_ _ _ _ -> pure ()

class GetHookName a where
  hookName :: a -> HookName

instance GetHookName FuncName where
  hookName (FuncName fn) = HookName $ B.unpack fn

instance GetHookName Job where
  hookName = hookName . getFuncName

instance GetHookName LockName where
  hookName (LockName ln) = HookName $ B.unpack ln

instance GetHookName JobHandle where
  hookName = hookName . fst . unHandle


genSocketHook
  :: Transport tp
  => L.Lock -> TVar (Maybe tp) -> TransportConfig tp
  -> db -> HookEvent -> HookName -> Double -> IO ()
genSocketHook lock tph config _ (HookEvent evt) (HookName name) count = L.with lock $ do
  mtp <- readTVarIO tph
  case mtp of
    Nothing -> do
      tp <- newTP config
      r <- timeout s10 $ tryAny $ sendData tp bs
      case r of
        Nothing       -> errTimeout >> closeTP tp
        Just (Left e) -> logErr e >> closeTP tp
        _             -> atomically $ writeTVar tph $ Just tp
    Just tp -> do
      r <- timeout s10 $ tryAny $ sendData tp bs

      let doErr = do
            closeTP tp
            atomically $ writeTVar tph Nothing

      case r of
        Nothing       -> errTimeout >> doErr
        Just (Left e) -> logErr e >> doErr
        _             -> pure ()

  where bs = B.pack $ evt ++ ":" ++ name ++  ":" ++ show count ++ "\r\n"
        s10 = 10000000 -- 10s
        errTimeout = errorM "Periodic.Server.Hook" $ "Send event timeout " ++ B.unpack bs
        logErr e = errorM "Periodic.Server.Hook" $ "Send event error " ++ show e


data MetricItem db = MetricItem db String String Int

metricQueueMaxSize :: Int
metricQueueMaxSize = 10000

metricDropLogEvery :: Int
metricDropLogEvery = 1000

readPositiveEnv :: String -> Int -> IO Int
readPositiveEnv key fallback = do
  mv <- lookupEnv key
  case mv >>= readMaybe of
    Just v | v > 0 -> pure v
    _              -> pure fallback

drainMetricQueue :: TBQueue (MetricItem db) -> MetricItem db -> IO [MetricItem db]
drainMetricQueue queue item0 = atomically $ go [item0]
  where
    go acc = do
      mItem <- tryReadTBQueue queue
      case mItem of
        Nothing   -> pure $ reverse acc
        Just item -> go (item : acc)

runPersistMetricWorker :: Persist db => TBQueue (MetricItem db) -> IO ()
runPersistMetricWorker queue = forever $ do
  item0@(MetricItem db _ _ _) <- atomically $ readTBQueue queue
  itemList <- drainMetricQueue queue item0
  let metrics = map (\(MetricItem _ evt name durationMs) -> (evt, name, durationMs)) itemList
  r <- tryAny $ insertMetrics db metrics
  case r of
    Left e  -> errorM "Periodic.Server.Hook" $ "Persist metric error " ++ show e
    Right _ -> pure ()

markMetricDropped :: TVar Int -> Int -> STM (Maybe Int)
markMetricDropped droppedCounter logEvery = do
  dropped <- readTVar droppedCounter
  let dropped' = dropped + 1
  writeTVar droppedCounter dropped'
  if dropped' `mod` logEvery == 0 then pure (Just dropped')
                               else pure Nothing

genPersistHook
  :: Persist db
  => TBQueue (MetricItem db)
  -> TVar Int
  -> Int
  -> db -> HookEvent -> HookName -> Double -> IO ()
genPersistHook queue droppedCounter dropLogEvery db (HookEvent evt) (HookName name) count = do
  mDropped <- atomically $ do
    full <- isFullTBQueue queue
    if full then markMetricDropped droppedCounter dropLogEvery
            else writeTBQueue queue (MetricItem db evt name (floor $ count * 1000)) >> pure Nothing

  forM_ mDropped $ \dropped ->
    errorM "Periodic.Server.Hook" $
      "Persist metric queue is full, dropped metrics count=" ++ show dropped


socketHook :: String -> IO (Hook db)
socketHook hostPort = do
  h <- newTVarIO Nothing
  lock <- L.new
  return . Hook $ genSocketHook lock h $ socket hostPort


persistHook :: Persist db => IO (Hook db)
persistHook = do
  queueSize <- readPositiveEnv "PERIODIC_METRIC_QUEUE_MAX_SIZE" metricQueueMaxSize
  dropLogEvery <- readPositiveEnv "PERIODIC_METRIC_DROP_LOG_EVERY" metricDropLogEvery
  queue <- newTBQueueIO $ fromIntegral queueSize
  droppedCounter <- newTVarIO 0
  void $ async $ runPersistMetricWorker queue
  infoM "Periodic.Server.Hook" $
    "Persist hook metric worker started, queueMaxSize="
    ++ show queueSize ++ ", dropLogEvery=" ++ show dropLogEvery
  return . Hook $ \db evt name count ->
    genPersistHook queue droppedCounter dropLogEvery db evt name count


genHook :: Persist db => String -> IO (Hook db)
genHook ""        = return emptyHook
genHook "persist" = persistHook
genHook hostPort  = socketHook hostPort
