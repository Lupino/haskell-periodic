{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Periodic.Server.Persist.Cache
  ( Cache
  , useCache
  ) where

import           Control.Applicative           ((<|>))
import           Data.Int                       (Int64)
import           Data.List                      (sortBy)
import           Metro.Utils                    (getEpochTime)
import           Periodic.Server.Persist        (FuncStats (..), Persist (..),
                                                 State (..))
import           Periodic.Server.Persist.Memory (Memory, memorySize, useMemory)
import           Periodic.Types.Job             (FuncName, Job, JobName,
                                                 getFuncName, getName,
                                                 getSchedAt)
import           Prelude                        hiding (foldr, lookup)
import           System.Log.Logger              (infoM)
import           UnliftIO                       (Exception, SomeException,
                                                 Typeable)

data Cache db = Cache
  { memory  :: Memory
  , backend :: db
  , maxSize :: Int64
  }

instance (Typeable db, Persist db) => Persist (Cache db) where
   data PersistConfig (Cache db) = UseCache Int64 (PersistConfig db)
   data PersistException (Cache db) = CacheException SomeException deriving Show

   newPersist (UseCache maxSize config) = do
     infoM "Periodic.Server.Persist.Cache" "Cache connected"
     backend <- newPersist config
     memory  <- newPersist useMemory
     return Cache {..}

   getOne           = doGetOne
   insert           = doInsert
   updateState      = doUpdateState
   delete           = doDelete
   insertPendingUnlessRunning = doInsertPendingUnlessRunning
   size             = doSize
   getRunningJob    = doGetRunningJob
   getPendingJob    = doGetPendingJob
   getLockedJob     = doGetLockedJob
   dumpJob          = doDumpJob
   configSet      m = configSet (backend m)
   configGet      m = configGet (backend m)
   insertFuncName m = insertFuncName (backend m)
   removeFuncName m = removeFuncName (backend m)
   funcList       m = funcList (backend m)
   minSchedAt       = doMinSchedAt
   getFuncStats     = doGetFuncStats
   getAllFuncStats  = doGetAllFuncStats
   countPending     = doCountPending
   insertMetric     = doInsertMetric
   insertMetrics    = doInsertMetrics

instance Typeable db => Exception (PersistException (Cache db))

useCache :: Int64 -> PersistConfig db -> PersistConfig (Cache db)
useCache = UseCache

doGetOne :: Persist db => Cache db -> State -> FuncName -> JobName -> IO (Maybe Job)
doGetOne m s f j = do
  r <- getOne (memory m) s f j
  case r of
    Just v  -> return $ Just v
    Nothing -> getOne (backend m) s f j

doInsert :: Persist db => Cache db -> State -> Job -> IO ()
doInsert Cache{..} s v = do
  now <- getEpochTime
  memSize <- memorySize memory
  if getSchedAt v > now + 60 || memSize > maxSize
     then doInsert0 backend memory
     else doInsert0 memory backend

  where doInsert0 :: (Persist db0, Persist db1) => db0 -> db1 -> IO ()
        doInsert0 db0 db1 = do
          insert db0 s v
          delete db1 f j

        f = getFuncName v
        j = getName     v

doInsertPendingUnlessRunning :: Persist db => Cache db -> Job -> IO Bool
doInsertPendingUnlessRunning m v = do
  let f = getFuncName v
      j = getName v
  memoryRunning <- getOne (memory m) Running f j
  backendRunning <- getOne (backend m) Running f j
  case memoryRunning <|> backendRunning of
    Just _  -> pure False
    Nothing -> doInsert m Pending v >> pure True

doUpdateState :: Persist db => Cache db -> State -> FuncName -> JobName -> IO ()
doUpdateState Cache{..} s f j = do
  updateState backend s f j
  updateState memory s f j

doDelete :: Persist db => Cache db -> FuncName -> JobName -> IO ()
doDelete m f j = do
  delete (memory m) f j
  delete (backend m) f j

doSize :: Persist db => Cache db -> State -> FuncName -> IO Int64
doSize m s f = do
  s1 <- size (memory m) s f
  s2 <- size (backend m) s f
  return $ s1 + s2

doGetRunningJob
  :: forall db . Persist db
  => Cache db -> Int64 -> IO [Job]
doGetRunningJob m ts = do
  r0 <- getRunningJob (memory m) ts
  (r0 ++) <$> getRunningJob (backend m) ts

doGetPendingJob
  :: forall db . Persist db
  => Cache db -> FuncName -> Int64 -> Int -> IO [Job]
doGetPendingJob m fn ts c = do
  r0 <- getPendingJob (memory m) fn ts c
  r1 <- getPendingJob (backend m) fn ts c
  pure $ take c $ sortJobs $ r0 ++ r1

doGetLockedJob
  :: forall db . Persist db
  => Cache db -> FuncName -> Int -> IO [Job]
doGetLockedJob m fn c = do
  r0 <- getLockedJob (memory m) fn c
  r1 <- getLockedJob (backend m) fn c
  pure $ take c $ sortJobs $ r0 ++ r1

doCountPending
  :: forall db . Persist db
  => Cache db -> FuncName -> Int64 -> IO Int
doCountPending m fn ts = do
  c1 <- countPending (backend m) fn ts
  c2 <- countPending (memory m) fn ts
  return $ c1 + c2

doDumpJob :: Persist db => Cache db -> IO [Job]
doDumpJob m = do
  r1 <- dumpJob (memory m)
  r2 <- dumpJob (backend m)
  return $ r1 ++ r2

doMinSchedAt :: Persist db => Cache db -> FuncName -> IO Int64
doMinSchedAt m fn = do
  r1 <- minSchedAt (memory m) fn
  r2 <- minSchedAt (backend m) fn
  return $
    if r1 > 0 && r2 > 0 then min r1 r2
                        else if r1 > 0 then r1
                        else r2

doGetFuncStats :: Persist db => Cache db -> FuncName -> IO FuncStats
doGetFuncStats m fn = do
  r1 <- getFuncStats (memory m) fn
  r2 <- getFuncStats (backend m) fn
  pure FuncStats
    { funcPending = funcPending r1 + funcPending r2
    , funcRunning = funcRunning r1 + funcRunning r2
    , funcLocked = funcLocked r1 + funcLocked r2
    , funcSchedAt = minNonZero (funcSchedAt r1) (funcSchedAt r2)
    }
  where
    minNonZero 0 y = y
    minNonZero x 0 = x
    minNonZero x y = min x y

doGetAllFuncStats :: Persist db => Cache db -> IO [(FuncName, FuncStats)]
doGetAllFuncStats m = do
  fns <- funcList (backend m)
  mapM (\fn -> do
    stats <- doGetFuncStats m fn
    pure (fn, stats)) fns

sortJobs :: [Job] -> [Job]
sortJobs = sortBy jobOrder
  where
    jobOrder a b =
      compare (getSchedAt a, getFuncName a, getName a) (getSchedAt b, getFuncName b, getName b)

doInsertMetric :: Persist db => Cache db -> String -> String -> Int -> IO ()
doInsertMetric m = insertMetric (backend m)

doInsertMetrics :: Persist db => Cache db -> [(String, String, Int)] -> IO ()
doInsertMetrics m = insertMetrics (backend m)
