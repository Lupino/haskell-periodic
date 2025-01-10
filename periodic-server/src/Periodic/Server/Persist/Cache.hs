{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Periodic.Server.Persist.Cache
  ( Cache
  , useCache
  ) where

import           Data.Int                       (Int64)
import           Metro.Utils                    (getEpochTime)
import           Periodic.Server.Persist        (Persist (..), State (..))
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
   data PersistException (Cache db) = CacheException SomeException deriving (Show, Typeable)

   newPersist (UseCache maxSize config) = do
     infoM "Periodic.Server.Persist.Cache" "Cache connected"
     backend <- newPersist config
     memory  <- newPersist useMemory
     return Cache {..}

   getOne           = doGetOne
   insert           = doInsert
   updateState      = doUpdateState
   delete           = doDelete
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
   countPending     = doCountPending
   insertMetric     = doInsertMetric

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
  if length r0 < c then (r0 ++) <$> getPendingJob (backend m) fn ts (c - length r0)
                   else pure r0

doGetLockedJob
  :: forall db . Persist db
  => Cache db -> FuncName -> Int -> IO [Job]
doGetLockedJob m fn c = do
  r0 <- getLockedJob (memory m) fn c
  if length r0 < c then (r0 ++) <$> getLockedJob (backend m) fn (c - length r0)
                   else pure r0

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

doInsertMetric :: Persist db => Cache db -> String -> String -> Int -> IO ()
doInsertMetric m = insertMetric (backend m)
