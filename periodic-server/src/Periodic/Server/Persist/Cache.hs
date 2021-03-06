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

   member           = doMember
   lookup           = doLookup
   insert           = doInsert
   delete           = doDelete
   size             = doSize
   foldr            = doFoldr
   foldrPending     = doFoldrPending
   foldrLocking     = doFoldrLocking
   dumpJob          = doDumpJob
   configSet      m = configSet (backend m)
   configGet      m = configGet (backend m)
   insertFuncName m = insertFuncName (backend m)
   removeFuncName m = removeFuncName (backend m)
   funcList       m = funcList (backend m)
   minSchedAt       = doMinSchedAt
   countPending     = doCountPending

instance Typeable db => Exception (PersistException (Cache db))

useCache :: Int64 -> PersistConfig db -> PersistConfig (Cache db)
useCache = UseCache

doMember :: Persist db => Cache db -> State -> FuncName -> JobName -> IO Bool
doMember m s f j = do
  r <- member (memory m) s f j
  if r then return True
       else member (backend m) s f j

doLookup :: Persist db => Cache db -> State -> FuncName -> JobName -> IO (Maybe Job)
doLookup m s f j = do
  r <- lookup (memory m) s f j
  case r of
    Just v  -> return $ Just v
    Nothing -> lookup (backend m) s f j

doInsert :: Persist db => Cache db -> State -> FuncName -> JobName -> Job -> IO ()
doInsert Cache{..} s f j v = do
  now <- getEpochTime
  memSize <- memorySize memory
  if getSchedAt v > now + 60 || memSize > maxSize
     then doInsert0 backend memory
     else doInsert0 memory backend

  where doInsert0 :: (Persist db0, Persist db1) => db0 -> db1 -> IO ()
        doInsert0 db0 db1 = do
          insert db0 s f j v
          delete db1 f j

doDelete :: Persist db => Cache db -> FuncName -> JobName -> IO ()
doDelete m f j = do
  delete (memory m) f j
  delete (backend m) f j

doSize :: Persist db => Cache db -> State -> FuncName -> IO Int64
doSize m s f = do
  s1 <- size (memory m) s f
  s2 <- size (backend m) s f
  return $ s1 + s2

doFoldr
  :: forall a db
  . Persist db
  => Cache db -> State -> (Job -> a -> a) -> a -> IO a
doFoldr m s f acc =
  foldr (backend m) s f =<< foldr (memory m) s f acc

doFoldrPending
  :: forall a db
  . Persist db
  => Cache db -> Int64 -> [FuncName] -> (Job -> a -> a) -> a -> IO a
doFoldrPending m st fns f acc =
  foldrPending (backend m) st fns f =<< foldrPending (memory m) st fns f acc

doFoldrLocking
  :: forall a db
  . Persist db
  => Cache db -> Int -> FuncName -> (Job -> a -> a) -> a -> IO a
doFoldrLocking m c fn f acc =
  foldrLocking (backend m) c fn f =<< foldrLocking (memory m) c fn f acc

doCountPending
  :: forall a db
  . Persist db
  => Cache db -> Int64 -> [FuncName] -> IO Int
doCountPending m st fns = do
  c1 <- countPending (backend m) st fns
  c2 <- countPending (memory m) st fns
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
