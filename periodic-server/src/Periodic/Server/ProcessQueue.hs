{-# LANGUAGE RecordWildCards #-}
module Periodic.Server.ProcessQueue
  (
    SubProcessQueue
  , ProcessQueue
  , insertJob
  , lookupJob
  , removeJob
  , memberJob
  , dumpJob
  , sizeJob
  ) where

import           Data.ByteString     (ByteString)
import           Data.HashMap.Strict (HashMap, delete, insert, member, size)
import qualified Data.HashMap.Strict as HM (elems, fromList, lookup)
import           Periodic.IOHashMap  (IOHashMap, adjust, alter, elems, lookup)
import           Periodic.Types      (FuncName, Job (..), hashJobName)
import           Prelude             hiding (lookup)

type SubProcessQueue = HashMap ByteString Job

type ProcessQueue = IOHashMap FuncName SubProcessQueue

insertJob :: ProcessQueue -> Job -> IO ()
insertJob q j@Job{..} = alter q update jFuncName
  where update :: Maybe SubProcessQueue -> Maybe SubProcessQueue
        update Nothing   = Just (HM.fromList [(hashJobName jName, j)])
        update (Just q') = Just (insert (hashJobName jName) j q')

lookupJob :: ProcessQueue -> FuncName -> ByteString -> IO (Maybe Job)
lookupJob q fn jn = maybe Nothing (HM.lookup jn) <$> lookup q fn

removeJob :: ProcessQueue -> FuncName -> ByteString -> IO ()
removeJob q fn jn = adjust q (delete jn) fn

memberJob :: ProcessQueue -> FuncName -> ByteString -> IO Bool
memberJob q fn jn = go <$> lookup q fn
  where go :: Maybe SubProcessQueue -> Bool
        go Nothing   = False
        go (Just q') = member jn q'

dumpJob :: ProcessQueue -> IO [Job]
dumpJob jq = concatMap go <$> elems jq

  where go :: SubProcessQueue -> [Job]
        go = HM.elems

sizeJob :: ProcessQueue -> FuncName -> IO Int
sizeJob q n = go <$> lookup q n
  where go :: Maybe SubProcessQueue -> Int
        go Nothing   = 0
        go (Just q') = size q'
