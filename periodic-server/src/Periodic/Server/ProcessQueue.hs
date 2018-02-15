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
  , dumpJobByFuncName
  , sizeJob
  ) where

import           Data.ByteString     (ByteString)
import           Data.HashMap.Strict (HashMap, delete, insert, member, size)
import qualified Data.HashMap.Strict as HM (elems, fromList, lookup)
import           Periodic.IOHashMap  (IOHashMap, adjust, alter, elems, lookup)
import           Periodic.Types      (FuncName, Job (..), JobName)
import           Prelude             hiding (lookup)

type SubProcessQueue = HashMap JobName Job

type ProcessQueue = IOHashMap FuncName SubProcessQueue

insertJob :: ProcessQueue -> Job -> IO ()
insertJob q j@Job{..} = alter q update jFuncName
  where update :: Maybe SubProcessQueue -> Maybe SubProcessQueue
        update Nothing   = Just (HM.fromList [(jName, j)])
        update (Just q') = Just (insert jName j q')

lookupJob :: ProcessQueue -> FuncName -> JobName -> IO (Maybe Job)
lookupJob q fn jn = maybe Nothing (HM.lookup jn) <$> lookup q fn

removeJob :: ProcessQueue -> FuncName -> JobName -> IO ()
removeJob q fn jn = adjust q (delete jn) fn

memberJob :: ProcessQueue -> FuncName -> JobName -> IO Bool
memberJob q fn jn = go <$> lookup q fn
  where go :: Maybe SubProcessQueue -> Bool
        go Nothing   = False
        go (Just q') = member jn q'

dumpJob :: ProcessQueue -> IO [Job]
dumpJob jq = concatMap go <$> elems jq

  where go :: SubProcessQueue -> [Job]
        go = HM.elems

dumpJobByFuncName :: ProcessQueue -> FuncName -> IO [Job]
dumpJobByFuncName jq n = do
  q <- lookup jq n
  case q of
    Nothing -> return []
    Just q' -> return $ HM.elems q'

sizeJob :: ProcessQueue -> FuncName -> IO Int
sizeJob q n = go <$> lookup q n
  where go :: Maybe SubProcessQueue -> Int
        go Nothing   = 0
        go (Just q') = size q'
