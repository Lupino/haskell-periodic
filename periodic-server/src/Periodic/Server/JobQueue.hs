{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Periodic.Server.JobQueue
  (
    SubJobQueue
  , JobQueue
  , pushJob
  , findMinJob
  , popJob
  , removeJob
  , memberJob
  , dumpJob
  , dumpJobByFuncName
  , sizeJob
  ) where

import           Data.ByteString    (ByteString)
import           Data.HashPSQ       (HashPSQ, delete, findMin, fromList, insert,
                                     member, size, toList)
import           Data.Int           (Int64)
import           Periodic.IOHashMap (IOHashMap, adjust, alter, elems, lookup)
import           Periodic.Types     (FuncName, Job (..))
import           Prelude            hiding (lookup, null)

type SubJobQueue = HashPSQ FuncName Int64 Job

type JobQueue = IOHashMap SubJobQueue

pushJob :: JobQueue -> Job -> IO ()
pushJob q j@(Job {..}) = alter q update jFuncName
  where update :: Maybe SubJobQueue -> Maybe SubJobQueue
        update Nothing   = Just (fromList [(jName, jSchedAt, j)])
        update (Just q') = Just (insert jName jSchedAt j q')

findMinJob :: JobQueue -> FuncName -> IO (Maybe Job)
findMinJob q n = doFindMin <$> lookup q n
  where doFindMin :: Maybe SubJobQueue -> Maybe Job
        doFindMin Nothing   = Nothing
        doFindMin (Just q') = case findMin q' of
                                Nothing        -> Nothing
                                Just (_, _, v) -> Just v

popJob :: JobQueue -> FuncName -> IO (Maybe Job)
popJob q n = do
  j <- findMinJob q n
  case j of
    Nothing -> return Nothing
    Just j' -> do
      adjust q (\v -> delete (jName j') v) n
      return j

removeJob :: JobQueue -> FuncName -> ByteString -> IO ()
removeJob q n jn = adjust q (\v -> delete jn v) n

memberJob :: JobQueue -> FuncName -> ByteString -> IO Bool
memberJob q n jn = go <$> lookup q n
  where go :: Maybe SubJobQueue -> Bool
        go Nothing   = False
        go (Just q') = member jn q'

dumpJob :: JobQueue -> IO [Job]
dumpJob jq = concat . map go <$> elems jq

  where go :: SubJobQueue -> [Job]
        go sq = map (\(_, _, v) -> v) $ toList sq

dumpJobByFuncName :: JobQueue -> FuncName -> IO [Job]
dumpJobByFuncName jq n = do
  q <- lookup jq n
  case q of
    Nothing -> return []
    Just q' -> return $ map (\(_, _, v) -> v) $ toList q'

sizeJob :: JobQueue -> FuncName -> IO Int
sizeJob q n = go <$> lookup q n
  where go :: Maybe SubJobQueue -> Int
        go Nothing   = 0
        go (Just q') = size q'
