{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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

import           Data.ByteString          (ByteString)
import           Data.HashMap.Strict      (HashMap, delete, fromList, insert,
                                           member, size, toList)
import qualified Data.HashMap.Strict      as HM (lookup)
import           Periodic.Server.FuncList (FuncList, FuncName, adjust, alter,
                                           elems, lookup)
import           Periodic.Types           (Job (..))
import           Prelude                  hiding (lookup, null)

type SubProcessQueue = HashMap FuncName Job

type ProcessQueue = FuncList SubProcessQueue

insertJob :: ProcessQueue -> Job -> IO ()
insertJob q j@(Job {..}) = alter q update jFuncName
  where update :: Maybe SubProcessQueue -> Maybe SubProcessQueue
        update Nothing   = Just (fromList [(jName, j)])
        update (Just q') = Just (insert jName j q')

lookupJob :: ProcessQueue -> FuncName -> ByteString -> IO (Maybe Job)
lookupJob q n jn = maybe Nothing (HM.lookup jn) <$> lookup q n

removeJob :: ProcessQueue -> FuncName -> ByteString -> IO ()
removeJob q n jn = adjust q (\v -> delete jn v) n

memberJob :: ProcessQueue -> FuncName -> ByteString -> IO Bool
memberJob q n jn = go <$> lookup q n
  where go :: Maybe SubProcessQueue -> Bool
        go Nothing   = False
        go (Just q') = member jn q'

dumpJob :: ProcessQueue -> IO [Job]
dumpJob jq = concat . map go <$> elems jq

  where go :: SubProcessQueue -> [Job]
        go sq = map (\(_, v) -> v) $ toList sq

dumpJobByFuncName :: ProcessQueue -> FuncName -> IO [Job]
dumpJobByFuncName jq n = do
  q <- lookup jq n
  case q of
    Nothing -> return []
    Just q' -> return $ map (\(_, v) -> v) $ toList q'

sizeJob :: ProcessQueue -> FuncName -> IO Int
sizeJob q n = go <$> lookup q n
  where go :: Maybe SubProcessQueue -> Int
        go Nothing   = 0
        go (Just q') = size q'
