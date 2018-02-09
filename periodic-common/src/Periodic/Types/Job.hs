{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Periodic.Types.Job
  (
    FuncName
  , JobName
  , Workload
  , JobHandle
  , Job (..)
  , newJob
  , decodeJob
  , encodeJob
  , jHandle
  , unHandle
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B (breakSubstring, concat, drop,
                                               empty, length, null, pack)
import           Data.Int                (Int64)

import           Data.Maybe              (catMaybes)
import           Data.Store              (Store)
import           Periodic.Types.Internal
import           Periodic.Utils          (breakBS, readBS)
import           TH.Derive               (Deriving, derive)

type FuncName  = ByteString
type JobName   = ByteString
type JobHandle = ByteString
type Workload  = ByteString

data Job = Job { jSchedAt  :: Int64
               , jFuncName :: FuncName
               , jName     :: JobName
               , jWorkload :: Workload
               , jCount    :: Int
               }
  deriving (Show)

$($(derive [d| instance Deriving (Store Job) |]))

instance Byteable Job where
  toBytes = encodeJob

instance Parser Job where
  runParser bs = case decodeJob bs of
                   Nothing  -> Left "InvalidJob"
                   Just job -> Right job

newJob :: FuncName -> JobName -> Job
newJob jFuncName jName = Job { jWorkload = B.empty
                             , jSchedAt = 0
                             , jCount = 0
                             , ..
                             }

encodeJob :: Job -> ByteString
encodeJob Job {..} = concatBS [ Just jFuncName
                              , Just jName
                              , schedAt
                              , counter
                              , workload
                              ]

 where join :: [ByteString] -> [ByteString]
       join []     = []
       join [x]    = [x]
       join (x:xs) = x:nullChar:join xs

       concatBS = B.concat . join . catMaybes

       sched = B.pack $ show jSchedAt
       count = B.pack $ show jCount

       schedAt | not (B.null jWorkload) = Just sched
               | jCount > 0             = Just sched
               | jSchedAt > 0           = Just sched
               | otherwise              = Nothing
       counter | not (B.null jWorkload) = Just count
               | jCount > 0             = Just count
               | otherwise              = Nothing

       workload | B.null jWorkload      = Nothing
                | otherwise             = Just jWorkload

decodeJob :: ByteString -> Maybe Job
decodeJob = go . breakBS 5
  where go :: [ByteString] -> Maybe Job
        go []            = Nothing
        go [_]           = Nothing
        go [x, y]        = Just $ newJob x y
        go [x, y, z]     = Just $ (newJob x y) { jSchedAt = readBS z }
        go [x, y, z, a]  = Just $ (newJob x y) { jSchedAt = readBS z
                                               , jCount   = readBS a
                                               }
        go (x:y:z:a:b:_) = Just $ (newJob x y) { jWorkload = b
                                               , jSchedAt  = readBS z
                                               , jCount    = readBS a
                                               }

sep :: ByteString
sep = "::"

sepLength :: Int
sepLength = B.length sep

jHandle :: Job -> JobHandle
jHandle Job{..} = B.concat [ jFuncName, sep, jName ]

unHandle :: JobHandle -> (FuncName, JobName)
unHandle = go . B.breakSubstring sep
  where go (fn, jn) = (fn, B.drop sepLength jn)
