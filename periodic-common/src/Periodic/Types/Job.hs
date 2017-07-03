{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Types.Job
  (
    FuncName
  , JobName
  , Workload
  , JobHandle
  , Job (..)
  , newJob
  , parseJob
  , unparseJob
  , decodeJob
  , encodeJob
  , jHandle
  , unHandle
  ) where

import           Data.Aeson             (FromJSON (..), ToJSON (..), decode,
                                         encode, object, withObject, (.!=),
                                         (.:), (.:?), (.=))

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B (breakSubstring, concat, drop,
                                              empty, length, null, pack)
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           Data.Int               (Int64)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

import           Data.Maybe             (catMaybes)
import           Periodic.Types.Payload (nullChar)
import           Periodic.Utils         (breakBS, readBS)

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

instance FromJSON Job where
  parseJSON = withObject "Job" $ \o -> do
    jName     <- encodeUtf8 <$> o .:  "name"
    jFuncName <- encodeUtf8 <$> o .:  "func"
    jWorkload <- encodeUtf8 <$> o .:? "workload" .!= ""
    jSchedAt  <- o .:? "sched_at" .!= 0
    jCount    <- o .:? "counter" .!= 0
    return Job {..}

instance ToJSON Job where
  toJSON Job{..} = object [ "name"     .= decodeUtf8 jName
                          , "func"     .= decodeUtf8 jFuncName
                          , "workload" .= decodeUtf8 jWorkload
                          , "sched_at" .= jSchedAt
                          , "counter"  .= jCount
                          ]

newJob :: FuncName -> JobName -> Job
newJob jFuncName jName = Job { jWorkload = B.empty
                             , jSchedAt = 0
                             , jCount = 0
                             , ..
                             }

parseJob :: ByteString -> Maybe Job
parseJob = decode . fromStrict

unparseJob :: Job -> ByteString
unparseJob = toStrict . encode

encodeJob :: Job -> ByteString
encodeJob Job {..} = concatBS [ Just jFuncName
                              , Just jName
                              , schedAt
                              , counter
                              , workload
                              ]

 where join :: [ByteString] -> [ByteString]
       join []     = []
       join (x:[]) = [x]
       join (x:xs) = (x:nullChar:join xs)

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
        go (_:[])        = Nothing
        go (x:y:[])      = Just $ newJob x y
        go (x:y:z:[])    = Just $ (newJob x y) { jSchedAt = readBS z }
        go (x:y:z:a:[])  = Just $ (newJob x y) { jSchedAt = readBS z
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
jHandle (Job {..}) = B.concat [ jFuncName, sep, jName ]

unHandle :: JobHandle -> (FuncName, JobName)
unHandle = go . B.breakSubstring sep
  where go (fn, jn) = (fn, B.drop sepLength jn)
