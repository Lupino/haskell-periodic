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
  , jHandle
  , unHandle
  ) where

import           Data.Aeson           (FromJSON (..), ToJSON (..), decode,
                                       encode, object, withObject, (.!=), (.:),
                                       (.:?), (.=))

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B (breakSubstring, concat, drop,
                                            length)
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Int             (Int64)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)

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
newJob jFuncName jName = Job { jWorkload = ""
                             , jSchedAt = 0
                             , jCount = 0
                             , ..
                             }

parseJob :: ByteString -> Maybe Job
parseJob = decode . fromStrict

unparseJob :: Job -> ByteString
unparseJob = toStrict . encode

sep :: ByteString
sep = "func:name"

sepLength :: Int
sepLength = B.length sep

jHandle :: Job -> JobHandle
jHandle (Job {..}) = B.concat [ jFuncName, sep, jName ]

unHandle :: JobHandle -> (FuncName, JobName)
unHandle = go . B.breakSubstring sep
  where go (fn, jn) = (fn, B.drop sepLength jn)
