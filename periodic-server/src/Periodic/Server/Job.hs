{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.Job
  (
    parseJob
  , unparseJob
  , jHandle
  , unHandle
  , JobHandle
  , Job (..)
  ) where

import           Data.Aeson               (FromJSON (..), ToJSON (..), decode,
                                           encode, object, withObject, (.!=),
                                           (.:), (.:?), (.=))

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B (breakSubstring, concat, drop)
import           Data.ByteString.Lazy     (fromStrict, toStrict)
import           Data.Int                 (Int64)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Periodic.Server.FuncList (FuncName)
import           Periodic.Types           (nullChar)

data Job = Job { jSchedAt  :: Int64
               , jFuncName :: FuncName
               , jName     :: ByteString
               , jWorkload :: ByteString
               , jCount    :: Int
               }
  deriving (Show)

instance FromJSON Job where
  parseJSON = withObject "Job" $ \o -> do
    jName     <- encodeUtf8 <$> o .:  "name"
    jFuncName <- encodeUtf8 <$> o .:  "func"
    jWorkload <- encodeUtf8 <$> o .:? "workload" .!= ""
    jSchedAt  <- o .:? "sched_at" .!= 0
    return Job { jCount = 0
               , ..
               }

instance ToJSON Job where
  toJSON Job{..} = object [ "name"     .= decodeUtf8 jName
                          , "func"     .= decodeUtf8 jFuncName
                          , "workload" .= decodeUtf8 jWorkload
                          , "sched_at" .= jSchedAt
                          , "counter"  .= jCount
                          ]

type JobHandle = ByteString

parseJob :: ByteString -> Maybe Job
parseJob = decode . fromStrict

unparseJob :: Job -> ByteString
unparseJob = toStrict . encode

jHandle :: Job -> JobHandle
jHandle (Job {..}) = B.concat [ jFuncName, nullChar, jName ]

unHandle :: JobHandle -> (FuncName, ByteString)
unHandle = go . B.breakSubstring nullChar
  where go (fn, jn) = (fn, B.drop 2 jn)
