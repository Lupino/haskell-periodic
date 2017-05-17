{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Job
  (
    Job (..)
  , newJob
  , done
  , fail
  , schedLater
  , schedLater'
  ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (breakSubstring, concat, drop, pack)
import           Data.ByteString.Lazy  (fromStrict)
import           Periodic.Agent        (send)
import           Periodic.BaseClient   (BaseClient, withAgent)
import           Periodic.Types        (Command (..), nullChar)

import           Data.Aeson            (FromJSON (..), decode, withObject, (.:))
import           Data.Int              (Int64)
import           Prelude               hiding (fail)

data RawJob = RawJob { _name     :: String
                     -- unique job name
                     , _func     :: String
                     -- refer worker func
                     , _workload :: String
                     -- workload
                     , _counter  :: Int64
                     }
  deriving (Show)

instance FromJSON RawJob where
  parseJSON = withObject "RawJob" $ \o -> do
    _name     <- o .: "name"
    _func     <- o .: "func"
    _workload <- o .: "workload"
    _counter  <- o .: "counter"
    return RawJob{ .. }

data Job = Job { name     :: String
               , func     :: ByteString
               , workload :: String
               , counter  :: Int64
               , bc       :: BaseClient
               , handle   :: ByteString
               }

newJob :: BaseClient -> ByteString -> Job
newJob c = parse . B.breakSubstring nullChar
  where parse :: (ByteString, ByteString) -> Job
        parse (h, dat) = parse1 h (B.drop 2 dat)

        parse1 :: ByteString -> ByteString -> Job
        parse1 h dat = Job { name = _name r
                           , func = B.pack $ _func r
                           , workload = _workload r
                           , counter = _counter r
                           , handle = h
                           , bc = c
                           }
          where (Just r) = decode $ fromStrict dat

done :: Job -> IO ()
done (Job { handle = h, bc = c }) = withAgent c $ \agent ->
  send agent WorkDone h

fail :: Job -> IO ()
fail (Job { handle = h, bc = c }) = withAgent c $ \agent ->
  send agent WorkFail h

schedLater :: Job -> Int64 -> IO ()
schedLater (Job { handle = h, bc =c }) later = withAgent c $ \agent ->
  send agent SchedLater $ B.concat [ h, nullChar, B.pack $ show later ]

schedLater' :: Job -> Int64 -> Int64 -> IO ()
schedLater' (Job { handle = h, bc = c }) later step = withAgent c $ \agent ->
  send agent SchedLater $ B.concat [ h
                                   , nullChar
                                   , B.pack $ show later
                                   , nullChar
                                   , B.pack $ show step
                                   ]
