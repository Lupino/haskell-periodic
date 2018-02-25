module Periodic.Utils
  (
    maxLength
  , tryIO
  , getEpochTime
  ) where

import           Control.Exception (IOException, catch)

import           Data.Int          (Int64)
import           Data.UnixTime     (getUnixTime, toEpochTime)

maxLength :: Int
maxLength = 0x7fffffff

tryIO :: IO a -> IO (Either IOException a)
tryIO m = catch (fmap Right m) (return . Left)

getEpochTime :: IO Int64
getEpochTime = read . show . toEpochTime <$> getUnixTime
