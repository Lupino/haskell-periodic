module Periodic.Utils
  ( tryIO
  , getEpochTime
  ) where

import           Control.Exception (IOException, catch)
import           Foreign.C.Types   (CTime (..))

import           Data.Int          (Int64)
import           Data.UnixTime     (getUnixTime, toEpochTime)

tryIO :: IO a -> IO (Either IOException a)
tryIO m = catch (fmap Right m) (return . Left)

getEpochTime :: IO Int64
getEpochTime = un . toEpochTime <$> getUnixTime
  where un :: CTime -> Int64
        un (CTime t) = t
