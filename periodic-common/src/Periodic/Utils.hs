module Periodic.Utils
  ( tryIO
  , getEpochTime
  ) where

import           Control.Exception (IOException, catch)
import           Data.UnixTime     (getUnixTime, toEpochTime)
import           Foreign.C.Types   (CTime (..))

tryIO :: IO a -> IO (Either IOException a)
tryIO m = catch (fmap Right m) (return . Left)

getEpochTime :: Num a => IO a
getEpochTime = un . toEpochTime <$> getUnixTime
  where un :: Num a => CTime -> a
        un (CTime t) = fromIntegral t
