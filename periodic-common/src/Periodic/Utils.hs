module Periodic.Utils
  ( getEpochTime
  ) where

import           Data.UnixTime   (getUnixTime, toEpochTime)
import           Foreign.C.Types (CTime (..))
import           UnliftIO        (MonadIO (..))

getEpochTime :: (MonadIO m, Num a) => m a
getEpochTime = liftIO $ un . toEpochTime <$> getUnixTime
  where un :: Num a => CTime -> a
        un (CTime t) = fromIntegral t
