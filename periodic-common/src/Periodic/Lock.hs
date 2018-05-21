{-# LANGUAGE RecordWildCards #-}
module Periodic.Lock
  (
    Lock
  , new
  , with
  , withM
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar,
                                          withMVar)
import           Control.Monad.Catch     (MonadMask, bracket_)
import           Control.Monad.IO.Class  (MonadIO (..))

newtype Lock = Lock { un :: MVar () }

new :: IO Lock
new = Lock <$> newMVar ()

with :: Lock -> IO a -> IO a
with Lock{..} = withMVar un . const

withM :: (MonadIO m, MonadMask m) => Lock -> m a -> m a
withM Lock{..} = bracket_ (liftIO $ takeMVar un) (liftIO $ putMVar un ())
