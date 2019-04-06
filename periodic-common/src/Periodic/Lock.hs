{-# LANGUAGE RecordWildCards #-}
module Periodic.Lock
  (
    Lock
  , new
  , with
  ) where


import           UnliftIO (MVar, MonadIO, MonadUnliftIO, newMVar, withMVar)

newtype Lock = Lock { un :: MVar () }

new :: MonadIO m => m Lock
new = Lock <$> newMVar ()

with :: MonadUnliftIO m => Lock -> m a -> m a
with Lock{..} = withMVar un . const
