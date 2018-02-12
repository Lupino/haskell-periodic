{-# LANGUAGE RecordWildCards #-}
module Periodic.Lock
  (
    Lock
  , new
  , with
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, withMVar)

newtype Lock = Lock { un :: MVar () }

new :: IO Lock
new = Lock <$> newMVar ()

with :: Lock -> IO a -> IO a
with Lock{..} = withMVar un . const
