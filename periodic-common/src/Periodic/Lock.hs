{-# LANGUAGE RecordWildCards #-}
module Periodic.Lock
  (
    Lock
  , new
  , with
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Exception       (bracket_)

newtype Lock = Lock { un :: MVar () }

new :: IO Lock
new = Lock <$> newMVar ()

with :: Lock -> IO a -> IO a
with Lock{..} = bracket_ (takeMVar un) (putMVar un ())
