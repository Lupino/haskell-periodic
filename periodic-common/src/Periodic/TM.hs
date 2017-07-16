module Periodic.TM
  (
    ThreadManager
  , newThreadManager
  , setThreadId
  , getThreadId
  , killThread
  ) where

import           Control.Concurrent (ThreadId)
import qualified Control.Concurrent as C (killThread)
import           Control.Monad      (when)
import           Data.IORef         (IORef, atomicModifyIORef', newIORef)
import           Data.Maybe         (fromJust, isJust)

type ThreadManager = IORef (Maybe ThreadId)

newThreadManager :: IO ThreadManager
newThreadManager = newIORef Nothing

setThreadId :: ThreadManager -> ThreadId -> IO ()
setThreadId ref t = atomicModifyIORef' ref $ \_ -> (Just t, ())

getThreadId :: ThreadManager -> IO (Maybe ThreadId)
getThreadId ref = atomicModifyIORef' ref $ \v -> (v, v)

killThread :: ThreadManager -> IO ()
killThread ref = do
  t <- getThreadId ref
  when (isJust t) $ C.killThread (fromJust t)