{-# LANGUAGE RecordWildCards #-}

module Periodic.Timer
  (
    Timer
  , newTimer
  , startTimer
  , startTimer'
  , repeatTimer
  , repeatTimer'
  , clearTimer
  ) where


import           Control.Concurrent          (ThreadId, forkIO, killThread,
                                              threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (forever, when)
import           Control.Monad.STM           (atomically)
import qualified Periodic.Lock               as L

data Timer = Timer { timer  :: TVar ThreadId
                   , locker :: L.Lock
                   , state  :: TVar Bool
                   }

newTimer :: IO Timer
newTimer = do
  timer  <- newTVarIO $ error "not started"
  locker <- L.new
  state <- newTVarIO False
  return Timer {..}

clearTimer :: Timer -> IO ()
clearTimer tm@Timer{..} = L.with locker $ clearTimer_ tm

clearTimer_ :: Timer -> IO ()
clearTimer_ Timer{..} = do
  started <- atomically $ do
    st <- readTVar state
    writeTVar state False
    return st
  when (started) $ do
    killThread =<< readTVarIO timer

-- | startTimer for a given number of microseconds
--
startTimer :: Timer -> Int -> IO () -> IO ()
startTimer tm@Timer{..} delay io = L.with locker $ do
  clearTimer_ tm
  atomically $ writeTVar state True

  t <- forkIO $ do
    when (delay > 0) $ threadDelay delay
    io

  atomically $ writeTVar timer t

-- | startTimer' for a given number of seconds
--
startTimer' :: Timer -> Int -> IO () -> IO ()
startTimer' t delay = startTimer t (delay * 1000000)

-- | repeatTimer for a given number of microseconds
--
repeatTimer :: Timer -> Int -> IO () -> IO ()
repeatTimer tm@Timer{..} delay io = L.with locker $ do
  clearTimer_ tm
  atomically $ writeTVar state True

  t <- forkIO $ forever $ do
    when (delay > 0) $ threadDelay delay
    io

  atomically $ writeTVar timer t


-- | repeatTimer' for a given number of seconds
--
repeatTimer' :: Timer -> Int -> IO () -> IO ()
repeatTimer' t delay = repeatTimer t (delay * 1000000)
