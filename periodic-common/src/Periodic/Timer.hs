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


import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (forever, when)
import qualified Periodic.Lock      as L
import           Periodic.TM

data Timer = Timer { timer  :: ThreadManager
                   , locker :: L.Lock
                   }

newTimer :: IO Timer
newTimer = do
  timer  <- newThreadManager
  locker <- L.new
  return Timer {..}

clearTimer :: Timer -> IO ()
clearTimer Timer {..} = do
  killThread timer

-- | startTimer for a given number of microseconds
--
startTimer :: Timer -> Int -> IO () -> IO ()
startTimer Timer{..} delay io = L.with locker $ do
  killThread timer

  t <- forkIO $ do
    when (delay > 0) $ threadDelay delay
    io

  setThreadId timer t

-- | startTimer' for a given number of seconds
--
startTimer' :: Timer -> Int -> IO () -> IO ()
startTimer' t delay = startTimer t (delay * 1000000)

-- | repeatTimer for a given number of microseconds
--
repeatTimer :: Timer -> Int -> IO () -> IO ()
repeatTimer Timer{..} delay io = L.with locker $ do
  killThread timer

  t <- forkIO $ forever $ do
    when (delay > 0) $ threadDelay delay
    io

  setThreadId timer t


-- | repeatTimer' for a given number of seconds
--
repeatTimer' :: Timer -> Int -> IO () -> IO ()
repeatTimer' t delay = repeatTimer t (delay * 1000000)
