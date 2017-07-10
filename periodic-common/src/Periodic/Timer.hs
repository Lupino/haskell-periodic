{-# LANGUAGE RecordWildCards #-}

module Periodic.Timer
  (
    Timer
  , newTimer
  , initTimer
  , startTimer
  , startTimer'
  , repeatTimer
  , repeatTimer'
  , clearTimer
  ) where


import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever, when)
import           Data.Maybe              (isNothing)
import qualified Periodic.Lock           as L
import           Periodic.TM

data Timer = Timer { timer  :: ThreadManager
                   , waiter :: MVar ()
                   , locker :: L.Lock
                   , runner :: ThreadManager
                   }

newTimer :: IO Timer
newTimer = do
  timer  <- newThreadManager
  waiter <- newEmptyMVar
  locker <- L.new
  runner <- newThreadManager

  return Timer {..}

initTimer :: Timer -> IO () -> IO ()
initTimer (Timer {..}) io = L.with locker $ do
  t <- getThreadId runner
  when (isNothing t) $ do
    t' <- forkIO $ forever $ do
      takeMVar waiter
      io

    setThreadId runner t'

clearTimer :: Timer -> IO ()
clearTimer Timer {..} = do
  killThread timer
  killThread runner

-- | startTimer for a given number of microseconds
--
startTimer :: Timer -> Int -> IO ()
startTimer (Timer {..}) delay = L.with locker $ do
  killThread timer

  t <- forkIO $ do
    when (delay > 0) $ threadDelay $ delay
    putMVar waiter ()

  setThreadId timer t

-- | startTimer' for a given number of seconds
--
startTimer' :: Timer -> Int -> IO ()
startTimer' t delay = startTimer t (delay * 1000000)

-- | repeatTimer for a given number of microseconds
--
repeatTimer :: Timer -> Int -> IO ()
repeatTimer (Timer {..}) delay = L.with locker $ do
  killThread timer

  t <- forkIO $ forever $ do
    when (delay > 0) $ threadDelay $ delay
    putMVar waiter ()

  setThreadId timer t


-- | repeatTimer' for a given number of seconds
--
repeatTimer' :: Timer -> Int -> IO ()
repeatTimer' t delay = startTimer t (delay * 1000000)
