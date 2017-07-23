module Periodic.WorkerM
  (
    WorkerM
  , runWorker
  , ping
  , addFunc
  , removeFunc
  , work
  , close
  , module Periodic.JobM
  ) where

import           Periodic.JobM
import           Periodic.Monad     (GenPeriodic, runPeriodic, unsafeLiftIO')
import           Periodic.Transport (Transport)
import           Periodic.Types.Job (FuncName)
import qualified Periodic.Worker    as W

type WorkerM = GenPeriodic W.Worker

runWorker :: Transport -> WorkerM a -> IO a
runWorker transport m = do
  w <- W.newWorker transport
  runPeriodic w m

ping :: WorkerM Bool
ping = unsafeLiftIO' W.ping

addFunc :: FuncName -> JobM -> WorkerM ()
addFunc f m = do
  unsafeLiftIO' $ \w -> do
    W.addFunc w f $ \j -> do
      runJob j m

removeFunc :: FuncName -> WorkerM ()
removeFunc f = unsafeLiftIO' $ flip W.removeFunc f

close :: WorkerM ()
close = unsafeLiftIO' W.close

work :: Int -> WorkerM ()
work size = unsafeLiftIO' $ flip W.work size
