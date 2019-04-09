module Periodic.Worker
  ( WorkerM
  , runWorkerM
  , ping
  , addFunc
  , broadcast
  , removeFunc
  , work
  , close
  ) where

import           Periodic.Trans.Worker
import           Periodic.Transport.Socket (Socket, socketUri)

type WorkerM = WorkerT Socket IO

runWorkerM :: String -> WorkerM a -> IO a
runWorkerM h = runWorkerT (socketUri h)
