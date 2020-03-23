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

import           Metro.TP.Socket       (Socket, socket)
import           Periodic.Trans.Worker

type WorkerM = WorkerT Socket IO

runWorkerM :: String -> WorkerM () -> IO ()
runWorkerM h = runWorkerT (socket h)
