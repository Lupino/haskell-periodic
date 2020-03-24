module Periodic.Worker
  ( WorkerM
  , startWorkerM
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

startWorkerM :: String -> WorkerM () -> IO ()
startWorkerM h = startWorkerT (socket h)
