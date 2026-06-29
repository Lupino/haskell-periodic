module Periodic.Worker
  ( WorkerM
  , startWorkerM
  , startWorkerMWithAuth
  , ping
  , addFunc
  , broadcast
  , removeFunc
  , work
  , close
  ) where

import           Metro.TP.Socket       (Socket, socket)
import           Periodic.Types        (ClientIdentity)
import           Periodic.Trans.Worker

type WorkerM = WorkerT Socket IO

startWorkerM :: String -> WorkerM () -> IO ()
startWorkerM h = startWorkerT (socket h)

startWorkerMWithAuth :: String -> ClientIdentity -> WorkerM () -> IO ()
startWorkerMWithAuth h ident = startWorkerTWithSignalWithAuth (Just ident) Nothing (pure ()) (socket h)
