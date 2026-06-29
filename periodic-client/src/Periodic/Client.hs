module Periodic.Client
  ( ClientM
  , ClientEnv
  , open
  , openWithAuth
  , close
  , runClientM

  -- re-exports
  , ping
  , submitJob_
  , submitJob
  , runJob_
  , runJob
  , recvJobData_
  , recvJobData
  , removeJob
  , dropFunc
  , status
  , configGet
  , configSet
  , load
  , dump
  , shutdown
  ) where

import           Metro.TP.Socket       (Socket, socket)
import           Periodic.Types        (ClientIdentity)
import           Periodic.Trans.Client hiding (ClientEnv, open, openWithAuth)
import qualified Periodic.Trans.Client as C (ClientEnv, open, openWithAuth)

type ClientM = ClientT Socket IO

type ClientEnv = C.ClientEnv Socket

open :: String -> IO ClientEnv
open = C.open . socket

openWithAuth :: String -> ClientIdentity -> IO ClientEnv
openWithAuth h = C.openWithAuth (socket h)

runClientM :: ClientEnv -> ClientM a -> IO a
runClientM = runClientT
