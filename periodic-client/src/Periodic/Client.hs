module Periodic.Client
  ( ClientM
  , ClientEnv
  , open
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
import           Periodic.Trans.Client hiding (ClientEnv, open)
import qualified Periodic.Trans.Client as C (ClientEnv, open)

type ClientM = ClientT Socket IO

type ClientEnv = C.ClientEnv Socket

open :: String -> IO ClientEnv
open = C.open . socket

runClientM :: ClientEnv -> ClientM a -> IO a
runClientM = runClientT
