module Periodic.Client
  ( ClientM
  , ClientEnv
  , open
  , runClientM

  -- re-exports
  , ping
  , submitJob_
  , submitJob
  , runJob_
  , runJob
  , removeJob
  , dropFunc
  , status
  , configGet
  , configSet
  , load
  , dump
  , shutdown
  ) where

import           Periodic.Trans.Client     hiding (ClientEnv, open)
import qualified Periodic.Trans.Client     as C (ClientEnv, open)
import           Periodic.Transport.Socket (Socket, socketUri)

type ClientM = ClientT Socket IO

type ClientEnv = C.ClientEnv Socket

open :: String -> IO ClientEnv
open = C.open . socketUri

runClientM :: ClientEnv -> ClientM a -> IO a
runClientM = runClientT
