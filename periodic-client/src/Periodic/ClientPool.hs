module Periodic.ClientPool
  ( module Periodic.Client
  , ClientPoolEnv
  , runClientPoolM
  , openPool
  ) where

import           Periodic.Client           hiding (close)
import           Periodic.Trans.ClientPool (runClientPoolT)
import qualified Periodic.Trans.ClientPool as P (ClientPoolEnv, openPool)
import           Periodic.Transport.Socket (Socket, socketUri)

type ClientPoolEnv = P.ClientPoolEnv Socket

runClientPoolM :: ClientPoolEnv -> ClientM a -> IO a
runClientPoolM = runClientPoolT

openPool :: String -> Int -> IO ClientPoolEnv
openPool h = P.openPool (socketUri h)
