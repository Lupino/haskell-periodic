module Periodic.ClientPool
  (
    module Periodic.Client
  , ClientEnv
  , runClientT
  , open
  ) where

import           Data.Pool          (Pool, createPool, withResource)
import           Periodic.Client    hiding (ClientEnv, close, open, runClientT)
import qualified Periodic.Client    as Client (ClientEnv, close, open,
                                               runClientT)
import           Periodic.Transport (Transport, TransportConfig)

type ClientEnv tp = Pool (Client.ClientEnv tp)

runClientT :: ClientEnv tp -> ClientT tp IO a -> IO a
runClientT pool m = withResource pool $ flip Client.runClientT m

open
  :: Transport tp => TransportConfig tp -> Int -> IO (ClientEnv tp)
open config =
  createPool (Client.open config) (`Client.runClientT` Client.close) 1 5000
