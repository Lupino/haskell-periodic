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
import           Periodic.Transport (Transport)

type ClientEnv = Pool Client.ClientEnv

runClientT :: ClientEnv -> ClientT IO a -> IO a
runClientT pool m = withResource pool $ flip Client.runClientT m

open
  :: (Transport -> IO Transport) -> String -> Int -> IO ClientEnv
open f h =
  createPool (Client.open f h) (`Client.runClientT` Client.close) 1 5000
