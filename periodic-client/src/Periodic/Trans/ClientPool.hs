module Periodic.Trans.ClientPool
  ( module Periodic.Trans.Client
  , ClientPoolEnv
  , runClientPoolT
  , openPool
  ) where

import           Data.Pool             (Pool, createPool, withResource)
import           Metro.Class           (Transport, TransportConfig)
import           Periodic.Trans.Client hiding (close)
import qualified Periodic.Trans.Client as C (close)

type ClientPoolEnv tp = Pool (ClientEnv tp)

runClientPoolT :: ClientPoolEnv tp -> ClientT tp IO a -> IO a
runClientPoolT pool m = withResource pool $ flip runClientT m

openPool :: Transport tp => TransportConfig tp -> Int -> IO (ClientPoolEnv tp)
openPool config = createPool (open config) (`runClientT` C.close) 1 5000
