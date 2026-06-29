module Periodic.Trans.ClientPool
  ( module Periodic.Trans.Client
  , ClientPoolEnv
  , runClientPoolT
  , openPool
  , openPoolWithAuth
  ) where

import           Data.Pool             (Pool, defaultPoolConfig, newPool,
                                        withResource)
import           Metro.Class           (Transport, TransportConfig)
import           Periodic.Types        (ClientIdentity)
import           Periodic.Trans.Client hiding (close)
import qualified Periodic.Trans.Client as C (closeClientEnv)

type ClientPoolEnv tp = Pool (ClientEnv tp)

runClientPoolT :: Transport tp => ClientPoolEnv tp -> ClientT tp IO a -> IO a
runClientPoolT pool m = withResource pool $ flip runClientT m

openPool :: Transport tp => TransportConfig tp -> Int -> IO (ClientPoolEnv tp)
openPool config = newPool . defaultPoolConfig (open config) C.closeClientEnv 5000

openPoolWithAuth :: Transport tp => TransportConfig tp -> ClientIdentity -> Int -> IO (ClientPoolEnv tp)
openPoolWithAuth config ident = newPool . defaultPoolConfig (openWithAuth config ident) C.closeClientEnv 5000
