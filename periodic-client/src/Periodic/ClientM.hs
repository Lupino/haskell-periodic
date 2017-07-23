module Periodic.ClientM
  (
    ClientM
  , runClient
  , ping
  , submitJob_
  , submitJob
  , removeJob_
  , removeJob
  , dropFunc
  , dump
  , load
  , status
  , shutdown
  , close
  ) where

import           Data.ByteString    (ByteString)
import           Data.Int           (Int64)
import           GHC.IO.Handle      (Handle)
import qualified Periodic.Client    as C
import           Periodic.Monad     (GenPeriodic, runPeriodic, unsafeLiftIO')
import           Periodic.Transport (Transport)
import           Periodic.Types.Job (FuncName, Job, JobName)

type ClientM = GenPeriodic C.Client

runClient :: Transport -> ClientM a -> IO a
runClient transport m = do
  c <- C.newClient transport
  runPeriodic c m


ping :: ClientM Bool
ping = unsafeLiftIO' C.ping

submitJob_ :: Job -> ClientM Bool
submitJob_ j = unsafeLiftIO' (flip C.submitJob_ j)

submitJob :: FuncName -> JobName -> Int64 -> ClientM Bool
submitJob f j l = unsafeLiftIO' $ \c -> C.submitJob c f j l

dropFunc :: FuncName -> ClientM Bool
dropFunc func = unsafeLiftIO' $ flip C.dropFunc func

removeJob_ :: Job -> ClientM Bool
removeJob_ j = unsafeLiftIO' $ flip C.removeJob_ j

removeJob :: FuncName -> JobName -> ClientM Bool
removeJob f n = unsafeLiftIO' $ \c -> C.removeJob c f n

dump :: Handle -> ClientM ()
dump h = unsafeLiftIO' $ flip C.dump h

load :: Handle -> ClientM ()
load h = unsafeLiftIO' $ flip C.load h

status :: ClientM [[ByteString]]
status = unsafeLiftIO' C.status

shutdown :: ClientM ()
shutdown = unsafeLiftIO' C.shutdown

close :: ClientM ()
close = unsafeLiftIO' C.close
