module Periodic.Server.Hook
  ( HookEvent (..)
  , HookName (..)
  , Hook (..)
  , emptyHook
  , socketHook
  , genHook
  , hookName
  , eventPushJob
  , eventPushGrab
  , eventFailJob
  , eventDoneJob
  , eventSchedLaterJob
  , eventAcquireLock
  , eventReleaseLock
  , eventRemoveJob
  , eventAssignJob
  , runHook
  , GetHookName
  ) where


import qualified Data.ByteString.Char8 as B (pack, unpack)
import           Metro.Class           (Transport (..))
import qualified Metro.Lock            as L (Lock, new, with)
import           Metro.TP.Socket       (socket)
import           Periodic.Types        (FuncName (..), Job, JobHandle,
                                        LockName (..), getFuncName, unHandle)
import           System.Log.Logger     (errorM)
import           UnliftIO              (MonadIO (..), TVar, atomically,
                                        newTVarIO, readTVarIO, timeout, tryAny,
                                        writeTVar)

data HookEvent = HookEvent String
  deriving (Show)

data HookName = HookName String
  deriving (Show)

newtype Hook = Hook
  { runHook_ :: HookEvent -> HookName -> IO ()
  }

eventPushJob       = HookEvent "pushJob"
eventPushGrab      = HookEvent "pushGrab"
eventFailJob       = HookEvent "failJob"
eventDoneJob       = HookEvent "doneJob"
eventSchedLaterJob = HookEvent "schedLaterJob"
eventAcquireLock   = HookEvent "acquireLock"
eventReleaseLock   = HookEvent "releaseLock"
eventRemoveJob     = HookEvent "removeJob"
eventAssignJob     = HookEvent "assignJob"

runHook :: (MonadIO m, GetHookName a) => Hook -> HookEvent -> a -> m ()
runHook hook evt n = liftIO $ runHook_ hook evt (hookName n)

emptyHook :: Hook
emptyHook = Hook $ \_ _ -> pure ()

class GetHookName a where
  hookName :: a -> HookName

instance GetHookName FuncName where
  hookName (FuncName fn) = HookName $ B.unpack fn

instance GetHookName Job where
  hookName = hookName . getFuncName

instance GetHookName LockName where
  hookName (LockName ln) = HookName $ B.unpack ln

instance GetHookName JobHandle where
  hookName = hookName . fst . unHandle


genSocketHook :: Transport tp => L.Lock -> TVar (Maybe tp) -> TransportConfig tp -> HookEvent -> HookName -> IO ()
genSocketHook lock tph config (HookEvent evt) (HookName name) = L.with lock $ do
  mtp <- readTVarIO tph
  case mtp of
    Nothing -> do
      tp <- newTP config
      r <- timeout s10 $ tryAny $ sendData tp bs
      case r of
        Nothing       -> errTimeout >> closeTP tp
        Just (Left e) -> logErr e >> closeTP tp
        _             -> atomically $ writeTVar tph $ Just tp
    Just tp -> do
      r <- timeout s10 $ tryAny $ sendData tp bs

      let doErr = do
            closeTP tp
            atomically $ writeTVar tph Nothing

      case r of
        Nothing       -> errTimeout >> doErr
        Just (Left e) -> logErr e >> doErr
        _             -> pure ()

  where bs = B.pack $ evt ++ ":" ++ name ++ "\r\n"
        s10 = 10000000 -- 10s
        errTimeout = errorM "Periodic.Server.Hook" $ "Send event timeout " ++ B.unpack bs
        logErr e = errorM "Periodic.Server.Hook" $ "Send event error " ++ show e


socketHook :: String -> IO Hook
socketHook hostPort = do
  h <- newTVarIO Nothing
  lock <- L.new
  return . Hook $ genSocketHook lock h $ socket hostPort


genHook :: String -> IO Hook
genHook ""       = return emptyHook
genHook hostPort = socketHook hostPort
