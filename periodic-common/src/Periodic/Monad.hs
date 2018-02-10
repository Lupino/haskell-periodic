{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}

module Periodic.Monad
  (
    Result (..)
  , GenPeriodic
  , runPeriodic
  , Env
  , initEnv
  , initEnv_
  , cloneEnv
  , withEnv
  , env
  , userEnv
  , unsafeLiftIO
  , withAgent

  , SpecEnv
  , specEnv
  , runPeriodicWithSpecEnv

  , stopPeriodic

  , catch
  , wapperIO
  ) where

import           Control.Concurrent     (forkIO)
import           Control.Exception      (Exception (..), SomeException, bracket,
                                         throw, try)
import           Control.Monad          (forever, void)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B (cons, empty, isInfixOf)
import           Data.Typeable          (Typeable)
import           Periodic.Agent         (Agent, feed, msgid)
import qualified Periodic.Agent         as Agent (newAgent, newEmptyAgent)
import           Periodic.Connection    (Connection, close, newClientConn,
                                         receive, send)
import           Periodic.IOHashMap     (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap     as HM (delete, elems, insert, lookup)
import           Periodic.TM
import           Periodic.Transport     (Transport)
import           Periodic.Types
import           Periodic.Utils         (breakBS2)
import           System.Entropy         (getEntropy)
import           System.Log.Logger      (errorM)

newtype MonadFail = MonadFail String
  deriving (Typeable, Show)

instance Exception MonadFail

data Env u = Env { uEnv :: u, conn :: Connection, runner :: ThreadManager, agentHandler :: Agent -> IO () }

data SpecEnv u = SpecEnv { sEnv :: Env u, sRef :: IOHashMap Agent }

newtype GenPeriodic u a = GenPeriodic { unPeriodic :: Env u -> IOHashMap Agent -> IO (Result a) }

data Result a = Done a
              | Throw SomeException

instance Monad (GenPeriodic u) where
  return a = GenPeriodic $ \_ _ -> return (Done a)
  GenPeriodic m >>= k = GenPeriodic $ \env ref -> do
    e <- m env ref
    case e of
      Done a  -> unPeriodic (k a) env ref
      Throw e -> return (Throw e)

  fail msg = GenPeriodic $ \_ _ -> return $ Throw $ toException $ MonadFail msg

  (>>) = (*>)

instance Functor (GenPeriodic u) where
  fmap f (GenPeriodic m) = GenPeriodic $ \env ref -> do
    e <- m env ref
    case e of
      Done a  -> return (Done (f a))
      Throw e -> return (Throw e)

instance Applicative (GenPeriodic u) where
  pure = return
  GenPeriodic f <*> GenPeriodic a = GenPeriodic $ \env ref -> do
    r <- f env ref
    case r of
      Throw e -> return (Throw e)
      Done f' -> do
        ra <- a env ref
        case ra of
          Done a' -> return (Done (f' a'))
          Throw e -> return (Throw e)

runPeriodic :: Env u -> GenPeriodic u a -> IO a
runPeriodic env (GenPeriodic m) = do
  ref <- newIOHashMap
  t <- forkIO $ forever $ unPeriodic mainLoop env ref
  setThreadId (runner env) t
  e <- m env ref
  case e of
    Done a  -> return a
    Throw e -> throw e

runPeriodicWithSpecEnv :: SpecEnv u -> GenPeriodic u a -> IO a
runPeriodicWithSpecEnv (SpecEnv env ref) (GenPeriodic m) = do
  e <- m env ref
  case e of
    Done a  -> return a
    Throw e -> throw e

initEnv :: Connection -> u -> IO (Env u)
initEnv = initEnv_ (const $ pure ())

initEnv_ :: (Agent -> IO ()) -> Connection -> u -> IO (Env u)
initEnv_ agentHandler conn uEnv = do
  runner <- newThreadManager
  return Env {..}

cloneEnv :: u1 -> GenPeriodic u (Env u1)
cloneEnv u = GenPeriodic $ \env _ -> return . Done $ env { uEnv = u }

withEnv :: Env u1 -> GenPeriodic u1 a -> GenPeriodic u a
withEnv newEnv (GenPeriodic m) = GenPeriodic $ \_ ref -> do
  r <- m newEnv ref
  case r of
    Done a  -> return (Done a)
    Throw e -> return (Throw e)

env :: GenPeriodic u (Env u)
env = GenPeriodic $ \env _ -> return (Done env)

userEnv :: GenPeriodic u u
userEnv = GenPeriodic $ \env _ -> return (Done $ uEnv env)

specEnv :: GenPeriodic u (SpecEnv u)
specEnv = GenPeriodic $ \env ref -> return (Done $ SpecEnv env ref)

-- Unsafe operations

-- | Under ordinary circumstances this is unnecessary; users of the Periodic
-- monad should generally /not/ perform arbitrary IO.
unsafeLiftIO :: IO a -> GenPeriodic u a
unsafeLiftIO m = GenPeriodic $ \_ _ -> Done <$> m

instance MonadIO (GenPeriodic u) where
  liftIO = unsafeLiftIO

withAgent :: (Agent -> IO a) -> GenPeriodic u a
withAgent f = GenPeriodic $ \env ref ->
  Done <$> bracket (newEmptyAgent env ref) (removeAgent ref) f

newEmptyAgent :: Env u -> IOHashMap Agent -> IO Agent
newEmptyAgent env ref = do
  aid <- genMsgid
  agent <- Agent.newEmptyAgent aid (conn env)
  HM.insert ref aid agent
  return agent

  where genMsgid :: IO ByteString
        genMsgid = do
          aid <- getEntropy 4
          if B.isInfixOf nullChar aid then genMsgid
                                      else return aid

removeAgent :: IOHashMap Agent -> Agent -> IO ()
removeAgent ref a = HM.delete ref (msgid a)

doFeedError :: GenPeriodic u ()
doFeedError = GenPeriodic $ \_ ref ->
  Done <$> (HM.elems ref >>= mapM_ (`feed` B.empty))

mainLoop :: GenPeriodic u ()
mainLoop = GenPeriodic $ \env ref -> do
  e <- try $ receive (conn env)
  case e of
    Left TransportClosed -> unPeriodic doFeedError env ref
    Left MagicNotMatch   -> unPeriodic doFeedError env ref
    Left _               -> return $ Done ()
    Right pl             -> unPeriodic (doFeed pl) env ref

doFeed :: ByteString -> GenPeriodic u ()
doFeed pl = GenPeriodic $ \env ref -> do
  let (pid, pl') = breakBS2 pl
  v <- HM.lookup ref pid
  case v of
    Just agent -> Done <$> feed agent pl'
    Nothing    -> do
      agent <- Agent.newAgent pl (conn env)
      Done <$> agentHandler env agent

stopPeriodic :: GenPeriodic u ()
stopPeriodic = do
  doFeedError
  GenPeriodic $ \env _ -> do
    killThread (runner env)
    close (conn env)
    return $ Done ()

-- | Catch an exception in the Haxl monad
catch :: Exception e => GenPeriodic u a -> (e -> GenPeriodic u a) -> GenPeriodic u a
catch (GenPeriodic m) h = GenPeriodic $ \env ref -> do
   r <- m env ref
   case r of
     Done a    -> return (Done a)
     Throw e | Just e' <- fromException e -> unPeriodic (h e') env ref
             | otherwise -> return (Throw e)

wapperIO :: (IO a -> IO b) -> GenPeriodic u a -> GenPeriodic u b
wapperIO f (GenPeriodic m) = GenPeriodic $ \env ref -> do
  e <- try $ f $ do r <- m env ref
                    case r of
                      Done a  -> return a
                      Throw e -> throw e
  case e of
    Left e  -> return $ Throw e
    Right a -> return $ Done a
