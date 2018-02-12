{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Monad
  (
    Result (..)
  , GenPeriodic
  , runPeriodic
  , startMainLoop
  , Env (conn)
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

  , isAlive
  , stopPeriodic

  , catch
  , wapperIO
  ) where

import           Control.Concurrent     (forkIO, killThread, myThreadId)
import           Control.Exception      (Exception (..), SomeException, bracket,
                                         throw, try)
import           Control.Monad          (forever, unless, void)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B (cons, empty, isInfixOf)
import           Data.IORef             (IORef, atomicModifyIORef', newIORef)
import           Data.Typeable          (Typeable)
import           Periodic.Agent         (Agent, feed, msgid)
import qualified Periodic.Agent         as Agent (newAgent, newEmptyAgent)
import           Periodic.Connection    (Connection, close, newClientConn,
                                         receive, send)
import           Periodic.IOHashMap     (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap     as HM (delete, elems, insert, lookup)
import           Periodic.Transport     (Transport)
import           Periodic.Types
import           Periodic.Utils         (breakBS2)
import           System.Entropy         (getEntropy)
import           System.Log.Logger      (errorM)

newtype MonadFail = MonadFail String
  deriving (Typeable, Show)

instance Exception MonadFail

data Env u = Env { uEnv :: u, conn :: Connection, agentHandler :: Agent -> IO () }

data SpecEnv u = SpecEnv { sEnv :: Env u, sState :: IORef Bool, sRef :: IOHashMap Agent }

newtype GenPeriodic u a = GenPeriodic { unPeriodic :: Env u -> IORef Bool -> IOHashMap Agent -> IO (Result a) }

data Result a = Done a
              | Throw SomeException

instance Monad (GenPeriodic u) where
  return a = GenPeriodic $ \_ _ _ -> return (Done a)
  GenPeriodic m >>= k = GenPeriodic $ \env state ref -> do
    e <- m env state ref
    case e of
      Done a  -> unPeriodic (k a) env state ref
      Throw e -> return (Throw e)

  fail msg = GenPeriodic $ \_ _ _ -> return $ Throw $ toException $ MonadFail msg

  (>>) = (*>)

instance Functor (GenPeriodic u) where
  fmap f (GenPeriodic m) = GenPeriodic $ \env state ref -> do
    e <- m env state ref
    case e of
      Done a  -> return (Done (f a))
      Throw e -> return (Throw e)

instance Applicative (GenPeriodic u) where
  pure = return
  GenPeriodic f <*> GenPeriodic a = GenPeriodic $ \env state ref -> do
    r <- f env state ref
    case r of
      Throw e -> return (Throw e)
      Done f' -> do
        ra <- a env state ref
        case ra of
          Done a' -> return (Done (f' a'))
          Throw e -> return (Throw e)

runPeriodic :: Env u -> GenPeriodic u a -> IO a
runPeriodic env (GenPeriodic m) = do
  ref <- newIOHashMap
  state <- newIORef True
  e <- m env state ref
  case e of
    Done a  -> return a
    Throw e -> throw e

runPeriodicWithSpecEnv :: SpecEnv u -> GenPeriodic u a -> IO a
runPeriodicWithSpecEnv (SpecEnv env state ref) (GenPeriodic m) = do
  e <- m env state ref
  case e of
    Done a  -> return a
    Throw e -> throw e

initEnv :: Connection -> u -> IO (Env u)
initEnv = initEnv_ handler
  where handler :: Agent -> IO ()
        handler agent =
          errorM "Periodic.Monad" $ "Agent [" ++ show pid ++ "] not found."
          where pid = msgid agent

initEnv_ :: (Agent -> IO ()) -> Connection -> u -> IO (Env u)
initEnv_ agentHandler conn uEnv =
  return Env {..}

cloneEnv :: u1 -> GenPeriodic u (Env u1)
cloneEnv u = GenPeriodic $ \env _ _ -> return . Done $ env { uEnv = u }

withEnv :: Env u1 -> GenPeriodic u1 a -> GenPeriodic u a
withEnv newEnv (GenPeriodic m) = GenPeriodic $ \_ state ref -> do
  r <- m newEnv state ref
  case r of
    Done a  -> return (Done a)
    Throw e -> return (Throw e)

env :: GenPeriodic u (Env u)
env = GenPeriodic $ \env _ _ -> return (Done env)

userEnv :: GenPeriodic u u
userEnv = GenPeriodic $ \env _ _ -> return (Done $ uEnv env)

specEnv :: GenPeriodic u (SpecEnv u)
specEnv = GenPeriodic $ \env state ref -> return (Done $ SpecEnv env state ref)

-- Unsafe operations

-- | Under ordinary circumstances this is unnecessary; users of the Periodic
-- monad should generally /not/ perform arbitrary IO.
unsafeLiftIO :: IO a -> GenPeriodic u a
unsafeLiftIO m = GenPeriodic $ \_ _ _ -> Done <$> m

instance MonadIO (GenPeriodic u) where
  liftIO = unsafeLiftIO

withAgent :: (Agent -> IO a) -> GenPeriodic u a
withAgent f = GenPeriodic $ \env state ref ->
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

doFeedError :: IOHashMap Agent -> IO ()
doFeedError ref = HM.elems ref >>= mapM_ (`feed` B.empty)

startMainLoop :: IO () -> GenPeriodic u ()
startMainLoop onClose = GenPeriodic $ \env state ref ->
  Done <$> forever (mainLoop onClose env state ref)

mainLoop :: IO () -> Env u -> IORef Bool -> IOHashMap Agent -> IO ()
mainLoop onClose env state ref = do
  alive <- atomicModifyIORef' state (\v -> (v, v))
  unless alive $ do
    onClose
    doFeedError ref
    close (conn env)
    killThread =<< myThreadId
  e <- try $ receive (conn env)
  case e of
    Left TransportClosed -> setClose state
    Left MagicNotMatch   -> setClose state
    Left _               -> return ()
    Right pl             -> doFeed env state ref pl

 where setClose :: IORef Bool -> IO ()
       setClose state = atomicModifyIORef' state (const (False, ()))

doFeed :: Env u -> IORef Bool -> IOHashMap Agent -> ByteString -> IO ()
doFeed env state ref bs = do
  let (pid, pl) = breakBS2 bs
  v <- HM.lookup ref pid
  case v of
    Just agent -> feed agent pl
    Nothing    -> do
      agent <- Agent.newAgent pl (conn env)
      void . forkIO $ do
        ret <- try $ agentHandler env agent
        case ret of
          Right _                 -> pure ()
          Left (e::SomeException) ->
            atomicModifyIORef' state (const (False, ()))


-- | Catch an exception in the Haxl monad
catch :: Exception e => GenPeriodic u a -> (e -> GenPeriodic u a) -> GenPeriodic u a
catch (GenPeriodic m) h = GenPeriodic $ \env state ref -> do
  r <- m env state ref
  case r of
    Done a    -> return (Done a)
    Throw e | Just e' <- fromException e -> unPeriodic (h e') env state ref
            | otherwise -> return (Throw e)

wapperIO :: (IO a -> IO b) -> GenPeriodic u a -> GenPeriodic u b
wapperIO f (GenPeriodic m) = GenPeriodic $ \env state ref -> do
  e <- try $ f $ do r <- m env state ref
                    case r of
                      Done a  -> return a
                      Throw e -> throw e
  case e of
    Left e  -> return $ Throw e
    Right a -> return $ Done a

isAlive :: GenPeriodic u Bool
isAlive = GenPeriodic $ \_ state _ -> Done <$> atomicModifyIORef' state (\v -> (v, v))

stopPeriodic :: GenPeriodic u ()
stopPeriodic = GenPeriodic $ \_ state _ ->
  Done <$> atomicModifyIORef' state (const (False, ()))
