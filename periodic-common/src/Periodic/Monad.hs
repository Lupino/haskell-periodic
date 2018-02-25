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
  , cloneEnv_
  , withEnv
  , env
  , userEnv
  , unsafeLiftIO
  , withAgent
  , newEmptyAgent

  , SpecEnv
  , specEnv
  , runPeriodicWithSpecEnv

  , isAlive
  , stopPeriodic

  , catch
  , wapperIO
  ) where

import           Control.Concurrent          (forkIO, killThread, myThreadId)
import           Control.Concurrent.STM.TVar
import           Control.Exception           (Exception (..), SomeException,
                                              bracket, throw, try)
import           Control.Monad               (forever, unless, void)
import           Control.Monad.STM           (atomically)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B (drop, empty, take)
import           Data.Typeable               (Typeable)
import           Periodic.Agent              (Agent, AgentList, feed, msgid,
                                              msgidLength)
import qualified Periodic.Agent              as Agent (newAgent, newEmptyAgent)
import           Periodic.Connection         (Connection, close, receive)
import           Periodic.IOHashMap          (newIOHashMap)
import qualified Periodic.IOHashMap          as HM (delete, elems, insert,
                                                    lookup, member)
import           Periodic.Types
import           System.Entropy              (getEntropy)
import           System.Log.Logger           (errorM)

newtype MonadFail = MonadFail String
  deriving (Typeable, Show)

instance Exception MonadFail

data Env u = Env { uEnv :: u, conn :: Connection, agentHandler :: Agent -> GenPeriodic u () }

data SpecEnv u = SpecEnv { sEnv :: Env u, sState :: TVar Bool, sRef :: AgentList }

newtype GenPeriodic u a = GenPeriodic { unPeriodic :: Env u -> TVar Bool -> AgentList -> IO (Result a) }

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
  state <- newTVarIO True
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
initEnv = initEnv_ defaultHandler

defaultHandler :: Agent -> GenPeriodic u ()
defaultHandler agent =
  unsafeLiftIO $ errorM "Periodic.Monad" $ "Agent [" ++ show pid ++ "] not found."
  where pid = msgid agent

initEnv_ :: (Agent -> GenPeriodic u ()) -> Connection -> u -> IO (Env u)
initEnv_ agentHandler conn uEnv =
  return Env {..}

cloneEnv :: u1 -> GenPeriodic u (Env u1)
cloneEnv u = cloneEnv_ u defaultHandler

cloneEnv_ :: u1 -> (Agent -> GenPeriodic u1 ()) -> GenPeriodic u (Env u1)
cloneEnv_ u h = GenPeriodic $ \env _ _ -> return . Done $ env { uEnv = u, agentHandler = h }

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

withAgent :: (Agent -> IO a) -> GenPeriodic u a
withAgent f = GenPeriodic $ \env _ ref ->
  Done <$> bracket (newEmptyAgent_ env ref) (removeAgent ref) f

newEmptyAgent :: GenPeriodic u Agent
newEmptyAgent = GenPeriodic $ \env _ ref ->
  Done <$> newEmptyAgent_ env ref

newEmptyAgent_ :: Env u -> AgentList -> IO Agent
newEmptyAgent_ env ref = do
  aid <- getEntropy msgidLength
  agent <- Agent.newEmptyAgent aid (conn env)
  has <- HM.member ref aid
  if has then newEmptyAgent_ env ref
         else do
          HM.insert ref aid agent
          return agent

removeAgent :: AgentList -> Agent -> IO ()
removeAgent ref a = HM.delete ref (msgid a)

doFeedError :: AgentList -> IO ()
doFeedError ref = HM.elems ref >>= mapM_ (`feed` B.empty)

startMainLoop :: IO () -> GenPeriodic u ()
startMainLoop onClose = GenPeriodic $ \env state ref ->
  Done <$> forever (mainLoop onClose env state ref)

mainLoop :: IO () -> Env u -> TVar Bool -> AgentList -> IO ()
mainLoop onClose env state ref = do
  alive <- readTVarIO state
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

 where setClose :: TVar Bool -> IO ()
       setClose state = atomically $ writeTVar state False

doFeed :: Env u -> TVar Bool -> AgentList -> ByteString -> IO ()
doFeed env state ref bs = do
  v <- HM.lookup ref $ B.take msgidLength bs
  case v of
    Just agent -> feed agent $ B.drop msgidLength bs
    Nothing    -> do
      agent <- Agent.newAgent bs (conn env)
      void . forkIO $ do
        ret <- try $ unPeriodic (agentHandler env agent) env state ref
        case ret of
          Right _                 -> pure ()
          Left (_::SomeException) -> atomically $ writeTVar state False


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
isAlive = GenPeriodic $ \_ state _ -> Done <$> readTVarIO state

stopPeriodic :: GenPeriodic u ()
stopPeriodic = GenPeriodic $ \_ state _ ->
  Done <$> atomically (writeTVar state False)
