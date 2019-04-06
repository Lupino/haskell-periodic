{-# LANGUAGE RecordWildCards #-}

module Periodic.Server.GrabQueue
  (
    GrabQueue
  , newGrabQueue
  , GrabItem (..)
  , pushAgent
  , popAgentSTM
  , popAgentList
  ) where

import           Control.Arrow     ((&&&))
import           Control.Monad     (unless)
import           Control.Monad.STM (STM, retry)
import           Data.ByteString   (ByteString)
import           Periodic.Agent    (AgentEnv', agentid)
import           Periodic.IOList   (IOList, append, delete, deleteSTM, elem,
                                    elemSTM, newIOList, toList, toListSTM)
import           Periodic.Types    (FuncName, JobHandle)
import           Prelude           hiding (elem)
import           UnliftIO          (MonadIO (..))

data GrabItem = GrabItem { gFuncList :: IOList FuncName
                         , gAgent    :: AgentEnv'
                         , gJobQueue :: IOList JobHandle
                         }

instance Eq GrabItem where
    (==) = eqGrabItem

key :: GrabItem -> ByteString
key GrabItem{gAgent = a} = agentid a

eqGrabItem :: GrabItem -> GrabItem -> Bool
eqGrabItem a b = key a == key b

type GrabQueue = IOList GrabItem

newGrabQueue :: MonadIO m => m GrabQueue
newGrabQueue = newIOList

pushAgent :: MonadIO m => GrabQueue -> IOList FuncName -> IOList JobHandle -> AgentEnv' -> m ()
pushAgent q gFuncList gJobQueue gAgent = do
  has <- elem q i
  unless has $ append q i
  where i = GrabItem {..}

popAgentSTM :: GrabQueue -> FuncName -> STM (IOList JobHandle, AgentEnv')
popAgentSTM q n = do
  item <- go =<< toListSTM q
  deleteSTM q item
  return (gJobQueue item, gAgent item)

 where go :: [GrabItem] -> STM GrabItem
       go [] = retry
       go (x:xs) = do
         has <- elemSTM (gFuncList x) n
         if has then return x
                else go xs

popAgentList :: MonadIO m => GrabQueue -> FuncName -> m [(IOList JobHandle, AgentEnv')]
popAgentList q n = do
  items <- go =<< toList q
  mapM_ (delete q) items
  pure $ map (gJobQueue &&& gAgent) items

 where go :: MonadIO m => [GrabItem] -> m [GrabItem]
       go [] = return []
       go (x:xs) = do
         has <- elem (gFuncList x) n
         xs' <- go xs
         if has then pure (x:xs')
                else pure xs'
