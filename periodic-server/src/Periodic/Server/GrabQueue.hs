{-# LANGUAGE RecordWildCards #-}

module Periodic.Server.GrabQueue
  ( GrabQueue
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

data GrabItem tp = GrabItem
  { gFuncList :: IOList FuncName
  , gAgent    :: AgentEnv' tp
  , gJobQueue :: IOList JobHandle
  }

instance Eq (GrabItem tp) where
    (==) = eqGrabItem

key :: GrabItem tp -> ByteString
key GrabItem{gAgent = a} = agentid a

eqGrabItem :: GrabItem tp -> GrabItem tp -> Bool
eqGrabItem a b = key a == key b

type GrabQueue tp = IOList (GrabItem tp)

newGrabQueue :: MonadIO m => m (GrabQueue tp)
newGrabQueue = newIOList

pushAgent :: MonadIO m => GrabQueue tp -> IOList FuncName -> IOList JobHandle -> AgentEnv' tp -> m ()
pushAgent q gFuncList gJobQueue gAgent = do
  has <- elem q i
  unless has $ append q i
  where i = GrabItem {..}

popAgentSTM :: GrabQueue tp -> FuncName -> STM (IOList JobHandle, AgentEnv' tp)
popAgentSTM q n = do
  item <- go =<< toListSTM q
  deleteSTM q item
  return (gJobQueue item, gAgent item)

 where go :: [GrabItem tp] -> STM (GrabItem tp)
       go [] = retry
       go (x:xs) = do
         has <- elemSTM (gFuncList x) n
         if has then return x
                else go xs

popAgentList :: MonadIO m => GrabQueue tp -> FuncName -> m [(IOList JobHandle, AgentEnv' tp)]
popAgentList q n = do
  items <- go =<< toList q
  mapM_ (delete q) items
  pure $ map (gJobQueue &&& gAgent) items

 where go :: MonadIO m => [GrabItem tp] -> m [GrabItem tp]
       go [] = return []
       go (x:xs) = do
         has <- elem (gFuncList x) n
         xs' <- go xs
         if has then pure (x:xs')
                else pure xs'
