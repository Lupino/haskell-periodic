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
import           Periodic.Agent    (Agent, agentid)
import           Periodic.IOList   (IOList, append, delete, deleteSTM, elem,
                                    elemSTM, newIOList, toList, toListSTM)
import           Periodic.Types    (FuncName, JobHandle)
import           Prelude           hiding (elem)

data GrabItem = GrabItem { gFuncList :: IOList FuncName
                         , gAgent    :: Agent
                         , gJobQueue :: IOList JobHandle
                         }

instance Eq GrabItem where
    (==) = eqGrabItem

key :: GrabItem -> ByteString
key GrabItem{gAgent = (_, a)} = agentid a

eqGrabItem :: GrabItem -> GrabItem -> Bool
eqGrabItem a b = key a == key b

type GrabQueue = IOList GrabItem

newGrabQueue :: IO GrabQueue
newGrabQueue = newIOList

pushAgent :: GrabQueue -> IOList FuncName -> IOList JobHandle -> Agent -> IO ()
pushAgent q gFuncList gJobQueue gAgent = do
  has <- elem q i
  unless has $ append q i
  where i = GrabItem {..}

popAgentSTM :: GrabQueue -> FuncName -> STM (IOList JobHandle, Agent)
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

popAgentList :: GrabQueue -> FuncName -> IO [(IOList JobHandle, Agent)]
popAgentList q n = do
  items <- go =<< toList q
  mapM_ (delete q) items
  pure $ map (gJobQueue &&& gAgent) items

 where go :: [GrabItem] -> IO [GrabItem]
       go [] = return []
       go (x:xs) = do
         has <- elem (gFuncList x) n
         xs' <- go xs
         if has then pure (x:xs')
                else pure xs'
