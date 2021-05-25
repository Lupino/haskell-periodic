{-# LANGUAGE RecordWildCards #-}

module Periodic.Server.GrabQueue
  ( GrabQueue
  , newGrabQueue
  , GrabItem (..)
  , pushAgent
  , popAgentSTM
  , popAgentList
  ) where

import           Control.Arrow         ((&&&))
import           Control.Monad.STM     (STM, retry)
import           Data.IOMap            (IOMap)
import qualified Data.IOMap            as IOMap
import qualified Data.IOMap.STM        as IOMapS
import           Metro.Session         (ident)
import           Periodic.Node         (Nid)
import           Periodic.Server.Types (CSEnv)
import           Periodic.Types        (FuncName, JobHandle, Msgid)
import           Prelude               hiding (elem)
import           UnliftIO              (MonadIO (..))

data GrabItem tp = GrabItem
  { gFuncList :: IOMap FuncName ()
  , gAgent    :: CSEnv tp
  , gJobQueue :: IOMap JobHandle ()
  }

instance Eq (GrabItem tp) where
    (==) = eqGrabItem

instance Ord (GrabItem tp) where
  compare x y = compare (key x) (key y)

key :: GrabItem tp -> (Nid, Msgid)
key GrabItem{gAgent = a} = ident a

eqGrabItem :: GrabItem tp -> GrabItem tp -> Bool
eqGrabItem a b = key a == key b

type GrabQueue tp = IOMap (Nid, Msgid) (GrabItem tp)

newGrabQueue :: MonadIO m => m (GrabQueue tp)
newGrabQueue = IOMap.empty

pushAgent :: MonadIO m => GrabQueue tp -> IOMap FuncName () -> IOMap JobHandle () -> CSEnv tp -> m ()
pushAgent q gFuncList gJobQueue gAgent = IOMap.insert (key i) i q
  where i = GrabItem {..}

popAgentSTM :: GrabQueue tp -> FuncName -> STM (IOMap JobHandle (), CSEnv tp)
popAgentSTM q n = do
  item <- go =<< IOMapS.elems q
  IOMapS.delete (key item) q
  return (gJobQueue item, gAgent item)

 where go :: [GrabItem tp] -> STM (GrabItem tp)
       go [] = retry
       go (x:xs) = do
         has <- IOMapS.member n (gFuncList x)
         if has then return x
                else go xs

popAgentList :: MonadIO m => GrabQueue tp -> FuncName -> m [(IOMap JobHandle (), CSEnv tp)]
popAgentList q n = do
  items <- go =<< IOMap.elems q
  mapM_ ((`IOMap.delete` q) . key) items
  pure $ map (gJobQueue &&& gAgent) items

 where go :: MonadIO m => [GrabItem tp] -> m [GrabItem tp]
       go [] = return []
       go (x:xs) = do
         has <- IOMap.member n (gFuncList x)
         xs' <- go xs
         if has then pure (x:xs')
                else pure xs'
