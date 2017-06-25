{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.GrabQueue
  (
    GrabQueue
  , GrabItem (..)
  , pushAgent
  , popMaybeAgent
  , hasAgent
  ) where

import           Data.ByteString.Char8    (ByteString)
import           Periodic.Server.Agent    (Agent, agentid)
import           Periodic.Server.FuncList (FuncList, FuncName, delete, elems,
                                           insert, member)

data GrabItem = GrabItem { gFuncList :: FuncList Bool
                         , gAgent    :: Agent
                         , gJobQueue :: FuncList Bool
                         }

key :: GrabItem -> ByteString
key (GrabItem { gAgent = a }) = agentid a

type GrabQueue = FuncList GrabItem

pushAgent :: GrabQueue -> FuncList Bool -> FuncList Bool -> Agent -> IO ()
pushAgent q gFuncList gJobQueue gAgent = insert q (key i) i
  where i = GrabItem {..}

popMaybeAgent :: GrabQueue -> FuncName -> IO (Maybe (FuncList Bool, Agent))
popMaybeAgent q n = do
  item <- go =<< elems q
  case item of
    Nothing -> return Nothing
    Just i  -> do
      delete q (key i)
      return (Just (gJobQueue i, gAgent i))

 where go :: [GrabItem] -> IO (Maybe GrabItem)
       go [] = return Nothing
       go (x:xs) = do
         has <- member (gFuncList x) n
         if has then return (Just x)
                else go xs

hasAgent :: GrabQueue -> FuncName -> IO Bool
hasAgent q n = go =<< elems q
 where go :: [GrabItem] -> IO Bool
       go [] = return False
       go (x:xs) = do
         has <- member (gFuncList x) n
         if has then return True
                else go xs
