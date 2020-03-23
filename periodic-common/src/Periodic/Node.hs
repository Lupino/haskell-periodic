module Periodic.Node
  ( Nid (..)
  , NodeEnv
  , NodeT
  , SessionT
  , SessionEnv
  , NodeEnvList
  , runNodeT
  , sessionGen
  , defaultSessionHandler
  ) where

import           Metro.IOHashMap   (IOHashMap)
import qualified Metro.Node        as M (NodeEnv1, NodeT, runNodeT1)
import qualified Metro.Session     as M (SessionEnv1, SessionT, getSessionId)
import           Periodic.Types    (Msgid (..), Nid (..), Packet, msgidLength)
import           System.Entropy    (getEntropy)
import           System.Log.Logger (errorM)
import           UnliftIO          (MonadIO, liftIO)

type NodeEnv u rpkt = M.NodeEnv1 u Nid Msgid (Packet rpkt)

type NodeT u rpkt = M.NodeT u Nid Msgid (Packet rpkt)

type SessionT u rpkt = M.SessionT u Nid Msgid (Packet rpkt)

type SessionEnv u rpkt = M.SessionEnv1 u Nid Msgid (Packet rpkt)

sessionGen :: IO Msgid
sessionGen = Msgid <$> getEntropy msgidLength

type NodeEnvList u rpkt tp = IOHashMap Nid (NodeEnv u rpkt tp)

runNodeT :: Monad m => NodeEnv u rpkt tp -> NodeT u rpkt tp m a -> m a
runNodeT  = M.runNodeT1

defaultSessionHandler :: MonadIO m => SessionT u rpkt tp m ()
defaultSessionHandler = do
  pid <- M.getSessionId
  liftIO $ errorM "Periodic.Node" $ "Session [" ++ show pid ++ "] not found."
