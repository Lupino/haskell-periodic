module Periodic.Node
  ( Nid (..)
  , NodeEnv
  , NodeT
  , SessionT
  , NodeEnvList
  , runNodeT
  ) where

import           Data.ByteString (ByteString)
import           Metro.IOHashMap (IOHashMap)
import qualified Metro.Node      as M (NodeEnv1, NodeT, runNodeT1)
import qualified Metro.Session   as M (SessionT)
import           Periodic.Types  (Msgid (..), Packet, msgidLength)
import           System.Entropy  (getEntropy)

newtype Nid = Nid ByteString

type NodeEnv u cmd = M.NodeEnv1 u Nid Msgid (Packet cmd)

type NodeT u cmd = M.NodeT u Nid Msgid (Packet cmd)

type SessionT u cmd = M.SessionT u Nid Msgid (Packet cmd)

sessionGen :: IO Msgid
sessionGen = Msgid <$> getEntropy msgidLength

type NodeEnvList u cmd tp = IOHashMap Nid (NodeEnv u cmd tp)

runNodeT :: Monad m => NodeEnv u cmd tp -> NodeT u cmd tp m a -> m a
runNodeT  = M.runNodeT1
