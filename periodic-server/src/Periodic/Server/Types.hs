{-# LANGUAGE MultiParamTypeClasses #-}

module Periodic.Server.Types
  ( Command (..)
  , ClientConfig (..)
  , CSEnv
  , ServerCommand (..)
  ) where


import           Data.Binary                  (Binary (..), getWord8)
import           Data.Binary.Get              (lookAhead)
import           Data.Int                     (Int64)
import           Data.IOMap                   (IOMap)
import           Metro.Class                  (RecvPacket (..))
import           Periodic.Node                (SessionEnv1)
import qualified Periodic.Types.ClientCommand as CC
import           Periodic.Types.Internal      (Msgid)
import           Periodic.Types.Job           (FuncName, JobHandle)
import           Periodic.Types.Packet        (Packet, recvRawPacket)
import           Periodic.Types.ServerCommand (ServerCommand (..))
import qualified Periodic.Types.WorkerCommand as WC
import           UnliftIO                     (TVar)

data Command = CC CC.ClientCommand
    | WC WC.WorkerCommand

instance Binary Command where
  get = do
    cmd <- lookAhead getWord8
    case cmd of
      1  -> WC <$> get
      2  -> WC <$> get
      3  -> WC <$> get
      4  -> WC <$> get
      11 -> WC <$> get
      -- 9  -> WC <$> get
      7  -> WC <$> get
      8  -> WC <$> get
      21 -> WC <$> get
      27 -> WC <$> get
      28 -> WC <$> get
      13 -> CC <$> get
      14 -> CC <$> get
      9  -> CC <$> get
      15 -> CC <$> get
      17 -> CC <$> get
      20 -> CC <$> get
      22 -> CC <$> get
      23 -> CC <$> get
      18 -> CC <$> get
      19 -> CC <$> get
      25 -> CC <$> get
      _  -> error $ "Error Command" ++ show cmd

  put (CC cmd) = put cmd
  put (WC cmd) = put cmd

data ClientConfig = ClientConfig
  { wFuncList  :: TVar [FuncName]
  , wJobQueue  :: IOMap JobHandle Int64
  , wMsgidList :: TVar [Msgid]
  }

type CSEnv = SessionEnv1 ClientConfig Command

instance (Binary a) => RecvPacket ClientConfig (Packet a) where
  recvPacket _ = recvRawPacket
