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

data Command
  = CC CC.ClientCommand
  | WC WC.WorkerCommand
  | SC ServerCommand


instance Binary Command where
  get = do
    cmd <- lookAhead getWord8
    case cmd of
      -- 0x00 SC.Noop
      0  -> SC <$> get
      -- 0x01 WC.GrabJob
      1  -> WC <$> get
      -- 0x02 WC.SchedLater
      2  -> WC <$> get
      -- 0x03 WC.WorkDone
      3  -> WC <$> get
      -- 0x04 WC.WorkFail
      4  -> WC <$> get
      -- 0x05 SC.JobAssign
      5  -> SC <$> get
      -- 0x06 SC.NoJob
      6  -> SC <$> get
      -- 0x07 WC.CanDo
      7  -> WC <$> get
      -- 0x08 WC.CantDo
      8  -> WC <$> get
      -- 0x09 WC.Ping
      -- 9  -> WC <$> get
      -- 0x09 CC.Ping
      9  -> CC <$> get
      -- 0x0A SC.Pong
      10 -> SC <$> get
      -- 0x0B WC.Sleep
      11 -> WC <$> get
      -- 0x0C SC.Unknown
      12 -> SC <$> get
      -- 0x0D CC.SubmitJob
      13 -> CC <$> get
      -- 0x0E CC.Status
      14 -> CC <$> get
      -- 0x0F CC.DropFunc
      15 -> CC <$> get
      -- 0x10 SC.Success
      16 -> SC <$> get
      -- 0x11 CC.RemoveJob
      17 -> CC <$> get
      -- 0x12 CC.Dump
      18 -> CC <$> get
      -- 0x13 CC.Load
      19 -> CC <$> get
      -- 0x14 CC.Shutdown
      20 -> CC <$> get
      -- 0x15 WC.Broadcast
      21 -> WC <$> get
      -- 0x16 CC.ConfigGet
      22 -> CC <$> get
      -- 0x17 CC.ConfigSet
      23 -> CC <$> get
      -- 0x18 SC.Config
      24 -> SC <$> get
      -- 0x19 CC.RunJob
      25 -> CC <$> get
      -- 0x1A SC.Acquired
      26 -> SC <$> get
      -- 0x1B WC.Acquire
      27 -> WC <$> get
      -- 0x1C WC.Release
      28 -> WC <$> get
      -- 0x1D SC.NoWorker
      29 -> SC <$> get
      -- 0x1E SC.Data
      30 -> SC <$> get
      -- 0x1F CC.RecvData
      31 -> CC <$> get
      -- 0x20 WC.WorkData
      32 -> WC <$> get
      33 -> WC <$> get
      _  -> error $ "Error Command " ++ show cmd

  put (CC cmd) = put cmd
  put (WC cmd) = put cmd
  put (SC cmd) = put cmd

data ClientConfig = ClientConfig
  { wFuncList  :: TVar [FuncName]
  , wJobQueue  :: IOMap JobHandle Int64
  , wMsgidList :: TVar [Msgid]
  }

type CSEnv = SessionEnv1 ClientConfig Command

instance (Binary a) => RecvPacket ClientConfig (Packet a) where
  recvPacket _ = recvRawPacket
