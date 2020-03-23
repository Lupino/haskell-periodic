module Periodic.Server.Types
  ( Command (..)
  , ClientConfig (..)
  , CSEnv
  ) where
import           Data.Binary                  (Binary (..), getWord8)
import           Data.Binary.Get              (lookAhead)
import           Periodic.IOList              (IOList)
import           Periodic.Node                (SessionEnv)
import qualified Periodic.Types.ClientCommand as CC
import           Periodic.Types.Job           (FuncName, JobHandle)
import qualified Periodic.Types.WorkerCommand as WC

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
    { wFuncList :: IOList FuncName
    , wJobQueue :: IOList JobHandle
    }

type CSEnv = SessionEnv ClientConfig Command
