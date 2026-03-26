{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Trans.Client
  ( ClientT
  , ClientEnv
  , open
  , close
  , runClientT
  , closeClientEnv

  , ping
  , submitJob_
  , submitJob
  , runJob_
  , runJob
  , recvJobData_
  , recvJobData
  , removeJob
  , dropFunc
  , status
  , configGet
  , configSet
  , load
  , dump
  , shutdown

  -- utils
  , formatStatus
  ) where

import           Control.Monad                (unless, void)
import           Data.Binary                  (decodeOrFail)
import           Data.Binary.Get              (getWord32be, runGetOrFail)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B (lines, split, unpack)
import qualified Data.ByteString.Lazy         as BL (fromStrict, null)
import           Data.List                    (transpose)
import           Data.UnixTime
import           Metro.Class                  (Transport, TransportConfig)
import           Metro.Conn                   (initConnEnv, runConnT)
import qualified Metro.Conn                   as Conn
import           Metro.Node                   (NodeMode (..), SessionMode (..),
                                               initEnv1, request,
                                               setDefaultSessionTimeout1,
                                               setNodeMode, setSessionMode,
                                               startNodeT, withSessionT)
import           Metro.Session                (send)
import           Periodic.Node
import           Periodic.Trans.BaseClient
import           Periodic.Types               (ClientType (TypeClient),
                                               getClientType, getResult,
                                               packetREQ, regPacketREQ)
import           Periodic.Types.ClientCommand
import           Periodic.Types.Internal      (ConfigKey (..))
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand
import           System.IO.Unsafe             (unsafePerformIO)
import qualified Text.PrettyPrint.Boxes       as T
import           Text.Read                    (readMaybe)
import           UnliftIO

data ClientEnv tp = ClientEnv
  { clientConfig        :: TransportConfig tp
  , clientEnvVar        :: TVar (BaseClientEnv () tp)
  , clientReconnectLock :: MVar ()
  }
type ClientT = BaseClientT ()

runClientT :: (MonadUnliftIO m, Transport tp) => ClientEnv tp -> ClientT tp m a -> m a
runClientT env@ClientEnv {..} m = do
  cenv <- readTVarIO clientEnvVar
  firstTry <- tryAny (runNodeT cenv m)
  case firstTry of
    Right v  -> pure v
    Left err -> do
      reconnectClientEnv env
      cenv' <- readTVarIO clientEnvVar
      secondTry <- tryAny (runNodeT cenv' m)
      case secondTry of
        Right v'   -> pure v'
        Left _err' -> throwIO err

open
  :: (MonadUnliftIO m, Transport tp)
  => TransportConfig tp -> m (ClientEnv tp)
open config = do
  cenv <- openClientEnv config
  cenvVar <- newTVarIO cenv
  reconnectLock <- newMVar ()
  pure $ ClientEnv
    { clientConfig = config
    , clientEnvVar = cenvVar
    , clientReconnectLock = reconnectLock
    }

closeClientEnv :: Transport tp => ClientEnv tp -> IO ()
closeClientEnv ClientEnv {..} = do
  cenv <- readTVarIO clientEnvVar
  void $ tryAny (runNodeT cenv close)

reconnectClientEnv :: (MonadUnliftIO m, Transport tp) => ClientEnv tp -> m ()
reconnectClientEnv ClientEnv {..} = withMVar clientReconnectLock $ \_ -> do
  cenv <- readTVarIO clientEnvVar
  healthy <- isClientHealthy cenv
  unless healthy $ do
    void $ tryAny (runNodeT cenv close)
    cenv' <- openClientEnv clientConfig
    atomically $ writeTVar clientEnvVar cenv'

isClientHealthy
  :: (MonadUnliftIO m, Transport tp)
  => BaseClientEnv () tp -> m Bool
isClientHealthy cenv = do
  ret <- tryAny (timeout 3000000 (runNodeT cenv ping))
  pure $ case ret of
    Right (Just True) -> True
    _                 -> False

openClientEnv
  :: (MonadUnliftIO m, Transport tp)
  => TransportConfig tp -> m (BaseClientEnv () tp)
openClientEnv config = do
  connEnv <- initConnEnv config
  r <- runConnT connEnv $ do
    Conn.send $ regPacketREQ TypeClient
    Conn.receive_

  let nid = case getClientType r of
              Data v ->
                case runGetOrFail getWord32be (BL.fromStrict v) of
                  Right (rest, _, nidV) | BL.null rest -> nidV
                  _                                    -> 0
              _      -> 0

  cenv <- initEnv1 mapEnv connEnv () (Nid nid) True sessionGen
  setDefaultSessionTimeout1 cenv 100

  runNodeT cenv $
    void $ async $ startNodeT defaultSessionHandler
  pure cenv

  where mapEnv =
          setNodeMode Multi
          . setSessionMode SingleAction

dropFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> BaseClientT u tp m Bool
dropFunc func = getResult False isSuccess <$> request Nothing (packetREQ (DropFunc func))

removeJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> BaseClientT u tp m Bool
removeJob f n = getResult False isSuccess <$> request Nothing (packetREQ (RemoveJob f n))

status
  :: (MonadUnliftIO m, Transport tp)
  => BaseClientT u tp m ByteString
status = getResult "" getRaw <$> request Nothing (packetREQ Status)
  where getRaw :: ServerCommand -> ByteString
        getRaw (Data bs) = bs
        getRaw _         = ""

unpackBS :: [[ByteString]] -> [[String]]
unpackBS = map (map B.unpack)

formatTime :: [String] -> [String]
formatTime []     = []
formatTime [x]    = [formatUnixTimeLocal x]
formatTime (x:xs) = x:formatTime xs

formatUnixTimeLocal :: String -> String
formatUnixTimeLocal s =
  case readMaybe s of
    Nothing -> s
    Just v ->
      B.unpack
      $ unsafePerformIO
      $ formatUnixTime "%Y-%m-%d %H:%M:%S"
      $ fromEpochTime
      $ fromIntegral (v :: Int)

renderTable :: [String] -> [[String]] -> String
renderTable header rows =
  T.render
  $ T.hsep 2 T.left
  (map (T.vcat T.left . map T.text)
  (transpose (header:rows)))


formatStatus :: ByteString -> String
formatStatus =
  renderTable header
  . map formatTime
  . unpackBS
  . map (B.split ',')
  . B.lines
  where header = ["FUNCTIONS", "WORKERS", "JOBS", "PROCESSING", "LOCKED", "SCHEDAT"]


configGet
  :: (MonadUnliftIO m, Transport tp)
  => String -> BaseClientT u tp m Int
configGet k = getResult 0 getV <$> request Nothing (packetREQ (ConfigGet (ConfigKey k)))
  where getV :: ServerCommand -> Int
        getV (Config v) = v
        getV _          = 0

configSet
  :: (MonadUnliftIO m, Transport tp)
  => String -> Int -> BaseClientT u tp m Bool
configSet k v = getResult False isSuccess <$> request Nothing (packetREQ (ConfigSet (ConfigKey k) v))

load :: (MonadUnliftIO m, Transport tp) => [Job] -> BaseClientT u tp m Bool
load jobs = getResult False isSuccess <$> request Nothing (packetREQ (Load jobs))

dump :: (MonadUnliftIO m, Transport tp) => BaseClientT u tp m [Job]
dump = getResult [] getV <$> request Nothing (packetREQ Dump)
  where getV :: ServerCommand -> [Job]
        getV (Data bs) =
          case decodeOrFail (BL.fromStrict bs) of
            Right (rest, _, jobs)
              | BL.null rest -> jobs
              | otherwise    -> []
            Left _ -> []
        getV _         = []

shutdown :: (MonadUnliftIO m, Transport tp) => BaseClientT u tp m ()
shutdown = withSessionT Nothing $ send $ packetREQ Shutdown
