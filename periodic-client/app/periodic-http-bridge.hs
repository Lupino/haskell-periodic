{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  (
    main
  ) where

import           Control.Monad                   (when)
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.ByteString.Char8           as B (pack)
import qualified Data.ByteString.Lazy            as LB (empty, fromStrict,
                                                        readFile, toStrict)
import           Data.Default.Class              (def)
import           Data.List                       (isPrefixOf)
import           Data.Maybe                      (fromMaybe)
import           Data.Streaming.Network.Internal (HostPreference (Host))
import           Data.String                     (fromString)
import           Network.HTTP.Types              (status204, status500)
import           Network.Wai.Handler.Warp        (setHost, setPort)
import           Periodic.ClientPool
import           Periodic.Socket                 (getHost, getService)
import           Periodic.Transport              (Transport)
import           Periodic.Transport.TLS
import qualified Periodic.Transport.WebSockets   as WS (makeClientTransport)
import           Periodic.Transport.XOR          (makeXORTransport)
import           Periodic.Types.Job
import           System.Environment              (getArgs, lookupEnv)
import           System.Exit                     (exitSuccess)
import           Web.Scotty                      (ActionM, ScottyM, body, get,
                                                  param, post, raw, rescue,
                                                  scottyOpts, settings)
import qualified Web.Scotty                      as WS (status)


data Options = Options
  { host     :: String
  , xorFile  :: FilePath
  , useTls   :: Bool
  , useWs    :: Bool
  , hostName :: String
  , certKey  :: FilePath
  , cert     :: FilePath
  , caStore  :: FilePath
  , httpHost :: String
  , httpPort :: Int
  , poolSize :: Int
  }

options :: Maybe String -> Maybe String -> Options
options h f = Options
  { host     = fromMaybe "unix:///tmp/periodic.sock" h
  , xorFile  = fromMaybe "" f
  , useTls   = False
  , useWs    = False
  , hostName = "localhost"
  , certKey  = "client-key.pem"
  , cert     = "client.pem"
  , caStore  = "ca.pem"
  , httpHost = "127.0.0.1"
  , httpPort = 8080
  , poolSize = 10
  }

parseOptions :: [String] -> Options -> Options
parseOptions []                   opt = opt
parseOptions ("-H":x:xs)          opt = parseOptions xs opt { host = x }
parseOptions ("--host":x:xs)      opt = parseOptions xs opt { host = x }
parseOptions ("--xor":x:xs)       opt = parseOptions xs opt { xorFile = x }
parseOptions ("--tls":xs)         opt = parseOptions xs opt { useTls = True }
parseOptions ("--ws":xs)          opt = parseOptions xs opt { useWs = True }
parseOptions ("--hostname":x:xs)  opt = parseOptions xs opt { hostName = x }
parseOptions ("--cert-key":x:xs)  opt = parseOptions xs opt { certKey = x }
parseOptions ("--cert":x:xs)      opt = parseOptions xs opt { cert = x }
parseOptions ("--ca":x:xs)        opt = parseOptions xs opt { caStore = x }
parseOptions ("--http-host":x:xs) opt = parseOptions xs opt { httpHost = x }
parseOptions ("--http-port":x:xs) opt = parseOptions xs opt { httpPort = read x }
parseOptions ("--pool-size":x:xs) opt = parseOptions xs opt { poolSize = read x }
parseOptions (_:xs)               opt = parseOptions xs opt

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-http-bridge - Periodic task system client http bridge"
  putStrLn ""
  putStrLn "Usage: periodic [--host|-H HOST] [--http-host HOST] [--http-port PORT] [--pool-size SIZE] [--xor FILE|--ws|--tls [--hostname HOSTNAME] [--cert-key FILE] [--cert FILE] [--ca FILE]]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host      Socket path [$PERIODIC_PORT]"
  putStrLn "                 eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "     --xor       XOR Transport encode file [$XOR_FILE]"
  putStrLn "     --tls       Use tls transport"
  putStrLn "     --ws        Use websockets transport"
  putStrLn "     --hostname  Host name"
  putStrLn "     --cert-key  Private key associated"
  putStrLn "     --cert      Public certificate (X.509 format)"
  putStrLn "     --ca        trusted certificates"
  putStrLn "     --http-host HTTP host (optional: 127.0.0.1)"
  putStrLn "     --http-port HTTP port (optional: 8080)"
  putStrLn "     --pool-size Connection pool size"
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"
  f <- lookupEnv "XOR_FILE"

  opts@Options{..} <- flip parseOptions (options h f) <$> getArgs

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  clientEnv <- open (makeTransport opts) host poolSize

  let sopts = def {settings = setPort httpPort
                           $ setHost (Host httpHost) (settings def)}
  scottyOpts sopts $ application clientEnv

makeTransport :: Options -> Transport -> IO Transport
makeTransport Options{..} transport
  | useTls = do
    prms <- makeClientParams' cert [] certKey caStore (hostName, B.pack $ getService host)
    makeTLSTransport prms transport
  | useWs =
    WS.makeClientTransport (fromMaybe "0.0.0.0" $ getHost host) (getService host) transport
  | otherwise = makeTransport' xorFile transport

makeTransport' :: FilePath -> Transport -> IO Transport
makeTransport' [] transport = return transport
makeTransport' p transport  = do
  key <- LB.readFile p
  makeXORTransport key transport

application :: ClientEnv ->  ScottyM ()
application clientEnv = do
  get "/periodic/status/" $ statusHandler clientEnv
  post "/periodic/submit/:func_name/:job_name/:sched_at/:timeout/" $ submitJobHandler clientEnv
  post "/periodic/submit/:func_name/:job_name/:sched_at/" $ submitJobHandler clientEnv
  post "/periodic/submit/:func_name/:job_name/" $ submitJobHandler clientEnv
  post "/periodic/run/:func_name/:job_name/:timeout/" $ runJobHandler clientEnv
  post "/periodic/run/:func_name/:job_name/" $ runJobHandler clientEnv
  post "/periodic/remove/:func_name/:job_name/" $ removeJobHandler clientEnv
  post "/periodic/drop/:func_name/" $ dropFuncHandler clientEnv

paramJob :: ActionM (FuncName, JobName)
paramJob = do
  fn <- fromString <$> param "func_name"
  jn <- fromString <$> param "job_name"
  return (fn, jn)

paramJob_ :: ActionM Job
paramJob_ = do
  (fn, jn) <- paramJob
  schedAt <- param "sched_at" `rescue` const (return 0)
  timeout <- param "timeout" `rescue` const (return 0)
  wb <- Workload . LB.toStrict <$> body
  pure $ setSchedAt schedAt $ setTimeout timeout $ setWorkload wb $ initJob fn jn

sampleRet :: Bool -> ActionM ()
sampleRet r = do
  WS.status $ if r then status204 else status500
  raw LB.empty

submitJobHandler :: ClientEnv -> ActionM ()
submitJobHandler clientEnv = do
  job <- paramJob_
  r <- liftIO $ runClientT clientEnv $ submitJob_ job
  sampleRet r

removeJobHandler :: ClientEnv -> ActionM ()
removeJobHandler clientEnv = do
  (fn, jn) <- paramJob
  r <- liftIO $ runClientT clientEnv $ removeJob fn jn
  sampleRet r

runJobHandler :: ClientEnv -> ActionM ()
runJobHandler clientEnv = do
  job <- paramJob_
  r <- liftIO $ runClientT clientEnv $ runJob_ job
  raw $ LB.fromStrict r

dropFuncHandler :: ClientEnv -> ActionM ()
dropFuncHandler clientEnv = do
  fn <- fromString <$> param "func_name"
  r <- liftIO $ runClientT clientEnv $ dropFunc fn
  sampleRet r

statusHandler :: ClientEnv -> ActionM ()
statusHandler clientEnv = do
  r <- liftIO $ runClientT clientEnv status
  raw $ LB.fromStrict r
