{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Control.Monad                   (when)
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.ByteString.Char8           as B (pack)
import qualified Data.ByteString.Lazy            as LB (empty, fromStrict,
                                                        toStrict)
import           Data.Default.Class              (def)
import           Data.List                       (isPrefixOf)
import           Data.Maybe                      (fromMaybe)
import           Data.Streaming.Network.Internal (HostPreference (Host))
import           Data.String                     (fromString)
import           Metro.Class                     (Transport)
import           Metro.Socket                    (getHost, getService)
import           Metro.TP.Socket                 (socket)
import           Metro.TP.TLS                    (makeClientParams', tlsConfig)
import           Metro.TP.WebSockets             (clientConfig)
import           Metro.TP.XOR                    (xorConfig)
import           Network.HTTP.Types              (status204, status500)
import           Network.Wai.Handler.Warp        (setHost, setPort)
import           Periodic.Trans.ClientPool
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
    , showHelp :: Bool
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
  , showHelp  = False
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
parseOptions ("--help":xs)        opt = parseOptions xs opt { showHelp = True }
parseOptions ("-h":xs)            opt = parseOptions xs opt { showHelp = True }
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
  putStrLn "  -h --help       Display help message"
  putStrLn ""
  putStrLn "Version: v1.1.7.0"
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"
  f <- lookupEnv "XOR_FILE"

  opts@Options{..} <- flip parseOptions (options h f) <$> getArgs

  when showHelp printHelp

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  run opts def
    { settings = setPort httpPort
               $ setHost (Host httpHost) (settings def)}

run Options {useTls = True, ..} sopts = do
  prms <- makeClientParams' cert [] certKey caStore (hostName, B.pack $ fromMaybe "" $ getService host)
  clientEnv <- openPool (tlsConfig prms (socket host)) poolSize
  scottyOpts sopts $ application clientEnv

run Options {useWs = True, ..} sopts = do
  clientEnv <- openPool (clientConfig (socket host) (fromMaybe "0.0.0.0" $ getHost host) (fromMaybe "" $ getService host)) poolSize
  scottyOpts sopts $ application clientEnv

run Options {xorFile = "", ..} sopts = do
  clientEnv <- openPool (socket host) poolSize
  scottyOpts sopts $ application clientEnv

run Options {..} sopts = do
  clientEnv <- openPool (xorConfig xorFile $ socket host) poolSize
  scottyOpts sopts $ application clientEnv

application :: Transport tp => ClientPoolEnv tp ->  ScottyM ()
application clientEnv = do
  get "/periodic/status/" $ statusHandler clientEnv
  post "/periodic/submit/:func_name/:job_name/" $ submitJobHandler clientEnv
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

submitJobHandler :: Transport tp => ClientPoolEnv tp -> ActionM ()
submitJobHandler clientEnv = do
  job <- paramJob_
  r <- liftIO $ runClientPoolT clientEnv $ submitJob_ job
  sampleRet r

removeJobHandler :: Transport tp => ClientPoolEnv tp -> ActionM ()
removeJobHandler clientEnv = do
  (fn, jn) <- paramJob
  r <- liftIO $ runClientPoolT clientEnv $ removeJob fn jn
  sampleRet r

runJobHandler :: Transport tp => ClientPoolEnv tp -> ActionM ()
runJobHandler clientEnv = do
  job <- paramJob_
  r <- liftIO $ runClientPoolT clientEnv $ runJob_ job
  case r of
    Nothing -> do
      WS.status status500
      raw "error"
    Just bs -> raw $ LB.fromStrict bs

dropFuncHandler :: Transport tp => ClientPoolEnv tp -> ActionM ()
dropFuncHandler clientEnv = do
  fn <- fromString <$> param "func_name"
  r <- liftIO $ runClientPoolT clientEnv $ dropFunc fn
  sampleRet r

statusHandler :: Transport tp => ClientPoolEnv tp -> ActionM ()
statusHandler clientEnv = do
  r <- liftIO $ runClientPoolT clientEnv status
  raw $ LB.fromStrict r
