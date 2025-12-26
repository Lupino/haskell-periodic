{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where


import           Control.Exception               (SomeException)
import           Control.Monad                   (when)
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.ByteString.Lazy            as LB (empty, fromStrict,
                                                        toStrict)
import           Data.Default.Class              (def)
import           Data.List                       (isPrefixOf)
import           Data.Maybe                      (fromMaybe)
import           Data.Streaming.Network.Internal (HostPreference (Host))
import           Data.String                     (fromString)
import           Data.Version                    (showVersion)
import           Metro.Class                     (Transport)
import qualified Metro.TP.RSA                    as RSA (RSAMode (AES),
                                                         configClient)
import           Metro.TP.Socket                 (socket)
import           Network.HTTP.Types              (status204, status500)
import           Network.Wai.Handler.Warp        (setHost, setPort)
import           Paths_periodic_client_exe       (version)
import           Periodic.Trans.ClientPool
import           Periodic.Types.Job
import           System.Environment              (getArgs, lookupEnv)
import           System.Exit                     (exitSuccess)
import qualified Web.Scotty                      as WS (status)
import           Web.Scotty                      (ActionM, ScottyM, body,
                                                  captureParam, catch, get,
                                                  post, queryParam, raw,
                                                  scottyOpts, settings)


data Options = Options
  { host           :: String
  , httpHost       :: String
  , httpPort       :: Int
  , poolSize       :: Int
  , showHelp       :: Bool
  , rsaPrivatePath :: FilePath
  , rsaPublicPath  :: FilePath
  , rsaMode        :: RSA.RSAMode
  }

options :: Maybe String -> Options
options h = Options
  { host           = fromMaybe "unix:///tmp/periodic.sock" h
  , httpHost       = "127.0.0.1"
  , httpPort       = 8080
  , poolSize       = 10
  , showHelp       = False
  , rsaPublicPath  = "public_key.pem"
  , rsaPrivatePath = ""
  , rsaMode        = RSA.AES
  }

parseOptions :: [String] -> Options -> Options
parseOptions []                          opt = opt
parseOptions ("-H":x:xs)                 opt = parseOptions xs opt { host = x }
parseOptions ("--host":x:xs)             opt = parseOptions xs opt { host = x }
parseOptions ("--http-host":x:xs)        opt = parseOptions xs opt { httpHost = x }
parseOptions ("--http-port":x:xs)        opt = parseOptions xs opt { httpPort = read x }
parseOptions ("--pool-size":x:xs)        opt = parseOptions xs opt { poolSize = read x }
parseOptions ("--rsa-private-path":x:xs) opt = parseOptions xs opt { rsaPrivatePath = x }
parseOptions ("--rsa-public-path":x:xs)  opt = parseOptions xs opt { rsaPublicPath  = x }
parseOptions ("--rsa-mode":x:xs)         opt = parseOptions xs opt { rsaMode  = read x }
parseOptions ("--help":xs)               opt = parseOptions xs opt { showHelp = True }
parseOptions ("-h":xs)                   opt = parseOptions xs opt { showHelp = True }
parseOptions (_:xs)                      opt = parseOptions xs opt

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-http-bridge - Periodic task system client http bridge"
  putStrLn ""
  putStrLn "Usage: periodic-http-bridge [--host|-H HOST] [--http-host HOST] [--http-port PORT] [--pool-size SIZE] [--rsa-private-path FILE --rsa-public-path FILE|PATH --rsa-mode Plain|RSA|AES]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host             Socket path [$PERIODIC_PORT]"
  putStrLn "                        eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "     --http-host        HTTP host (optional: 127.0.0.1)"
  putStrLn "     --http-port        HTTP port (optional: 8080)"
  putStrLn "     --pool-size        Connection pool size"
  putStrLn "     --rsa-private-path RSA private key file path (optional: null)"
  putStrLn "     --rsa-public-path  RSA public key file path or dir (optional: public_key.pem)"
  putStrLn "     --rsa-mode         RSA mode Plain RSA AES (optional: AES)"
  putStrLn "  -h --help             Display help message"
  putStrLn ""
  putStrLn $ "Version: v" ++ showVersion version
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"

  Options{..} <- flip parseOptions (options h) <$> getArgs

  when showHelp printHelp

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  let sopts = def
        { settings = setPort httpPort
                   $ setHost (Host httpHost) (settings def)}

  case rsaPrivatePath of
    "" -> do
      clientEnv <- openPool (socket host) poolSize
      scottyOpts sopts $ application clientEnv
    _ -> do
      genTP <- RSA.configClient rsaMode rsaPrivatePath rsaPublicPath
      clientEnv <- openPool (genTP $ socket host) poolSize
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
  fn <- fromString <$> captureParam "func_name"
  jn <- fromString <$> captureParam "job_name"
  return (fn, jn)

paramJob_ :: ActionM Job
paramJob_ = do
  (fn, jn) <- paramJob
  schedAt <- queryParam "sched_at" `catch` (\(_ :: SomeException) -> return 0)
  timeout <- queryParam "timeout" `catch` (\(_ :: SomeException) -> return 0)
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
  fn <- fromString <$> captureParam "func_name"
  r <- liftIO $ runClientPoolT clientEnv $ dropFunc fn
  sampleRet r

statusHandler :: Transport tp => ClientPoolEnv tp -> ActionM ()
statusHandler clientEnv = do
  r <- liftIO $ runClientPoolT clientEnv status
  raw $ LB.fromStrict r
