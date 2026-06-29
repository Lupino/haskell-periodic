{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where


import           Control.Applicative             ((<|>))
import           Control.Exception               (SomeException)
import           Control.Monad                   (when)
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.ByteString.Lazy            as LB (empty, fromStrict,
                                                        toStrict)
import           Data.List                       (isPrefixOf)
import           Data.Maybe                      (fromMaybe)
import qualified Data.ByteString.Char8           as B (pack)
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
import           Periodic.Exec.Util              (lookupNonEmptyEnv,
                                                  lookupReadEnv, parseReadArg)
import           Periodic.Trans.ClientPool
import           Periodic.Types                  (ClientIdentity (ClientIdentity))
import           Periodic.Types.Job
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure, exitSuccess)
import qualified Web.Scotty                      as WS (status)
import           Web.Scotty                      (ActionM, ScottyM, body,
                                                  captureParam, catch,
                                                  defaultOptions, get, post,
                                                  queryParam, raw, scottyOpts,
                                                  settings)


data Options = Options
  { host           :: String
  , httpHost       :: String
  , httpPort       :: Int
  , poolSize       :: Int
  , showHelp       :: Bool
  , rsaPrivatePath :: FilePath
  , rsaPublicPath  :: FilePath
  , rsaMode        :: RSA.RSAMode
  , clientName     :: Maybe String
  , clientToken    :: Maybe String
  , parseError     :: Maybe String
  , invalidOption  :: Maybe String
  }

options :: Maybe String -> Maybe FilePath -> Maybe FilePath -> RSA.RSAMode -> Maybe String -> Maybe String -> Options
options h mRsaPrivate mRsaPublic mode mClientName mClientToken = Options
  { host           = fromMaybe "unix:///tmp/periodic.sock" h
  , httpHost       = "127.0.0.1"
  , httpPort       = 8080
  , poolSize       = 10
  , showHelp       = False
  , rsaPublicPath  = fromMaybe "public_key.pem" mRsaPublic
  , rsaPrivatePath = fromMaybe "" mRsaPrivate
  , rsaMode        = mode
  , clientName     = mClientName
  , clientToken    = mClientToken
  , parseError     = Nothing
  , invalidOption  = Nothing
  }

parseOptions :: [String] -> Options -> Options
parseOptions []                          opt = opt
parseOptions ("-H":x:xs)                 opt = parseOptions xs opt { host = x }
parseOptions ("--host":x:xs)             opt = parseOptions xs opt { host = x }
parseOptions ("--http-host":x:xs)        opt = parseOptions xs opt { httpHost = x }
parseOptions ("--http-port":x:xs)        opt = parseOptions xs $ parseReadArg "--http-port" x (\v o -> o { httpPort = v }) onParseErr opt
parseOptions ("--pool-size":x:xs)        opt = parseOptions xs $ parseReadArg "--pool-size" x (\v o -> o { poolSize = v }) onParseErr opt
parseOptions ("--rsa-private-path":x:xs) opt = parseOptions xs opt { rsaPrivatePath = x }
parseOptions ("--rsa-public-path":x:xs)  opt = parseOptions xs opt { rsaPublicPath  = x }
parseOptions ("--rsa-mode":x:xs)         opt = parseOptions xs $ parseReadArg "--rsa-mode" x (\v o -> o { rsaMode = v }) onParseErr opt
parseOptions ("--client-name":x:xs)      opt = parseOptions xs opt { clientName = Just x }
parseOptions ("--client-token":x:xs)     opt = parseOptions xs opt { clientToken = Just x }
parseOptions ("--help":xs)               opt = parseOptions xs opt { showHelp = True }
parseOptions ("-h":xs)                   opt = parseOptions xs opt { showHelp = True }
parseOptions (x:xs)                      opt = parseOptions xs opt { invalidOption = invalidOption opt <|> Just x }

onParseErr :: String -> Options -> Options
onParseErr err o = o { parseError = parseError o <|> Just err }

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-http-bridge - RESTful HTTP gateway for Periodic task system"
  putStrLn ""
  putStrLn "Usage: periodic-http-bridge [OPTIONS]"
  putStrLn ""
  putStrLn "Global Options:"
  putStrLn "  -H, --host <HOST>         Periodic server socket path or address"
  putStrLn "                            (Default: unix:///tmp/periodic.sock, Env: $PERIODIC_PORT)"
  putStrLn "      --pool-size <INT>     Periodic server connection pool size (Default: 10)"
  putStrLn ""
  putStrLn "HTTP Server Options:"
  putStrLn "      --http-host <HOST>    HTTP server bind address (Default: 127.0.0.1)"
  putStrLn "      --http-port <PORT>    HTTP server listen port (Default: 8080)"
  putStrLn ""
  putStrLn "Security Options:"
  putStrLn "      --rsa-mode <MODE>     RSA mode: Plain, RSA, or AES (Default: AES)"
  putStrLn "                            Env: $PERIODIC_RSA_MODE"
  putStrLn "      --rsa-public-path <P> RSA public key file or directory [$PERIODIC_RSA_PUBLIC_PATH]"
  putStrLn "      --rsa-private-path <P>RSA private key file path [$PERIODIC_RSA_PRIVATE_PATH]"
  putStrLn "      --client-name <NAME>  Auth client name [$PERIODIC_CLIENT_NAME]"
  putStrLn "      --client-token <TOK>  Auth client token [$PERIODIC_CLIENT_TOKEN]"
  putStrLn ""
  putStrLn "Help:"
  putStrLn "  -h, --help                Display this help message"
  putStrLn ""
  putStrLn "Endpoints:"
  putStrLn "  GET  /periodic/status/                        Get system statistics"
  putStrLn "  POST /periodic/submit/:func_name/:job_name/   Submit a job (Async)"
  putStrLn "  POST /periodic/run/:func_name/:job_name/      Execute a job (Sync)"
  putStrLn "  POST /periodic/remove/:func_name/:job_name/   Remove a specific job"
  putStrLn "  POST /periodic/drop/:func_name/               Drop a function"
  putStrLn ""
  putStrLn $ "Version: v" ++ showVersion version
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupNonEmptyEnv "PERIODIC_PORT"
  rsaPrivate <- lookupNonEmptyEnv "PERIODIC_RSA_PRIVATE_PATH"
  rsaPublic <- lookupNonEmptyEnv "PERIODIC_RSA_PUBLIC_PATH"
  rsaModeEnv <- lookupReadEnv "PERIODIC_RSA_MODE" >>= either (\err -> putStrLn err >> exitFailure) (pure . fromMaybe RSA.AES)
  clientNameEnv <- lookupNonEmptyEnv "PERIODIC_CLIENT_NAME"
  clientTokenEnv <- lookupNonEmptyEnv "PERIODIC_CLIENT_TOKEN"

  opts@Options{..} <- flip parseOptions (options h rsaPrivate rsaPublic rsaModeEnv clientNameEnv clientTokenEnv) <$> getArgs

  when showHelp printHelp
  case parseError of
    Just err -> putStrLn err >> putStrLn "Use --help to see supported options." >> exitFailure
    Nothing  -> pure ()
  case invalidOption of
    Just opt -> putStrLn ("Unknown option: " ++ opt) >> putStrLn "Use --help to see supported options." >> exitFailure
    Nothing  -> pure ()

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Error: Invalid host address " ++ host
    printHelp

  let sopts = defaultOptions
        { settings = setPort httpPort
                   $ setHost (Host httpHost) (settings defaultOptions)}

  auth <- requireAuthPair opts
  case rsaPrivatePath of
    "" -> do
      clientEnv <- maybe (openPool (socket host) poolSize) (\ident -> openPoolWithAuth (socket host) ident poolSize) auth
      scottyOpts sopts $ application clientEnv
    _ -> do
      genTP <- RSA.configClient rsaMode rsaPrivatePath rsaPublicPath
      clientEnv <- maybe (openPool (genTP $ socket host) poolSize) (\ident -> openPoolWithAuth (genTP $ socket host) ident poolSize) auth
      scottyOpts sopts $ application clientEnv

requireAuthPair :: Options -> IO (Maybe ClientIdentity)
requireAuthPair Options {clientName = Nothing, clientToken = Nothing} = pure Nothing
requireAuthPair Options {clientName = Just n, clientToken = Just t} =
  pure $ Just $ ClientIdentity (B.pack n) (B.pack t)
requireAuthPair _ = do
  putStrLn "Error: --client-name and --client-token must be provided together"
  exitFailure


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
      raw "Error: Job execution failed or timed out"
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
