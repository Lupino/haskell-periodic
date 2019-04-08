{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  (
    main
  ) where

import           Control.Monad                  (when)
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.ByteString.Char8          as B (ByteString, pack)
import qualified Data.ByteString.Lazy           as LB (null, toStrict)
import qualified Data.ByteString.Lazy.Char8     as LB (hPut, lines, putStr)
import           Data.List                      (isPrefixOf)
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T (unpack)
import           Data.Text.Encoding             (decodeUtf8With)
import           Data.Text.Encoding.Error       (ignore)
import           Periodic.Job                   (JobT, name, schedLater,
                                                 withLock, workDone, workDone_,
                                                 workFail, workload)
import           Periodic.Socket                (getHost, getService)
import           Periodic.Transport             (Transport)
import           Periodic.Transport.Socket      (socketUri)
import           Periodic.Transport.TLS         (makeClientParams', tlsConfig)
import           Periodic.Transport.WebSockets  (clientConfig)
import           Periodic.Transport.XOR         (xorConfig)
import           Periodic.Types                 (FuncName (..), LockName (..))
import           Periodic.Worker                (WorkerT, addFunc, broadcast,
                                                 runWorkerT, work)
import           System.Environment             (getArgs, lookupEnv)
import           System.Exit                    (ExitCode (..), exitSuccess)
import           System.IO                      (stderr)
import           System.Process.ByteString.Lazy (readProcessWithExitCode)
import           Text.Read                      (readMaybe)


data Options = Options
  { host      :: String
  , xorFile   :: FilePath
  , useTls    :: Bool
  , useWs     :: Bool
  , hostName  :: String
  , certKey   :: FilePath
  , cert      :: FilePath
  , caStore   :: FilePath
  , thread    :: Int
  , lockCount :: Int
  , lockName  :: Maybe LockName
  , notify    :: Bool
  , useData   :: Bool
  , useStdout :: Bool
  , useName   :: Bool
  , showHelp  :: Bool
  }

options :: Maybe Int -> Maybe String -> Maybe String -> Options
options t h f = Options
  { host      = fromMaybe "unix:///tmp/periodic.sock" h
  , xorFile   = fromMaybe "" f
  , useTls    = False
  , useWs     = False
  , hostName  = "localhost"
  , certKey   = "client-key.pem"
  , cert      = "client.pem"
  , caStore   = "ca.pem"
  , thread    = fromMaybe 1 t
  , lockCount = 1
  , lockName  = Nothing
  , notify    = False
  , useData   = False
  , useStdout = True
  , useName   = True
  , showHelp  = False
  }

parseOptions :: [String] -> Options -> (Options, FuncName, String, [String])
parseOptions ("-H":x:xs)           opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs)       opt = parseOptions xs opt { host      = x }
parseOptions ("--xor":x:xs)        opt = parseOptions xs opt { xorFile   = x }
parseOptions ("--tls":xs)          opt = parseOptions xs opt { useTls = True }
parseOptions ("--ws":xs)           opt = parseOptions xs opt { useWs = True }
parseOptions ("--hostname":x:xs)   opt = parseOptions xs opt { hostName = x }
parseOptions ("--cert-key":x:xs)   opt = parseOptions xs opt { certKey = x }
parseOptions ("--cert":x:xs)       opt = parseOptions xs opt { cert = x }
parseOptions ("--ca":x:xs)         opt = parseOptions xs opt { caStore = x }
parseOptions ("--thread":x:xs)     opt = parseOptions xs opt { thread = read x }
parseOptions ("--lock-count":x:xs) opt = parseOptions xs opt { lockCount = read x }
parseOptions ("--lock-name":x:xs)  opt = parseOptions xs opt { lockName = Just (LockName $ B.pack x) }
parseOptions ("--help":xs)         opt = parseOptions xs opt { showHelp = True }
parseOptions ("--broadcast":xs)    opt = parseOptions xs opt { notify = True }
parseOptions ("--data":xs)         opt = parseOptions xs opt { useData = True }
parseOptions ("--no-stdout":xs)    opt = parseOptions xs opt { useStdout = False }
parseOptions ("--no-name":xs)      opt = parseOptions xs opt { useName = False }
parseOptions ("-h":xs)             opt = parseOptions xs opt { showHelp = True }
parseOptions []                    opt = (opt { showHelp = True }, "", "", [])
parseOptions [_]                   opt = (opt { showHelp = True }, "", "", [])
parseOptions (x:y:xs)              opt = (opt, FuncName $ B.pack x, y, xs)

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-run - Periodic task system worker"
  putStrLn ""
  putStrLn "Usage: periodic-run [--host|-H HOST] [--xor FILE|--ws|--tls [--hostname HOSTNAME] [--cert-key FILE] [--cert FILE] [--ca FILE] [--thread THREAD] [--lock-name NAME] [--lock-count COUNT] [--broadcast] [--data] [--no-stdout] [--no-name]] funcname command [options]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host       Socket path [$PERIODIC_PORT]"
  putStrLn "                  Eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "     --xor        XOR Transport encode file [$XOR_FILE]"
  putStrLn "     --tls        Use tls transport"
  putStrLn "     --ws         Use websockets transport"
  putStrLn "     --hostname   Host name"
  putStrLn "     --cert-key   Private key associated"
  putStrLn "     --cert       Public certificate (X.509 format)"
  putStrLn "     --ca         Trusted certificates"
  putStrLn "     --thread     Worker thread [$THREAD]"
  putStrLn "     --lock-count Max lock count (optional: 1)"
  putStrLn "     --lock-name  The lock name (optional: no lock)"
  putStrLn "     --broadcast  Is broadcast worker"
  putStrLn "     --data       Send work data to client"
  putStrLn "     --no-stdout  Hidden the stdout"
  putStrLn "     --no-name    Ignore the job name"
  putStrLn "  -h --help       Display help message"
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"
  f <- lookupEnv "XOR_FILE"
  t <- fmap read <$> lookupEnv "THREAD"

  (opts@Options {..}, func, cmd, argv) <- flip parseOptions (options t h f) <$> getArgs

  when showHelp printHelp

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  run opts func cmd argv

doWork :: Transport tp => Options -> FuncName -> String -> [String] -> WorkerT tp IO ()
doWork opts@Options{..} func cmd argv = do
  let w = processWorker opts cmd argv
  if notify then broadcast func w
  else
    case lockName of
      Nothing -> addFunc func w
      Just n  -> addFunc func $ withLock n lockCount w
  liftIO $ putStrLn "Worker started."
  work thread

run :: Options -> FuncName -> String -> [String] -> IO ()
run opts@Options {useTls = True, ..} func cmd argv = do
  prms <- makeClientParams' cert [] certKey caStore (hostName, B.pack $ getService host)
  runWorkerT (tlsConfig prms (socketUri host)) $ doWork opts func cmd argv

run opts@Options {useWs = True, ..} func cmd argv =
  runWorkerT (clientConfig (socketUri host) (fromMaybe "0.0.0.0" $ getHost host) (getService host)) $ doWork opts func cmd argv

run opts@Options {xorFile = "", ..} func cmd argv =
  runWorkerT (socketUri host) $ doWork opts func cmd argv

run opts@Options {..} func cmd argv =
  runWorkerT (xorConfig xorFile $ socketUri host) $ doWork opts func cmd argv

processWorker :: Transport tp => Options -> String -> [String] -> JobT tp IO ()
processWorker Options{..} cmd argv = do
  n <- name
  rb <- workload
  let argv' = if useName then argv ++ [n] else argv
  (code, out, err) <- liftIO $ readProcessWithExitCode cmd argv' rb
  when useStdout $ liftIO $ LB.putStr out
  liftIO $ LB.hPut stderr err
  case code of
    ExitFailure _ -> workFail
    ExitSuccess | useData -> workDone_ (LB.toStrict out)
                | LB.null err -> workDone
                | otherwise -> do
      let lastLine = last $ LB.lines err
      case (readMaybe . unpackBS . LB.toStrict) lastLine of
        Nothing    -> workDone
        Just later -> schedLater later

unpackBS :: B.ByteString -> String
unpackBS = T.unpack . decodeUtf8With ignore
