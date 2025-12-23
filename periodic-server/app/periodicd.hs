{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where


import           Control.Monad                  (when)
import           Data.Int                       (Int64)
import           Data.Maybe                     (fromMaybe)
import           Data.Version                   (showVersion)
import           Metro.SocketServer             (socketServer)
import           Metro.TP.RSA                   (rsa)
import           Metro.Utils                    (setupLog)
import           Paths_periodic_server          (version)
import           Periodic.Server                (startServer)
import           Periodic.Server.Hook           (genHook)
import           Periodic.Server.Persist        (Persist, PersistConfig)
import           Periodic.Server.Persist.Cache  (useCache)
import           Periodic.Server.Persist.Memory (useMemory)
import           Periodic.Server.Persist.PSQL   (usePSQL)
import           Periodic.Server.Persist.SQLite (useSQLite)
import           System.Environment             (getArgs, lookupEnv)
import           System.Exit                    (exitSuccess)
import           System.Log                     (Priority (..))

data Options = Options
  { host           :: String
  , storePath      :: FilePath
  , showHelp       :: Bool
  , logLevel       :: Priority
  , maxCacheSize   :: Int64
  , hookHostPort   :: String
  , pushTaskSize   :: Int
  , schedTaskSize  :: Int
  , rsaPrivatePath :: FilePath
  , rsaPublicPath  :: FilePath
  }

options :: Maybe String -> Maybe String -> Options
options h p = Options
  { host           = fromMaybe "unix:///tmp/periodic.sock" h
  , storePath      = fromMaybe ":memory:" p
  , showHelp       = False
  , logLevel       = ERROR
  , maxCacheSize   = 1000
  , hookHostPort   = ""
  , pushTaskSize   = 4
  , schedTaskSize  = 2
  , rsaPublicPath  = "public_keys.pem"
  , rsaPrivatePath = ""
  }

parseOptions :: [String] -> Options -> Options
parseOptions []                          opt = opt
parseOptions ("-H":x:xs)                 opt = parseOptions xs opt { host          = x }
parseOptions ("--host":x:xs)             opt = parseOptions xs opt { host          = x }
parseOptions ("-p":x:xs)                 opt = parseOptions xs opt { storePath     = x }
parseOptions ("--path":x:xs)             opt = parseOptions xs opt { storePath     = x }
parseOptions ("-h":xs)                   opt = parseOptions xs opt { showHelp      = True }
parseOptions ("--help":xs)               opt = parseOptions xs opt { showHelp      = True }
parseOptions ("--hook":x:xs)             opt = parseOptions xs opt { hookHostPort  = x }
parseOptions ("--log":x:xs)              opt = parseOptions xs opt { logLevel      = read x }
parseOptions ("--max-cache-size":x:xs)   opt = parseOptions xs opt { maxCacheSize  = read x }
parseOptions ("--push-task-size":x:xs)   opt = parseOptions xs opt { pushTaskSize  = read x }
parseOptions ("--sched-task-size":x:xs)  opt = parseOptions xs opt { schedTaskSize = read x }
parseOptions ("--rsa-private-path":x:xs) opt = parseOptions xs opt { rsaPrivatePath = x }
parseOptions ("--rsa-public-path":x:xs)  opt = parseOptions xs opt { rsaPublicPath = x }
parseOptions (_:xs)                      opt = parseOptions xs opt

printHelp :: IO ()
printHelp = do
  putStrLn "periodicd - Periodic task system server"
  putStrLn ""
  putStrLn "Usage: periodicd [--host|-H HOST] [--path|-p PATH] [--max-cache-size SIZE] [--push-task-size SIZE] [--sched-task-size SIZE] [--rsa-private-path FILE --rsa-public-path FILE|PATH]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host             Socket path [$PERIODIC_PORT]"
  putStrLn "                        eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "  -p --path             State store path [$PERIODIC_PATH] (optional: :memory:)"
  putStrLn "                        eg: file://data.sqlite"
  putStrLn "                        eg: postgres://host='127.0.0.1' port=5432 dbname='periodicd' user='postgres' password=''"
  putStrLn "                        eg: cache+file://data.sqlite"
  putStrLn "                        eg: cache+postgres://host='127.0.0.1' port=5432 dbname='periodicd' user='postgres' password=''"
  putStrLn "     --hook             Event hook name or socket uri (optional: null)"
  putStrLn "                        eg: persist save to postgresql or sqlite table metrics"
  putStrLn "                        eg: udp://127.0.0.1:1000"
  putStrLn "                        eg: tcp://127.0.0.1:1000"
  putStrLn "     --max-cache-size   Max cache size only effect use cache (optional: 1000)"
  putStrLn "     --push-task-size   Push Job queue size (optional: 4)"
  putStrLn "     --sched-task-size  Sched job queue size (optional: 2)"
  putStrLn "     --rsa-private-path RSA private key file path (optional: null)"
  putStrLn "     --rsa-public-path  RSA public keys file path or dir (optional: public_keys.pem)"
  putStrLn "     --log              Set log level DEBUG INFO NOTICE WARNING ERROR CRITICAL ALERT EMERGENCY (optional: ERROR)"
  putStrLn "  -h --help             Display help message"
  putStrLn ""
  putStrLn $ "Version: v" ++ showVersion version
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"
  p <- lookupEnv "PERIODIC_PATH"

  opts@Options {..} <- flip parseOptions (options h p) <$> getArgs

  when showHelp printHelp

  setupLog logLevel

  if take 11 storePath == "postgres://" then
    run opts (usePSQL $ drop 11 storePath)
  else if take 7 storePath == "file://" then
    run opts (useSQLite $ drop 7 storePath)
  else if storePath == ":memory:" then
    run opts useMemory
  else if take 13 storePath == "cache+file://" then
    run opts (useCache maxCacheSize $ useSQLite $ drop 13 storePath)
  else if take 17 storePath == "cache+postgres://" then
    run opts (useCache maxCacheSize $ usePSQL $ drop 17 storePath)
  else
    run opts (useSQLite storePath)

run :: Persist db => Options -> PersistConfig db -> IO ()
run Options {host, pushTaskSize, schedTaskSize, hookHostPort, rsaPrivatePath = ""} config = do
  hook <- genHook hookHostPort
  startServer config id (socketServer host) hook pushTaskSize schedTaskSize
run Options {host, pushTaskSize, schedTaskSize, hookHostPort, rsaPrivatePath, rsaPublicPath} config = do
  hook <- genHook hookHostPort
  mGenTP <- rsa rsaPrivatePath rsaPublicPath False
  case mGenTP of
    Left err -> putStrLn $ "Error " ++ err
    Right genTP ->
      startServer config genTP (socketServer host) hook pushTaskSize schedTaskSize
