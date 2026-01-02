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
import qualified Metro.TP.RSA                   as RSA (configServer)
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
  putStrLn "periodicd - Central server for the Periodic task system"
  putStrLn ""
  putStrLn "Usage: periodicd [OPTIONS]"
  putStrLn ""
  putStrLn "Network Options:"
  putStrLn "  -H, --host <URI>      Socket path or address [$PERIODIC_PORT]"
  putStrLn "                        (Default: unix:///tmp/periodic.sock, e.g., tcp://:5000)"
  putStrLn ""
  putStrLn "Persistence Options:"
  putStrLn "  -p, --path <URI>      Backend storage URI [$PERIODIC_PATH]"
  putStrLn "                        - :memory: (In-memory storage)"
  putStrLn "                        - file://data.sqlite (SQLite)"
  putStrLn "                        - postgres://user:pass@host:port/db (PostgreSQL)"
  putStrLn "                        - cache+file://... (SQLite with memory cache)"
  putStrLn "                        - cache+postgres://... (PostgreSQL with memory cache)"
  putStrLn ""
  putStrLn "Event Hook & Logging:"
  putStrLn "      --hook <STR|URI>  Metrics and event notification hook (Default: null)"
  putStrLn "                        - persist: Save system events to the backend database"
  putStrLn "                        - udp://127.0.0.1:1000: Stream events via UDP"
  putStrLn "                        - tcp://127.0.0.1:1000: Stream events via TCP"
  putStrLn "      --log <LEVEL>     Log level: DEBUG, INFO, NOTICE, WARNING, ERROR, etc."
  putStrLn ""
  putStrLn "Performance & Tuning:"
  putStrLn "      --max-cache-size <INT>  Max cache items (Only for 'cache+' backends, Default: 1000)"
  putStrLn "      --push-task-size <INT>  Max concurrent job push queue size (Default: 4)"
  putStrLn "      --sched-task-size <INT> Max concurrent job scheduling size (Default: 2)"
  putStrLn ""
  putStrLn "Security Options:"
  putStrLn "      --rsa-private-path <P>  RSA private key file path"
  putStrLn "      --rsa-public-path <P>   RSA public keys file path or directory"
  putStrLn ""
  putStrLn "Help:"
  putStrLn "  -h, --help            Display this help message"
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
  genTP <- RSA.configServer rsaPrivatePath rsaPublicPath
  startServer config genTP (socketServer host) hook pushTaskSize schedTaskSize
