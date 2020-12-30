{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where


import           Control.Monad                  (when)
import           Data.Int                       (Int64)
import           Data.Maybe                     (fromMaybe)
import           Metro.SocketServer             (socketServer)
import           Metro.TP.TLS                   (makeServerParams', tlsConfig)
import           Metro.TP.WebSockets            (serverConfig)
import           Metro.TP.XOR                   (xorConfig)
import           Metro.Utils                    (setupLog)
import           Periodic.Server                (startServer)
import           Periodic.Server.Persist        (Persist, PersistConfig)
import           Periodic.Server.Persist.Cache  (useCache)
import           Periodic.Server.Persist.Memory (useMemory)
import           Periodic.Server.Persist.PSQL   (usePSQL)
import           Periodic.Server.Persist.SQLite (useSQLite)
import           System.Environment             (getArgs, lookupEnv)
import           System.Exit                    (exitSuccess)
import           System.Log                     (Priority (..))

data Options = Options
    { host         :: String
    , xorFile      :: FilePath
    , storePath    :: FilePath
    , useTls       :: Bool
    , useWs        :: Bool
    , certKey      :: FilePath
    , cert         :: FilePath
    , caStore      :: FilePath
    , showHelp     :: Bool
    , logLevel     :: Priority
    , maxCacheSize :: Int64
    }

options :: Maybe String -> Maybe String -> Maybe String -> Options
options h f p = Options
  { host         = fromMaybe "unix:///tmp/periodic.sock" h
  , xorFile      = fromMaybe "" f
  , storePath    = fromMaybe ":memory:" p
  , useTls       = False
  , useWs        = False
  , certKey      = "server-key.pem"
  , cert         = "server.pem"
  , caStore      = "ca.pem"
  , showHelp     = False
  , logLevel     = ERROR
  , maxCacheSize = 1000
  }

parseOptions :: [String] -> Options -> Options
parseOptions []                  opt       = opt
parseOptions ("-H":x:xs)         opt       = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs)     opt       = parseOptions xs opt { host      = x }
parseOptions ("--xor":x:xs)      opt       = parseOptions xs opt { xorFile   = x }
parseOptions ("-p":x:xs)         opt       = parseOptions xs opt { storePath = x }
parseOptions ("--path":x:xs)     opt       = parseOptions xs opt { storePath = x }
parseOptions ("-h":xs)           opt       = parseOptions xs opt { showHelp  = True }
parseOptions ("--help":xs)       opt       = parseOptions xs opt { showHelp  = True }
parseOptions ("--tls":xs)        opt       = parseOptions xs opt { useTls = True }
parseOptions ("--ws":xs)         opt       = parseOptions xs opt { useWs = True }
parseOptions ("--cert-key":x:xs) opt       = parseOptions xs opt { certKey = x }
parseOptions ("--cert":x:xs)     opt       = parseOptions xs opt { cert = x }
parseOptions ("--ca":x:xs)       opt       = parseOptions xs opt { caStore = x }
parseOptions ("--log":x:xs)      opt       = parseOptions xs opt { logLevel = read x }
parseOptions ("--max-cache-size":x:xs) opt = parseOptions xs opt { maxCacheSize = read x }
parseOptions (_:xs)              opt       = parseOptions xs opt

printHelp :: IO ()
printHelp = do
  putStrLn "periodicd - Periodic task system server"
  putStrLn ""
  putStrLn "Usage: periodicd [--host|-H HOST] [--path|-p PATH] [--xor FILE|--ws|--tls [--hostname HOSTNAME] [--cert-key FILE] [--cert FILE] [--ca FILE]] [--max-cache-size SIZE]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host           Socket path [$PERIODIC_PORT]"
  putStrLn "                      eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "  -p --path           State store path [$PERIODIC_PATH] (optional: :memory:)"
  putStrLn "                      eg: file://data.sqlite"
  putStrLn "                      eg: postgres://host='127.0.0.1' port=5432 dbname='periodicd' user='postgres' password=''"
  putStrLn "                      eg: cache+file://data.sqlite"
  putStrLn "                      eg: cache+postgres://host='127.0.0.1' port=5432 dbname='periodicd' user='postgres' password=''"
  putStrLn "     --max-cache-size Max cache size only effect use cache (optional: 1000)"
  putStrLn "     --xor            XOR Transport encode file [$XOR_FILE]"
  putStrLn "     --tls            Use tls transport"
  putStrLn "     --ws             Use websockets transport"
  putStrLn "     --cert-key       Private key associated"
  putStrLn "     --cert           Public certificate (X.509 format)"
  putStrLn "     --ca             Server will use these certificates to validate clients"
  putStrLn "     --log            Set log level DEBUG INFO NOTICE WARNING ERROR CRITICAL ALERT EMERGENCY (optional: ERROR)"
  putStrLn "  -h --help           Display help message"
  putStrLn ""
  putStrLn "Version: v1.1.7.2"
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"
  p <- lookupEnv "PERIODIC_PATH"
  f <- lookupEnv "XOR_FILE"

  opts@Options {..} <- flip parseOptions (options h f p) <$> getArgs

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
run Options {useTls = True, ..} config = do
    prms <- makeServerParams' cert [] certKey caStore
    startServer config (tlsConfig prms) (socketServer host)

run Options {useWs = True, host} config =
    startServer config serverConfig (socketServer host)

run Options {xorFile = "", host} config =
    startServer config id (socketServer host)

run Options {xorFile = f, host} config =
    startServer config (xorConfig f) (socketServer host)
