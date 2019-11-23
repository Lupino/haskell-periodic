{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  (
    main
  ) where

import           Control.Monad                  (when)
import           Data.List                      (isPrefixOf)
import           Data.Maybe                     (fromMaybe)
import           Data.String                    (fromString)
import           Periodic.Server                (startServer)
import           Periodic.Server.Persist        (Persist, PersistConfig)
import           Periodic.Server.Persist.SQLite (SQLite)
import           Periodic.Socket                (Socket, listen)
import           Periodic.Transport.Socket      (rawSocket)
import           Periodic.Transport.TLS         (makeServerParams', tlsConfig)
import           Periodic.Transport.WebSockets  (serverConfig)
import           Periodic.Transport.XOR         (xorConfig)
import           System.Environment             (getArgs, lookupEnv)
import           System.Exit                    (exitSuccess)

data Options = Options { host      :: String
                       , xorFile   :: FilePath
                       , storePath :: FilePath
                       , useTls    :: Bool
                       , useWs     :: Bool
                       , certKey   :: FilePath
                       , cert      :: FilePath
                       , caStore   :: FilePath
                       , showHelp  :: Bool
                       }

options :: Maybe String -> Maybe String -> Options
options h f = Options { host    = fromMaybe "unix:///tmp/periodic.sock" h
                      , xorFile = fromMaybe "" f
                      , storePath = "data.sqlite"
                      , useTls = False
                      , useWs = False
                      , certKey = "server-key.pem"
                      , cert = "server.pem"
                      , caStore = "ca.pem"
                      , showHelp  = False
                      }

parseOptions :: [String] -> Options -> Options
parseOptions []                  opt = opt
parseOptions ("-H":x:xs)         opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs)     opt = parseOptions xs opt { host      = x }
parseOptions ("--xor":x:xs)      opt = parseOptions xs opt { xorFile   = x }
parseOptions ("-p":x:xs)         opt = parseOptions xs opt { storePath = x }
parseOptions ("--path":x:xs)     opt = parseOptions xs opt { storePath = x }
parseOptions ("-h":xs)           opt = parseOptions xs opt { showHelp  = True }
parseOptions ("--help":xs)       opt = parseOptions xs opt { showHelp  = True }
parseOptions ("--tls":xs)        opt = parseOptions xs opt { useTls = True }
parseOptions ("--ws":xs) opt = parseOptions xs opt { useWs = True }
parseOptions ("--cert-key":x:xs) opt = parseOptions xs opt { certKey = x }
parseOptions ("--cert":x:xs)     opt = parseOptions xs opt { cert = x }
parseOptions ("--ca":x:xs)       opt = parseOptions xs opt { caStore = x }
parseOptions (_:xs)              opt = parseOptions xs opt

printHelp :: IO ()
printHelp = do
  putStrLn "periodicd - Periodic task system server"
  putStrLn ""
  putStrLn "Usage: periodicd [--host|-H HOST] [--path|-p PATH] [--xor FILE|--ws|--tls [--hostname HOSTNAME] [--cert-key FILE] [--cert FILE] [--ca FILE]]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host     Socket path [$PERIODIC_PORT]"
  putStrLn "                eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "  -p --path     State store path (optional: data.sqlite)"
  putStrLn "     --xor      XOR Transport encode file [$XOR_FILE]"
  putStrLn "     --tls      Use tls transport"
  putStrLn "     --ws       Use websockets transport"
  putStrLn "     --cert-key Private key associated"
  putStrLn "     --cert     Public certificate (X.509 format)"
  putStrLn "     --ca       Server will use these certificates to validate clients"
  putStrLn "  -h --help     Display help message"
  putStrLn ""
  putStrLn "Version: v1.1.5.5"
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"
  f <- lookupEnv "XOR_FILE"

  opts@Options {..} <- flip parseOptions (options h f) <$> getArgs

  when showHelp printHelp

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  let sqlite = fromString storePath :: PersistConfig SQLite

  run opts sqlite =<< listen host

run :: Persist db => Options -> PersistConfig db -> Socket -> IO ()
run Options {useTls = True, ..} config sock = do
    prms <- makeServerParams' cert [] certKey caStore
    startServer config sock $ tlsConfig prms . rawSocket

run Options {useWs = True} config sock =
    startServer config sock $ serverConfig . rawSocket

run Options {xorFile = ""} config sock =
    startServer config sock rawSocket

run Options {xorFile = f} config sock =
    startServer config sock $ xorConfig f . rawSocket
