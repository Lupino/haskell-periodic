{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  (
    main
  ) where

import           Control.Monad          (when)
import qualified Data.ByteString.Lazy   as LB (readFile)
import           Data.List              (isPrefixOf)
import           Data.Maybe             (fromMaybe)
import           Periodic.Server
import           Periodic.Socket        (listen)
import           Periodic.Transport     (makeSocketTransport)
import           Periodic.Transport.TLS
import           Periodic.Transport.XOR (makeXORTransport)
import           System.Environment     (getArgs, lookupEnv)
import           System.Exit            (exitSuccess)

data Options = Options { host      :: String
                       , xorFile   :: FilePath
                       , storePath :: FilePath
                       , useTls    :: Bool
                       , certKey   :: FilePath
                       , cert      :: FilePath
                       , caStore   :: FilePath
                       , showHelp  :: Bool
                       }

options :: Maybe String -> Maybe String -> Options
options h f = Options { host    = fromMaybe "unix:///tmp/periodic.sock" h
                      , xorFile = fromMaybe "" f
                      , storePath = "data"
                      , useTls = False
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
parseOptions ("--cert-key":x:xs) opt = parseOptions xs opt { certKey = x }
parseOptions ("--cert":x:xs)     opt = parseOptions xs opt { cert = x }
parseOptions ("--ca":x:xs)       opt = parseOptions xs opt { caStore = x }
parseOptions (_:xs)              opt = parseOptions xs opt

printHelp :: IO ()
printHelp = do
  putStrLn "periodicd - Periodic task system server"
  putStrLn ""
  putStrLn "Usage: periodicd [--host|-H HOST] [--path|-p PATH] [--xor FILE|--tls [--hostname HOSTNAME] [--cert-key FILE] [--cert FILE] [--ca FILE]]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host     Socket path [$PERIODIC_PORT]"
  putStrLn "                eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "  -p --path     State store path (optional: data)"
  putStrLn "     --xor      XOR Transport encode file [$XOR_FILE]"
  putStrLn "     --tls      Use tls transport"
  putStrLn "     --cert-key Private key associated"
  putStrLn "     --cert     Public certificate (X.509 format)"
  putStrLn "     --ca       Server will use these certificates to validate clients"
  putStrLn "  -h --help     Display help message"
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

  startServer (makeTransport opts) storePath =<< listen host

makeTransport Options{..} sock =
  if useTls then do
    prms <- makeServerParams' cert [] certKey caStore
    makeTLSTransport prms =<< makeSocketTransport sock
  else makeTransport' xorFile sock

makeTransport' [] sock = makeSocketTransport sock
makeTransport' p sock  = do
  key <- LB.readFile p
  transport <- makeSocketTransport sock
  makeXORTransport key transport
