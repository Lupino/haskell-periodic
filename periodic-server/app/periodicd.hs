{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  (
    main
  ) where

import           Control.Monad          (when)
import qualified Data.ByteString        as B (readFile)
import           Periodic.Server
import           Periodic.Socket        (HostName, ServiceName, listenOn,
                                         listenOnFile)
import           Periodic.Transport     (makeSocketTransport)
import           Periodic.Transport.XOR (makeXORTransport)
import           System.Directory       (removeFile)
import           System.Environment     (getArgs)

data Options = Options { host      :: Maybe HostName
                       , service   :: ServiceName
                       , sockFile  :: FilePath
                       , useSock   :: Bool
                       , xorFile   :: FilePath
                       , storePath :: FilePath
                       , showHelp  :: Bool
                       }

defaultOptions :: Options
defaultOptions = Options { host      = Nothing
                         , service   = "5000"
                         , sockFile  = "/tmp/periodicd.sock"
                         , useSock   = True
                         , xorFile   = ""
                         , storePath = "state"
                         , showHelp  = False
                         }

parseOptions :: [String] -> Options
parseOptions []              = defaultOptions
parseOptions ("-P":x:xs)     = (parseOptions xs) { service   = x,      useSock = False }
parseOptions ("--port":x:xs) = (parseOptions xs) { service   = x,      useSock = False }
parseOptions ("-H":x:xs)     = (parseOptions xs) { host      = Just x, useSock = False }
parseOptions ("--host":x:xs) = (parseOptions xs) { host      = Just x, useSock = False }
parseOptions ("--sock":x:xs) = (parseOptions xs) { sockFile  = x }
parseOptions ("--xor":x:xs)  = (parseOptions xs) { xorFile   = x }
parseOptions ("-p":x:xs)     = (parseOptions xs) { storePath = x }
parseOptions ("--path":x:xs) = (parseOptions xs) { storePath = x }
parseOptions ("-h":xs)       = (parseOptions xs) { showHelp  = True }
parseOptions ("--help":xs)   = (parseOptions xs) { showHelp  = True }
parseOptions (_:xs)          = parseOptions xs

printHelp :: IO ()
printHelp = do
  putStrLn "periodicd [--port|-P 5000] [--host|-H 0.0.0.0] [--path|-p state] [--xor FILE]"
  putStrLn "periodicd [--sock /tmp/periodicd.sock] [--path|-p state] [--xor FILE]"
  putStrLn "periodicd --help"
  putStrLn "periodicd -h"

main :: IO ()
main = do
  (Options {..}) <- parseOptions <$> getArgs

  if showHelp then printHelp
              else startServer (makeTransport xorFile) storePath
                     =<< if useSock then listenOnFile sockFile
                                    else listenOn host service

  when (useSock && not showHelp) $ removeFile sockFile

 where makeTransport [] sock = makeSocketTransport sock
       makeTransport p sock  = do
         key <- B.readFile p
         transport <- makeSocketTransport sock
         makeXORTransport key transport
