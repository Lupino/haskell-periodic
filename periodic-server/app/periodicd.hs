{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  (
    main
  ) where

import           Control.Monad          (when)
import qualified Data.ByteString.Lazy   as LB (readFile)
import           Data.List              (isPrefixOf)
import           Periodic.Server
import           Periodic.Socket        (connectToFile, listenOn, listenOnFile)
import           Periodic.Transport     (makeSocketTransport)
import           Periodic.Transport.XOR (makeXORTransport)
import           Periodic.Utils         (tryIO)
import           System.Directory       (doesFileExist, removeFile)
import           System.Environment     (getArgs, lookupEnv)
import           System.Exit            (exitFailure, exitSuccess)

data Options = Options { host      :: String
                       , xorFile   :: FilePath
                       , storePath :: FilePath
                       , showHelp  :: Bool
                       }

defaultOptions :: Options
defaultOptions = Options { host      = "unix:///tmp/periodic.sock"
                         , xorFile   = ""
                         , storePath = "state"
                         , showHelp  = False
                         }

parseOptions :: Maybe String -> [String] -> Options
parseOptions Nothing []        = defaultOptions
parseOptions (Just v) []       = defaultOptions { host = v }
parseOptions e ("-H":x:xs)     = (parseOptions e xs) { host      = x }
parseOptions e ("--host":x:xs) = (parseOptions e xs) { host      = x }
parseOptions e ("--xor":x:xs)  = (parseOptions e xs) { xorFile   = x }
parseOptions e ("-p":x:xs)     = (parseOptions e xs) { storePath = x }
parseOptions e ("--path":x:xs) = (parseOptions e xs) { storePath = x }
parseOptions e ("-h":xs)       = (parseOptions e xs) { showHelp  = True }
parseOptions e ("--help":xs)   = (parseOptions e xs) { showHelp  = True }
parseOptions e (_:xs)          = parseOptions e xs

printHelp :: IO ()
printHelp = do
  putStrLn "periodicd - Periodic task system server"
  putStrLn ""
  putStrLn "Usage: periodicd [--host|-H HOST] [--path|-p PATH] [--xor FILE]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host Socket path [$PERIODIC_PORT]"
  putStrLn "            eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "  -p --path State store path (optional: state)"
  putStrLn "     --xor  XOR Transport encode file (optional: \"\")"
  putStrLn "  -h --help Display help message"
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  env <- lookupEnv "PERIODIC_PORT"
  (Options {..}) <- parseOptions env <$> getArgs

  when showHelp $ printHelp

  when (not (isPrefixOf "tcp" host) && not (isPrefixOf "unix" host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  sock <- if isPrefixOf "tcp" host then listenOn (getHost host) (getService host)
                                   else listenOnFile' (dropS host)


  startServer (makeTransport xorFile) storePath sock

  where makeTransport [] sock = makeSocketTransport sock
        makeTransport p sock  = do
          key <- LB.readFile p
          transport <- makeSocketTransport sock
          makeXORTransport key transport

        dropS :: String -> String
        dropS = drop 3 . dropWhile (/= ':')

        toMaybe :: String -> Maybe String
        toMaybe [] = Nothing
        toMaybe xs = Just xs

        getHost :: String -> Maybe String
        getHost = toMaybe . takeWhile (/=':') . dropS

        getService :: String -> String
        getService = drop 1 . dropWhile (/=':') . dropS

        listenOnFile' sockFile = do
          exists <- doesFileExist sockFile
          when exists $ do
            e <- tryIO $ connectToFile sockFile
            case e of
              Left _ -> removeFile sockFile
              Right _ -> do
                putStrLn "periodicd: bind: resource busy (Address already in use)"
                exitFailure
          listenOnFile sockFile
