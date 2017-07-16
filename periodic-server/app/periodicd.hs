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

options :: Maybe String -> Maybe String -> Options
options h f = Options { host    = fromMaybe "unix:///tmp/periodic.sock" h
                      , xorFile = fromMaybe "" f
                      , storePath = "data"
                      , showHelp  = False
                      }

parseOptions :: [String] -> Options -> Options
parseOptions []              opt = opt
parseOptions ("-H":x:xs)     opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs) opt = parseOptions xs opt { host      = x }
parseOptions ("--xor":x:xs)  opt = parseOptions xs opt { xorFile   = x }
parseOptions ("-p":x:xs)     opt = parseOptions xs opt { storePath = x }
parseOptions ("--path":x:xs) opt = parseOptions xs opt { storePath = x }
parseOptions ("-h":xs)       opt = parseOptions xs opt { showHelp  = True }
parseOptions ("--help":xs)   opt = parseOptions xs opt { showHelp  = True }
parseOptions (_:xs)          opt = parseOptions xs opt

printHelp :: IO ()
printHelp = do
  putStrLn "periodicd - Periodic task system server"
  putStrLn ""
  putStrLn "Usage: periodicd [--host|-H HOST] [--path|-p PATH] [--xor FILE]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host Socket path [$PERIODIC_PORT]"
  putStrLn "            eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "  -p --path State store path (optional: data)"
  putStrLn "     --xor  XOR Transport encode file [$XOR_FILE]"
  putStrLn "  -h --help Display help message"
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"
  f <- lookupEnv "XOR_FILE"

  (Options {..}) <- flip parseOptions (options h f) <$> getArgs

  when showHelp $ printHelp

  when (not (isPrefixOf "tcp" host) && not (isPrefixOf "unix" host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  sock <- if isPrefixOf "tcp" host then listenOn (getHost host) (getService host)
                                   else listenOnFile' (dropS host)


  startServer (makeTransport xorFile) storePath sock

makeTransport [] sock = makeSocketTransport sock
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
