{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  (
    main
  ) where

import           Control.Monad          (void, when)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B (pack, unpack)
import qualified Data.ByteString.Lazy   as LB (readFile)
import           Data.List              (isPrefixOf, transpose)
import           Data.Maybe             (fromMaybe)
import           Periodic.Client
import           Periodic.Socket        (Socket, connectTo, connectToFile)
import           Periodic.Transport     (Transport, makeSocketTransport)
import           Periodic.Transport.XOR (makeXORTransport)
import           System.Environment     (getArgs, lookupEnv)
import           System.Exit            (exitSuccess)
import           System.IO              (IOMode (ReadMode, WriteMode), openFile)
import           Text.Read              (readMaybe)

import           Data.UnixTime
import           System.IO.Unsafe       (unsafePerformIO)
import qualified Text.PrettyPrint.Boxes as T


data Command = Status
             | Dump
             | Load
             | Submit
             | Remove
             | Drop
             | Help

  deriving (Eq)

parseCommand :: String -> Command
parseCommand "status" = Status
parseCommand "dump"   = Dump
parseCommand "load"   = Load
parseCommand "submit" = Submit
parseCommand "remove" = Remove
parseCommand "drop"   = Drop
parseCommand _        = Help

data Options = Options { host    :: String
                       , xorFile :: FilePath
                       }

options :: Maybe String -> Maybe String -> Options
options h f = Options { host    = fromMaybe "unix:///tmp/periodic.sock" h
                      , xorFile = fromMaybe "" f
                      }

parseOptions :: [String] -> Options -> (Command, Options, [String])
parseOptions []              opt = (Help, opt, [])
parseOptions ("-H":x:xs)     opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs) opt = parseOptions xs opt { host      = x }
parseOptions ("--xor":x:xs)  opt = parseOptions xs opt { xorFile   = x }
parseOptions (x:xs)          opt = (parseCommand x, opt, xs)

printHelp :: IO ()
printHelp = do
  putStrLn "periodic - Periodic task system client"
  putStrLn ""
  putStrLn "Usage: periodic [--host|-H HOST] [--xor FILE] command [options]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "     status   Show status"
  putStrLn "     submit   Submit job"
  putStrLn "     remove   Remove job"
  putStrLn "     drop     Drop function"
  putStrLn "     dump     Dump database to file"
  putStrLn "     load     Load file to database"
  putStrLn "     help     Shows a list of commands or help for one command"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host Socket path [$PERIODIC_PORT]"
  putStrLn "            eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "     --xor  XOR Transport encode file [$XOR_FILE]"
  putStrLn ""
  exitSuccess

printSubmitHelp :: IO ()
printSubmitHelp = do
  putStrLn "periodic submit - Submit job"
  putStrLn ""
  putStrLn "Usage: periodic submit funcname jobname [-w|--workload WORKLOAD] [--later 0]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -w --workload WORKLOAD"
  putStrLn "     --later    Sched job later"
  putStrLn ""
  exitSuccess

printRemoveHelp :: IO ()
printRemoveHelp = do
  putStrLn "periodic remove - Remove job"
  putStrLn ""
  putStrLn "Usage: periodic remove funcname jobname [jobname...]"
  putStrLn ""
  exitSuccess

printDropHelp :: IO ()
printDropHelp = do
  putStrLn "periodic drop - Drop function"
  putStrLn ""
  putStrLn "Usage: periodic drop funcname [funcname...]"
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"
  f <- lookupEnv "XOR_FILE"

  (cmd, Options {..}, argv) <- flip parseOptions (options h f) <$> getArgs

  let argc = length argv

  when (cmd == Help) $ printHelp

  when (cmd == Submit && argc < 2) $ printSubmitHelp
  when (cmd == Remove && argc < 2) $ printRemoveHelp
  when (cmd == Drop   && argc < 1) $ printDropHelp

  when (not (isPrefixOf "tcp" host) && not (isPrefixOf "unix" host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  client <- newClient =<< makeTransport xorFile =<< connectSock host

  processCommand client cmd argv

makeTransport :: FilePath -> Socket -> IO Transport
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

connectSock :: String -> IO Socket
connectSock h | isPrefixOf "tcp" h = connectTo (getHost h) (getService h)
              | otherwise          = connectToFile (dropS h)

processCommand :: Client -> Command -> [String] -> IO ()
processCommand _ Help _    = printHelp
processCommand c Status _  = doStatus c
processCommand c Load   _  = doLoad c
processCommand c Dump   _  = doDump c
processCommand c Submit xs = doSubmitJob c xs
processCommand c Remove xs = doRemoveJob c xs
processCommand c Drop xs   = doDropFunc c xs

doRemoveJob c (x:xs) = mapM_ (void . removeJob c (B.pack x) . B.pack) xs
doRemoveJob _ []     = printRemoveHelp

doDropFunc c = mapM_ (void . dropFunc c . B.pack)

doSubmitJob _ []       = printSubmitHelp
doSubmitJob _ (_:[])      = printSubmitHelp
doSubmitJob c (x:y:xs) = void $ submitJob c (B.pack x) (B.pack y) later
  where later = case xs of
                  []              -> 0
                  ("--later":l:_) -> fromMaybe 0 (readMaybe l)
                  _               -> 0

doDump c = openFile "dump.db" WriteMode >>= dump c

doLoad c = openFile "dump.db" ReadMode >>= load c

doStatus c = do
  st <- map formatTime . unpackBS <$> status c
  print_table (["FUNCTIONS", "WORKERS", "JOBS", "PROCESSING", "SCHEDAT"]:st)

unpackBS :: [[ByteString]] -> [[String]]
unpackBS = map (map B.unpack)

formatTime :: [String] -> [String]
formatTime []     = []
formatTime (x:[]) = [formatUnixTimeLocal x]
formatTime (x:xs) = (x:formatTime xs)

formatUnixTimeLocal :: String -> String
formatUnixTimeLocal = B.unpack
                    . unsafePerformIO
                    . formatUnixTime "%Y-%m-%d %H:%M:%S"
                    . fromEpochTime
                    . fromIntegral
                    . read

print_table :: [[String]] -> IO ()
print_table rows = T.printBox $ T.hsep 2 T.left (map (T.vcat T.left . map T.text) (transpose rows))
