{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  (
    main
  ) where

import           Control.Monad          (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B (pack, unpack)
import qualified Data.ByteString.Lazy   as LB (readFile)
import           Data.List              (isPrefixOf, transpose)
import           Data.Maybe             (fromMaybe)
import           Periodic.Client
import           Periodic.Transport     (Transport)
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
             | Shutdown
             | Help

  deriving (Eq)

parseCommand :: String -> Command
parseCommand "status"   = Status
parseCommand "dump"     = Dump
parseCommand "load"     = Load
parseCommand "submit"   = Submit
parseCommand "remove"   = Remove
parseCommand "drop"     = Drop
parseCommand "shutdown" = Shutdown
parseCommand _          = Help

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
  putStrLn "     shutdown Shutdown periodicd"
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

  runClient (makeTransport xorFile) host $ processCommand cmd argv

makeTransport :: FilePath -> Transport -> IO Transport
makeTransport [] transport = return transport
makeTransport p transport  = do
  key <- LB.readFile p
  makeXORTransport key transport

processCommand :: Command -> [String] -> Client ()
processCommand Help _     = liftIO $ printHelp
processCommand Status _   = doStatus
processCommand Load   _   = doLoad
processCommand Dump   _   = doDump
processCommand Submit xs  = doSubmitJob xs
processCommand Remove xs  = doRemoveJob xs
processCommand Drop xs    = doDropFunc xs
processCommand Shutdown _ = shutdown

doRemoveJob (x:xs) = mapM_ (void . removeJob (B.pack x) . B.pack) xs
doRemoveJob []     = liftIO $ printRemoveHelp

doDropFunc = mapM_ (void . dropFunc . B.pack)

doSubmitJob []       = liftIO $ printSubmitHelp
doSubmitJob (_:[])   = liftIO $ printSubmitHelp
doSubmitJob (x:y:xs) = void $ submitJob (B.pack x) (B.pack y) later
  where later = case xs of
                  []              -> 0
                  ("--later":l:_) -> fromMaybe 0 (readMaybe l)
                  _               -> 0

doDump = liftIO (openFile "dump.db" WriteMode) >>= dump

doLoad = liftIO (openFile "dump.db" ReadMode) >>= load

doStatus = do
  st <- map formatTime . unpackBS <$> status
  liftIO $ print_table (["FUNCTIONS", "WORKERS", "JOBS", "PROCESSING", "SCHEDAT"]:st)

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
