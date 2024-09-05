{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Control.Monad             (void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Binary               (decodeFile, encodeFile)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as B (lines, putStrLn, readFile,
                                                 split, unpack)
import           Data.Int                  (Int64)
import           Data.List                 (isPrefixOf, transpose)
import           Data.Maybe                (fromMaybe)
import           Data.Version              (showVersion)
import           Metro.Class               (Transport)
import           Metro.TP.Socket           (socket)
import           Paths_periodic_client_exe (version)
import           Periodic.Trans.Client
import           Periodic.Types            (Workload (..))
import           System.Environment        (getArgs, lookupEnv)
import           System.Exit               (exitSuccess)
import           Text.Read                 (readMaybe)

import           Data.String               (fromString)
import           Data.UnixTime
import           System.IO.Unsafe          (unsafePerformIO)
import qualified Text.PrettyPrint.Boxes    as T
import           UnliftIO                  (async, cancel)


data Command = Status
    | Submit
    | Run
    | Remove
    | Drop
    | Config
    | Shutdown
    | Dump
    | Load
    | Ping
    | Help
    deriving (Eq)

parseCommand :: String -> Command
parseCommand "status"   = Status
parseCommand "submit"   = Submit
parseCommand "run"      = Run
parseCommand "remove"   = Remove
parseCommand "drop"     = Drop
parseCommand "config"   = Config
parseCommand "shutdown" = Shutdown
parseCommand "dump"     = Dump
parseCommand "load"     = Load
parseCommand "ping"     = Ping
parseCommand _          = Help

newtype Options = Options
    { host     :: String
    }

options :: Maybe String -> Options
options h = Options
  { host    = fromMaybe "unix:///tmp/periodic.sock" h
  }

parseOptions :: [String] -> Options -> (Command, Options, [String])
parseOptions []              opt = (Help, opt, [])
parseOptions ("-H":x:xs)     opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs) opt = parseOptions xs opt { host      = x }
parseOptions (x:xs)          opt = (parseCommand x, opt, xs)

printHelp :: IO ()
printHelp = do
  putStrLn "periodic - Periodic task system client"
  putStrLn ""
  putStrLn "Usage: periodic [--host|-H HOST] command [options]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "     status   Show status"
  putStrLn "     submit   Submit job"
  putStrLn "     run      Run job and output result"
  putStrLn "     remove   Remove job"
  putStrLn "     drop     Drop function"
  putStrLn "     config   Set or Get config"
  putStrLn "     shutdown Shutdown periodicd"
  putStrLn "     dump     Dump jobs"
  putStrLn "     load     Load jobs"
  putStrLn "     ping     Ping server"
  putStrLn "     help     Shows a list of commands or help for one command"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host     Socket path [$PERIODIC_PORT]"
  putStrLn "                eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn ""
  putStrLn $ "Version: v" ++ showVersion version
  putStrLn ""
  exitSuccess

printWorkloadHelp :: IO ()
printWorkloadHelp = do
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -w --workload WORKLOAD or @FILE"

printSubmitHelp :: IO ()
printSubmitHelp = do
  putStrLn "periodic submit - Submit job"
  putStrLn ""
  putStrLn "Usage: periodic submit funcname jobname [-w|--workload WORKLOAD|@FILE] [--later 0] [--timeout 0]"
  printWorkloadHelp
  putStrLn "     --later    Sched job later"
  putStrLn "     --timeout  Run job timeout"
  putStrLn ""
  exitSuccess

printRunHelp :: IO ()
printRunHelp = do
  putStrLn "periodic run - Run job and output result"
  putStrLn ""
  putStrLn "Usage: periodic run funcname jobname [-w|--workload WORKLOAD|@FILE] [--timeout 10]"
  printWorkloadHelp
  putStrLn "     --timeout  Run job timeout"
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

printConfigHelp :: IO ()
printConfigHelp = do
  putStrLn "periodic config - Set or get config"
  putStrLn ""
  putStrLn "Usage: periodic config command [options]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "     get   Get config"
  putStrLn "     set   Set config"
  putStrLn ""
  exitSuccess

printConfigKeyList :: IO ()
printConfigKeyList = do
  putStrLn ""
  putStrLn "Available keys:"
  putStrLn "  timeout         - job process timeout"
  putStrLn "  lock-timeout    - lock timeout"
  putStrLn "  keepalive       - client keepalive"
  putStrLn "  max-batch-size  - max poll batch size"
  putStrLn "  max-pool-size   - max sched pool size"
  putStrLn ""
  exitSuccess

printConfigGetHelp :: IO ()
printConfigGetHelp = do
  putStrLn "periodic config get - Get config"
  putStrLn ""
  putStrLn "Usage: periodic config get key"
  printConfigKeyList

printConfigSetHelp :: IO ()
printConfigSetHelp = do
  putStrLn "periodic config set - Set config"
  putStrLn ""
  putStrLn "Usage: periodic config set key val"
  printConfigKeyList

printLoadHelp :: IO ()
printLoadHelp = do
  putStrLn "periodic load - Load jobs"
  putStrLn ""
  putStrLn "Usage: periodic load file"
  putStrLn ""
  exitSuccess

printDumpHelp :: IO ()
printDumpHelp = do
  putStrLn "periodic dump - Dump jobs"
  putStrLn ""
  putStrLn "Usage: periodic dump file"
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"

  (cmd, Options {..}, argv) <- flip parseOptions (options h) <$> getArgs

  let argc = length argv

  when (cmd == Help) printHelp

  when (cmd == Submit && argc < 2) printSubmitHelp
  when (cmd == Run && argc < 2) printRunHelp
  when (cmd == Remove && argc < 2) printRemoveHelp
  when (cmd == Drop   && argc < 1) printDropHelp
  when (cmd == Config && argc < 1) printConfigHelp
  when (cmd == Load && argc < 1) printLoadHelp
  when (cmd == Dump && argc < 1) printDumpHelp

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  clientEnv <- open (socket host)
  runClientT clientEnv $ processCommand cmd argv

processCommand :: Transport tp => Command -> [String] -> ClientT tp IO ()
processCommand Help _     = liftIO printHelp
processCommand Status _   = doStatus
processCommand Submit xs  = doSubmitJob xs
processCommand Run xs     = doRunJob xs
processCommand Remove xs  = doRemoveJob xs
processCommand Drop xs    = doDropFunc xs
processCommand Config xs  = doConfig xs
processCommand Load xs    = doLoad xs
processCommand Ping _     = void ping
processCommand Dump xs    = doDump xs
processCommand Shutdown _ = shutdown

doRemoveJob :: Transport tp => [String] -> ClientT tp IO ()
doRemoveJob (x:xs) = mapM_ (removeJob (fromString x) . fromString) xs
doRemoveJob []     = liftIO printRemoveHelp

doDropFunc :: Transport tp => [String] -> ClientT tp IO ()
doDropFunc = mapM_ (dropFunc . fromString)

doConfig :: Transport tp => [String] -> ClientT tp IO ()
doConfig ["get"] = liftIO printConfigGetHelp
doConfig ["get", k] = do
  v <- configGet k
  liftIO $ print v
doConfig ("get":_:_) = liftIO printConfigGetHelp
doConfig ["set"] = liftIO printConfigSetHelp
doConfig ["set", _] = liftIO printConfigSetHelp
doConfig ["set", k, v] = void $ configSet k (read v)
doConfig ("set":_:_:_) = liftIO printConfigSetHelp
doConfig _ = liftIO printConfigHelp

doLoad :: Transport tp => [String] -> ClientT tp IO ()
doLoad [fn] = do
  jobs <- liftIO $ decodeFile fn
  void $ load jobs

doLoad _ = liftIO printLoadHelp

doDump :: Transport tp => [String] -> ClientT tp IO ()
doDump [fn] = do
  jobs <- dump
  liftIO $ encodeFile fn jobs

doDump _ = liftIO printDumpHelp

doSubmitJob :: Transport tp => [String] -> ClientT tp IO ()
doSubmitJob []       = liftIO printSubmitHelp
doSubmitJob [_]      = liftIO printSubmitHelp
doSubmitJob (x:y:xs) = do
  wl <- liftIO $ getWorkload xs
  void $ submitJob (fromString x) (fromString y) wl l t
  where l = getLater xs
        t = getTimeout 0 xs

safeRead :: Read a => a -> String -> a
safeRead def "" = def
safeRead def s  = fromMaybe def $ readMaybe s

getWorkload :: [String] -> IO Workload
getWorkload argv =
  case getFlag ["-w", "--workload"] argv of
    ""      -> pure ""
    ('@':f) -> Workload <$> B.readFile f
    w       -> pure $ fromString w

getTimeout :: Int -> [String] -> Int
getTimeout def = safeRead def . getFlag ["--timeout"]

getLater :: [String] -> Int64
getLater = safeRead 0 . getFlag ["--later"]


getFlag :: [String] -> [String] -> String
getFlag _ []        = ""
getFlag _ [_]       = ""
getFlag ks (x:y:xs) = if x `elem` ks then y else getFlag ks (y:xs)

getFile :: String -> IO Workload
getFile fn = Workload <$> B.readFile fn

doRunJob :: Transport tp => [String] -> ClientT tp IO ()
doRunJob []       = liftIO printRunHelp
doRunJob [_]      = liftIO printRunHelp
doRunJob (x:y:xs) = do
  w <- liftIO $ getWorkload xs
  io <- async $ recvJobData putD (fromString x) (fromString y)
  liftIO . putR
    =<< runJob (fromString x) (fromString y) w t
  cancel io

  where t = getTimeout 10 xs

        putR :: Maybe ByteString -> IO ()
        putR Nothing   = putStrLn "Error: run job failed"
        putR (Just bs) = B.putStrLn $ "Result: " <> bs

        putD :: ByteString -> IO ()
        putD bs = B.putStrLn $ "Data: " <> bs

doStatus :: Transport tp => ClientT tp IO ()
doStatus = do
  st <- map formatTime . unpackBS . map (B.split ',') . B.lines <$> status
  liftIO $ printTable (["FUNCTIONS", "WORKERS", "JOBS", "PROCESSING", "LOCKED", "SCHEDAT"]:st)

unpackBS :: [[ByteString]] -> [[String]]
unpackBS = map (map B.unpack)

formatTime :: [String] -> [String]
formatTime []     = []
formatTime [x]    = [formatUnixTimeLocal x]
formatTime (x:xs) = x:formatTime xs

formatUnixTimeLocal :: String -> String
formatUnixTimeLocal = B.unpack
                    . unsafePerformIO
                    . formatUnixTime "%Y-%m-%d %H:%M:%S"
                    . fromEpochTime
                    . fromIntegral
                    . read

printTable :: [[String]] -> IO ()
printTable rows = T.printBox $ T.hsep 2 T.left (map (T.vcat T.left . map T.text) (transpose rows))
