{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Control.Monad          (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Binary            (decodeFile, encodeFile)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B (lines, pack, putStr, readFile,
                                              split, unpack)
import           Data.Int               (Int64)
import           Data.List              (isPrefixOf, transpose)
import           Data.Maybe             (fromMaybe)
import           Metro.Class            (Transport)
import           Metro.Socket           (getHost, getService)
import           Metro.TP.Socket        (socket)
import           Metro.TP.TLS           (makeClientParams', tlsConfig)
import           Metro.TP.WebSockets    (clientConfig)
import           Metro.TP.XOR           (xorConfig)
import           Periodic.Trans.Client
import           Periodic.Types         (Workload (..))
import           System.Environment     (getArgs, lookupEnv)
import           System.Exit            (exitSuccess)
import           Text.Read              (readMaybe)

import           Data.String            (fromString)
import           Data.UnixTime
import           System.IO.Unsafe       (unsafePerformIO)
import qualified Text.PrettyPrint.Boxes as T


data Command = Status
    | Submit
    | Run
    | Remove
    | Drop
    | Config
    | Shutdown
    | Dump
    | Load
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
parseCommand _          = Help

data Options = Options
    { host     :: String
    , xorFile  :: FilePath
    , useTls   :: Bool
    , useWs    :: Bool
    , hostName :: String
    , certKey  :: FilePath
    , cert     :: FilePath
    , caStore  :: FilePath
    }

options :: Maybe String -> Maybe String -> Options
options h f = Options { host    = fromMaybe "unix:///tmp/periodic.sock" h
                      , xorFile = fromMaybe "" f
                      , useTls = False
                      , useWs = False
                      , hostName = "localhost"
                      , certKey = "client-key.pem"
                      , cert = "client.pem"
                      , caStore = "ca.pem"
                      }

parseOptions :: [String] -> Options -> (Command, Options, [String])
parseOptions []                  opt = (Help, opt, [])
parseOptions ("-H":x:xs)         opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs)     opt = parseOptions xs opt { host      = x }
parseOptions ("--xor":x:xs)      opt = parseOptions xs opt { xorFile   = x }
parseOptions ("--tls":xs)        opt = parseOptions xs opt { useTls = True }
parseOptions ("--ws":xs)        opt  = parseOptions xs opt { useWs = True }
parseOptions ("--hostname":x:xs) opt = parseOptions xs opt { hostName = x }
parseOptions ("--cert-key":x:xs) opt = parseOptions xs opt { certKey = x }
parseOptions ("--cert":x:xs)     opt = parseOptions xs opt { cert = x }
parseOptions ("--ca":x:xs)       opt = parseOptions xs opt { caStore = x }
parseOptions (x:xs)              opt = (parseCommand x, opt, xs)

printHelp :: IO ()
printHelp = do
  putStrLn "periodic - Periodic task system client"
  putStrLn ""
  putStrLn "Usage: periodic [--host|-H HOST] [--xor FILE|--ws|--tls [--hostname HOSTNAME] [--cert-key FILE] [--cert FILE] [--ca FILE]] command [options]"
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
  putStrLn "     help     Shows a list of commands or help for one command"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host     Socket path [$PERIODIC_PORT]"
  putStrLn "                eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "     --xor      XOR Transport encode file [$XOR_FILE]"
  putStrLn "     --tls      Use tls transport"
  putStrLn "     --ws       Use websockets transport"
  putStrLn "     --hostname Host name"
  putStrLn "     --cert-key Private key associated"
  putStrLn "     --cert     Public certificate (X.509 format)"
  putStrLn "     --ca       trusted certificates"
  putStrLn ""
  putStrLn "Version: v1.1.7.1"
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
  putStrLn "Usage: periodic submit funcname jobname [-w|--workload WORKLOAD|@FILE] [--later 0]"
  printWorkloadHelp
  putStrLn "     --later    Sched job later"
  putStrLn ""
  exitSuccess

printRunHelp :: IO ()
printRunHelp = do
  putStrLn "periodic run - Run job and output result"
  putStrLn ""
  putStrLn "Usage: periodic run funcname jobname [-w|--workload WORKLOAD|@FILE]"
  printWorkloadHelp
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
  putStrLn "  poll-interval   - poll loop every time interval"
  putStrLn "  revert-interval - revert process queue loop every time interval"
  putStrLn "  timeout         - job process timeout"
  putStrLn "  keepalive       - client keepalive"
  putStrLn "  max-batch-size  - max poll batch size"
  putStrLn "  expiration      - run job result cache expiration"
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
  f <- lookupEnv "XOR_FILE"

  (cmd, opts@Options {..}, argv) <- flip parseOptions (options h f) <$> getArgs

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

  run opts cmd argv

run Options {useTls = True, ..} cmd argv = do
  prms <- makeClientParams' cert [] certKey caStore (hostName, B.pack $ fromMaybe "" $ getService host)
  clientEnv <- open (tlsConfig prms (socket host))
  runClientT clientEnv $ processCommand cmd argv

run Options {useWs = True, ..} cmd argv = do
  clientEnv <- open (clientConfig (socket host) (fromMaybe "0.0.0.0" $ getHost host) (fromMaybe "" $ getService host))
  runClientT clientEnv $ processCommand cmd argv

run Options {xorFile = "", ..} cmd argv = do
  clientEnv <- open (socket host)
  runClientT clientEnv $ processCommand cmd argv

run Options {..} cmd argv = do
  clientEnv <- open (xorConfig xorFile $ socket host)
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
  (wl, l) <- liftIO $ go (Nothing, Nothing) xs
  void $ submitJob (fromString x) (fromString y) wl l

  where go :: (Maybe Workload, Maybe Int64) -> [String] -> IO (Maybe Workload, Maybe Int64)
        go v []                              = pure v
        go (_, l0) ("-w":('@':f):ys)         = do
          w <- B.readFile f
          go (Just (Workload w), l0) ys
        go (_, l0) ("--workload":('@':f):ys) = do
          w <- B.readFile f
          go (Just (Workload w), l0) ys
        go (_, l0) ("-w":w:ys)               = go (Just (fromString w), l0) ys
        go (_, l0) ("--workload":w:ys)       = go (Just (fromString w), l0) ys
        go (w, _) ("--later":l0:ys)          = go (w, readMaybe l0) ys
        go v (_:ys)                          = go v ys

doRunJob :: Transport tp => [String] -> ClientT tp IO ()
doRunJob []       = liftIO printRunHelp
doRunJob [_]      = liftIO printRunHelp
doRunJob (x:y:xs) =
  liftIO . putR
    =<< runJob (fromString x) (fromString y)
    =<< liftIO (go xs)

  where go :: [String] -> IO (Maybe Workload)
        go []                       = pure Nothing
        go ("-w":('@':f):_)         = getFile f
        go ("--workload":('@':f):_) = getFile f
        go ("-w":w:_)               = pure $ Just (fromString w)
        go ("--workload":w:_)       = pure $ Just (fromString w)
        go (_:ys)                   = go ys

        getFile :: String -> IO (Maybe Workload)
        getFile fn = Just . Workload <$> B.readFile fn

        putR :: Maybe ByteString -> IO ()
        putR Nothing   = putStrLn "Error: run job failed"
        putR (Just bs) = B.putStr bs

doStatus :: Transport tp => ClientT tp IO ()
doStatus = do
  st <- map formatTime . unpackBS . map (B.split ',') . B.lines <$> status
  liftIO $ printTable (["FUNCTIONS", "WORKERS", "JOBS", "PROCESSING", "LOCKING", "SCHEDAT"]:st)

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
