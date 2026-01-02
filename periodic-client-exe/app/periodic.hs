{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Control.Monad             (void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Binary               (decodeFile, encodeFile)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as B (putStrLn, readFile)
import           Data.Int                  (Int64)
import           Data.List                 (isPrefixOf)
import           Data.Maybe                (fromMaybe)
import           Data.String               (fromString)
import           Data.Version              (showVersion)
import           Metro.Class               (Transport)
import           Metro.TP.RSA              (generateKeyPair)
import qualified Metro.TP.RSA              as RSA (RSAMode (AES), configClient)
import           Metro.TP.Socket           (socket)
import           Paths_periodic_client_exe (version)
import           Periodic.Trans.Client
import           Periodic.Types            (Workload (..))
import           System.Environment        (getArgs, lookupEnv)
import           System.Exit               (exitSuccess)
import           Text.Read                 (readMaybe)
import           UnliftIO                  (async, atomically, newEmptyTMVarIO,
                                            takeTMVar, wait)


data Command = Status
  | Submit
  | Run
  | Recv
  | Remove
  | Drop
  | Config
  | Shutdown
  | Dump
  | Load
  | Ping
  | KeyGen
  | Help
  deriving (Eq)

parseCommand :: String -> Command
parseCommand "status"   = Status
parseCommand "submit"   = Submit
parseCommand "run"      = Run
parseCommand "recv"     = Recv
parseCommand "remove"   = Remove
parseCommand "drop"     = Drop
parseCommand "config"   = Config
parseCommand "shutdown" = Shutdown
parseCommand "dump"     = Dump
parseCommand "load"     = Load
parseCommand "ping"     = Ping
parseCommand "keygen"   = KeyGen
parseCommand _          = Help

data Options = Options
  { host           :: String
  , rsaPrivatePath :: FilePath
  , rsaPublicPath  :: FilePath
  , rsaMode        :: RSA.RSAMode
  }

options :: Maybe String -> Options
options h = Options
  { host           = fromMaybe "unix:///tmp/periodic.sock" h
  , rsaPublicPath  = "public_key.pem"
  , rsaPrivatePath = ""
  , rsaMode        = RSA.AES
  }

parseOptions :: [String] -> Options -> (Command, Options, [String])
parseOptions []                          opt = (Help, opt, [])
parseOptions ("-H":x:xs)                 opt = parseOptions xs opt { host           = x }
parseOptions ("--host":x:xs)             opt = parseOptions xs opt { host           = x }
parseOptions ("--rsa-private-path":x:xs) opt = parseOptions xs opt { rsaPrivatePath = x }
parseOptions ("--rsa-public-path":x:xs)  opt = parseOptions xs opt { rsaPublicPath  = x }
parseOptions ("--rsa-mode":x:xs)         opt = parseOptions xs opt { rsaMode  = read x }
parseOptions (x:xs)                      opt = (parseCommand x, opt, xs)

printHelp :: IO ()
printHelp = do
  putStrLn "periodic - A distributed periodic task system client"
  putStrLn ""
  putStrLn "Usage: periodic [GLOBAL_OPTIONS] COMMAND [ARGS]"
  putStrLn ""
  putStrLn "Global Options:"
  putStrLn "  -H, --host <HOST>         Socket path or address [$PERIODIC_PORT]"
  putStrLn "                            (Default: unix:///tmp/periodic.sock)"
  putStrLn "      --rsa-mode <MODE>     Encryption mode: Plain, RSA, or AES (Default: AES)"
  putStrLn "      --rsa-public-path <P> RSA public key file or directory"
  putStrLn "      --rsa-private-path <P>RSA private key file path"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  status      Show server and task statistics"
  putStrLn "  submit      Submit a new job to the system"
  putStrLn "  run         Execute a job and capture the output"
  putStrLn "  recv        Receive specific job work data"
  putStrLn "  remove      Remove jobs by name"
  putStrLn "  drop        Delete a function and all its jobs"
  putStrLn "  config      Get or set server-side configurations"
  putStrLn "  shutdown    Safely shut down the periodic server"
  putStrLn "  dump        Export jobs to a file"
  putStrLn "  load        Import jobs from a file"
  putStrLn "  ping        Test connection to the server"
  putStrLn "  keygen      Generate a new RSA key pair"
  putStrLn "  help        Show this help message"
  putStrLn ""
  putStrLn $ "Version: v" ++ showVersion version
  putStrLn ""
  exitSuccess

printWorkloadHelp :: IO ()
printWorkloadHelp = do
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  -w, --workload <DATA|@FILE>  Job workload content or file path"

printSubmitHelp :: IO ()
printSubmitHelp = do
  putStrLn "periodic submit - Submit a new job"
  putStrLn ""
  putStrLn "Usage: periodic submit <funcname> <jobname> [OPTIONS]"
  printWorkloadHelp
  putStrLn "      --later <INT>    Delay job execution (s) (Default: 0)"
  putStrLn "      --timeout <INT>  Job execution timeout (s) (Default: 0)"
  putStrLn ""
  exitSuccess

printRunHelp :: IO ()
printRunHelp = do
  putStrLn "periodic run - Execute job and stream results"
  putStrLn ""
  putStrLn "Usage: periodic run <funcname> <jobname> [OPTIONS]"
  printWorkloadHelp
  putStrLn "      --timeout <INT>  Job execution timeout (s) (Default: 10)"
  putStrLn ""
  exitSuccess

printRecvHelp :: IO ()
printRecvHelp = do
  putStrLn "periodic recv - Receive job work data once"
  putStrLn ""
  putStrLn "Usage: periodic recv <funcname> <jobname>"
  putStrLn ""
  exitSuccess

printRemoveHelp :: IO ()
printRemoveHelp = do
  putStrLn "periodic remove - Remove jobs"
  putStrLn ""
  putStrLn "Usage: periodic remove <funcname> <jobname> [jobname...]"
  putStrLn ""
  exitSuccess

printDropHelp :: IO ()
printDropHelp = do
  putStrLn "periodic drop - Delete a function"
  putStrLn ""
  putStrLn "Usage: periodic drop <funcname> [funcname...]"
  putStrLn ""
  exitSuccess

printConfigHelp :: IO ()
printConfigHelp = do
  putStrLn "periodic config - Get or set server configuration"
  putStrLn ""
  putStrLn "Usage: periodic config <command> [ARGS]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  get   Retrieve a configuration value"
  putStrLn "  set   Update a configuration value"
  putStrLn ""
  exitSuccess

printConfigKeyList :: IO ()
printConfigKeyList = do
  putStrLn ""
  putStrLn "Available configuration keys:"
  putStrLn "  timeout         - Max job processing duration (s)"
  putStrLn "  lock-timeout    - Resource lock acquisition timeout (s)"
  putStrLn "  assign-wait     - Max wait time for job assignment (s)"
  putStrLn "  keepalive       - Client-server keepalive interval (s)"
  putStrLn "  max-batch-size  - Maximum number of jobs per poll"
  putStrLn "  max-pool-size   - Maximum capacity of the scheduler pool"
  putStrLn ""
  exitSuccess

printConfigGetHelp :: IO ()
printConfigGetHelp = do
  putStrLn "periodic config get - Retrieve configuration"
  putStrLn ""
  putStrLn "Usage: periodic config get <key>"
  printConfigKeyList

printConfigSetHelp :: IO ()
printConfigSetHelp = do
  putStrLn "periodic config set - Update configuration"
  putStrLn ""
  putStrLn "Usage: periodic config set <key> <val>"
  printConfigKeyList

printLoadHelp :: IO ()
printLoadHelp = do
  putStrLn "periodic load - Import jobs"
  putStrLn ""
  putStrLn "Usage: periodic load <file>"
  putStrLn ""
  exitSuccess

printDumpHelp :: IO ()
printDumpHelp = do
  putStrLn "periodic dump - Export jobs"
  putStrLn ""
  putStrLn "Usage: periodic dump <file>"
  putStrLn ""
  exitSuccess

printKeyGenHelp :: IO ()
printKeyGenHelp = do
  putStrLn "periodic keygen - Generate RSA key pair"
  putStrLn ""
  putStrLn "Usage: periodic keygen <file> [OPTIONS]"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  -s, --size <INT>  RSA key bit length (Default: 256)"
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"

  (cmd, opt@(Options {..}), argv) <- flip parseOptions (options h) <$> getArgs

  let argc = length argv

  when (cmd == Help) printHelp

  when (cmd == Submit && argc < 2) printSubmitHelp
  when (cmd == Run && argc < 2) printRunHelp
  when (cmd == Remove && argc < 2) printRemoveHelp
  when (cmd == Drop   && argc < 1) printDropHelp
  when (cmd == Config && argc < 1) printConfigHelp
  when (cmd == Load && argc < 1) printLoadHelp
  when (cmd == Dump && argc < 1) printDumpHelp
  when (cmd == KeyGen && argc < 1) printKeyGenHelp

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Error: Invalid host " ++ host
    printHelp

  run opt cmd argv

run :: Options -> Command -> [String] -> IO ()
run _ KeyGen [] = printKeyGenHelp
run _ KeyGen (x:xs) = generateKeyPair x (getKeySize xs)

run Options {host, rsaPrivatePath = ""} cmd argv = do
  clientEnv <- open (socket host)
  runClientT clientEnv $ processCommand cmd argv
run Options {host, rsaPrivatePath, rsaPublicPath, rsaMode} cmd argv = do
  genTP <- RSA.configClient rsaMode rsaPrivatePath rsaPublicPath
  clientEnv <- open (genTP $ socket host)
  runClientT clientEnv $ processCommand cmd argv

processCommand :: Transport tp => Command -> [String] -> ClientT tp IO ()
processCommand Help _     = liftIO printHelp
processCommand Status _   = doStatus
processCommand Submit xs  = doSubmitJob xs
processCommand Run xs     = doRunJob xs
processCommand Recv xs    = doRecvJob xs
processCommand Remove xs  = doRemoveJob xs
processCommand Drop xs    = doDropFunc xs
processCommand Config xs  = doConfig xs
processCommand Load xs    = doLoad xs
processCommand Ping _     = void ping
processCommand Dump xs    = doDump xs
processCommand Shutdown _ = shutdown
processCommand KeyGen _   = pure ()

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

getKeySize :: [String] -> Int
getKeySize = safeRead 256 . getFlag ["--size", "-s"]


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
  waiter <- newEmptyTMVarIO
  io <- async $ recvJobData (Just waiter) putD (fromString x) (fromString y)
  atomically $ takeTMVar waiter
  liftIO . putR
    =<< runJob (fromString x) (fromString y) w t
  wait io

  where t = getTimeout 10 xs

        putR :: Maybe ByteString -> IO ()
        putR Nothing   = putStrLn "Error: Run job failed"
        putR (Just bs) = B.putStrLn $ "Result: " <> bs

        putD :: ByteString -> IO ()
        putD bs = B.putStrLn $ "Data: " <> bs

doRecvJob :: Transport tp => [String] -> ClientT tp IO ()
doRecvJob []       = liftIO printRecvHelp
doRecvJob [_]      = liftIO printRecvHelp
doRecvJob (x:y:_) = do
  recvJobData Nothing putD (fromString x) (fromString y)

  where putD :: ByteString -> IO ()
        putD bs = B.putStrLn $ "Data: " <> bs

doStatus :: Transport tp => ClientT tp IO ()
doStatus = do
  st <- formatStatus <$> status
  liftIO $ putStrLn st
