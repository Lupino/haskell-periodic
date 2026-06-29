{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Control.Monad             (when)
import           Control.Applicative       ((<|>))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT, hoistMaybe, runMaybeT)
import           Data.Aeson                (Value (..), object, (.=))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as B
import           Data.List                 (isPrefixOf)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Version              (showVersion)
import           Metro.Class               (Transport)
import qualified Metro.TP.RSA              as RSA (RSAMode (AES), configClient)
import           Metro.TP.Socket           (socket)
import           Network.MCP.Server
import           Network.MCP.Server.StdIO
import           Network.MCP.Types
import           Paths_periodic_client_exe (version)
import           Periodic.Exec.Util        (parseReadArg)
import           Periodic.Trans.ClientPool
import           Periodic.Types            (ClientIdentity (ClientIdentity),
                                            FuncName (..), JobName (..))
import           System.Environment        (getArgs, lookupEnv)
import           System.Exit               (exitFailure, exitSuccess)


data Options = Options
  { host           :: String
  , poolSize       :: Int
  , showHelp       :: Bool
  , rsaPrivatePath :: FilePath
  , rsaPublicPath  :: FilePath
  , rsaMode        :: RSA.RSAMode
  , clientName     :: Maybe String
  , clientToken    :: Maybe String
  , parseError     :: Maybe String
  , invalidOption  :: Maybe String
  }

options :: Maybe String -> Options
options h = Options
  { host           = fromMaybe "unix:///tmp/periodic.sock" h
  , poolSize       = 10
  , showHelp       = False
  , rsaPublicPath  = "public_key.pem"
  , rsaPrivatePath = ""
  , rsaMode        = RSA.AES
  , clientName     = Nothing
  , clientToken    = Nothing
  , parseError     = Nothing
  , invalidOption  = Nothing
  }

parseOptions :: [String] -> Options -> Options
parseOptions []                          opt = opt
parseOptions ("-H":x:xs)                 opt = parseOptions xs opt { host = x }
parseOptions ("--host":x:xs)             opt = parseOptions xs opt { host = x }
parseOptions ("--pool-size":x:xs)        opt = parseOptions xs $ parseReadArg "--pool-size" x (\v o -> o { poolSize = v }) onParseErr opt
parseOptions ("--rsa-private-path":x:xs) opt = parseOptions xs opt { rsaPrivatePath = x }
parseOptions ("--rsa-public-path":x:xs)  opt = parseOptions xs opt { rsaPublicPath  = x }
parseOptions ("--rsa-mode":x:xs)         opt = parseOptions xs $ parseReadArg "--rsa-mode" x (\v o -> o { rsaMode = v }) onParseErr opt
parseOptions ("--client-name":x:xs)      opt = parseOptions xs opt { clientName = Just x }
parseOptions ("--client-token":x:xs)     opt = parseOptions xs opt { clientToken = Just x }
parseOptions ("--help":xs)               opt = parseOptions xs opt { showHelp = True }
parseOptions ("-h":xs)                   opt = parseOptions xs opt { showHelp = True }
parseOptions (x:xs)                      opt = parseOptions xs opt { invalidOption = invalidOption opt <|> Just x }

onParseErr :: String -> Options -> Options
onParseErr err o = o { parseError = parseError o <|> Just err }

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-mcp - MCP server bridge for the Periodic task system"
  putStrLn ""
  putStrLn "Usage: periodic-mcp [OPTIONS]"
  putStrLn ""
  putStrLn "Global Options:"
  putStrLn "  -H, --host <HOST>         Socket path or address [$PERIODIC_PORT]"
  putStrLn "                            (Default: unix:///tmp/periodic.sock)"
  putStrLn "      --pool-size <INT>     Connection pool size (Default: 10)"
  putStrLn ""
  putStrLn "Security Options:"
  putStrLn "      --rsa-mode <MODE>     RSA mode: Plain, RSA, or AES (Default: AES)"
  putStrLn "      --rsa-public-path <P> RSA public key file or directory"
  putStrLn "      --rsa-private-path <P>RSA private key file path"
  putStrLn "      --client-name <NAME>  Auth client name"
  putStrLn "      --client-token <TOK>  Auth client token"
  putStrLn ""
  putStrLn "Help:"
  putStrLn "  -h, --help                Display this help message"
  putStrLn ""
  putStrLn $ "Version: v" ++ showVersion version
  putStrLn ""
  exitSuccess


textResult :: Text -> CallToolResult
textResult val = CallToolResult
  [ ToolContent
    { toolContentType = TextualContent
    , toolContentText = Just val
    }
  ]
  False


stringResult :: String -> CallToolResult
stringResult = textResult . T.pack

maybeBoolResult :: Maybe Bool -> CallToolResult
maybeBoolResult Nothing      = textResult "Error: Required arguments missing"
maybeBoolResult (Just True)  = textResult "Success"
maybeBoolResult (Just False) = textResult "Operation failed"

maybeBSResult :: Maybe (Maybe ByteString) -> CallToolResult
maybeBSResult Nothing          = textResult "Error: Required arguments missing"
maybeBSResult (Just Nothing)   = textResult "Job executed but returned no data"
maybeBSResult (Just (Just bs)) = textResult $ TE.decodeUtf8 bs


getFuncName :: Monad m => Map Text Value -> MaybeT m FuncName
getFuncName args = do
  String fn <- hoistMaybe $ Map.lookup "func" args
  return $ FuncName $ TE.encodeUtf8 fn


getJobName :: Monad m => Map Text Value -> MaybeT m JobName
getJobName args = do
  String jn <- hoistMaybe $ Map.lookup "name" args
  return $ JobName $ TE.encodeUtf8 jn


toolCallHandler :: Transport tp => CallToolRequest -> ClientT tp IO CallToolResult
toolCallHandler CallToolRequest {callToolName = "status"} =
  stringResult . formatStatus <$> status

toolCallHandler CallToolRequest {callToolName = "drop", callToolArguments = args } = do
  r <- runMaybeT $ do
    fn <- getFuncName args
    lift $ dropFunc fn

  return $ maybeBoolResult r

toolCallHandler CallToolRequest {callToolName = "submit", callToolArguments = args } = do
  r <- runMaybeT $ do
    fn <- getFuncName args
    jn <- getJobName args
    lift $ submitJob fn jn "" 0 0

  return $ maybeBoolResult r

toolCallHandler CallToolRequest {callToolName = "run", callToolArguments = args } = do
  r <- runMaybeT $ do
    fn <- getFuncName args
    jn <- getJobName args
    lift $ runJob fn jn "" 0

  return $ maybeBSResult r

toolCallHandler CallToolRequest {callToolName = "remove", callToolArguments = args } = do
  r <- runMaybeT $ do
    fn <- getFuncName args
    jn <- getJobName args
    lift $ removeJob fn jn

  return $ maybeBoolResult r

toolCallHandler CallToolRequest {callToolName = name } =
  return . textResult $ "Error: Unknown tool name - " <> name


funcAndNameSchema :: Value
funcAndNameSchema = object
  [ "type" .= ("object" :: String)
  , "properties" .= object
    [ "func" .= object
      [ "type" .= ("string" :: String)
      , "description" .= ("The name of the function/worker" :: String)
      ]
    , "name" .= object
      [ "type" .= ("string" :: String)
      , "description" .= ("The unique name of the job" :: String)
      ]
    ]
  , "required" .= ([ "func", "name" ]::[String])
  ]


funcSchema :: Value
funcSchema = object
  [ "type" .= ("object" :: String)
  , "properties" .= object
    [ "func" .= object
      [ "type" .= ("string" :: String)
      , "description" .= ("The name of the function to be dropped" :: String)
      ]
    ]
  , "required" .= ([ "func" ]::[String])
  ]


main :: IO ()
main = do

  h <- lookupEnv "PERIODIC_PORT"

  opts@Options{..} <- flip parseOptions (options h) <$> getArgs

  when showHelp printHelp
  case parseError of
    Just err -> putStrLn err >> putStrLn "Use --help to see supported options." >> exitFailure
    Nothing  -> pure ()
  case invalidOption of
    Just opt -> putStrLn ("Unknown option: " ++ opt) >> putStrLn "Use --help to see supported options." >> exitFailure
    Nothing  -> pure ()

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Error: Invalid host " ++ host
    printHelp


  auth <- requireAuthPair opts
  case rsaPrivatePath of
    "" -> maybe (openPool (socket host) poolSize) (\ident -> openPoolWithAuth (socket host) ident poolSize) auth >>= run
    _ -> do
      genTP <- RSA.configClient rsaMode rsaPrivatePath rsaPublicPath
      maybe (openPool (genTP $ socket host) poolSize) (\ident -> openPoolWithAuth (genTP $ socket host) ident poolSize) auth >>= run

requireAuthPair :: Options -> IO (Maybe ClientIdentity)
requireAuthPair Options {clientName = Nothing, clientToken = Nothing} = pure Nothing
requireAuthPair Options {clientName = Just n, clientToken = Just t} =
  pure $ Just $ ClientIdentity (B.pack n) (B.pack t)
requireAuthPair _ = do
  putStrLn "Error: --client-name and --client-token must be provided together"
  exitFailure


run :: Transport tp => ClientPoolEnv tp -> IO ()
run clientEnv = do
  let serverInfo = Implementation
        { serverName = "periodic-mcp-server"
        , serverVersion = "1.0.0"
        }
      serverCapabilities = ServerCapabilities
        { resourcesCapability = Nothing
        , toolsCapability = Just $ ToolsCapability True
        , promptsCapability = Nothing
        }

  server <- createServer serverInfo serverCapabilities "Bridge for managing Periodic task system via MCP."

  registerTools server
    [ Tool
      { toolName = "status"
      , toolDescription = Just "Fetch periodic system statistics and job status."
      , toolInputSchema = Null
      }
    , Tool
      { toolName = "drop"
      , toolDescription = Just "Delete a function and all its associated jobs from the server."
      , toolInputSchema = funcSchema
      }
    , Tool
      { toolName = "submit"
      , toolDescription = Just "Submit a new job for background/asynchronous execution."
      , toolInputSchema = funcAndNameSchema
      }
    , Tool
      { toolName = "run"
      , toolDescription = Just "Execute a job immediately and return the resulting output."
      , toolInputSchema = funcAndNameSchema
      }
    , Tool
      { toolName = "remove"
      , toolDescription = Just "Remove a specific job from a function."
      , toolInputSchema = funcAndNameSchema
      }
    ]

  registerToolCallHandler server $ runClientPoolT clientEnv . toolCallHandler

  runServerWithSTDIO server
