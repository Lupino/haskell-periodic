{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad             (when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT, hoistMaybe, runMaybeT)
import           Data.Aeson                (Value (..), object, (.=))
import           Data.ByteString           (ByteString)
import           Data.List                 (isPrefixOf)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Version              (showVersion)
import           Metro.Class               (Transport)
import           Metro.TP.Socket           (socket)
import           Network.MCP.Server
import           Network.MCP.Server.StdIO
import           Network.MCP.Types
import           Paths_periodic_client_exe (version)
import           Periodic.Trans.ClientPool
import           Periodic.Types            (FuncName (..), JobName (..))
import           System.Environment        (getArgs, lookupEnv)
import           System.Exit               (exitSuccess)


data Options = Options
    { host     :: String
    , poolSize :: Int
    , showHelp :: Bool
    }

options :: Maybe String -> Options
options h = Options
  { host     = fromMaybe "unix:///tmp/periodic.sock" h
  , poolSize = 10
  , showHelp  = False
  }

parseOptions :: [String] -> Options -> Options
parseOptions []                   opt = opt
parseOptions ("-H":x:xs)          opt = parseOptions xs opt { host = x }
parseOptions ("--host":x:xs)      opt = parseOptions xs opt { host = x }
parseOptions ("--pool-size":x:xs) opt = parseOptions xs opt { poolSize = read x }
parseOptions ("--help":xs)        opt = parseOptions xs opt { showHelp = True }
parseOptions ("-h":xs)            opt = parseOptions xs opt { showHelp = True }
parseOptions (_:xs)               opt = parseOptions xs opt

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-mcp - Periodic task system client mcp server"
  putStrLn ""
  putStrLn "Usage: periodic-mcp [--host|-H HOST] [--pool-size SIZE]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host      Socket path [$PERIODIC_PORT]"
  putStrLn "                 eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "     --pool-size Connection pool size"
  putStrLn "  -h --help       Display help message"
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
maybeBoolResult Nothing      = textResult "No input provided"
maybeBoolResult (Just True)  = textResult "True"
maybeBoolResult (Just False) = textResult "False"

maybeBSResult :: Maybe (Maybe ByteString) -> CallToolResult
maybeBSResult Nothing          = textResult "No input provided"
maybeBSResult (Just Nothing)   = textResult "No result"
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
  return . textResult $ "Unknown tool: " <> name


funcAndNameSchema :: Value
funcAndNameSchema = object
  [ "type" .= ("object" :: String)
  , "properties" .= object
    [ "func" .= object
      [ "type" .= ("string" :: String)
      ]
    , "name" .= object
      [ "type" .= ("string" :: String)
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
      ]
    ]
  , "required" .= ([ "func" ]::[String])
  ]


main :: IO ()
main = do

  h <- lookupEnv "PERIODIC_PORT"

  Options{..} <- flip parseOptions (options h) <$> getArgs

  when showHelp printHelp

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  clientEnv <- openPool (socket host) poolSize

  let serverInfo = Implementation
        { serverName = "periodic-mcp-server"
        , serverVersion = "1.0.0"
        }
      serverCapabilities = ServerCapabilities
        { resourcesCapability = Nothing
        , toolsCapability = Just $ ToolsCapability True
        , promptsCapability = Nothing
        }

  server <- createServer serverInfo serverCapabilities "Periodic task system."

  registerTools server
    [ Tool
      { toolName = "status"
      , toolDescription = Just "Show status"
      , toolInputSchema = Null
      }
    , Tool
      { toolName = "drop"
      , toolDescription = Just "Drop function"
      , toolInputSchema = funcSchema
      }
    , Tool
      { toolName = "submit"
      , toolDescription = Just "Submit job"
      , toolInputSchema = funcAndNameSchema
      }
    , Tool
      { toolName = "run"
      , toolDescription = Just "Run job"
      , toolInputSchema = funcAndNameSchema
      }
    , Tool
      { toolName = "remove"
      , toolDescription = Just "Remove job"
      , toolInputSchema = funcAndNameSchema
      }
    ]

  registerToolCallHandler server $ runClientPoolT clientEnv . toolCallHandler

  runServerWithSTDIO server
