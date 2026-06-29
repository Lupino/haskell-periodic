{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.Auth
  ( FuncAuth
  , FuncAuthMap
  , isFuncAllowed
  , loadFuncAuth
  , startFuncAuthReloader
  ) where

import           Control.Monad              (forever, when)
import qualified Data.ByteString.Char8      as B
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Periodic.Types.ClientType  (ClientIdentity (..))
import           Periodic.Types.Internal    (validate)
import           Periodic.Types.Job         (FuncName (..))
import           System.Directory           (doesFileExist, getModificationTime)
import           System.Log.Logger          (errorM, infoM)
import           UnliftIO                   (Async, TVar, atomically, async,
                                             catchAny, newTVarIO, readTVarIO,
                                             writeTVar)
import           UnliftIO.Concurrent        (threadDelay)

type FuncAuthMap = Map B.ByteString (B.ByteString, Set FuncName)

data FuncAuth = FuncAuth
  { authFilePath :: FilePath
  , authRules    :: TVar FuncAuthMap
  , authMTime    :: TVar String
  }

loadFuncAuth :: FilePath -> IO FuncAuth
loadFuncAuth path = do
  rules <- readAuthFile path
  mtime <- show <$> getModificationTime path
  FuncAuth path <$> newTVarIO rules <*> newTVarIO mtime

startFuncAuthReloader :: FuncAuth -> IO (Async ())
startFuncAuthReloader auth@FuncAuth {..} = async $ forever $ do
  threadDelay 1000000
  reloadFuncAuth auth `catchAny` \err ->
    errorM "Periodic.Server.Auth" $ "Reload auth file failed: " ++ show err

reloadFuncAuth :: FuncAuth -> IO ()
reloadFuncAuth FuncAuth {..} = do
  exists <- doesFileExist authFilePath
  when exists $ do
    mtime <- show <$> getModificationTime authFilePath
    oldMTime <- readTVarIO authMTime
    when (mtime /= oldMTime) $ do
      parsed <- readAuthFile authFilePath
      atomically $ do
        writeTVar authRules parsed
        writeTVar authMTime mtime
      infoM "Periodic.Server.Auth" $ "Reloaded auth file: " ++ authFilePath

isFuncAllowed :: FuncAuth -> Maybe ClientIdentity -> FuncName -> IO Bool
isFuncAllowed _ Nothing _ = pure False
isFuncAllowed FuncAuth {..} (Just ClientIdentity {..}) fn = do
  rules <- readTVarIO authRules
  pure $
    case Map.lookup clientName rules of
      Just (token, funcs) ->
        token == clientToken && fn `Set.member` funcs
      Nothing -> False

readAuthFile :: FilePath -> IO FuncAuthMap
readAuthFile path = do
  bs <- B.readFile path
  either fail pure $ parseAuthFile bs

parseAuthFile :: B.ByteString -> Either String FuncAuthMap
parseAuthFile =
  foldl parseLine (Right Map.empty) . zip [(1 :: Int)..] . B.lines
  where
    parseLine (Left err) _ = Left err
    parseLine (Right acc) (lineNo, rawLine)
      | B.null line = Right acc
      | "#" `B.isPrefixOf` line = Right acc
      | otherwise =
          case B.words line of
            [name, token, funcsRaw] -> do
              validateName "client name" lineNo name
              validateName "client token" lineNo token
              funcs <- parseFuncs lineNo funcsRaw
              pure $ Map.insert name (token, funcs) acc
            _ -> Left $ "Invalid auth line " ++ show lineNo ++ ": expected '<client> <token> <func1,func2>'"
      where
        line = stripComment $ B.dropWhile isSpace rawLine

    stripComment bs =
      let (before, _) = B.break (== '#') bs
      in B.reverse $ B.dropWhile isSpace $ B.reverse before

    parseFuncs lineNo funcsRaw = do
      funcs <- mapM (parseFunc lineNo) $ filter (not . B.null) $ B.split ',' funcsRaw
      if null funcs
        then Left $ "Invalid auth line " ++ show lineNo ++ ": empty func list"
        else pure $ Set.fromList funcs

    parseFunc lineNo raw = do
      let fn = FuncName raw
      case validate fn of
        Left err -> Left $ "Invalid func on auth line " ++ show lineNo ++ ": " ++ err
        Right () -> pure fn

    validateName what lineNo raw =
      if B.null raw
        then Left $ "Invalid auth line " ++ show lineNo ++ ": empty " ++ what
        else pure ()

    isSpace c = c == ' ' || c == '\t' || c == '\r'
