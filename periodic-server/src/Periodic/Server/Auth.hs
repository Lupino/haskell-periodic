{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.Auth
  ( FuncAuth
  , FuncAuthMap
  , isFuncAllowed
  , isAdminAllowed
  , isRoleAllowed
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

type FuncAuthMap = Map B.ByteString (B.ByteString, B.ByteString, FuncAuthRule)

data FuncAuthRule = FuncAuthRule
  { exactFuncs  :: Set FuncName
  , prefixFuncs :: Set B.ByteString
  , suffixFuncs :: Set B.ByteString
  }

data FuncAuthEntry
  = AuthExact FuncName
  | AuthPrefix B.ByteString
  | AuthSuffix B.ByteString

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

isRoleAllowed :: FuncAuth -> Maybe ClientIdentity -> B.ByteString -> IO Bool
isRoleAllowed _ Nothing _ = pure False
isRoleAllowed FuncAuth {..} (Just ClientIdentity {..}) role = do
  rules <- readTVarIO authRules
  pure $
    case Map.lookup clientName rules of
      Just (token, expectedRole, _) ->
        token == clientToken && (expectedRole == "admin" || role == expectedRole)
      Nothing -> False

isAdminAllowed :: FuncAuth -> Maybe ClientIdentity -> IO Bool
isAdminAllowed _ Nothing = pure False
isAdminAllowed FuncAuth {..} (Just ClientIdentity {..}) = do
  rules <- readTVarIO authRules
  pure $
    case Map.lookup clientName rules of
      Just (token, expectedRole, _) ->
        token == clientToken && expectedRole == "admin"
      Nothing -> False

isFuncAllowed :: FuncAuth -> Maybe ClientIdentity -> FuncName -> IO Bool
isFuncAllowed _ Nothing _ = pure False
isFuncAllowed FuncAuth {..} (Just ClientIdentity {..}) fn = do
  rules <- readTVarIO authRules
  pure $
    case Map.lookup clientName rules of
      Just (token, expectedRole, funcs) ->
        token == clientToken && (expectedRole == "admin" || matchFuncAuthRule funcs fn)
      Nothing -> False

matchFuncAuthRule :: FuncAuthRule -> FuncName -> Bool
matchFuncAuthRule FuncAuthRule {..} fn@(FuncName raw) =
  fn `Set.member` exactFuncs
  || any (`B.isPrefixOf` raw) (Set.toList prefixFuncs)
  || any (`B.isSuffixOf` raw) (Set.toList suffixFuncs)

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
            [roleRaw, name, token, funcsRaw] -> do
              role <- parseRole lineNo roleRaw
              validateName "client name" lineNo name
              validateName "client token" lineNo token
              funcs <- parseFuncs lineNo funcsRaw
              pure $ Map.insert name (token, role, funcs) acc
            _ -> Left $ "Invalid auth line " ++ show lineNo ++ ": expected '<role> <client> <token> <func1,func2>'"
      where
        line = stripComment $ B.dropWhile isSpace rawLine

    stripComment bs =
      let (before, _) = B.break (== '#') bs
      in B.reverse $ B.dropWhile isSpace $ B.reverse before

    parseFuncs lineNo funcsRaw = do
      rules <- mapM (parseFunc lineNo) $ filter (not . B.null) $ B.split ',' funcsRaw
      if null rules
        then Left $ "Invalid auth line " ++ show lineNo ++ ": empty func list"
        else pure $ foldr insertFuncAuthRule emptyFuncAuthRule rules

    parseFunc lineNo raw = do
      case B.unsnoc raw of
        Just (prefix, '*') -> parseFuncPrefix lineNo prefix
        _                 ->
          case B.uncons raw of
            Just ('*', suffix) -> parseFuncSuffix lineNo suffix
            _                  -> parseExactFunc lineNo raw

    parseExactFunc lineNo raw = do
      if B.any (== '*') raw
        then Left $ "Invalid func on auth line " ++ show lineNo ++ ": wildcard is only supported at the start or end"
        else do
          let fn = FuncName raw
          case validate fn of
            Left err -> Left $ "Invalid func on auth line " ++ show lineNo ++ ": " ++ err
            Right () -> pure $ AuthExact fn

    parseFuncPrefix lineNo prefix
      | B.null prefix = Left $ "Invalid func on auth line " ++ show lineNo ++ ": prefix wildcard must not be empty"
      | B.any (== '*') prefix = Left $ "Invalid func on auth line " ++ show lineNo ++ ": wildcard is only supported at the start or end"
      | otherwise =
          case validate (FuncName prefix) of
            Left err -> Left $ "Invalid func on auth line " ++ show lineNo ++ ": " ++ err
            Right () -> pure $ AuthPrefix prefix

    parseFuncSuffix lineNo suffix
      | B.null suffix = Left $ "Invalid func on auth line " ++ show lineNo ++ ": suffix wildcard must not be empty"
      | B.any (== '*') suffix = Left $ "Invalid func on auth line " ++ show lineNo ++ ": wildcard is only supported at the start or end"
      | otherwise =
          case validate (FuncName suffix) of
            Left err -> Left $ "Invalid func on auth line " ++ show lineNo ++ ": " ++ err
            Right () -> pure $ AuthSuffix suffix

    emptyFuncAuthRule = FuncAuthRule Set.empty Set.empty Set.empty

    insertFuncAuthRule (AuthExact fn) rule =
      rule { exactFuncs = Set.insert fn $ exactFuncs rule }
    insertFuncAuthRule (AuthPrefix prefix) rule =
      rule { prefixFuncs = Set.insert prefix $ prefixFuncs rule }
    insertFuncAuthRule (AuthSuffix suffix) rule =
      rule { suffixFuncs = Set.insert suffix $ suffixFuncs rule }

    parseRole lineNo raw =
      case raw of
        "client" -> Right "client"
        "worker" -> Right "worker"
        "admin" -> Right "admin"
        _ -> Left $ "Invalid role on auth line " ++ show lineNo ++ ": expected 'client', 'worker', or 'admin'"

    validateName what lineNo raw =
      if B.null raw
        then Left $ "Invalid auth line " ++ show lineNo ++ ": empty " ++ what
        else pure ()

    isSpace c = c == ' ' || c == '\t' || c == '\r'
