{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.ClientType
  ( ClientType (..)
  , ClientIdentity (..)
  ) where

import           Data.Binary
import           Data.ByteString (ByteString)

data ClientIdentity = ClientIdentity
  { clientName  :: ByteString
  , clientToken :: ByteString
  }
  deriving (Eq, Show)

instance Binary ClientIdentity where
  get = ClientIdentity <$> get <*> get
  put (ClientIdentity n t) = put n >> put t

data ClientType = TypeClient
    | TypeWorker
    | TypeAuthClient ClientIdentity
    | TypeAuthWorker ClientIdentity
    deriving (Eq, Show)

instance Binary ClientType where
  get = do
    tp <- getWord8
    case tp of
      1 -> pure TypeClient
      2 -> pure TypeWorker
      3 -> TypeAuthClient <$> get
      4 -> TypeAuthWorker <$> get
      _ -> fail $ "Error ClientType " ++ show tp

  put TypeClient = putWord8 1
  put TypeWorker = putWord8 2
  put (TypeAuthClient ident) = putWord8 3 >> put ident
  put (TypeAuthWorker ident) = putWord8 4 >> put ident
