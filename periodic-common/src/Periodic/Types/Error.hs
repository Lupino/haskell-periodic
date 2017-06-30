module Periodic.Types.Error
  (
    Error (..)
  ) where

import           Control.Exception (Exception)

data Error = MagicNotMatch
           | SocketClosed
           | SocketTimeout
           | DataTooLarge
           | EmptyError

  deriving (Show, Eq, Ord)

instance Exception Error
