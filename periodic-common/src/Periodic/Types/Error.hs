module Periodic.Types.Error
  (
    Error (..)
  ) where

import           Control.Exception (Exception)

data Error = MagicNotMatch
           | TransportClosed
           | TransportTimeout
           | DataTooLarge
           | EmptyError

  deriving (Show, Eq, Ord)

instance Exception Error
