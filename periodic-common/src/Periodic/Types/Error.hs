module Periodic.Types.Error
  ( Error (..)
  ) where

import           Control.Exception (Exception)

data Error = MagicNotMatch
    | PacketDecodeError String
    | TransportClosed
    | TransportTimeout
    | DataTooLarge
    | InValidError String
    | CRCNotMatch
    | EmptyError
    deriving (Show, Eq, Ord)

instance Exception Error
