module Periodic.Job
  ( JobM
  , module Periodic.Trans.Job
  ) where

import           Metro.TP.Socket    (Socket)
import           Periodic.Trans.Job hiding (JobT)
import qualified Periodic.Trans.Job as J (JobT)

type JobM = J.JobT Socket IO
