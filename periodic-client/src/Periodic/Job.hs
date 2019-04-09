module Periodic.Job
  ( JobM
  , module Periodic.Trans.Job
  ) where

import           Periodic.Trans.Job        hiding (JobT)
import qualified Periodic.Trans.Job        as J (JobT)
import           Periodic.Transport.Socket (Socket)

type JobM = J.JobT Socket IO
