-- | Clock handle: wall time and monotonic time.
module HaskellReviewer.Effects.Clock
  ( -- * Types
    Clock (..),
    MonotonicTime (..),
    Nanoseconds (..),
    Microseconds (..),

    -- * Handles
    systemClock,

    -- * Operations
    elapsed,
  )
where

import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

-- | Where the program gets the time from. Tests plug in a fake.
data Clock = Clock
  { -- | Wall clock time. For timestamps, not for durations.
    now :: IO UTCTime,
    -- | Monotonic time. For durations.
    monotonic :: IO MonotonicTime
  }

-- | Nanoseconds since some fixed, unspecified point.
newtype MonotonicTime = MonotonicTime Word64
  deriving (Eq, Ord, Show)

-- | A duration in nanoseconds. A newtype so it can't be mixed up with
-- other units.
newtype Nanoseconds = Nanoseconds Word64
  deriving (Eq, Ord, Show)

-- | A duration in microseconds, the unit of 'threadDelay' and
-- 'Control.Concurrent.STM.registerDelay'. An 'Int', like theirs.
newtype Microseconds = Microseconds Int
  deriving (Eq, Ord, Show)

-- | The real clock.
systemClock :: Clock
systemClock =
  Clock
    { now = getCurrentTime,
      monotonic = MonotonicTime <$> getMonotonicTimeNSec
    }

-- | Time from the first point to the second. Zero if the second is
-- earlier (it shouldn't be, but a fake clock might do it).
elapsed :: MonotonicTime -> MonotonicTime -> Nanoseconds
elapsed (MonotonicTime t0) (MonotonicTime t1)
  | t1 >= t0 = Nanoseconds (t1 - t0)
  | otherwise = Nanoseconds 0
