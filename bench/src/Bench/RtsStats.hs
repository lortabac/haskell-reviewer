-- | Parses the file GHC's RTS writes for
-- @+RTS -t\<file\> --machine-readable -RTS@.
--
-- That file is a Haskell @[(String, String)]@ 'show', plus one extra line
-- in front (the echoed command line) that we skip.
module Bench.RtsStats
  ( -- * Types
    RtsStats (..),
    RtsStatsError (..),

    -- * Parsing
    parseRtsStats,
    renderRtsStatsError,
  )
where

import Data.Char (isSpace)
import Text.Read (readMaybe)

{- Note [What this memory excludes]

'peakMemBytes' and the other fields here come from the GHC RTS, so they
only cover the GHC heap. They miss two things: non-heap C memory inside
the binary, and anything used by child processes (git). There's no C
helper to read real (RSS) memory instead, so this is a known gap, not an
oversight.
-}

-- * Types

-- | The GHC heap stats we care about from one run. See
-- Note [What this memory excludes].
data RtsStats = RtsStats
  { -- | @max_mem_in_use_bytes@: the peak we report.
    peakMemBytes :: Integer,
    maxLiveBytes :: Maybe Integer,
    allocatedBytes :: Maybe Integer,
    gcCpuUs :: Maybe Integer,
    mutatorCpuUs :: Maybe Integer
  }
  deriving (Eq, Show)

-- | What went wrong reading the stats file.
data RtsStatsError
  = CannotParse String
  | MissingKey String
  | -- | 'peakMemBytes' outside the 1 MiB-16 GiB sanity range.
    PeakOutOfRange Integer
  deriving (Eq, Show)

-- | A message for the user.
renderRtsStatsError :: RtsStatsError -> String
renderRtsStatsError (CannotParse why) = "cannot parse RTS stats: " <> why
renderRtsStatsError (MissingKey k) = "RTS stats: missing key " <> show k
renderRtsStatsError (PeakOutOfRange n) =
  "RTS stats: peak memory "
    <> show n
    <> " bytes is outside the sane 1 MiB-16 GiB range"

-- * Parsing

-- | Parse the content of a @-t ... --machine-readable@ file.
parseRtsStats :: String -> Either RtsStatsError RtsStats
parseRtsStats raw = do
  pairs <- parsePairs raw
  peak <- require pairs "max_mem_in_use_bytes"
  if peak < oneMiB || peak > sixteenGiB
    then Left (PeakOutOfRange peak)
    else
      Right
        RtsStats
          { peakMemBytes = peak,
            maxLiveBytes = optional pairs "max_live_bytes",
            allocatedBytes = optional pairs "allocated_bytes",
            gcCpuUs = optionalSeconds pairs "GC_cpu_seconds",
            mutatorCpuUs = optionalSeconds pairs "mut_cpu_seconds"
          }
  where
    oneMiB = 1024 * 1024
    sixteenGiB = 16 * 1024 * 1024 * 1024

-- * Internal

-- | The list literal starts after the first line (the echoed command),
-- so we skip to the first @[@ and 'read' from there.
parsePairs :: String -> Either RtsStatsError [(String, String)]
parsePairs raw = case break (== '[') raw of
  (_, rest@('[' : _)) -> case reads rest of
    [(xs, trailing)] | all isSpace trailing -> Right xs
    _ -> Left (CannotParse "bad list literal")
  _ -> Left (CannotParse "no '[' found")

require :: [(String, String)] -> String -> Either RtsStatsError Integer
require pairs key = case lookup key pairs >>= readMaybe of
  Just n -> Right n
  Nothing -> Left (MissingKey key)

optional :: [(String, String)] -> String -> Maybe Integer
optional pairs key = lookup key pairs >>= readMaybe

-- | A seconds value (e.g. @"0.000139"@), as whole microseconds.
optionalSeconds :: [(String, String)] -> String -> Maybe Integer
optionalSeconds pairs key = do
  s <- lookup key pairs
  d <- readMaybe s :: Maybe Double
  pure (round (d * 1_000_000))
