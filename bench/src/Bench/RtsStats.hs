-- | Parses the file GHC's RTS writes for
-- @+RTS -t\<file\> --machine-readable -RTS@.
--
-- That file is a Haskell @[(String, String)]@ 'show', plus one extra line
-- in front (the echoed command line) that we skip.
--
-- The RTS has one stats file: @-t@, @-s@ and @-S@ all write to it, and
-- the last one given wins. So a run with @-S@ (GC pauses, see
-- "Bench.GcLog") has no @-t@ file; 'parseRtsSummary' reads the same
-- numbers from the human summary at the end of the @-S@ file instead.
module Bench.RtsStats
  ( -- * Types
    RtsStats (..),
    RtsStatsError (..),

    -- * Parsing
    parseRtsStats,
    parseRtsSummary,
    renderRtsStatsError,

    -- * User options
    checkUserRtsOpts,
  )
where

import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
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

-- | Parse the @-s@ style summary at the end of a @-S@ file:
--
-- >      163,884,200 bytes allocated in the heap
-- >       20,740,984 bytes maximum residency (12 sample(s))
-- >               58 MiB total memory in use (0 MiB lost due to fragmentation)
-- >   MUT     time    0.019s  (  0.019s elapsed)
-- >   GC      time    0.096s  (  0.074s elapsed)
--
-- The peak is in whole MiB here, not bytes.
parseRtsSummary :: String -> Either RtsStatsError RtsStats
parseRtsSummary raw = do
  let peakKey = "MiB total memory in use"
  peakMiB <- maybe (Left (MissingKey peakKey)) Right (bytesBefore peakKey)
  let peak = peakMiB * 1024 * 1024
  if peak < oneMiB || peak > sixteenGiB
    then Left (PeakOutOfRange peak)
    else
      Right
        RtsStats
          { peakMemBytes = peak,
            maxLiveBytes = bytesBefore "bytes maximum residency",
            allocatedBytes = bytesBefore "bytes allocated in the heap",
            gcCpuUs = timeOf "GC",
            mutatorCpuUs = timeOf "MUT"
          }
  where
    ls = lines raw
    -- The number in front of the text, commas dropped.
    bytesBefore text =
      case [w | l <- ls, text `isInfixOf` l, w : _ <- [words l]] of
        w : _ -> readMaybe (filter (/= ',') w)
        [] -> Nothing
    -- "MUT     time    0.019s  (  0.019s elapsed)": the first time.
    timeOf name =
      case [t | l <- ls, name' : "time" : t : _ <- [words l], name' == name] of
        t : _ -> do
          d <- readMaybe (takeWhile (/= 's') t) :: Maybe Double
          Just (round (d * 1_000_000))
        [] -> Nothing

oneMiB, sixteenGiB :: Integer
oneMiB = 1024 * 1024
sixteenGiB = 16 * 1024 * 1024 * 1024

-- | Refuse user @+RTS@ options that would move or change the stats
-- file we read (@-s@, @-S@, @-t@, @--machine-readable@): the run would
-- then fail with a confusing parse error, or read the wrong numbers.
-- Also @+RTS@\/@-RTS@, which would end our own block.
checkUserRtsOpts :: [String] -> Either String ()
checkUserRtsOpts opts = case filter bad opts of
  [] -> Right ()
  o : _ ->
    Left
      ( "--rts: "
          <> o
          <> " is not allowed: the bench sets the RTS stats file itself (-t or -S)"
      )
  where
    bad o =
      any (`isPrefixOf` o) ["-s", "-S", "-t", "--machine-readable"]
        || o `elem` ["+RTS", "-RTS"]

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
