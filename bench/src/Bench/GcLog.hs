-- | Parses the file GHC's RTS writes for @+RTS -S\<file\>@: one line per
-- garbage collection, then the @-s@ summary. @-t@ (see "Bench.RtsStats")
-- has no pause times, this has.
--
-- Same format on GHC 9.10, 9.12 and 9.14, copying and nonmoving GC.
module Bench.GcLog
  ( -- * Types
    GcLog (..),
    Pause (..),
    PauseKind (..),
    GenTotal (..),
    GcLogError (..),
    GcRun (..),
    GcSummary (..),

    -- * Parsing
    parseGcLog,
    renderGcLogError,

    -- * Summary
    summarizeGc,
  )
where

import Bench.Stats (percentile)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

{- Note [Reading -S]

A collection line has nine numbers and the generation:

>    Alloc    Copied     Live     GC     GC      TOT      TOT  Page Flts
>    bytes     bytes     bytes   user   elap     user     elap
>  1109608    640896    665488  0.000  0.000    0.002    0.001    0    0  (Gen:  0)

"GC elap" is the pause, "TOT elap" the time since the RTS started (so
roughly since the process started). Both in seconds, 1 ms steps.

The nonmoving GC adds "# sync  0.000" lines: the short pause at the end
of a concurrent mark. They have no timestamp, so we give them the one of
the line before.

The summary at the end has the max pause per generation, in 0.1 ms
steps:

>  Gen  0       146 colls,   146 par    0.046s   0.046s     0.0003s    0.0009s
>  Gen  1         4 syncs,                       0.000s     0.0001s    0.0001s
>  Gen  1      concurrent,              0.036s   0.082s     0.0205s    0.0462s

The "concurrent" line is work done next to the program, not a pause:
skipped. The summary is missing when the process didn't exit normally.
-}

-- * Types

-- | What we read from a @-S@ file. See Note [Reading -S].
data GcLog = GcLog
  { pauses :: [Pause],
    totals :: [GenTotal]
  }
  deriving (Eq, Show)

-- | One stop of the program for the GC.
data Pause = Pause
  { kind :: PauseKind,
    -- | 0 is the nursery; 1 and up are major collections.
    gen :: Int,
    durationUs :: Integer,
    -- | When it ended, since the RTS started.
    atUs :: Integer
  }
  deriving (Eq, Show)

-- | A normal collection, or the sync at the end of a nonmoving mark.
data PauseKind = Collection | Sync
  deriving (Eq, Show)

-- | A summary line: collections (or syncs) of one generation.
data GenTotal = GenTotal
  { kind :: PauseKind,
    gen :: Int,
    count :: Int,
    maxPauseUs :: Integer
  }
  deriving (Eq, Show)

-- | Why a @-S@ file can't be used.
data GcLogError
  = -- | Not one collection line: wrong file, or the process died at once.
    NoGcLines
  deriving (Eq, Show)

-- | A message for the user.
renderGcLogError :: GcLogError -> String
renderGcLogError NoGcLines = "no GC lines in the -S file"

-- | One run's log, and when indexing ended in it (since the process
-- started).
data GcRun = GcRun
  { doneUs :: Integer,
    gcLog :: GcLog
  }
  deriving (Eq, Show)

-- | Pauses of one or more runs, for the report.
data GcSummary = GcSummary
  { collections :: Int,
    -- | Collections of generation 1 and up.
    majors :: Int,
    maxPauseUs :: Integer,
    -- | 0 if there was no major collection.
    maxMajorPauseUs :: Integer,
    p99PauseUs :: Integer,
    -- | Max pause up to the end of indexing, and after it. 'Nothing':
    -- no pause there.
    maxDuringUs :: Maybe Integer,
    maxAfterUs :: Maybe Integer,
    majorsAfter :: Int,
    -- | Max pause of a major collection after indexing, from the
    -- per-GC lines. The Q-GC rule's input (plan §4).
    maxMajorAfterUs :: Maybe Integer,
    -- | Sum of all pauses.
    pausedUs :: Integer
  }
  deriving (Eq, Show)

-- * Parsing

-- | Parse a whole file. Lines we don't know are skipped; a file with no
-- collection line at all is an error (wrong file, or not @-S@).
parseGcLog :: String -> Either GcLogError GcLog
parseGcLog raw
  | null collections = Left NoGcLines
  | otherwise = Right GcLog {pauses = ps, totals = mapMaybe totalLine ls}
  where
    ls = lines raw
    ps = withTimes 0 (mapMaybe pauseLine ls)
    collections = [() | Pause {kind = Collection} <- ps]
    -- Syncs take the time of the line before.
    withTimes _ [] = []
    withTimes prev (p : rest) = case p.kind of
      Sync -> p {atUs = prev} : withTimes prev rest
      Collection -> p : withTimes p.atUs rest

-- | A collection or sync line. Syncs get 'atUs' 0 here.
pauseLine :: String -> Maybe Pause
pauseLine l = case words l of
  ["#", "sync", secs] -> do
    d <- seconds secs
    Just Pause {kind = Sync, gen = 1, durationUs = d, atUs = 0}
  ws
    | [_, _, _, _, elap, _, tot, _, _, "(Gen:", genClose] <- ws,
      Just g <- readMaybe (takeWhile (/= ')') genClose) -> do
        d <- seconds elap
        at <- seconds tot
        Just Pause {kind = Collection, gen = g, durationUs = d, atUs = at}
  _ -> Nothing

-- | A summary line with a max pause.
totalLine :: String -> Maybe GenTotal
totalLine l = case words l of
  ("Gen" : g : n : what : rest)
    | not (null rest) -> do
        gen <- readMaybe g
        count <- readMaybe n
        kind <- case what of
          "colls," -> Just Collection
          "syncs," -> Just Sync
          _ -> Nothing
        maxPause <- seconds (dropEnd 's' (last rest))
        Just GenTotal {kind = kind, gen = gen, count = count, maxPauseUs = maxPause}
  _ -> Nothing
  where
    dropEnd c s = if not (null s) && last s == c then init s else s

-- | Seconds (@"0.0009"@) as whole microseconds.
seconds :: String -> Maybe Integer
seconds s = do
  d <- readMaybe s :: Maybe Double
  Just (round (d * 1_000_000))

-- * Summary

-- | Pool the pauses of some runs. Each run comes with the time indexing
-- ended (since the process started): a pause that ended at or before
-- it counts as "during". 'Nothing' if there's no pause at all.
--
-- The max pause also looks at the summary lines, which are more
-- precise (Note [Reading -S]); the p99 and the during\/after split only
-- have the per-line times.
summarizeGc :: [GcRun] -> Maybe GcSummary
summarizeGc runs = do
  all' <- NE.nonEmpty (map fromInteger durations)
  Just
    GcSummary
      { collections = length colls,
        majors = length majorColls,
        maxPauseUs = maximum (durations <> totalMaxes),
        maxMajorPauseUs = maximum (0 : map (.durationUs) majorColls <> majorTotals),
        p99PauseUs = round (percentile 99 all' :: Double),
        maxDuringUs = maxOf [p.durationUs | (d, p) <- tagged, p.atUs <= d],
        maxAfterUs = maxOf [p.durationUs | (d, p) <- tagged, p.atUs > d],
        majorsAfter = length majorsAfterDone,
        maxMajorAfterUs = maxOf (map (.durationUs) majorsAfterDone),
        pausedUs = sum durations
      }
  where
    tagged = [(r.doneUs, p) | r <- runs, p <- r.gcLog.pauses]
    majorsAfterDone = [p | (d, p) <- tagged, p.atUs > d, isMajor p]
    ps = map snd tagged
    durations = map (.durationUs) ps
    colls = [p | p <- ps, p.kind == Collection]
    majorColls = filter isMajor ps
    isMajor p = p.kind == Collection && p.gen >= 1
    totals = [t | r <- runs, t <- r.gcLog.totals]
    totalMaxes = map (.maxPauseUs) totals
    majorTotals = [t.maxPauseUs | t <- totals, t.kind == Collection, t.gen >= 1]
    maxOf [] = Nothing
    maxOf xs = Just (maximum xs)
