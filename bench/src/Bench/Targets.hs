-- | The SPEC §10.1 targets (and D-208's search, D-303's refs, D-308's
-- text search and D-358's diff targets), and whether a run met them.
-- Missing one never fails the bench (§10.3): the table says MISS and
-- the human decides.
module Bench.Targets
  ( -- * Types
    Stat (..),
    Bound (..),
    Target (..),
    Verdict (..),

    -- * The targets
    urlAndAccepting,
    uiUsable,
    onDemandModule,
    navigation,
    navigationIdle,
    navigationSource,
    navigationSourceIdle,
    searchAfterMedian,
    searchAfterP95,
    refsAfterP95,
    textSearchAfterMedian,
    textSearchAfterP95,
    diffAfterP95,
    memoryBudgetBytes,

    -- * Verdicts
    verdict,
    verdictOf,
    sideVerdicts,
    renderVerdict,
    statText,
  )
where

import Bench.Mode (Side)
import Bench.Projects (Role (..))
import Bench.Stats (Latency (..), Summary (..), latency, summarize)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Printf (printf)

-- * Types

-- | Which number of the samples is checked.
data Stat = Median | P95
  deriving (Eq, Show)

-- | "< limit" (SPEC §10.1) or "≤ limit" (D-208).
data Bound = Below | AtMost
  deriving (Eq, Show)

-- | A §10.1 (or D-208) target: which statistic, under what limit.
data Target = Target
  { -- | For the JSON report.
    key :: Text,
    -- | For the table, e.g. "§10.1 UI usable < 1 s".
    label :: Text,
    stat :: Stat,
    bound :: Bound,
    limitUs :: Integer
  }
  deriving (Eq, Show)

-- | A target, the value we got, and whether it's met.
data Verdict = Verdict
  { target :: Target,
    -- | Which side the samples are from, in diff mode. 'Nothing' in
    -- browse mode, and for the start-up targets.
    side :: Maybe Side,
    valueUs :: Double,
    -- | The worst sample, shown next to the value. 'Nothing' if the
    -- value is already the max.
    maxUs :: Maybe Double,
    met :: Bool
  }
  deriving (Eq, Show)

-- * The targets

-- | URL printed and the server answering.
urlAndAccepting :: Target
urlAndAccepting =
  Target
    { key = "url_and_accepting",
      label = "§10.1 URL + accepting < 300 ms",
      stat = Median,
      bound = Below,
      limitUs = 300_000
    }

-- | 10 s for the large project, else 1 s. The small project is under
-- 300 modules, so the typical limit is the one that applies.
uiUsable :: Role -> Target
uiUsable role =
  Target
    { key = "ui_usable",
      label = "§10.1 UI usable < " <> (if large then "10 s" else "1 s"),
      stat = Median,
      bound = Below,
      limitUs = if large then 10_000_000 else 1_000_000
    }
  where
    large = role == Large

-- | A module page asked for while indexing (it jumps the queue).
onDemandModule :: Target
onDemandModule =
  Target
    { key = "on_demand_module",
      label = "§10.1 on-demand module < 300 ms",
      stat = P95,
      bound = Below,
      limitUs = 300_000
    }

-- | Every module once, back to back, after indexing.
navigation :: Target
navigation =
  Target
    { key = "navigation",
      label = "§10.1 navigation < 100 ms (back to back)",
      stat = P95,
      bound = Below,
      limitUs = 100_000
    }

-- | Pages with 0.2–1 s gaps, so idle and major GCs can get in the way.
navigationIdle :: Target
navigationIdle =
  Target
    { key = "navigation_idle",
      label = "§10.1 navigation < 100 ms (idle gaps)",
      stat = P95,
      bound = Below,
      limitUs = 100_000
    }

-- | Every source page once, back to back, after indexing. Apart from
-- 'navigation', so the module pages stay comparable with M1.
navigationSource :: Target
navigationSource =
  Target
    { key = "navigation_source",
      label = "§10.1 navigation < 100 ms (source, back to back)",
      stat = P95,
      bound = Below,
      limitUs = 100_000
    }

-- | Source pages with the same gaps as 'navigationIdle'.
navigationSourceIdle :: Target
navigationSourceIdle =
  Target
    { key = "navigation_source_idle",
      label = "§10.1 navigation < 100 ms (source, idle gaps)",
      stat = P95,
      bound = Below,
      limitUs = 100_000
    }

-- | Search after indexing, median.
searchAfterMedian :: Target
searchAfterMedian =
  Target
    { key = "search_after_median",
      label = "D-208 search after indexing ≤ 30 ms",
      stat = Median,
      bound = AtMost,
      limitUs = 30_000
    }

-- | Search after indexing, p95.
searchAfterP95 :: Target
searchAfterP95 =
  Target
    { key = "search_after_p95",
      label = "D-208 search after indexing ≤ 100 ms",
      stat = P95,
      bound = AtMost,
      limitUs = 100_000
    }

-- | Find references after indexing, pooled over the project's queries.
refsAfterP95 :: Target
refsAfterP95 =
  Target
    { key = "refs_after_p95",
      label = "D-303 find references after indexing < 300 ms",
      stat = P95,
      bound = Below,
      limitUs = 300_000
    }

-- | Text search after indexing, the store loaded, limit 8 (the box),
-- median over the text queries.
textSearchAfterMedian :: Target
textSearchAfterMedian =
  Target
    { key = "text_search_after_median",
      label = "D-308 text search after load ≤ 50 ms",
      stat = Median,
      bound = AtMost,
      limitUs = 50_000
    }

-- | The same, p95.
textSearchAfterP95 :: Target
textSearchAfterP95 =
  Target
    { key = "text_search_after_p95",
      label = "D-308 text search after load ≤ 150 ms",
      stat = P95,
      bound = AtMost,
      limitUs = 150_000
    }

-- | A changed file's diff after indexing (D-358): opening it is
-- navigation, so the navigation bound. Over the bench's sample of the
-- largest changed files.
diffAfterP95 :: Target
diffAfterP95 =
  Target
    { key = "diff_after_p95",
      label = "§10.1 diff after indexing < 100 ms",
      stat = P95,
      bound = Below,
      limitUs = 100_000
    }

-- | SPEC §10.2's budget (D-361), for the large project only.
memoryBudgetBytes :: Role -> Maybe Integer
memoryBudgetBytes Large = Just (2 * 1024 * 1024 * 1024)
memoryBudgetBytes _ = Nothing

-- * Verdicts

-- | Check a value (and the worst sample, shown next to it).
verdict :: Target -> Double -> Maybe Double -> Verdict
verdict t value worst =
  Verdict {target = t, side = Nothing, valueUs = value, maxUs = worst, met = ok}
  where
    limit = fromInteger t.limitUs
    ok = case t.bound of
      Below -> value < limit
      AtMost -> value <= limit

-- | The target's statistic over the samples (microseconds), with the
-- max. 'Nothing' without samples.
verdictOf :: Target -> [Integer] -> Maybe Verdict
verdictOf t xs = do
  ne <- NE.nonEmpty (map fromInteger xs)
  let value = case t.stat of
        Median -> (summarize ne).median
        P95 -> (latency ne).p95
  Just (verdict t value (Just (maximum ne)))

-- | One verdict per side, so a slow side can't hide behind the other
-- one (D-357). Sides without samples get none.
sideVerdicts :: Target -> [(Maybe Side, [Integer])] -> [Verdict]
sideVerdicts t perSide =
  [v {side = s} | (s, xs) <- perSide, Just v <- [verdictOf t xs]]

-- | One line of the table, e.g.
--
-- > §10.1 UI usable < 1 s (median): pandoc: ok (312.4 ms; max 340.1)
renderVerdict :: Text -> Verdict -> Text
renderVerdict project v =
  v.target.label
    <> " ("
    <> statText v.target.stat
    <> "): "
    <> project
    <> ": "
    <> (if v.met then "ok" else "MISS")
    <> " ("
    <> ms v.valueUs
    <> " ms"
    <> maybe "" (\m -> "; max " <> ms m) v.maxUs
    <> ")"
    <> (if v.met then "" else " → §10.3: record, decide, log")
  where
    ms us = Text.pack (printf "%.1f" (us / 1000))

-- | The name in the table and the JSON.
statText :: Stat -> Text
statText Median = "median"
statText P95 = "p95"
