-- | Find-references samples (D-303), and what the report and the
-- warnings make of them. Pure, so it can have unit tests.
module Bench.Refs
  ( -- * Types
    RefsTime (..),
    RefsSummary (..),

    -- * Sums
    byQuery,
    startedBefore,
    slowdownPercent,

    -- * Messages
    staleQueryMessage,
    refsWarnings,
  )
where

import Bench.Queries (RefsQuery, refsQueryText)
import Bench.Stats (Latency, latency)
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)

-- | One refs request.
data RefsTime = RefsTime
  { query :: RefsQuery,
    -- | From request to the last byte.
    us :: Integer,
    -- | When the request started, since the session started.
    startUs :: Integer,
    partial :: Bool,
    total :: Int,
    sent :: Int,
    wireBytes :: Int
  }
  deriving (Eq, Show)

-- | The samples of one query.
data RefsSummary = RefsSummary
  { query :: RefsQuery,
    latency :: Latency,
    -- | From the last sample: after indexing they don't change.
    total :: Int,
    sent :: Int,
    wireBytes :: Int
  }
  deriving (Eq, Show)

-- | One summary per query, in the order given. Queries with no sample
-- are left out.
byQuery :: [RefsQuery] -> [RefsTime] -> [RefsSummary]
byQuery qs ts = [s | q <- qs, Just s <- [one q]]
  where
    one q = do
      mine <- NE.nonEmpty [t | t <- ts, t.query == q]
      let lastOne = NE.last mine
      Just
        RefsSummary
          { query = q,
            latency = latency (NE.map (fromInteger . (.us)) mine),
            total = lastOne.total,
            sent = lastOne.sent,
            wireBytes = lastOne.wireBytes
          }

-- | Did the request start before the done line (at @doneUs@)? Then it
-- counts as "during indexing", even if the answer came later (the
-- on-demand rule, D-215).
startedBefore :: Integer -> RefsTime -> Bool
startedBefore doneUs t = t.startUs < doneUs

-- | How much slower indexing was, in percent: @+14@ if @slow@ is 14%
-- over @base@. 'Nothing' if @base@ is not positive.
slowdownPercent :: Double -> Double -> Maybe Double
slowdownPercent base slow
  | base <= 0 = Nothing
  | otherwise = Just ((slow / base - 1) * 100)

-- | The message for a refs query that got a 404 (with the body's
-- @what@, if any): the list is stale for the pin.
staleQueryMessage :: RefsQuery -> Maybe Text -> Text
staleQueryMessage q what =
  "refs query "
    <> refsQueryText q
    <> ": 404 (what: "
    <> fromMaybe "?" what
    <> "); update refsQueries in bench/src/Bench/Queries.hs"

-- | Refs answers after indexing that look wrong: still partial, or no
-- ref at all (the declaration's own binding always counts). Once per
-- query.
refsWarnings :: [RefsTime] -> [Text]
refsWarnings ts =
  [ "refs query " <> refsQueryText q <> ": partial after indexing"
  | q <- nub [t.query | t <- ts, t.partial]
  ]
    <> [ "refs query " <> refsQueryText q <> ": total 0 after indexing"
       | q <- nub [t.query | t <- ts, t.total == 0]
       ]
