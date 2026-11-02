-- | Summary statistics for the benchmark: median, min and max over
-- repeated runs, and median, p95 and max over pooled request times.
module Bench.Stats
  ( Summary (..),
    summarize,
    Latency (..),
    latency,
    percentile,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

-- | Min, median and max of one field across a project's runs.
data Summary = Summary
  { median :: Double,
    min :: Double,
    max :: Double
  }
  deriving (Eq, Show)

-- | Summarise a non-empty list of samples. Even count: the mean of the
-- middle two (no single sample to call "the" median then).
summarize :: NonEmpty Double -> Summary
summarize samples =
  Summary
    { median = medianOf sorted,
      min = NE.head sorted,
      max = NE.last sorted
    }
  where
    sorted = NE.sort samples

-- | Request times pooled over runs: how many, median, p95, max.
data Latency = Latency
  { count :: Int,
    median :: Double,
    p95 :: Double,
    max :: Double
  }
  deriving (Eq, Show)

-- | 'Latency' of some samples. The median is the same as 'summarize''s.
latency :: NonEmpty Double -> Latency
latency samples =
  Latency
    { count = NE.length samples,
      median = medianOf sorted,
      p95 = percentile 95 samples,
      max = NE.last sorted
    }
  where
    sorted = NE.sort samples

-- | Nearest rank percentile: the smallest sample with at least @p@
-- percent of the samples at or below it. Always one of the samples, no
-- interpolation. @p@ is clamped to 1..100.
percentile :: Int -> NonEmpty Double -> Double
percentile p samples = NE.toList sorted !! (rank - 1)
  where
    sorted = NE.sort samples
    n = NE.length samples
    p' = Prelude.max 1 (Prelude.min 100 p)
    -- ceiling (p' * n / 100), in integers.
    rank = Prelude.max 1 ((p' * n + 99) `div` 100)

medianOf :: NonEmpty Double -> Double
medianOf sorted
  | odd n = xs !! mid
  | otherwise = (xs !! (mid - 1) + xs !! mid) / 2
  where
    xs = NE.toList sorted
    n = length xs
    mid = n `div` 2
