-- | Summary statistics (median, min, max) over repeated benchmark runs.
module Bench.Stats
  ( Summary (..),
    summarize,
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

medianOf :: NonEmpty Double -> Double
medianOf sorted
  | odd n = xs !! mid
  | otherwise = (xs !! (mid - 1) + xs !! mid) / 2
  where
    xs = NE.toList sorted
    n = length xs
    mid = n `div` 2
