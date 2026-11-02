-- | Reads the product's verbose "indexing: done" log line (D-164).
module Bench.IndexLog
  ( -- * Types
    IndexDone (..),

    -- * Parsing
    parseDoneLine,
  )
where

import Data.Char (isDigit)
import Data.List (isPrefixOf, stripPrefix, tails)
import Text.Read (readMaybe)

-- | What the line says:
--
-- > indexing: done, 1972 modules (1972 ready, 0 failed, 11 text only) in 1056 ms
data IndexDone = IndexDone
  { modules :: Int,
    ready :: Int,
    failed :: Int,
    -- | Ready, but only as text (parse error, preprocessed, too slow,
    -- not text, too large, ...).
    textOnly :: Int,
    -- | The product's own indexing time.
    ms :: Integer
  }
  deriving (Eq, Show)

-- | 'Nothing' for any other line. The line may have a prefix (a log
-- level, say): we look for @indexing: done, @ anywhere in it.
parseDoneLine :: String -> Maybe IndexDone
parseDoneLine l = do
  rest <- afterMarker l
  (n, r1) <- number rest
  r2 <- stripPrefix " modules (" r1
  (ready, r3) <- number r2
  r4 <- stripPrefix " ready, " r3
  (failed, r5) <- number r4
  r6 <- stripPrefix " failed, " r5
  (textOnly, r7) <- number r6
  r8 <- stripPrefix " text only) in " r7
  (ms, r9) <- number r8
  if r9 == " ms"
    then Just IndexDone {modules = n, ready, failed, textOnly, ms}
    else Nothing
  where
    marker = "indexing: done, "
    afterMarker s = case [drop (length marker) t | t <- tails s, marker `isPrefixOf` t] of
      (r : _) -> Just r
      [] -> Nothing
    number :: (Read a) => String -> Maybe (a, String)
    number s = case span isDigit s of
      ("", _) -> Nothing
      (ds, r) -> (\x -> (x, r)) <$> readMaybe ds
