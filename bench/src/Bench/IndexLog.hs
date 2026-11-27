-- | Reads the product's verbose "indexing: done" log line (D-164), and
-- the diff-mode lines (D-358).
module Bench.IndexLog
  ( -- * Types
    IndexDone (..),
    ChangedLine (..),
    PairingLine (..),
    WorkTreeLine (..),
    DiffLog (..),

    -- * Parsing
    parseDoneLine,
    parseChangedLine,
    parsePairingLine,
    parseWorkTreeLine,
    noDiffLog,
    addDiffLine,
  )
where

import Control.Applicative ((<|>))
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
    afterMarker = after "indexing: done, "

-- | > changed: 56 on head, 55 on base, indexed first
data ChangedLine = ChangedLine
  { head :: Int,
    base :: Int
  }
  deriving (Eq, Show)

-- | > pairing: 154 changed, 0 renamed in 12 ms
--
-- With ", over the rename limit" (or work budget) before " in". The
-- time is whole ms: not kept, the @pairing@ trace span has it exactly.
data PairingLine = PairingLine
  { changed :: Int,
    renamed :: Int,
    -- | The text after the counts, e.g. "over the rename limit".
    limit :: Maybe String
  }
  deriving (Eq, Show)

-- | > files: working tree: 69 entries, 0 hashed, 0 read from disk
--
-- More than 0 hashed on a clean checkout: the index was not fresh.
data WorkTreeLine = WorkTreeLine
  { entries :: Int,
    hashed :: Int,
    fromDisk :: Int
  }
  deriving (Eq, Show)

-- | 'Nothing' for any other line.
parseChangedLine :: String -> Maybe ChangedLine
parseChangedLine l = do
  r0 <- after "changed: " l
  (h, r1) <- number r0
  r2 <- stripPrefix " on head, " r1
  (b, r3) <- number r2
  if r3 == " on base, indexed first"
    then Just ChangedLine {head = h, base = b}
    else Nothing

-- | 'Nothing' for any other line.
parsePairingLine :: String -> Maybe PairingLine
parsePairingLine l = do
  r0 <- after "pairing: " l
  (n, r1) <- number r0
  r2 <- stripPrefix " changed, " r1
  (r, r3) <- number r2
  r4 <- stripPrefix " renamed" r3
  (lim, r5) <- case stripPrefix ", " r4 of
    Just rest -> case breakOn " in " rest of
      (x, Just r') | not (null x) -> Just (Just x, r')
      _ -> Nothing
    Nothing -> (Nothing,) <$> stripPrefix " in " r4
  (_ :: Integer, r6) <- number r5
  if r6 == " ms"
    then Just PairingLine {changed = n, renamed = r, limit = lim}
    else Nothing
  where
    breakOn sep s = case [i | (i, t) <- zip [0 ..] (tails s), sep `isPrefixOf` t] of
      (i : _) -> (take i s, Just (drop (i + length sep) s))
      [] -> (s, Nothing)

-- | 'Nothing' for any other line.
parseWorkTreeLine :: String -> Maybe WorkTreeLine
parseWorkTreeLine l = do
  r0 <- after "files: working tree: " l
  (n, r1) <- number r0
  r2 <- stripPrefix " entries, " r1
  (h, r3) <- number r2
  r4 <- stripPrefix " hashed, " r3
  (d, r5) <- number r4
  if r5 == " read from disk"
    then Just WorkTreeLine {entries = n, hashed = h, fromDisk = d}
    else Nothing

-- | The diff-mode lines of one run: the first of each kind. Strict, so
-- adding a line doesn't keep every line seen in a thunk.
data DiffLog = DiffLog
  { changed :: !(Maybe ChangedLine),
    pairing :: !(Maybe PairingLine),
    workTree :: !(Maybe WorkTreeLine)
  }
  deriving (Eq, Show)

-- | No line seen yet.
noDiffLog :: DiffLog
noDiffLog = DiffLog {changed = Nothing, pairing = Nothing, workTree = Nothing}

-- | Keep the line if it is one of ours and the first of its kind.
addDiffLine :: String -> DiffLog -> DiffLog
addDiffLine l d =
  DiffLog
    { changed = d.changed <|> parseChangedLine l,
      pairing = d.pairing <|> parsePairingLine l,
      workTree = d.workTree <|> parseWorkTreeLine l
    }

-- | The rest after the first @marker@ in the line (a log prefix is
-- fine).
after :: String -> String -> Maybe String
after marker s = case [drop (length marker) t | t <- tails s, marker `isPrefixOf` t] of
  (r : _) -> Just r
  [] -> Nothing

number :: (Read a) => String -> Maybe (a, String)
number s = case span isDigit s of
  ("", _) -> Nothing
  (ds, r) -> (\x -> (x, r)) <$> readMaybe ds
