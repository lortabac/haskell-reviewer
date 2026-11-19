{-# LANGUAGE MultiWayIf #-}

{- Part of the line diff: a port of git's change compaction and indent
heuristic (xdl_change_compact and its helpers in xdiff/xdiffi.c): git
2.44.1, plus the re-diff of moved groups that git 2.54.0 added.

Copyright (C) 2003 Davide Libenzi
Haskell port: Copyright (C) 2026 Lorenzo Tabacchini

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at
your option) any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this library; if not, see <http://www.gnu.org/licenses/>.

SPDX-License-Identifier: LGPL-2.1-or-later
-}

-- | git's compaction: slide groups of changed lines up and down, merge
-- the ones that touch, line them up with the other side's groups, and
-- else place them with the indent heuristic (on by default in git).
-- Names, constants and 0-based indexes are git's. See Note [Line diff]
-- in "HaskellReviewer.Core.LineDiff".
module HaskellReviewer.Core.LineDiff.Compact
  ( -- * Compaction
    Side (..),
    GitRule (..),
    changeCompact,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import HaskellReviewer.Core.LineDiff.Env
  ( Counter (..),
    DiffSide (..),
    Env (..),
    bump,
    isChanged,
    setChanged,
  )
import HaskellReviewer.Core.LineDiff.Myers (fallBackDiff)

-- | One side, as compaction sees it: lines, flags and texts.
data Side s = Side
  { diff :: DiffSide s,
    -- | The line texts, for indents.
    texts :: V.Vector Text
  }

-- | Which git to follow. git 2.54 added a step: a group that moved
-- (slid, merged or lined up) and faces a non-empty group on the other
-- side is diffed again with Myers, which can find equal lines inside.
data GitRule = Git254 | GitBefore254
  deriving (Eq, Show)

-- | git's @struct xdlgroup@: changed lines @start .. end - 1@. An empty
-- group sits just above line @start@.
data Group = Group {start :: !Int, end :: !Int}

nrec :: Side s -> Int
nrec x = VU.length x.diff.classes

recsMatch :: Side s -> Int -> Int -> Bool
recsMatch x i j = x.diff.classes VU.! i == x.diff.classes VU.! j

-- | Grow @end@ over changed lines.
skipChanged :: Side s -> Int -> ST s Int
skipChanged x i =
  isChanged x.diff.changed i >>= \c -> if c then skipChanged x (i + 1) else pure i

-- | Grow @start@ back over changed lines.
skipChangedBack :: Side s -> Int -> ST s Int
skipChangedBack x i =
  isChanged x.diff.changed (i - 1) >>= \c ->
    if c then skipChangedBack x (i - 1) else pure i

groupInit :: Side s -> ST s Group
groupInit x = Group 0 <$> skipChanged x 0

groupNext :: Side s -> Group -> ST s (Maybe Group)
groupNext x g
  | g.end == nrec x = pure Nothing
  | otherwise = Just . Group (g.end + 1) <$> skipChanged x (g.end + 1)

groupPrevious :: Side s -> Group -> ST s (Maybe Group)
groupPrevious x g
  | g.start == 0 = pure Nothing
  | otherwise = (\s -> Just (Group s (g.start - 1))) <$> skipChangedBack x (g.start - 1)

groupSlideDown :: Side s -> Group -> ST s (Maybe Group)
groupSlideDown x g
  | g.end < nrec x && recsMatch x g.start g.end = do
      setChanged x.diff.changed g.start False
      setChanged x.diff.changed g.end True
      Just . Group (g.start + 1) <$> skipChanged x (g.end + 1)
  | otherwise = pure Nothing

groupSlideUp :: Side s -> Group -> ST s (Maybe Group)
groupSlideUp x g
  | g.start > 0 && recsMatch x (g.start - 1) (g.end - 1) = do
      setChanged x.diff.changed (g.start - 1) True
      setChanged x.diff.changed (g.end - 1) False
      (\s -> Just (Group s (g.end - 1))) <$> skipChangedBack x (g.start - 1)
  | otherwise = pure Nothing

-- | A broken invariant: git calls @BUG()@ here.
bug :: String -> a
bug what = error ("LineDiff.Compact: " <> what)

-- | Step the other side's group too; it must exist.
must :: String -> ST s (Maybe Group) -> ST s Group
must what m = fromMaybe (bug what) <$> m

-- | git's @xdl_change_compact@ on side @x@, with @o@ the other side.
-- Run it on base, then on head.
changeCompact :: GitRule -> Env s -> Side s -> Side s -> ST s ()
changeCompact rule env x o = do
  g0 <- groupInit x
  go0 <- groupInit o
  loop g0 go0
  where
    loop g go = do
      (g', go') <- if g.end == g.start then pure (g, go) else compactGroup g go
      groupNext x g' >>= \case
        Nothing ->
          groupNext o go' >>= \case
            Nothing -> pure ()
            Just _ -> bug "group sync broken at end of file"
        Just gn -> do
          gon <- must "group sync broken moving to next group" (groupNext o go')
          loop gn gon

    counters = env.counters

    compactGroup g0 go0 = do
      (g1, go1, earliestEnd, endMatching) <- slideAll g0 go0
      (g, go) <-
        if
          | g1.end == earliestEnd -> pure (g1, go1)
          | endMatching /= -1 -> do
              bump counters Slides
              bump counters Alignments
              alignOther g1 go1
          | otherwise -> do
              bump counters Slides
              indentHeuristic g1 go1 earliestEnd
      -- git 2.54: diff a moved group again against the other side's.
      when
        ( rule == Git254
            && go.end /= go.start
            && (g.start /= g0.start || g.end /= g0.end)
        )
        $ do
          bump counters Rediffs
          fallBackDiff
            env
            x.diff
            o.diff
            (g.start + 1)
            (g.end - g.start)
            (go.start + 1)
            (go.end - go.start)
      pure (g, go)

    -- Slide up, then down as far as possible, merging, until the size
    -- stops changing.
    slideAll g0 go0 = do
      let groupsize = g0.end - g0.start
      (g1, go1) <- slideUpAll g0 go0
      let earliestEnd = g1.end
          endMatching0 = if go1.end > go1.start then g1.end else -1
      (g, go, endMatching) <- slideDownAll g1 go1 endMatching0
      if groupsize /= g.end - g.start
        then bump counters Merges >> slideAll g go
        else pure (g, go, earliestEnd, endMatching)

    slideUpAll g go =
      groupSlideUp x g >>= \case
        Nothing -> pure (g, go)
        Just g' -> do
          go' <- must "group sync broken sliding up" (groupPrevious o go)
          slideUpAll g' go'

    slideDownAll g go endMatching =
      groupSlideDown x g >>= \case
        Nothing -> pure (g, go, endMatching)
        Just g' -> do
          go' <- must "group sync broken sliding down" (groupNext o go)
          slideDownAll g' go' (if go'.end > go'.start then g'.end else endMatching)

    -- Back up until the other side's group is not empty.
    alignOther g go
      | go.end == go.start = do
          g' <- must "match disappeared" (groupSlideUp x g)
          go' <- must "group sync broken sliding to match" (groupPrevious o go)
          alignOther g' go'
      | otherwise = pure (g, go)

    indentHeuristic g go earliestEnd = do
      let groupsize = g.end - g.start
          shift0 = maximum [earliestEnd, g.end - groupsize - 1, g.end - maxSliding]
          scoreAt shift =
            scoreAddSplit
              (measureSplit x (shift - groupsize))
              (scoreAddSplit (measureSplit x shift) (Score 0 0))
          pick best shift
            | shift > g.end = best
            | otherwise =
                let sc = scoreAt shift
                 in case best of
                      Just (_, bs) | scoreCmp sc bs > 0 -> pick best (shift + 1)
                      _ -> pick (Just (shift, sc)) (shift + 1)
          bestShift = maybe g.end fst (pick Nothing shift0)
          up gx gox
            | gx.end > bestShift = do
                gx' <- must "best shift unreached" (groupSlideUp x gx)
                gox' <-
                  must "group sync broken sliding to blank line" (groupPrevious o gox)
                up gx' gox'
            | otherwise = pure (gx, gox)
      when (bestShift < g.end) (bump counters IndentShifts)
      up g go

-- | git's @INDENT_HEURISTIC_MAX_SLIDING@.
maxSliding :: Int
maxSliding = 100

-- * Indent heuristic

-- | git's @get_indent@: a tab goes to the next multiple of 8, CR and
-- LF count nothing, capped at 200; -1 for a blank line. Whitespace is
-- git's: space, tab, CR, LF (not VT or FF).
getIndent :: Text -> Int
getIndent = go 0
  where
    go ret t = case Text.uncons t of
      Nothing -> -1
      Just (c, rest)
        | c == ' ' -> step (ret + 1) rest
        | c == '\t' -> step (ret + 8 - ret `mod` 8) rest
        | c == '\r' || c == '\n' -> step ret rest
        | otherwise -> ret
    step ret rest = if ret >= 200 then 200 else go ret rest

-- | git's @struct split_measurement@.
data Measure = Measure
  { endOfFile :: !Bool,
    indent :: !Int,
    preBlank :: !Int,
    preIndent :: !Int,
    postBlank :: !Int,
    postIndent :: !Int
  }

-- | git's @measure_split@: the split just above line @s@.
measureSplit :: Side s -> Int -> Measure
measureSplit x s =
  Measure
    { endOfFile = s >= n,
      indent = if s >= n then -1 else ind s,
      preBlank = preB,
      preIndent = preI,
      postBlank = postB,
      postIndent = postI
    }
  where
    n = nrec x
    ind i = getIndent (x.texts V.! i)
    (preB, preI) = walk (>= 0) (subtract 1) (s - 1)
    (postB, postI) = walk (< n) (+ 1) (s + 1)
    -- Count blank lines from @i@ on; stop at a non-blank one (its
    -- indent), at 20 blanks (indent 0), or at the end (-1).
    walk inside next = go 0
      where
        go blanks i
          | not (inside i) = (blanks, -1)
          | v <- ind i, v /= -1 = (blanks, v)
          | blanks + 1 == maxBlanks = (blanks + 1, 0)
          | otherwise = go (blanks + 1) (next i)

-- | git's @MAX_BLANKS@.
maxBlanks :: Int
maxBlanks = 20

-- | git's @struct split_score@: smaller is better.
data Score = Score {effectiveIndent :: !Int, penalty :: !Int}

-- | git's @score_add_split@, with git's weights.
scoreAddSplit :: Measure -> Score -> Score
scoreAddSplit m s =
  Score
    { effectiveIndent = s.effectiveIndent + ind,
      penalty = s.penalty + startPen + eofPen + blankPen + relPen
    }
  where
    startPen = if m.preIndent == -1 && m.preBlank == 0 then 1 else 0
    eofPen = if m.endOfFile then 21 else 0
    postBlank = if m.indent == -1 then 1 + m.postBlank else 0
    totalBlank = m.preBlank + postBlank
    blankPen = -30 * totalBlank + 6 * postBlank
    ind = if m.indent /= -1 then m.indent else m.postIndent
    anyBlanks = totalBlank /= 0
    relPen
      | ind == -1 = 0
      | m.preIndent == -1 = 0
      | ind > m.preIndent = if anyBlanks then 10 else -4
      | ind == m.preIndent = 0
      | m.postIndent /= -1 && m.postIndent > ind = if anyBlanks then 17 else 24
      | otherwise = if anyBlanks then 17 else 23

-- | git's @score_cmp@.
scoreCmp :: Score -> Score -> Int
scoreCmp a b =
  60 * signum (a.effectiveIndent - b.effectiveIndent) + (a.penalty - b.penalty)
