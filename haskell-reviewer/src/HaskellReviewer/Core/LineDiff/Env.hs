{- Part of the line diff, a port of git's xdiff (git 2.44.1): the state
that xdiff keeps in xdfenv_t (xdiff/xtypes.h), shared by the other
LineDiff modules.

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

-- | What the line diff passes around: line classes, change flags
-- (git's @rchg@), scratch arrays and counters. Internal to
-- "HaskellReviewer.Core.LineDiff".
module HaskellReviewer.Core.LineDiff.Env
  ( -- * Env
    Env (..),
    DiffSide (..),
    newEnv,

    -- * Change flags
    Changed,
    newChanged,
    isChanged,
    setChanged,
    markRange,

    -- * Counters
    Counter (..),
    Counters,
    newCounters,
    bump,
    bumpBy,
    readCounter,
  )
where

import Control.Monad.ST (ST)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word (Word8)

-- | One diff's state. Side 1 is base, side 2 is head, as in git.
data Env s = Env
  { side1 :: DiffSide s,
    side2 :: DiffSide s,
    -- | Classes are @0 .. classCount - 1@.
    classCount :: Int,
    -- | Histogram scratch, by class: its count in the region. All 0
    -- between calls.
    histCount :: VUM.MVector s Int,
    -- | Histogram scratch, by class: its first line in the region (0 =
    -- none). All 0 between calls.
    histFirst :: VUM.MVector s Int,
    -- | Histogram scratch, by base line (1-based): the next line of the
    -- same class in the region (0 = none).
    histNext :: VUM.MVector s Int,
    -- | Myers scratch, by class: its count on side 1 of the region.
    -- All 0 between calls.
    myersCount1 :: VUM.MVector s Int,
    -- | The same for side 2.
    myersCount2 :: VUM.MVector s Int,
    counters :: Counters s
  }

-- | One side's lines and change flags (git's @xdfile_t@).
data DiffSide s = DiffSide
  { -- | The class of each line, 0-based. Two lines are equal when
    -- their classes are.
    classes :: VU.Vector Int,
    -- | git's @rchg@.
    changed :: Changed s
  }

-- | A fresh env, nothing changed yet.
newEnv :: VU.Vector Int -> VU.Vector Int -> Int -> ST s (Env s)
newEnv c1 c2 count = do
  ch1 <- newChanged (VU.length c1)
  ch2 <- newChanged (VU.length c2)
  hc <- VUM.replicate count 0
  hf <- VUM.replicate count 0
  hn <- VUM.replicate (VU.length c1 + 1) 0
  m1 <- VUM.replicate count 0
  m2 <- VUM.replicate count 0
  cs <- newCounters
  pure
    Env
      { side1 = DiffSide c1 ch1,
        side2 = DiffSide c2 ch2,
        classCount = count,
        histCount = hc,
        histFirst = hf,
        histNext = hn,
        myersCount1 = m1,
        myersCount2 = m2,
        counters = cs
      }

-- * Change flags

-- | One side's change flags, by 0-based line. Lines -1 and n read as
-- unchanged, as in git (its @rchg@ has a 0 on each end).
newtype Changed s = Changed (VUM.MVector s Word8)

-- | Flags for n lines, all unchanged.
newChanged :: Int -> ST s (Changed s)
newChanged n = Changed <$> VUM.replicate (n + 2) 0

-- | Is the line changed? Lines -1 and n are not.
isChanged :: Changed s -> Int -> ST s Bool
isChanged (Changed v) i = (/= 0) <$> VUM.read v (i + 1)

-- | Set or clear a line's flag.
setChanged :: Changed s -> Int -> Bool -> ST s ()
setChanged (Changed v) i b = VUM.write v (i + 1) (if b then 1 else 0)

-- | Mark @count@ lines changed, from line @from@ (0-based).
markRange :: Changed s -> Int -> Int -> ST s ()
markRange ch from count = mapM_ (\i -> setChanged ch i True) [from .. from + count - 1]

-- * Counters

-- | What the tests count, to check that every path runs.
data Counter
  = -- | A histogram region went to Myers.
    Fallbacks
  | -- | Myers took a heuristic split, forward (a diagonal ending in
    -- a snake).
    HeuristicSplitsForward
  | -- | The same, backward (a diagonal starting with a snake).
    HeuristicSplitsBackward
  | -- | Myers hit its cost limit and took the forward path.
    CostCutoffsForward
  | -- | The same, and took the backward path.
    CostCutoffsBackward
  | -- | Lines dropped by the discard pass.
    Discards
  | -- | Groups that could slide.
    Slides
  | -- | Groups that grew by sliding into another.
    Merges
  | -- | Groups lined up with the other side's group.
    Alignments
  | -- | Groups moved by the indent heuristic.
    IndentShifts
  | -- | Moved groups diffed again with Myers (git 2.54 and later).
    Rediffs
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One count per 'Counter'.
newtype Counters s = Counters (VUM.MVector s Int)

-- | All 0.
newCounters :: ST s (Counters s)
newCounters = Counters <$> VUM.replicate (fromEnum (maxBound :: Counter) + 1) 0

-- | Add one.
bump :: Counters s -> Counter -> ST s ()
bump cs c = bumpBy cs c 1

-- | Add n.
bumpBy :: Counters s -> Counter -> Int -> ST s ()
bumpBy (Counters v) c n = VUM.modify v (+ n) (fromEnum c)

-- | The count so far.
readCounter :: Counters s -> Counter -> ST s Int
readCounter (Counters v) c = VUM.read v (fromEnum c)
