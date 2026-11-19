{-# LANGUAGE MultiWayIf #-}

{- Part of the line diff: a port of git's xdiff/xhistogram.c (git
2.44.1), which git took from JGit.

The C file's notice, kept as its licence asks:

  Copyright (C) 2010, Google Inc.
  and other copyright owners as documented in JGit's IP log.

  This program and the accompanying materials are made available
  under the terms of the Eclipse Distribution License v1.0 which
  accompanies this distribution, is reproduced below, and is
  available at http://www.eclipse.org/org/documents/edl-v10.php

  All rights reserved.

  Redistribution and use in source and binary forms, with or
  without modification, are permitted provided that the following
  conditions are met:

  - Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

  - Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following
    disclaimer in the documentation and/or other materials provided
    with the distribution.

  - Neither the name of the Eclipse Foundation, Inc. nor the
    names of its contributors may be used to endorse or promote
    products derived from this software without specific prior
    written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Haskell port: Copyright (C) 2026 Lorenzo Tabacchini. Like the other
line diff modules, it is under the GNU Lesser General Public License,
version 2.1 or (at your option) any later version (see LICENSE.LGPL-2.1).
The notice above still applies to the parts that come from JGit.

SPDX-License-Identifier: LGPL-2.1-or-later AND BSD-3-Clause
-}

-- | git's histogram diff. Lines are 1-based and names are git's, so the
-- code reads side by side with xhistogram.c. See Note [Line diff] in
-- "HaskellReviewer.Core.LineDiff".
--
-- Not ported: git's hash buckets. git gives up ("unable to generate
-- diff") when 64 distinct lines share a bucket; we look classes up
-- directly, so that never happens.
module HaskellReviewer.Core.LineDiff.Histogram
  ( -- * Histogram diff
    histogramDiff,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import HaskellReviewer.Core.LineDiff.Env
  ( Counter (..),
    DiffSide (..),
    Env (..),
    bump,
    markRange,
  )
import HaskellReviewer.Core.LineDiff.Myers (fallBackDiff)

-- | git's @struct region@: the best common run so far, 1-based,
-- inclusive. All 0: none yet.
data Region = Region
  { begin1 :: !Int,
    end1 :: !Int,
    begin2 :: !Int,
    end2 :: !Int
  }

-- | What 'tryLcs' updates: git's @index->cnt@, @index->has_common@
-- (0 or 1) and the region, in one unboxed array so the hot loop
-- doesn't allocate. See 'cntAt' and friends.
newtype Search s = Search (VUM.MVector s Int)

cntAt, hasCommonAt, begin1At, end1At, begin2At, end2At :: Int
cntAt = 0
hasCommonAt = 1
begin1At = 2
end1At = 3
begin2At = 4
end2At = 5

-- | Read a slot. Unchecked: the slots are the six constants above.
getS :: Search s -> Int -> ST s Int
getS (Search v) = VUM.unsafeRead v

-- | Write a slot, as 'getS'.
setS :: Search s -> Int -> Int -> ST s ()
setS (Search v) = VUM.unsafeWrite v

-- | What 'findLcs' found.
data Found
  = -- | Common lines exist, but each occurs more than 64 times: Myers.
    FallBack
  | -- | No common line.
    NoLcs
  | Lcs Region

-- | What the histogram loops read, unpacked so they don't go through
-- 'Env' at each step.
--
-- The hot loops read these unchecked (checked reads cost a lot there).
-- Why every index is in range:
--
-- * Base lines (@ptr@, @as@, @ae@, @np@, @as0@) are in the region
--   @[line1, lineEnd1]@, inside @[1, n1]@: the loops test the region's
--   ends before each step, and 'histNext' only holds region lines or 0.
--   'histNext' has n1 + 1 slots.
-- * Head lines (@bPtr@, @bs@, @be@) are in @[line2, lineEnd2]@, inside
--   @[1, n2]@, the same way.
-- * Classes come from 'classes1' and 'classes2', so they are below
--   'classCount', the size of 'histCount' and 'histFirst'.
data H s = H
  { classes1 :: {-# UNPACK #-} !(VU.Vector Int),
    classes2 :: {-# UNPACK #-} !(VU.Vector Int),
    histCount :: !(VUM.MVector s Int),
    histFirst :: !(VUM.MVector s Int),
    histNext :: !(VUM.MVector s Int),
    env :: Env s
  }

-- | git's @max_chain_length@.
maxChainLength :: Int
maxChainLength = 64

-- | Diff the whole of both sides into the env's change flags
-- (@xdl_do_histogram_diff@). git doesn't trim common ends first for
-- histogram, so neither do we.
histogramDiff :: Env s -> ST s ()
histogramDiff env =
  histogram h 1 (VU.length h.classes1) 1 (VU.length h.classes2)
  where
    h =
      H
        { classes1 = env.side1.classes,
          classes2 = env.side2.classes,
          histCount = env.histCount,
          histFirst = env.histFirst,
          histNext = env.histNext,
          env = env
        }

-- | @histogram_diff@. Recurses on the left part and loops on the right
-- part, as git does.
histogram :: H s -> Int -> Int -> Int -> Int -> ST s ()
histogram h line1 count1 line2 count2
  | count1 <= 0 && count2 <= 0 = pure ()
  | count1 == 0 = markRange h.env.side2.changed (line2 - 1) count2
  | count2 == 0 = markRange h.env.side1.changed (line1 - 1) count1
  | otherwise =
      findLcs h line1 count1 line2 count2 >>= \case
        FallBack -> do
          bump h.env.counters Fallbacks
          fallBackDiff h.env h.env.side1 h.env.side2 line1 count1 line2 count2
        NoLcs -> do
          markRange h.env.side1.changed (line1 - 1) count1
          markRange h.env.side2.changed (line2 - 1) count2
        Lcs r -> do
          histogram h line1 (r.begin1 - line1) line2 (r.begin2 - line2)
          histogram
            h
            (r.end1 + 1)
            (lineEnd1 - r.end1)
            (r.end2 + 1)
            (lineEnd2 - r.end2)
  where
    lineEnd1 = line1 + count1 - 1
    lineEnd2 = line2 + count2 - 1

-- | @find_lcs@: index the base region (@scanA@), then walk the head
-- region (@try_lcs@). Leaves the scratch arrays at 0.
findLcs :: H s -> Int -> Int -> Int -> Int -> ST s Found
findLcs h line1 count1 line2 count2 = do
  scanA h line1 count1
  ref <- Search <$> VUM.replicate 6 0
  setS ref cntAt (maxChainLength + 1)
  let walk bPtr
        | bPtr <= lineEnd2 =
            tryLcs h ref line1 count1 line2 count2 bPtr >>= walk
        | otherwise = pure ()
  walk line2
  -- Back to 0 for the next call. Unchecked: see H.
  let clear ptr
        | ptr <= lineEnd1 = do
            let c = class1 h ptr
            VUM.unsafeWrite h.histCount c 0
            VUM.unsafeWrite h.histFirst c 0
            clear (ptr + 1)
        | otherwise = pure ()
  clear line1
  hasCommon <- getS ref hasCommonAt
  cnt <- getS ref cntAt
  r <-
    Region
      <$> getS ref begin1At
      <*> getS ref end1At
      <*> getS ref begin2At
      <*> getS ref end2At
  pure $
    if
      | hasCommon == 1 && maxChainLength < cnt -> FallBack
      | r.begin1 == 0 && r.begin2 == 0 -> NoLcs
      | otherwise -> Lcs r
  where
    lineEnd1 = line1 + count1 - 1
    lineEnd2 = line2 + count2 - 1

-- | @scanA@: from the region's end to its start, count each class and
-- chain its lines, first one first. Unchecked: see H.
scanA :: H s -> Int -> Int -> ST s ()
scanA h line1 count1 = go (line1 + count1 - 1)
  where
    go ptr
      | line1 <= ptr = do
          let c = class1 h ptr
          first <- VUM.unsafeRead h.histFirst c
          VUM.unsafeWrite h.histNext ptr first
          VUM.unsafeWrite h.histFirst c ptr
          VUM.unsafeModify h.histCount (+ 1) c
          go (ptr - 1)
      | otherwise = pure ()

-- | @try_lcs@ for one head line. Gives the next head line to try.
--
-- The helpers below are top-level with strict Int arguments: as local
-- closures they allocated about 100 bytes per match, and some inputs
-- make millions of them. Their reads are unchecked: see H.
tryLcs :: H s -> Search s -> Int -> Int -> Int -> Int -> Int -> ST s Int
tryLcs h ref line1 count1 line2 count2 bPtr = do
  let c = class2 h bPtr
  recCnt <- VUM.unsafeRead h.histCount c
  cnt0 <- getS ref cntAt
  if
    | recCnt == 0 ->
        -- The class isn't in the base region.
        pure (bPtr + 1)
    | recCnt > cnt0 -> do
        setS ref hasCommonAt 1
        pure (bPtr + 1)
    | otherwise -> do
        setS ref hasCommonAt 1
        first <- VUM.unsafeRead h.histFirst c
        occurrence
          h
          ref
          line1
          (line1 + count1 - 1)
          line2
          (line2 + count2 - 1)
          bPtr
          recCnt
          first
          (bPtr + 1)

-- | One occurrence @as0@ in the base region of head line @bPtr@'s
-- class: extend the match both ways, keep it if better, go to the next
-- occurrence after it. Gives git's @b_next@.
occurrence ::
  H s -> Search s -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s Int
occurrence h ref !line1 !lineEnd1 !line2 !lineEnd2 !bPtr !recCnt !as0 !bNext0 = do
  np0 <- VUM.unsafeRead h.histNext as0
  let as = extendBack h line1 line2 as0 bPtr
      bs = bPtr - (as0 - as)
      ae = extendFwd h lineEnd1 lineEnd2 as0 bPtr
      be = bPtr + (ae - as0)
  rc <- minCount h ae as recCnt
  let bNext = if bNext0 <= be then be + 1 else bNext0
  b1 <- getS ref begin1At
  e1 <- getS ref end1At
  cnt <- getS ref cntAt
  when (e1 - b1 < ae - as || rc < cnt) $ do
    setS ref begin1At as
    setS ref end1At ae
    setS ref begin2At bs
    setS ref end2At be
    setS ref cntAt rc
  if np0 == 0
    then pure bNext
    else do
      np <- nextAfter h ae np0
      if np == 0
        then pure bNext
        else occurrence h ref line1 lineEnd1 line2 lineEnd2 bPtr recCnt np bNext

-- | How far back the match at (as, bs) goes: the new @as@.
extendBack :: H s -> Int -> Int -> Int -> Int -> Int
extendBack h !line1 !line2 = go
  where
    go !as !bs
      | line1 < as && line2 < bs && class1 h (as - 1) == class2 h (bs - 1) =
          go (as - 1) (bs - 1)
      | otherwise = as

-- | How far forward the match at (ae, be) goes: the new @ae@.
extendFwd :: H s -> Int -> Int -> Int -> Int -> Int
extendFwd h !lineEnd1 !lineEnd2 = go
  where
    go !ae !be
      | ae < lineEnd1 && be < lineEnd2 && class1 h (ae + 1) == class2 h (be + 1) =
          go (ae + 1) (be + 1)
      | otherwise = ae

-- | git's @rc@: the lowest count over base lines @i .. end@, starting
-- from @rc@. git stops looking once it is 1, and so do we.
minCount :: H s -> Int -> Int -> Int -> ST s Int
minCount h !end = go
  where
    go !i !rc
      | rc <= 1 || i > end = pure rc
      | otherwise = do
          k <- VUM.unsafeRead h.histCount (class1 h i)
          go (i + 1) (min rc k)

-- | The first occurrence after line @ae@, following the chain from
-- @np@; 0 if none.
nextAfter :: H s -> Int -> Int -> ST s Int
nextAfter h !ae = go
  where
    go !np
      | np == 0 = pure 0
      | np <= ae = VUM.unsafeRead h.histNext np >>= go
      | otherwise = pure np

-- | The class of a base line, 1-based. Unchecked: callers pass region
-- lines only (see H).
class1 :: H s -> Int -> Int
class1 h i = VU.unsafeIndex h.classes1 (i - 1)

-- | The class of a head line, 1-based. Unchecked, as 'class1'.
class2 :: H s -> Int -> Int
class2 h i = VU.unsafeIndex h.classes2 (i - 1)
