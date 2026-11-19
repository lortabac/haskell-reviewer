{-# LANGUAGE MultiWayIf #-}

{- Part of the line diff: a port of git's classic (Myers) diff, as the
histogram diff runs it on one region (git 2.44.1: xdl_fall_back_diff in
xdiff/xutils.c, xdl_trim_ends and xdl_cleanup_records in
xdiff/xprepare.c, xdl_split and xdl_recs_cmp in xdiff/xdiffi.c).

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

-- | git's Myers diff for one region: trim the common ends, drop lines
-- with no match (the discard pass), then split with git's heuristics
-- and cost limit. Names, constants and 0-based indexes are git's, so
-- this reads side by side with the C. See Note [Line diff] in
-- "HaskellReviewer.Core.LineDiff".
module HaskellReviewer.Core.LineDiff.Myers
  ( -- * Myers on a region
    fallBackDiff,

    -- * For tests
    MyersResult (..),
    myersCore,
  )
where

import Control.Monad (forM_, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word (Word8)
import HaskellReviewer.Core.LineDiff.Env
  ( Counter (..),
    Counters,
    DiffSide (..),
    Env (..),
    bump,
    bumpBy,
    isChanged,
    newChanged,
    newCounters,
    readCounter,
    setChanged,
  )

-- | Myers on lines @line1 .. line1 + count1 - 1@ of side 1 and
-- @line2 .. line2 + count2 - 1@ of side 2 (1-based, both non-empty):
-- their change flags become the result, as git's @memcpy@ does
-- (@xdl_fall_back_diff@ and @xdl_do_diff@). Counts are over the region
-- only: git diffs the region as two new files. The env gives scratch
-- and counters; side 1 need not be base.
fallBackDiff :: Env s -> DiffSide s -> DiffSide s -> Int -> Int -> Int -> Int -> ST s ()
fallBackDiff env s1 s2 line1 count1 line2 count2 = do
  let a = VU.slice (line1 - 1) count1 s1.classes
      b = VU.slice (line2 - 1) count2 s2.classes
      mark1 i = setChanged s1.changed (line1 - 1 + i) True
      mark2 i = setChanged s2.changed (line2 - 1 + i) True
  forM_ [0 .. count1 - 1] $ \i -> setChanged s1.changed (line1 - 1 + i) False
  forM_ [0 .. count2 - 1] $ \i -> setChanged s2.changed (line2 - 1 + i) False
  -- Each class's count on each side (xdl_classify_record's len1, len2).
  VU.mapM_ (VUM.modify env.myersCount1 (+ 1)) a
  VU.mapM_ (VUM.modify env.myersCount2 (+ 1)) b
  -- xdl_trim_ends.
  let lim = min count1 count2
      prefix = commonLength (\i -> a VU.! i == b VU.! i) lim
      suffix =
        commonLength
          (\i -> a VU.! (count1 - 1 - i) == b VU.! (count2 - 1 - i))
          (lim - prefix)
      dstart = prefix
      dend1 = count1 - suffix - 1
      dend2 = count2 - suffix - 1
  -- xdl_cleanup_records.
  dis1 <- kinds a (bogosqrt count1) env.myersCount2 dstart dend1
  dis2 <- kinds b (bogosqrt count2) env.myersCount1 dstart dend2
  VU.mapM_ (\c -> VUM.write env.myersCount1 c 0) a
  VU.mapM_ (\c -> VUM.write env.myersCount2 c 0) b
  let keep dis s e i
        | dis VU.! i == keepKind = True
        | dis VU.! i == investigateKind = not (cleanMmatch dis i s e)
        | otherwise = False
      rindex1 = VU.filter (keep dis1 dstart dend1) (VU.enumFromTo dstart dend1)
      rindex2 = VU.filter (keep dis2 dstart dend2) (VU.enumFromTo dstart dend2)
  forM_ [dstart .. dend1] $ \i -> unless (keep dis1 dstart dend1 i) (mark1 i)
  forM_ [dstart .. dend2] $ \i -> unless (keep dis2 dstart dend2 i) (mark2 i)
  bumpBy
    env.counters
    Discards
    ((dend1 - dstart + 1 - VU.length rindex1) + (dend2 - dstart + 1 - VU.length rindex2))
  core
    env.counters
    (VU.map (a VU.!) rindex1)
    (VU.map (b VU.!) rindex2)
    (mark1 . (rindex1 VU.!))
    (mark2 . (rindex2 VU.!))

-- | How many of @0 .. n - 1@ pass, from the start.
commonLength :: (Int -> Bool) -> Int -> Int
commonLength ok n = go 0
  where
    go i
      | i < n && ok i = go (i + 1)
      | otherwise = i

-- | git's @xdl_bogosqrt@.
bogosqrt :: Int -> Int
bogosqrt = go 1
  where
    go i n
      | n > 0 = go (i `shiftL` 1) (n `shiftR` 2)
      | otherwise = i

-- | The discard pass's kinds of line (git's @dis[]@ values; git 2.56
-- names them DISCARD, KEEP, INVESTIGATE). Word8, to sit in an unboxed
-- vector.
--
-- 'discardKind': the class isn't on the other side.
discardKind :: Word8
discardKind = 0

-- | The class is on the other side, fewer than @mlim@ times.
keepKind :: Word8
keepKind = 1

-- | The class is on the other side @mlim@ times or more: drop the line
-- if it sits among discarded lines ('cleanMmatch').
investigateKind :: Word8
investigateKind = 2

-- | The discard pass's kind of each line in @s .. e@ (lines outside
-- are 'discardKind' and never read). @counts@ are the other side's.
kinds :: VU.Vector Int -> Int -> VUM.MVector s Int -> Int -> Int -> ST s (VU.Vector Word8)
kinds side bs counts s e =
  VU.generateM (VU.length side) $ \i ->
    if i < s || i > e
      then pure discardKind
      else do
        nm <- VUM.read counts (side VU.! i)
        pure $
          if
            | nm == 0 -> discardKind
            | nm >= mlim -> investigateKind
            | otherwise -> keepKind
  where
    mlim = min 1024 bs

-- | git's @xdl_clean_mmatch@: drop a multimatch line @i@ when it sits in
-- a run of lines with no match.
cleanMmatch :: VU.Vector Word8 -> Int -> Int -> Int -> Bool
cleanMmatch dis i s0 e0
  | rdis0 == 0 = False
  | rdis1 == 0 = False
  | otherwise = (rpdis0 + rpdis1) * 4 < (rpdis0 + rpdis1) + (rdis0 + rdis1)
  where
    s = if i - s0 > 100 then i - 100 else s0
    e = if e0 - i > 100 then i + 100 else e0
    (rdis0, rpdis0) = run (-1)
    (rdis1, rpdis1) = run 1
    -- From the line next to i, in direction dir, within [s, e]: count
    -- discarded and multimatch lines until a kept one. First order on
    -- purpose: as a loop over closures it allocated on every step, and
    -- this runs up to 200 steps per line.
    run :: Int -> (Int, Int)
    run dir = go (i + dir) 0 1
      where
        go !j !rdis !rpdis
          | j < s || j > e = (rdis, rpdis)
          | dis VU.! j == discardKind = go (j + dir) (rdis + 1) rpdis
          | dis VU.! j == investigateKind = go (j + dir) rdis (rpdis + 1)
          | otherwise = (rdis, rpdis)

-- * The core

-- | Whether a box must get a minimal diff (git's @need_min@).
data Cost = MinimalCost | AnyCost

-- | git's @xdpsplit_t@: where to split a box, and the cost rule for the
-- low and high halves.
data Split = Split
  { i1 :: !Int,
    i2 :: !Int,
    lo :: !Cost,
    hi :: !Cost
  }

-- | What the core needs (git's @diffdata_t@ and @xdalgoenv_t@).
data Core s = Core
  { ha1 :: VU.Vector Int,
    ha2 :: VU.Vector Int,
    -- | Both K vectors: forward, then backward.
    kvd :: VUM.MVector s Int,
    -- | Where diagonal 0 of the forward vector is in 'kvd'.
    fOff :: Int,
    -- | Where diagonal 0 of the backward vector is in 'kvd'.
    bOff :: Int,
    mxcost :: Int,
    mark1 :: Int -> ST s (),
    mark2 :: Int -> ST s (),
    counters :: Counters s
  }

-- | git's @XDL_SNAKE_CNT@.
snakeCnt :: Int
snakeCnt = 20

-- | git's @XDL_HEUR_MIN_COST@.
heurMin :: Int
heurMin = 256

-- | git's @XDL_K_HEUR@.
kHeur :: Int
kHeur = 4

-- | Run the core on the kept lines (classes @ha1@, @ha2@); @mark1 k@
-- marks the k-th kept base line changed.
core ::
  Counters s ->
  VU.Vector Int ->
  VU.Vector Int ->
  (Int -> ST s ()) ->
  (Int -> ST s ()) ->
  ST s ()
core counters ha1 ha2 mark1 mark2 = do
  let n1 = VU.length ha1
      n2 = VU.length ha2
      ndiags = n1 + n2 + 3
  kvd <- VUM.replicate (2 * ndiags + 2) 0
  let k =
        Core
          { ha1 = ha1,
            ha2 = ha2,
            kvd = kvd,
            fOff = n2 + 1,
            bOff = ndiags + n2 + 1,
            mxcost = max 256 (bogosqrt ndiags),
            mark1 = mark1,
            mark2 = mark2,
            counters = counters
          }
  recsCmp k 0 n1 0 n2 AnyCost

-- | git's @xdl_recs_cmp@: divide and conquer on the box
-- @[off1, lim1) × [off2, lim2)@.
recsCmp :: Core s -> Int -> Int -> Int -> Int -> Cost -> ST s ()
recsCmp k off1_ lim1_ off2_ lim2_ cost
  | off1 == lim1 = forM_ [off2 .. lim2 - 1] k.mark2
  | off2 == lim2 = forM_ [off1 .. lim1 - 1] k.mark1
  | otherwise = do
      spl <- split k off1 lim1 off2 lim2 cost
      recsCmp k off1 spl.i1 off2 spl.i2 spl.lo
      recsCmp k spl.i1 lim1 spl.i2 lim2 spl.hi
  where
    -- Shrink the box by the snakes at both ends.
    (off1, off2) = front off1_ off2_
    front x y
      | x < lim1_ && y < lim2_ && k.ha1 VU.! x == k.ha2 VU.! y = front (x + 1) (y + 1)
      | otherwise = (x, y)
    (lim1, lim2) = back lim1_ lim2_
    back x y
      | off1 < x && off2 < y && k.ha1 VU.! (x - 1) == k.ha2 VU.! (y - 1) =
          back (x - 1) (y - 1)
      | otherwise = (x, y)

-- | What a scan over the diagonals gives: a split, or whether it saw a
-- snake longer than 'snakeCnt'.
data Scan = Found Split | GotSnake Bool

-- | git's @xdl_split@.
split :: Core s -> Int -> Int -> Int -> Int -> Cost -> ST s Split
split k off1 lim1 off2 lim2 cost = do
  wf fmid off1
  wb bmid lim1
  loop 1 fmid fmid bmid bmid
  where
    dmin = off1 - lim2
    dmax = lim1 - off2
    fmid = off1 - off2
    bmid = lim1 - lim2
    isOdd = odd (fmid - bmid)
    rf d = VUM.read k.kvd (k.fOff + d)
    wf d = VUM.write k.kvd (k.fOff + d)
    rb d = VUM.read k.kvd (k.bOff + d)
    wb d = VUM.write k.kvd (k.bOff + d)
    -- Unchecked, as checked reads cost a lot here. Every read stays in
    -- the box [off1, lim1) x [off2, lim2), and the box is inside ha1 x
    -- ha2: the snakes check the upper ends and Myers keeps the lower
    -- ends (see snakeF), bestF reads back from i1 >= off1 + 20 at most
    -- 20 lines, bestB reads forward from i1 <= lim1 - 20 at most 19.
    h1 i = VU.unsafeIndex k.ha1 i
    h2 i = VU.unsafeIndex k.ha2 i

    loop ec fmin0 fmax0 bmin0 bmax0 = do
      -- Widen the forward domain by one; -1 just outside.
      fmin <-
        if fmin0 > dmin
          then wf (fmin0 - 2) (-1) >> pure (fmin0 - 1)
          else pure (fmin0 + 1)
      fmax <-
        if fmax0 < dmax
          then wf (fmax0 + 2) (-1) >> pure (fmax0 + 1)
          else pure (fmax0 - 1)
      forward fmax fmin bmin0 bmax0 False >>= \case
        Found s -> pure s
        GotSnake snake1 -> do
          bmin <-
            if bmin0 > dmin
              then wb (bmin0 - 2) maxBound >> pure (bmin0 - 1)
              else pure (bmin0 + 1)
          bmax <-
            if bmax0 < dmax
              then wb (bmax0 + 2) maxBound >> pure (bmax0 + 1)
              else pure (bmax0 - 1)
          backward bmax bmin fmin fmax snake1 >>= \case
            Found s -> pure s
            GotSnake gotSnake -> case cost of
              MinimalCost -> loop (ec + 1) fmin fmax bmin bmax
              AnyCost -> do
                h <-
                  if gotSnake && ec > heurMin
                    then heuristic ec fmin fmax bmin bmax
                    else pure Nothing
                case h of
                  Just s -> pure s
                  Nothing
                    | ec >= k.mxcost -> cutoff fmin fmax bmin bmax
                    | otherwise -> loop (ec + 1) fmin fmax bmin bmax

    forward d fmin bmin bmax got
      | d < fmin = pure (GotSnake got)
      | otherwise = do
          lo' <- rf (d - 1)
          hi' <- rf (d + 1)
          let start = if lo' >= hi' then lo' + 1 else hi'
              i1 = snakeF start (start - d)
              i2 = i1 - d
              got' = got || i1 - start > snakeCnt
          wf d i1
          met <-
            if isOdd && bmin <= d && d <= bmax
              then (<= i1) <$> rb d
              else pure False
          if met
            then pure (Found (Split i1 i2 MinimalCost MinimalCost))
            else forward (d - 2) fmin bmin bmax got'
    -- The snakes check the upper ends only. The lower ends hold by
    -- Myers' invariant: on diagonal d in [fmin, fmax], i1 >= off1 and
    -- i2 = i1 - d >= off2 (git's C relies on the same).
    snakeF i1 i2
      | i1 < lim1 && i2 < lim2 && h1 i1 == h2 i2 = snakeF (i1 + 1) (i2 + 1)
      | otherwise = i1

    backward d bmin fmin fmax got
      | d < bmin = pure (GotSnake got)
      | otherwise = do
          lo' <- rb (d - 1)
          hi' <- rb (d + 1)
          let start = if lo' < hi' then lo' else hi' - 1
              i1 = snakeB start (start - d)
              i2 = i1 - d
              got' = got || start - i1 > snakeCnt
          wb d i1
          met <-
            if not isOdd && fmin <= d && d <= fmax
              then (i1 <=) <$> rf d
              else pure False
          if met
            then pure (Found (Split i1 i2 MinimalCost MinimalCost))
            else backward (d - 2) bmin fmin fmax got'
    -- Mirrored: lower ends checked, upper ends by the invariant.
    snakeB i1 i2
      | i1 > off1 && i2 > off2 && h1 (i1 - 1) == h2 (i2 - 1) = snakeB (i1 - 1) (i2 - 1)
      | otherwise = i1

    -- The heuristic split: a far-reaching diagonal that ends (or, going
    -- backward, starts) with a snake of 'snakeCnt' lines.
    heuristic ec fmin fmax bmin bmax = do
      f <- bestF ec fmin fmax 0 Nothing
      case f of
        Just (s1, s2) -> do
          bump k.counters HeuristicSplitsForward
          pure (Just (Split s1 s2 MinimalCost AnyCost))
        Nothing ->
          bestB ec bmin bmax 0 Nothing >>= \case
            Just (s1, s2) -> do
              bump k.counters HeuristicSplitsBackward
              pure (Just (Split s1 s2 AnyCost MinimalCost))
            Nothing -> pure Nothing
    bestF ec fmin d best spl
      | d < fmin = pure spl
      | otherwise = do
          i1 <- rf d
          let i2 = i1 - d
              v = (i1 - off1) + (i2 - off2) - abs (d - fmid)
          if v > kHeur * ec
            && v > best
            && off1 + snakeCnt <= i1
            && i1 < lim1
            && off2 + snakeCnt <= i2
            && i2 < lim2
            && all (\j -> h1 (i1 - j) == h2 (i2 - j)) [1 .. snakeCnt]
            then bestF ec fmin (d - 2) v (Just (i1, i2))
            else bestF ec fmin (d - 2) best spl
    bestB ec bmin d best spl
      | d < bmin = pure spl
      | otherwise = do
          i1 <- rb d
          let i2 = i1 - d
              v = (lim1 - i1) + (lim2 - i2) - abs (d - bmid)
          if v > kHeur * ec
            && v > best
            && off1 < i1
            && i1 <= lim1 - snakeCnt
            && off2 < i2
            && i2 <= lim2 - snakeCnt
            && all (\j -> h1 (i1 + j) == h2 (i2 + j)) [0 .. snakeCnt - 1]
            then bestB ec bmin (d - 2) v (Just (i1, i2))
            else bestB ec bmin (d - 2) best spl

    -- The cost limit: take the furthest-reaching path, by i1 + i2.
    cutoff fmin fmax bmin bmax = do
      (fbest, fbest1) <- fScan fmin fmax (-1) (-1)
      (bbest, bbest1) <- bScan bmin bmax maxBound maxBound
      if (lim1 + lim2) - bbest < fbest - (off1 + off2)
        then do
          bump k.counters CostCutoffsForward
          pure (Split fbest1 (fbest - fbest1) MinimalCost AnyCost)
        else do
          bump k.counters CostCutoffsBackward
          pure (Split bbest1 (bbest - bbest1) AnyCost MinimalCost)
    fScan fmin d fbest fbest1
      | d < fmin = pure (fbest, fbest1)
      | otherwise = do
          v <- rf d
          let i1' = min v lim1
              (i1, i2) = if lim2 < i1' - d then (lim2 + d, lim2) else (i1', i1' - d)
          if fbest < i1 + i2
            then fScan fmin (d - 2) (i1 + i2) i1
            else fScan fmin (d - 2) fbest fbest1
    bScan bmin d bbest bbest1
      | d < bmin = pure (bbest, bbest1)
      | otherwise = do
          v <- rb d
          let i1' = max off1 v
              (i1, i2) = if i1' - d < off2 then (off2 + d, off2) else (i1', i1' - d)
          if i1 + i2 < bbest
            then bScan bmin (d - 2) (i1 + i2) i1
            else bScan bmin (d - 2) bbest bbest1

-- | What 'myersCore' gives.
data MyersResult = MyersResult
  { -- | Changed flags of side 1.
    changed1 :: VU.Vector Bool,
    -- | Changed flags of side 2.
    changed2 :: VU.Vector Bool,
    -- | How often the heuristics fired (heuristic splits plus cost
    -- cutoffs).
    heuristics :: Int
  }

-- | The core alone (no trim, no discard pass) on two class arrays. For
-- the minimality test.
myersCore :: VU.Vector Int -> VU.Vector Int -> MyersResult
myersCore a b = runST $ do
  ch1 <- newChanged (VU.length a)
  ch2 <- newChanged (VU.length b)
  cs <- newCounters
  core cs a b (\i -> setChanged ch1 i True) (\i -> setChanged ch2 i True)
  f1 <- VU.generateM (VU.length a) (isChanged ch1)
  f2 <- VU.generateM (VU.length b) (isChanged ch2)
  hs <-
    mapM
      (readCounter cs)
      [ HeuristicSplitsForward,
        HeuristicSplitsBackward,
        CostCutoffsForward,
        CostCutoffsBackward
      ]
  pure MyersResult {changed1 = f1, changed2 = f2, heuristics = sum hs}
