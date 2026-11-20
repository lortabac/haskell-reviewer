{-# LANGUAGE StrictData #-}

{- How similar two files are, for rename detection: a port of git's
diffcore-delta.c (hash_chars, diffcore_count_changes) and of
estimate_similarity in diffcore-rename.c, git 2.44.1. Nothing in them
changed in behaviour up to git 2.56.0.

diffcore-delta.c has no copyright line of its own (it is git's, under
git's COPYING); diffcore-rename.c says:
Copyright (C) 2005 Junio C Hamano
Haskell port: Copyright (C) 2026 Lorenzo Tabacchini

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; version 2 of the License only.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, see <http://www.gnu.org/licenses/>.

SPDX-License-Identifier: GPL-2.0-only
-}

-- | git's similarity score of two files (SPEC §11.4.7, D-324). Pure.
-- Some names are git's, so the code reads side by side with the C.
-- See Note [Similarity].
module HaskellReviewer.Core.Similarity
  ( -- * Scores
    Score (..),
    maxScore,
    renameScore,
    basenameScore,
    scorePercent,

    -- * Span tables
    Spans (size, hashes, counts),
    hashChars,

    -- * Similarity
    passesSizeTest,
    estimateSimilarity,
    countCopied,
    mergeSteps,
  )
where

import Data.Bits (shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS.Unsafe
import Data.IntMap.Strict qualified as IntMap
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word8)

{- Note [Similarity]

As git's diffcore-delta.c. A file is cut into chunks: a chunk ends
after an LF or at 64 bytes. In a text file (no NUL in the first 8000
bytes, git's buffer_is_binary), a CR right before an LF is skipped: not
hashed, not counted. Each chunk is hashed into one of 107,927 buckets
(the formula is git's, 32-bit unsigned). A file's 'Spans' maps each
bucket to the bytes of its chunks.

copied = sum over buckets of min(source, destination). The score is
copied * 60000 / max(size), sizes in bytes with the CRs. 60000 is
100%; a rename needs 50% ('renameScore').

Before that, a size test: if the sizes differ too much for the minimum
score, the score is 0 and nothing is read. Git doesn't read file
contents for pairs that fail it, so neither do we: see
'passesSizeTest'.

We ignore diff attributes: a file is binary only by the NUL rule
(D-324).
-}

-- * Scores

-- | A similarity score, 0 to 'maxScore'.
newtype Score = Score Int
  deriving (Eq, Ord, Show)

-- | 100%. git's MAX_SCORE.
maxScore :: Score
maxScore = Score 60000

-- | The minimum for a rename: 50%, git's DEFAULT_RENAME_SCORE.
renameScore :: Score
renameScore = Score 30000

-- | The minimum in git's basename step: halfway from 'renameScore' to
-- 100%, so 75%.
basenameScore :: Score
basenameScore = Score 45000

-- | The percent git shows (@R075@), rounded down.
scorePercent :: Score -> Int
scorePercent (Score s) = s * 100 `div` maxInt

-- | 'maxScore' as a number.
maxInt :: Int
maxInt = let Score m = maxScore in m

-- * Span tables

-- | One file's chunks, by hash bucket. git's spanhash_top, sorted.
data Spans = Spans
  { -- | The file's size in bytes, CRs included.
    size :: Int,
    -- | Buckets, ascending.
    hashes :: VU.Vector Word32,
    -- | Bytes in each bucket, same order.
    counts :: VU.Vector Int
  }
  deriving (Eq, Show)

-- | A file's span table. git's hash_chars. See Note [Similarity].
hashChars :: ByteString -> Spans
hashChars bytes =
  Spans
    { size = len,
      hashes = VU.fromListN (IntMap.size table) (map (fromIntegral . fst) pairs),
      counts = VU.fromListN (IntMap.size table) (map snd pairs)
    }
  where
    len = BS.length bytes
    isText = BS.notElem 0 (BS.take 8000 bytes)
    table = go 0 0 0 0 IntMap.empty
    pairs = IntMap.toAscList table
    at :: Int -> Word8
    at = BS.Unsafe.unsafeIndex bytes
    go :: Int -> Int -> Word32 -> Word32 -> IntMap.IntMap Int -> IntMap.IntMap Int
    go !i !n !accum1 !accum2 !m
      | i >= len = if n > 0 then add accum1 accum2 n m else m
      | otherwise =
          let c = at i
           in if isText && c == 13 && i + 1 < len && at (i + 1) == 10
                then go (i + 1) n accum1 accum2 m
                else
                  let a1 =
                        ((accum1 `shiftL` 7) `xor` (accum2 `shiftR` 25))
                          + fromIntegral c
                      a2 = (accum2 `shiftL` 7) `xor` (accum1 `shiftR` 25)
                      n' = n + 1
                   in if n' < 64 && c /= 10
                        then go (i + 1) n' a1 a2 m
                        else go (i + 1) 0 0 0 (add a1 a2 n' m)
    add a1 a2 n = IntMap.insertWith (+) (hashval a1 a2) n
    hashval :: Word32 -> Word32 -> Int
    hashval a1 a2 = fromIntegral ((a1 + a2 * 0x61) `rem` 107927)

-- * Similarity

-- | git's size test: can the pair reach this score at all? Sizes in
-- bytes. Fails when @max * (100% - minimum) < (max - min) * 100%@.
passesSizeTest :: Score -> Int -> Int -> Bool
passesSizeTest (Score minimum') a b =
  not (big * (maxInt - minimum') < (big - small) * maxInt)
  where
    big = max a b
    small = min a b

-- | The score of a source and a destination, both regular files, as
-- git's estimate_similarity: 0 if they fail the size test for the
-- minimum.
estimateSimilarity :: Score -> Spans -> Spans -> Score
estimateSimilarity minimum' src dst
  | not (passesSizeTest minimum' src.size dst.size) = Score 0
  | dst.size == 0 = Score 0
  | otherwise = Score (countCopied src dst * maxInt `div` max src.size dst.size)

-- | Bytes of the destination that come from the source: git's
-- src_copied in diffcore_count_changes.
countCopied :: Spans -> Spans -> Int
countCopied src dst = go 0 0 0
  where
    ns = VU.length src.hashes
    nd = VU.length dst.hashes
    go !i !j !acc
      | i >= ns || j >= nd = acc
      | otherwise =
          let hs = VU.unsafeIndex src.hashes i
              hd = VU.unsafeIndex dst.hashes j
           in case compare hs hd of
                LT -> go (i + 1) j acc
                GT -> go i (j + 1) acc
                EQ ->
                  let c = min (VU.unsafeIndex src.counts i) (VU.unsafeIndex dst.counts j)
                   in go (i + 1) (j + 1) (acc + c)

-- | What 'countCopied' costs at most, in steps: for the work budget.
mergeSteps :: Spans -> Spans -> Int
mergeSteps a b = VU.length a.hashes + VU.length b.hashes
