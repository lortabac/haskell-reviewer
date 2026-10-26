module Extents where

{-# INLINE far #-}
-- | Far.
far :: Int -> Int

other :: Int
other = 0

far n = n
  where
    go = n
    stop = go

-- | Before.
before :: Int
-- ^ After the signature.
before = 1
-- ^ After the binding.

-- * A section

-- $chunk
-- Some named text.

-- | Left alone at the end.
