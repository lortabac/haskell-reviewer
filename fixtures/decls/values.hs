{-# LANGUAGE BangPatterns #-}
module Values where

-- | Adds.
add :: Int -> Int -> Int
add x y
  | x > 0 = x + y
  | otherwise = y
  where
    z = 1

{-# INLINE add #-}
{-# SPECIALISE add :: Int -> Int -> Int #-}

-- | Shared.
f, g :: Int

f = 1
g = 2

(+++) :: [a] -> [a] -> [a]
xs +++ ys = xs ++ ys
infixr 5 +++

a `op` b = a + b
infixl 6 `op`

-- | A pattern binding.
(p, q) = (1, 2)

Just r = Just 3

-- | No binding.
lonely :: Int

{-# NOINLINE later #-}
later :: Int
later = 4
{-# OPAQUE later #-}
{-# SCC later #-}

later = 5

noSig = 6

{-# INLINE nothingHere #-}
infixl 3 >>>
