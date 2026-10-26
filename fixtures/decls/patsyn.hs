{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module PatSyn (pattern Zero, Point (.., X), pattern Cons) where

-- | Zero.
pattern Zero :: Int
pattern Zero = 0

pattern Cons :: a -> [a] -> [a]
pattern Cons x xs <- (x : xs)
  where
    Cons x xs = x : xs

-- | A record one.
pattern Point :: Int -> Int -> (Int, Int)
pattern Point {px, py} = (px, py)

pattern X :: Int
pattern X <- ((> 0) -> True)

infixr 5 `Cons`

data Point = P
