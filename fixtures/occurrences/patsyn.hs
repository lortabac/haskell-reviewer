{-# LANGUAGE PatternSynonyms, ScopedTypeVariables, ViewPatterns #-}
module PatSyn where

-- Implicit: a binds in the signature only.
pattern Head :: a -> [a]
pattern Head x <- (x : _)

-- A forall: in scope in the pattern synonym too.
pattern Single :: forall a. a -> [a]
pattern Single x <- [(x :: a)]
  where
    Single x = [x :: a]

-- Two contexts and an existential.
data Box = forall s. (Show s) => Box s

pattern Shown :: () => forall s. (Show s) => s -> Box
pattern Shown s = Box s

-- A record pattern synonym.
pattern Point :: Int -> Int -> (Int, Int)
pattern Point {px, py} = (px, py)
