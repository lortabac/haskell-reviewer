{-# LANGUAGE PatternSynonyms, TypeOperators #-}
module A (T (..), R (R, size), f, g, C (..), pattern P, type (+++), Shape (Circle)) where

data T = MkT Int | Other

data R = R {size :: Int, label :: String}

f :: Int -> Int
f = id

g :: Int
g = 1

class C a where
  method :: a -> Int

pattern P :: Int
pattern P = 0

type a +++ b = Either a b

data Shape = Circle | Square
