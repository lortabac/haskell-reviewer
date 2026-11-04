{-# LANGUAGE ImplicitParams #-}
module ImplicitParams where

f :: (?x :: Int) => Int
f = ?x + 1

g = let ?x = 2 in f + ?x

h = ?y
