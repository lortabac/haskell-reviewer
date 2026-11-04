module Shadowing (f, g) where

x :: Int
x = 1

f :: Int -> Int
f x = x + y
  where
    y = let x = 2 in x * Shadowing.x

g = \x -> case x of
  Just x -> x
  Nothing -> x'
  where
    x' = 0

h a b c = (a, b, c, h)
