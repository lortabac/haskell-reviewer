module B (f, h, Shape (..), k) where

f :: Int -> Int
f = negate

h :: Int
h = 2

data Shape = Shape Int

k :: Int
k = 3
