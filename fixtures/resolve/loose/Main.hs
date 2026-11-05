module Main (main) where

import A (a)
import B (b)
import Data.Char (toUpper)

main :: IO ()
main = print (a + b, toUpper 'x', c)
  where
    c = 1 :: Int
