{-# LANGUAGE LambdaCase #-}
module LambdaCase where

f = \case
  Just x -> x
  Nothing -> 0

g = \cases
  a b | a > b -> a
  _ b -> b

h = \x y -> x y
