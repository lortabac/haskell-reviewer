{-# LANGUAGE MagicHash #-}
module Tokens where
-- a comment
{- block -}
-- | Doc.
f :: Int -> Int#
f x = case x of { 1 -> 'c' `seq` 2; _ -> negate 3.5e1 }
  where s = "str" ++ [] ; t = (:) 1 []
data a :* b = a :* b
