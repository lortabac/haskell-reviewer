{-# LANGUAGE TemplateHaskellQuotes #-}
module Defs (Rec (..), Cls (cm), Point (Point), px, qq, Nat (..)) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))

data Rec = Rec {ra :: Int, rb :: Int}

class Cls a where
  cm :: a -> Int
  cm2 :: a -> Int
  cm2 _ = 0

data Point = Point {px :: Int, py :: Int}

data Nat = Z | S Nat

qq :: QuasiQuoter
qq = QuasiQuoter {quoteExp = \s -> [|s|], quotePat = undefined, quoteType = undefined, quoteDec = undefined}
