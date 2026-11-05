{-# LANGUAGE PatternSynonyms, TypeOperators #-}
module Use where

import A (C (method), R (..), T (..), g, pattern P, type (+++))
import qualified A as Q
import B hiding (Shape, f)
import qualified B
import qualified B as Q (Shape (..))
import Data.List
import Data.Maybe (fromMaybe)

u1 :: T -> Int
u1 (MkT n) = n + g + h + k
u1 Other = method (0 :: Int) + P

u2 :: Int
u2 = Q.f 1 + B.f 2 + size (R 1 "x")

u3 :: Int +++ Bool
u3 = Left (fromMaybe 0 Nothing)

u4 :: B.Shape
u4 = Q.Shape 3

u5 :: Int
u5 = case Q.Circle of
  _ -> length (intercalate ", " ["a"])

instance C Int where
  method = id
