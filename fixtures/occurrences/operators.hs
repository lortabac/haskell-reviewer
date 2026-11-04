module Operators ((<+>), (|>)) where

import qualified Data.List as L

infixl 6 <+>

(<+>) :: Int -> Int -> Int
a <+> b = a + b

x |> f = f x

sections = (<+> 1) . (1 <+>) . (`div` 2) . (- 1) . subtract 1

neg y = - y

minus = (-)

compose f g = f . g

dot = (.)

qualified = [1] L.\\ [2]

selfOp = 1 Operators.<+> 2

backtick a b = a `elem` b
