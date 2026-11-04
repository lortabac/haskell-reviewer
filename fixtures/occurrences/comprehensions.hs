{-# LANGUAGE ParallelListComp, TransformListComp, MonadComprehensions, RecordWildCards #-}
module Comprehensions where

import GHC.Exts (groupWith, sortWith, the)

pairs xs = [(x, y) | x <- xs, let z = x * 2, y <- [z, x], odd y]

zipped as bs = [a + b | a <- as | b <- bs]

grouped rows = [(the k, sum v) | (k, v) <- rows, then group by k using groupWith]

sorted rows = [r | r <- rows, then sortWith by r]

nested = [[y | y <- ys] | ys <- [[1], [2]]]

g = 1

usingOutside ps = [x | (g, x) <- ps, then g]

data W = W {wf :: Int}

parWild xs ys = [wf | W {..} <- xs | y <- ys]
