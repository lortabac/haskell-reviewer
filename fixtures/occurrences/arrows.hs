{-# LANGUAGE Arrows #-}
module Arrows where

import Control.Arrow

addA f g = proc x -> do
  y <- f -< x
  z <- g -< x
  let s = y + z
  returnA -< s

caseA f = proc x -> case x of
  Left a -> f -< a
  Right b -> returnA -< b

formA f g = proc x -> (| untilA (f -< x) (g -< x) |)

escape f = proc f -> f -< 1
