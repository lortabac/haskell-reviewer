module CycA (A (..), module CycB) where

import CycB

data A = A | AB B
