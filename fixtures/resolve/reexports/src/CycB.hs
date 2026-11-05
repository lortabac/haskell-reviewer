module CycB (module CycB, module CycA) where

import {-# SOURCE #-} CycA (A)

data B = B A
