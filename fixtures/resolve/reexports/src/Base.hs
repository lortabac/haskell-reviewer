module Base (Base (..), base, Ext) where

import Data.Map (Map)

type Ext = Map Int Int

data Base = Base {bf :: Int}

base :: Int
base = 1
