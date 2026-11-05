module Y (module X, y) where

import {-# SOURCE #-} X

y :: Int
y = 2
