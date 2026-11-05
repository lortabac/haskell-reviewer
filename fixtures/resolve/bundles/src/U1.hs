{-# LANGUAGE PatternSynonyms #-}
-- An import item keeps the bundle, so T(..) has P.
module U1 (T (..)) where

import A (T)
import A (pattern P)
