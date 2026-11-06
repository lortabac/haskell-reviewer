{-# LANGUAGE PatternSynonyms #-}
-- Hiding Q takes it out of T: T(..) has P only.
module H2 (T (..)) where

import Bund hiding (pattern Q)
