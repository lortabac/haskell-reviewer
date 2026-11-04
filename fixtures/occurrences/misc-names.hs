{-# LANGUAGE RoleAnnotations, PatternSynonyms, OverloadedLabels #-}
module Misc.Names
  ( T (..),
    as,
    qualified,
    module Data.Maybe,
    pattern P,
    Misc.Names.hiding,
  )
where

import Data.Maybe (fromMaybe, Maybe (..))
import qualified Data.Map as M hiding (lookup)

data T a = T a

type role T nominal

as = 1

qualified = as

hiding = qualified

family = hiding

pattern P :: Int
pattern P = 1

{-# INLINE as #-}
{-# RULES "as" forall n. fromMaybe n Nothing = n #-}
{-# SCC hiding #-}

label = #foo

used = M.empty
