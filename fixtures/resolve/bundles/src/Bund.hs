{-# LANGUAGE PatternSynonyms #-}
-- Q is bundled with a type from another module.
module Bund (T (P, Q)) where

import A
import Syn
