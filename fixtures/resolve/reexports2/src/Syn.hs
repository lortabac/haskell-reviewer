{-# LANGUAGE PatternSynonyms #-}
module Syn (pattern Q) where

import A

pattern Q :: T
pattern Q = MkT 0
