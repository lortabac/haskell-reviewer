{-# LANGUAGE PatternSynonyms, TypeFamilies #-}

-- | Some names: 'T' (the type, not the constructor), v'T' (the constructor),
-- t'T', 'f', '(+++)', '`f`', 'field', 'Identifiers.f', 'Other.f', 'C',
-- 'm', 'Assoc', 'P', 'Ext', 'missing', 'case', 'Identifiers.case', don't.
module Identifiers (T (..), C (..), f, (+++), pattern P) where

-- | See 'f' and 'T'.
data T = T {field :: Int}

-- | Uses 'm' and 'Assoc'.
class C a where
  type Assoc a
  -- | 'C' and 'T'.
  m :: a -> Int

-- | 'f' again.
f :: Int -> Int
f = id

-- | Operator.
(+++) :: Int -> Int -> Int
(+++) = (+)

-- | 'P' and v'P'.
pattern P :: Int
pattern P = 1

-- | Instances have no name: 'Show' isn't local.
instance Show T where
  show _ = "T"
