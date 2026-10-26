{-# LANGUAGE TypeFamilies, DataKinds #-}
module Orphans where

import qualified Other

data Local = Local
class LocalC a
type Syn = Int
type family F a

-- Not orphans: a local type, class, synonym, promoted constructor.
instance Show Local
instance LocalC Int
instance Show Syn
instance Show (Proxy 'Local)
instance Show Orphans.Local
-- Orphans: nothing local, or local names qualified with another module.
instance Show (Maybe Int)
instance Show Other.Local
deriving instance Eq (Maybe Bool)

-- Family instances.
type instance F Int = Bool
type instance G Int = Bool
type instance G Local = Bool

-- A deriving clause never gives an orphan.
data X = X deriving (Other.Show)
