module ArgDocs where

f :: Int -- ^ arg
  -> Int -- ^ result
f = id

data D = D Int -- ^ last arg

data T = A Int -- ^ con A
           Bool -- ^ arg 2
       | B { fld :: Int -- ^ fld doc
           }

data G where
  GC :: Int -- ^ gadt arg
     -> G -- ^ result
  GR :: { gf :: Int -- ^ gr field
        } -> G

class C a where
  n :: a -- ^ after n
  m :: a
