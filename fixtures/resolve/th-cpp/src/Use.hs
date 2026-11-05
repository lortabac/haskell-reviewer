{-# LANGUAGE CPP, TemplateHaskell #-}
module Use where

#if MIN_VERSION_base(4,0,0)
import Gen (made)
#else
import Old (oldThing)
#endif
import Gen (helper)
import GenNoList
import Language.Haskell.TH (Exp (VarE))

u :: Int
u = made + helper + fromSplice + plain

v :: Int
v = $(pure (VarE 'helper))
