{-# LANGUAGE CPP, NoImplicitPrelude, OverloadedRecordDot #-}
module Use where

import Broken (x)
import qualified Broken as B
#ifdef NOT_DEFINED
import Gone (gone)
#endif

u = x B.y

v = gone

data R = R {f :: R}

g r = r.notAField
