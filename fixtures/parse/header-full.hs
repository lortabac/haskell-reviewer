{-# LANGUAGE PatternSynonyms, NamedDefaults #-}

-- | The module's doc.
--
-- Second paragraph.
module Data.Full
  {-# DEPRECATED "Use Data.Other" #-}
  ( -- * Values
    f,
    -- | Doc before g.
    g,
    h, -- ^ Doc after h.
    {-# DEPRECATED "Use f" #-} old,
    (+++),
    Q.qualified,

    -- ** Types
    T,
    U (..),
    V (A, b),
    W (.., C),
    X (),
    type (:+:),
    data D,
    pattern P,
    default Num,

    -- $chunk

    -- * Re-exports
    module Data.Map,
    {-# WARNING in "x-gone" "Going away" #-} module Data.Set,
  )
where

import Data.Map
import Data.Map qualified as Q
import Data.Set

-- $chunk
-- Some text.

f, g, h, old :: Int
old = 0
f = 1
g = 2
h = 3
