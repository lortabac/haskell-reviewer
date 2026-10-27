module Headings
  ( -- * Section with 'f' and /emphasis/
    f,

    -- ** A subsection
    g,

    -- | A doc in the export list.

    -- $chunk
  )
where

-- $chunk
-- A named chunk, with 'g'.

-- * Among the declarations

-- | F.
f :: Int
f = 1

-- | G.
g :: Int
g = 2
