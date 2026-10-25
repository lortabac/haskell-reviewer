{-# LANGUAGE StrictData #-}

-- | Places in a source file. Shared by the module model and the HIE
-- index (D-87).
module HaskellReviewer.Core.Span
  ( -- * Types
    Line (..),
    Col (..),
    Span (..),

    -- * Rendering
    renderSpan,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)

{- Note [Span convention]

Lines are 1-based. Columns are 1-based and count code points, with a tab
counting as one column. The end column is exclusive. This is GHC's
convention except for tabs (GHC jumps to the next multiple of 8). Both
tiers convert GHC columns at extraction, with the line's text at hand
(the companion has hie_hs_src). Files without tabs need no work.

A leading UTF-8 BOM: GHC skips it, but hie_hs_src and our blob keep it.
Columns on line 1 don't count it, on both tiers. A lone \r is not a
newline for GHC, so not for us either.

The syntactic module model uses the same convention, so joining the two
tiers is an equality lookup. The browser needs UTF-16 offsets: the
server or the client converts per line, at the edge.

CPP: our cpp keeps every line in place, and spans on its output are
mapped back to the file (Note [Position map] in
HaskellReviewer.Core.Cpp.Positions). A name made by a macro gets the
whole call's span. HIE spans can differ only on lines where a macro was
expanded or lines were joined; such occurrences fail to join and keep
their syntactic link.
-}

-- * Types

-- | 1-based.
newtype Line = Line Word32
  deriving (Eq, Ord, Show)

-- | 1-based, in code points. See Note [Span convention].
newtype Col = Col Word32
  deriving (Eq, Ord, Show)

-- | A range of text, end exclusive. See Note [Span convention].
data Span = Span
  { startLine :: Line,
    startCol :: Col,
    endLine :: Line,
    endCol :: Col
  }
  deriving (Eq, Ord, Show)

-- * Rendering

-- | @3:5-3:9@.
renderSpan :: Span -> Text
renderSpan s =
  Text.concat
    [line s.startLine, ":", col s.startCol, "-", line s.endLine, ":", col s.endCol]
  where
    line (Line n) = Text.pack (show n)
    col (Col n) = Text.pack (show n)
