-- | The project root under review.
module HaskellReviewer.Core.Root
  ( -- * Types
    Root (..),
  )
where

-- | The directory the tool reviews (ROOT on the command line).
--
-- Invariant: an absolute path to a directory we can read and list. Only
-- 'HaskellReviewer.Effects.FileSystem.checkRoot' should build one. The
-- constructor is still exported: the check needs IO, so it can't live in
-- a pure smart constructor, and tests need fake roots.
newtype Root = Root {path :: FilePath}
  deriving (Eq, Show)
