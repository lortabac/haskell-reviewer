-- | The project root under review.
module HaskellReviewer.Core.Root
  ( -- * Types
    Root (..),
    CanonicalRoot (..),

    -- * Paths
    isWithin,
  )
where

import Data.ByteString (ByteString)
import System.FilePath (splitDirectories)

-- | The directory the tool reviews (ROOT on the command line).
--
-- Invariant: an absolute path to a directory we can read and list. Only
-- 'HaskellReviewer.Effects.FileSystem.checkRoot' should build one. The
-- constructor is still exported: the check needs IO, so it can't live in
-- a pure smart constructor, and tests need fake roots.
newtype Root = Root {path :: FilePath}
  deriving (Eq, Show)

-- | ROOT with symlinks resolved, made once at start-up. File access
-- goes through this one, so a ROOT that is a symlink is resolved once,
-- not per file.
data CanonicalRoot = CanonicalRoot
  { path :: FilePath,
    -- | The same path as bytes, for the unix ByteString API.
    bytes :: ByteString
  }
  deriving (Eq, Show)

-- | @path \`isWithin\` dir@: is @path@ equal to @dir@ or below it?
--
-- Compares whole components, so @\/a\/bc@ is not within @\/a\/b@. Doesn't
-- touch the disk: pass canonical paths if symlinks or @..@ matter.
isWithin :: FilePath -> FilePath -> Bool
isWithin path dir = dirParts == take (length dirParts) pathParts
  where
    dirParts = splitDirectories dir
    pathParts = splitDirectories path
