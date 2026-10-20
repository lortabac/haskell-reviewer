{-# LANGUAGE MultiWayIf #-}

-- | File system access.
module HaskellReviewer.Effects.FileSystem
  ( -- * Types
    RootError (..),

    -- * Operations
    checkRoot,
    renderRootError,
  )
where

import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Root (Root (..))
import System.Directory qualified as Dir

-- | Why ROOT can't be used. Each one holds the path as the user gave it.
data RootError
  = RootMissing FilePath
  | RootNotADirectory FilePath
  | -- | No read or list permission, or some other IO error.
    RootUnreadable FilePath
  deriving (Eq, Show)

-- | Message for the user.
renderRootError :: RootError -> Text
renderRootError = \case
  RootMissing p -> "ROOT does not exist: " <> Text.pack p
  RootNotADirectory p -> "ROOT is not a directory: " <> Text.pack p
  RootUnreadable p -> "cannot read ROOT: " <> Text.pack p

-- | Check that a path is a readable directory and make it absolute.
--
-- Symlinks are not resolved: we only make the path absolute.
checkRoot :: FilePath -> IO (Either RootError Root)
checkRoot p = do
  result <- try @IOException check
  pure $ case result of
    Left _ -> Left (RootUnreadable p)
    Right r -> r
  where
    check = do
      exists <- Dir.doesPathExist p
      isDir <- Dir.doesDirectoryExist p
      canRead <-
        if isDir
          then readableDir <$> Dir.getPermissions p
          else pure False
      if
        | not exists -> pure (Left (RootMissing p))
        | not isDir -> pure (Left (RootNotADirectory p))
        | not canRead -> pure (Left (RootUnreadable p))
        | otherwise -> Right . Root <$> Dir.makeAbsolute p
    readableDir perms = Dir.readable perms && Dir.searchable perms
