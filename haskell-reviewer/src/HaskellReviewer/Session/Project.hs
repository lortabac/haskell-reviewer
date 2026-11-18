-- | Load the project model of a snapshot, from its source (disk or a
-- commit). The logic is pure, in "HaskellReviewer.Core.ProjectLoad";
-- this only reads the files.
module HaskellReviewer.Session.Project
  ( readProject,
    readProjectFile,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import HaskellReviewer.Core.FileContent (FileContent (..))
import HaskellReviewer.Core.FileList (EntryKind (..), FileList (..))
import HaskellReviewer.Core.Project
  ( FileProblem (..),
    GhcVersion,
    Platform,
    Project,
    ProjectConfig (..),
    detectGhc,
  )
import HaskellReviewer.Core.ProjectLoad (loadProjectConfig, resolveProject)
import HaskellReviewer.Core.RelPath (RelPath)
import HaskellReviewer.Effects.FileSystem (ReadError (..))
import HaskellReviewer.Effects.Source (EntrySource (..), readSourceEntry)

-- | Both phases in one go. @cliGhc@ is @--ghc@, if given: it wins over
-- @with-compiler@ (§7.1).
readProject :: EntrySource -> FileList -> Platform -> Maybe GhcVersion -> IO Project
readProject source fl platform cliGhc = do
  config <- loadProjectConfig (readProjectFile source fl) fl
  let detected = detectGhc cliGhc config.withCompiler
  resolveProject (readProjectFile source fl) platform detected fl config

-- | Read a project file. On disk, safely (Note [File containment] in
-- "HaskellReviewer.Core.RelPath"). The disk tells what a path is; a
-- commit only knows its list, so we take the kind from there. A
-- submodule or nested repo is a directory on disk, so 'Gone' there:
-- the same for a commit. The working-tree side takes it from its list
-- too: a dirty file's kind there came from lstat.
readProjectFile :: EntrySource -> FileList -> RelPath -> IO (Either FileProblem Text)
readProjectFile source fl p = case source of
  DiskEntries _ -> classify <$> readSourceEntry source p EntryFile
  CommitEntries _ -> fromList
  WorkTreeEntries _ -> fromList
  where
    fromList = case Map.findWithDefault EntryFile p fl.entries of
      EntrySubmodule _ -> pure (Left Gone)
      EntryNestedRepo -> pure (Left Gone)
      kind -> classify <$> readSourceEntry source p kind
    classify = \case
      Right (TextContent t) -> Right t
      Right (Binary _) -> Left NotText
      Right (TooLarge _) -> Left TooBig
      Right (Link _) -> Left IsSymlink
      Right _ -> Left NotAFile
      Left ReadGone -> Left Gone
      Left (ReadFailed e) -> Left (CantRead e)
