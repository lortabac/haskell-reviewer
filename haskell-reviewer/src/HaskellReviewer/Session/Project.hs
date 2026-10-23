-- | Load the project model of ROOT from disk. The logic is pure, in
-- "HaskellReviewer.Core.ProjectLoad"; this only reads the files.
module HaskellReviewer.Session.Project
  ( readProject,
    readProjectFile,
  )
where

import Data.Text (Text)
import HaskellReviewer.Core.FileContent (FileContent (..))
import HaskellReviewer.Core.FileList (EntryKind (..), FileList)
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
import HaskellReviewer.Core.Root (CanonicalRoot)
import HaskellReviewer.Effects.FileSystem (ReadError (..), readEntry)

-- | Both phases in one go, for a snapshot on disk. @cliGhc@ is
-- @--ghc@, if given: it wins over @with-compiler@ (§7.1).
readProject :: CanonicalRoot -> FileList -> Platform -> Maybe GhcVersion -> IO Project
readProject canon fl platform cliGhc = do
  config <- loadProjectConfig (readProjectFile canon) fl
  let detected = detectGhc cliGhc config.withCompiler
  resolveProject (readProjectFile canon) platform detected fl config

-- | Read a project file safely (Note [File containment] in
-- "HaskellReviewer.Core.RelPath").
readProjectFile :: CanonicalRoot -> RelPath -> IO (Either FileProblem Text)
readProjectFile canon p = do
  r <- readEntry canon p EntryFile
  pure $ case r of
    Right (TextContent t) -> Right t
    Right (Binary _) -> Left NotText
    Right (TooLarge _) -> Left TooBig
    Right (Link _) -> Left IsSymlink
    Right _ -> Left NotAFile
    Left ReadGone -> Left Gone
    Left (ReadFailed e) -> Left (CantRead e)
