-- | Load the project model in two phases. See Note [Project model] in
-- "HaskellReviewer.Core.Project".
module HaskellReviewer.Core.ProjectLoad
  ( -- * Types
    FileReader,

    -- * Loading
    loadProjectConfig,
    resolveProject,
  )
where

import Control.Applicative ((<|>))
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.CabalFile (evalCondition, evalConfVar, readCabalFile)
import HaskellReviewer.Core.FileList (EntryKind (..), FileList (..))
import HaskellReviewer.Core.PackageGlob (expandEntry, mkDirIndex, rootCabalFiles)
import HaskellReviewer.Core.Project
  ( BrokenPackage (..),
    CabalProblem (..),
    FileProblem (..),
    FlagsField,
    Loc (..),
    Package (..),
    PackageEntry,
    Project (..),
    ProjectConfig (..),
    ProjectItem (..),
    ProjectSettings,
    ProjectSource (..),
    ProjectWarning (..),
    WithCompiler (..),
  )
import HaskellReviewer.Core.ProjectFile
  ( RawItem (..),
    parseProjectFile,
    parseWithCompilerVersion,
  )
import HaskellReviewer.Core.RelPath
  ( RelPath,
    parentDir,
    relDirFile,
    relPathFromSegments,
    resolveUnder,
  )

{- Note [cabal.project conditionals]

cabal.project can have if / elif / else at top level. Like cabal:

- Only os(), arch(), impl(), true and false make sense there. They are
  decided with 'ProjectSettings', the same way as in .cabal files.
- flag() has no meaning in a project file: false, with a warning.
- Conditionals are applied last: the fields outside any if come first,
  then those in the taken branches, in file order. That matters for
  flags, where the last value wins.
- with-compiler inside a conditional is ignored, with a warning: the
  conditions need the GHC version, which with-compiler gives.
- An import inside a branch is read, and its fields belong to that
  branch.

Imports are relative to the dir of the file that imports them (like
cabal 3.14+), must stay in the snapshot, and are read only once per
chain: a file that imports one of its importers is a cycle, skipped
with a warning. Remote imports are skipped with a warning.
-}

-- | Reads a file of the snapshot. Only called for entries that are
-- plain files in the file list.
type FileReader m = RelPath -> m (Either FileProblem Text)

-- | Phase 1: cabal.project and its imports. Without a cabal.project,
-- or when it can't be read or doesn't lex, the *.cabal files in ROOT
-- are used, with a warning.
loadProjectConfig :: (Monad m) => FileReader m -> FileList -> m ProjectConfig
loadProjectConfig readIn fl = case projectPath of
  Nothing -> pure (implicit [])
  Just path -> case Map.lookup path fl.entries of
    Just EntryFile -> do
      r <- readIn path
      case r of
        Left e -> pure (implicit [ProjectUnreadable path e])
        Right txt -> case parseProjectFile path txt of
          Left e -> pure (implicit [ProjectSyntax path e])
          Right (raw, warns) -> do
            loaded <- expand readIn fl [path] TopLevel raw
            pure
              ProjectConfig
                { source = ProjectFile loaded.imports,
                  items = loaded.items,
                  withCompiler = loaded.withCompiler,
                  warnings = warns <> loaded.warnings
                }
    Just EntrySymlink -> pure (implicit [ProjectUnreadable path IsSymlink])
    _ -> pure (implicit [])
  where
    projectPath = relPathFromSegments ["cabal.project"]
    implicit ws =
      ProjectConfig
        { source = ImplicitRootCabalFiles,
          items = [],
          withCompiler = Nothing,
          warnings = ws
        }

-- | Phase 2: decide the conditionals, find and read the .cabal files.
resolveProject ::
  (Monad m) =>
  FileReader m ->
  ProjectSettings ->
  FileList ->
  ProjectConfig ->
  m Project
resolveProject readIn settings fl config = do
  results <- traverse readOne cabalPaths
  let packages = [p | Right (p, _) <- results]
      (kept, dupWarnings) = dropDuplicates packages
  pure
    Project
      { source = config.source,
        withCompiler = config.withCompiler,
        packages = kept,
        brokenPackages = [b | Left b <- results],
        warnings =
          config.warnings
            <> entryWarnings
            <> concat [ws | Right (_, ws) <- results]
            <> dupWarnings
            <> [FileListTruncated | fl.truncated]
      }
  where
    idx = mkDirIndex fl
    (entries, flags) = flatten settings config.items
    (found, entryWarnings) = case config.source of
      ProjectFile _ -> mconcat (map (expandEntry idx) entries)
      ImplicitRootCabalFiles -> (rootCabalFiles idx, [])
    cabalPaths = Set.toList (Set.fromList found)

    readOne path = case Map.lookup path fl.entries of
      Just EntryFile -> do
        r <- readIn path
        pure $ case r of
          Left e -> Left (broken path (CabalUnreadable e))
          Right txt -> case readCabalFile settings flags path txt of
            Left e -> Left (broken path e)
            Right ok -> Right ok
      Just EntrySymlink -> pure (Left (broken path (CabalUnreadable IsSymlink)))
      _ -> pure (Left (broken path (CabalUnreadable NotAFile)))
    broken path e = BrokenPackage {cabalFile = path, problem = e}

-- * Internal

-- | What phase 1 found so far.
data Loaded = Loaded
  { items :: [ProjectItem],
    warnings :: [ProjectWarning],
    -- | Imports read, in read order.
    imports :: [RelPath],
    withCompiler :: Maybe WithCompiler
  }

-- | Join in file order. A later with-compiler wins.
instance Semigroup Loaded where
  a <> b =
    Loaded
      { items = a.items <> b.items,
        warnings = a.warnings <> b.warnings,
        imports = a.imports <> b.imports,
        withCompiler = b.withCompiler <|> a.withCompiler
      }

instance Monoid Loaded where
  mempty = Loaded {items = [], warnings = [], imports = [], withCompiler = Nothing}

-- | Where the items being read are.
data Place = TopLevel | InConditional

-- | Follow imports and pick with-compiler. The chain is the files
-- being read, the current one first (for cycles). See
-- Note [cabal.project conditionals].
expand ::
  (Monad m) => FileReader m -> FileList -> [RelPath] -> Place -> [RawItem] -> m Loaded
expand readIn fl chain place = fmap mconcat . traverse one
  where
    one = \case
      RawPackage e -> item (ItemPackage e)
      RawFlags f -> item (ItemFlags f)
      RawWithCompiler l t -> case place of
        InConditional -> warn (WithCompilerInConditional l)
        TopLevel ->
          pure
            Loaded
              { items = [],
                warnings = [],
                imports = [],
                withCompiler =
                  Just
                    WithCompiler
                      { written = t,
                        version = parseWithCompilerVersion t,
                        file = l.file
                      }
              }
      RawImport l t -> importFile l t
      RawIf c yes no -> do
        y <- expand readIn fl chain InConditional yes
        n <- expand readIn fl chain InConditional no
        pure
          Loaded
            { items = [ItemIf c y.items n.items],
              warnings = y.warnings <> n.warnings,
              imports = y.imports <> n.imports,
              withCompiler = Nothing
            }

    item i = pure (only [i] [])
    warn w = pure (only [] [w])
    only is ws = Loaded {items = is, warnings = ws, imports = [], withCompiler = Nothing}

    importFile l t
      | "://" `Text.isInfixOf` t = warn (ImportRemote l t)
      | otherwise = case resolveUnder (parentDir l.file) t of
          Left e -> warn (ImportBadPath l t e)
          Right d -> case relDirFile d of
            Nothing -> warn (ImportMissing l t)
            Just path
              | path `elem` chain -> warn (ImportCycle l t)
              | otherwise -> case Map.lookup path fl.entries of
                  Just EntryFile -> readImport path
                  Just EntrySymlink -> warn (ProjectUnreadable path IsSymlink)
                  _ -> warn (ImportMissing l t)

    readImport path = do
      r <- readIn path
      case r of
        Left e -> warn (ProjectUnreadable path e)
        Right txt -> case parseProjectFile path txt of
          Left e -> warn (ProjectSyntax path e)
          Right (raw, ws) -> do
            sub <- expand readIn fl (path : chain) place raw
            pure
              Loaded
                { items = sub.items,
                  warnings = ws <> sub.warnings,
                  imports = path : sub.imports,
                  withCompiler = sub.withCompiler
                }

-- | The package entries and flags fields that apply, in the order cabal
-- applies them. See Note [cabal.project conditionals].
flatten :: ProjectSettings -> [ProjectItem] -> ([PackageEntry], [FlagsField])
flatten settings items = plain <> mconcat (map taken conds)
  where
    plain = ([e | ItemPackage e <- items], [f | ItemFlags f <- items])
    conds = [(c, yes, no) | ItemIf c yes no <- items]
    taken (c, yes, no)
      | evalCondition (evalConfVar settings (const False)) c = flatten settings yes
      | otherwise = flatten settings no

-- | Keep the first package of each name (they come sorted by path),
-- warn about the others.
dropDuplicates :: [Package] -> ([Package], [ProjectWarning])
dropDuplicates pkgs = (kept, warnings)
  where
    byName = Map.fromListWith (flip (<>)) [(p.name, [p]) | p <- pkgs]
    kept = sortOn (.cabalFile) [p | p : _ <- Map.elems byName]
    warnings =
      [ DuplicatePackage n (map (.cabalFile) ps)
      | (n, ps@(_ : _ : _)) <- Map.toList byName
      ]
