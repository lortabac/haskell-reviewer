{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Dev only: the GHC oracle for resolution. Compiles each oracle case
-- of fixtures/resolve with GHC (-fno-code -ddump-minimal-imports, boot
-- packages only), one component at a time, and writes
-- fixtures/resolve/<case>.imports: a hash of the case's files, then
-- each module's minimal imports. Tests read those files and never run
-- GHC (Note [Oracle files] in test/ResolveDump.hs). Run with
-- `make resolve-check`.
module Main (main) where

import Control.Monad (forM, unless)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import HaskellReviewer.Core.FileList (ListSource (..), fileListCap, mkFileList)
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    HaskellFile (..),
    ModuleEntry (..),
    ModuleId (..),
    ModuleIndex (..),
    ModuleSource (..),
    PackageModules (..),
    buildModuleIndex,
    renderModuleId,
  )
import HaskellReviewer.Core.Project
  ( Component (..),
    ComponentName (..),
    Extension (..),
    Language (..),
    LibraryDep (..),
    LibraryName (..),
    ModuleName (..),
    Package (..),
    PackageName (..),
    Platform (..),
    Project (..),
    renderComponentName,
  )
import HaskellReviewer.Core.RelPath (relDirText, relPathText)
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.FileSystem (WalkResult (..), canonicalRoot, walkFiles)
import HaskellReviewer.Session.Project (readProject)
import ResolveDump (OracleFile (..), oracleCases, renderOracleFile, sourceHash)
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    getTemporaryDirectory,
    removePathForcibly,
  )
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

main :: IO ()
main = do
  args <- getArgs
  let dir = case args of
        d : _ -> d
        [] -> "fixtures/resolve"
  ghc <- fromMaybe "ghc-9.14.1" <$> lookupEnv "RESOLVE_CHECK_GHC"
  tmp <- (</> "haskell-reviewer-resolve-check") <$> getTemporaryDirectory
  oks <- forM oracleCases $ \c -> do
    let caseDir = dir </> c
    hash <- sourceHash caseDir
    mods <- compileCase ghc (tmp </> c) caseDir
    case mods of
      Nothing -> pure False
      Just ms -> do
        Text.IO.writeFile
          (dir </> c <> ".imports")
          (renderOracleFile OracleFile {hash = hash, modules = ms})
        putStrLn (c <> ": " <> show (length ms) <> " modules")
        pure True
  unless (and oks) exitFailure

-- | Every component of the case, compiled: each module's id and minimal
-- imports.
compileCase :: String -> FilePath -> FilePath -> IO (Maybe [(Text, Text)])
compileCase ghc out caseDir = do
  canon <- canonicalRoot (Root caseDir)
  w <- walkFiles canon (fileListCap + 1)
  let fl = mkFileList FromDisk w.skippedNames w.entries
  project <- readProject canon fl Platform {os = "linux", arch = "x86_64"} Nothing
  let idx = buildModuleIndex project fl
  results <- forM [(p, c) | p <- project.packages, c <- p.components] $ \(p, c) -> do
    let own =
          [ (m.name, f.path)
          | pm <- idx.packages,
            pm.name == p.name,
            cm <- pm.components,
            cm.name == c.name,
            m <- cm.modules,
            HaskellSource f <- [m.source]
          ]
        deps =
          [ d
          | dep <- c.libraryDepends,
            dp <- project.packages,
            dp.name == dep.package,
            d <- dp.components,
            d.name == case dep.library of
              MainLibrary -> Lib
              SubLibrary n -> SubLib n
          ]
        dirs = map relDirText (concatMap (.sourceDirs) (c : deps))
        PackageName pn = p.name
        cdir =
          out
            </> Text.unpack
              ( pn
                  <> "-"
                  <> Text.replace
                    ":"
                    "-"
                    (renderComponentName c.name)
              )
    removePathForcibly cdir
    createDirectoryIfMissing True cdir
    let ghcArgs =
          [ "-package-env",
            "-",
            "-fno-code",
            "-ddump-minimal-imports",
            "-dumpdir",
            cdir,
            "-outputdir",
            cdir
          ]
            <> ["-i" <> Text.unpack d | d <- dirs]
            <> [language c.language]
            <> [ "-X"
                   <> Text.unpack
                     (if e.enabled then e.name else "No" <> e.name)
               | e <- c.extensions
               ]
            <> [Text.unpack (relPathText f) | (_, f) <- own]
    (code, _, err) <-
      readCreateProcessWithExitCode
        (proc ghc ghcArgs)
          { cwd = Just caseDir
          }
        ""
    case code of
      ExitFailure _ -> do
        putStrLn ("ghc failed on " <> caseDir <> ":\n" <> err)
        pure Nothing
      ExitSuccess -> do
        ms <- forM (sort [n | (n, _) <- own]) $ \n@(ModuleName t) -> do
          let file = cdir </> Text.unpack t <> ".imports"
          there <- doesFileExist file
          body <- if there then Text.IO.readFile file else pure ""
          pure (renderModuleId (ComponentModule p.name c.name n), body)
        pure (Just ms)
  pure (concat <$> sequence results)
  where
    language = \case
      Haskell98 -> "-XHaskell98"
      Haskell2010 -> "-XHaskell2010"
      GHC2021 -> "-XGHC2021"
      GHC2024 -> "-XGHC2024"
      UnknownLanguage t -> "-X" <> Text.unpack t
