{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Dev only, Linux gcc only (macOS cpp is clang, which reads indented
-- directives): compare our CPP with gcc -traditional run as GHC runs
-- it, line by line, on every .hs file with a directive under the given
-- dirs. Run with `make cpp-check`.
--
-- Both sides use the same assumptions: GHC 9.14.1, the host platform,
-- MIN_VERSION_x(a,b,c) = 1 and defined for every x the file uses,
-- #include read as empty, no flags. So this checks the mechanics only.
module Main (main) where

import Control.Monad (forM)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.IntMap.Strict qualified as IntMap
import Data.List (isPrefixOf, nub, sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import HaskellReviewer.Core.Cpp (CppEnv (..), CppOutput (..), isDirective, runCpp)
import HaskellReviewer.Core.Project
  ( GhcVersion (..),
    PackageName (..),
    Platform (..),
    hostPlatform,
  )
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
  dirs <- getArgs
  files <- concat <$> mapM findHs dirs
  tmp <- (</> "haskell-reviewer-cpp-check") <$> getTemporaryDirectory
  createDirectoryIfMissing True tmp
  let header = tmp </> "ghcversion.h"
  writeFile header ghcVersionH
  results <- forM files $ \f -> do
    t <- Text.IO.readFile f
    if any isDirective (Text.lines t) then Just <$> check tmp header f t else pure Nothing
  let rs = [r | Just r <- results]
  putStrLn $
    "same: "
      <> show (length [() | Same <- rs])
      <> ", differ: "
      <> show (length [() | Differ <- rs])
      <> ", gcc failed: "
      <> show (length [() | GccFailed <- rs])

data Result = Same | Differ | GccFailed

check :: FilePath -> FilePath -> FilePath -> Text -> IO Result
check tmp header f t = do
  let input = tmp </> "input.hs"
      names = minVersions t
      env =
        CppEnv
          { ghc = GhcVersion [9, 14, 1],
            platform = hostPlatform,
            flags = [],
            buildDepends = map PackageName names
          }
      ours = Text.splitOn "\n" (runCpp env [] t).text
  Text.IO.writeFile input (Text.unlines (map blankInclude (Text.splitOn "\n" t)))
  (code, out, err) <- readProcessWithExitCode "gcc" (gccArgs header names input) ""
  let theirs = byLine input (lines out)
      diffs =
        [ (n, o, g)
        | (n, o) <- zip [1 ..] ours,
          let g = IntMap.findWithDefault "" n theirs,
          strip o /= strip g
        ]
  case (code, diffs) of
    (ExitFailure _, _) -> do
      putStrLn ("gcc failed: " <> f <> ": " <> take 200 err)
      pure GccFailed
    (_, []) -> pure Same
    (_, ds) -> do
      putStrLn ("differs: " <> f)
      mapM_ showDiff (take 3 ds)
      pure Differ
  where
    strip = Text.stripEnd
    showDiff (n, o, g) = do
      putStrLn ("  " <> show (n :: Int) <> " ours: " <> Text.unpack o)
      putStrLn ("  " <> show n <> " gcc:  " <> Text.unpack g)

-- | GHC's cpp flags (ghc -v3 shows them) for the host, plus MIN_VERSION_x.
gccArgs :: FilePath -> [Text] -> FilePath -> [String]
gccArgs header names input =
  ["-E", "-undef", "-traditional", "-x", "assembler-with-cpp", "-include", header]
    <> map ("-D" <>) platformMacros
    <> ["-D__GLASGOW_HASKELL_TH__", "-D__IO_MANAGER_MIO__=1"]
    <> (if arch `elem` ["x86_64", "i386"] then ["-D__SSE__", "-D__SSE2__"] else [])
    <> ["-DMIN_VERSION_" <> Text.unpack n <> "(a,b,c)=1" | n <- names]
    <> [input]
  where
    os = Text.unpack hostPlatform.os
    arch = Text.unpack hostPlatform.arch
    platformMacros =
      [os <> "_BUILD_OS", arch <> "_BUILD_ARCH", os <> "_HOST_OS", arch <> "_HOST_ARCH"]

-- | gcc's output by line of our file, read from its line markers.
byLine :: FilePath -> [String] -> IntMap.IntMap Text
byLine input = go False 1 IntMap.empty
  where
    go mine n acc = \case
      [] -> acc
      l : ls
        | Just (k, file) <- marker l -> go (file == input) k acc ls
        | mine -> go mine (n + 1) (IntMap.insert n (Text.pack l) acc) ls
        | otherwise -> go mine n acc ls
    marker l = case words l of
      "#" : k : file : _
        | all isDigit k, "\"" `isPrefixOf` file -> Just (read k, init (drop 1 file))
      _ -> Nothing

-- | The MIN_VERSION_x names a file uses, without GLASGOW_HASKELL.
minVersions :: Text -> [Text]
minVersions t =
  sort . nub $
    [ n
    | w <- Text.split (not . isWordChar) t,
      Just n <- [Text.stripPrefix "MIN_VERSION_" w],
      n /= "GLASGOW_HASKELL",
      not (Text.null n)
    ]

isWordChar :: Char -> Bool
isWordChar c = c == '_' || isAsciiLower c || isAsciiUpper c || isDigit c

-- | gcc would fail on a missing file; we read #include as empty.
blankInclude :: Text -> Text
blankInclude l = case Text.stripPrefix "#" l of
  Just r | any (`Text.isPrefixOf` Text.dropWhile (== ' ') r) ["include", "import"] -> ""
  _ -> l

-- | ghcversion.h of GHC 9.14.1.
ghcVersionH :: String
ghcVersionH =
  unlines
    [ "#define __GHCVERSION_H__",
      "#define __GLASGOW_HASKELL__ 914",
      "#define __GLASGOW_HASKELL_FULL_VERSION__ \"9.14.1\"",
      "#define __GLASGOW_HASKELL_PATCHLEVEL1__ 1",
      "#define __GLASGOW_HASKELL_PATCHLEVEL2__ 0",
      "#define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) ( \\",
      "   ((ma)*100+(mi)) <  __GLASGOW_HASKELL__ || \\",
      "   ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \\",
      "          && (pl1) <  __GLASGOW_HASKELL_PATCHLEVEL1__ || \\",
      "   ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \\",
      "          && (pl1) == __GLASGOW_HASKELL_PATCHLEVEL1__ \\",
      "          && (pl2) <= __GLASGOW_HASKELL_PATCHLEVEL2__ )"
    ]

-- | Every .hs file under a dir (or the file itself).
findHs :: FilePath -> IO [FilePath]
findHs p = do
  isDir <- doesDirectoryExist p
  if isDir
    then do
      entries <- sort <$> listDirectory p
      concat <$> mapM (findHs . (p </>)) [e | e <- entries, not ("." `isPrefixOf` e)]
    else do
      isFile <- doesFileExist p
      pure [p | isFile, takeExtension p == ".hs"]
