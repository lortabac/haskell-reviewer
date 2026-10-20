-- | Find helper programs (@xdg-open@, @git@, ...) on PATH, safely.
module HaskellReviewer.Effects.Helper
  ( -- * Lookup
    findHelper,
    findHelperIn,
  )
where

import Control.Exception (IOException, try)
import Data.Maybe (fromMaybe)
import HaskellReviewer.Core.Root (Root (..), isWithin)
import System.Directory qualified as Dir
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, splitSearchPath, (</>))

{- Note [Helper lookup]

SPEC §11.4.1, D-75: the code under review is untrusted (SPEC §9.1). If PATH
has "." or "bin" in it and the user runs us from ROOT, a PR could add
bin/git or bin/xdg-open and we would run it. So:

- relative PATH entries (and empty ones, which mean ".") are skipped;
- a PATH dir inside ROOT is skipped, after resolving symlinks on both;
- the program found is skipped if its real path (symlinks resolved) is
  inside ROOT. This catches /usr/local/bin/foo -> ROOT/foo.

We return the canonical dir plus the name, not the fully resolved path:
some programs look at argv[0].
-}

-- | Look up a program on @PATH@. See Note [Helper lookup].
findHelper :: Root -> String -> IO (Maybe FilePath)
findHelper root name = do
  path <- fromMaybe "" <$> lookupEnv "PATH"
  findHelperIn (splitSearchPath path) root name

-- | Like 'findHelper', with the PATH entries given. For tests.
findHelperIn :: [FilePath] -> Root -> String -> IO (Maybe FilePath)
findHelperIn dirs root name = do
  rootDir <- fromMaybe root.path <$> canonical root.path
  let go [] = pure Nothing
      go (dir : rest)
        | not (isAbsolute dir) = go rest
        | otherwise = do
            canonDir <- canonical dir
            case canonDir of
              Just d | not (d `isWithin` rootDir) -> do
                let candidate = d </> name
                ok <- isExecutableFile candidate
                real <- canonical candidate
                case real of
                  Just r | ok && not (r `isWithin` rootDir) -> pure (Just candidate)
                  _ -> go rest
              _ -> go rest
  go dirs

-- * Internal

-- | Resolve symlinks, @.@ and @..@. 'Nothing' on an IO error. A missing
-- path is not an error: it comes back as is.
canonical :: FilePath -> IO (Maybe FilePath)
canonical p = either (const Nothing) Just <$> try @IOException (Dir.canonicalizePath p)

isExecutableFile :: FilePath -> IO Bool
isExecutableFile p = do
  result <- try @IOException $ do
    isFile <- Dir.doesFileExist p
    if isFile then Dir.executable <$> Dir.getPermissions p else pure False
  pure (either (const False) id result)
