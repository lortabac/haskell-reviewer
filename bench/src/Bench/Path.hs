-- | Paths for reports, without the user's home directory in them.
module Bench.Path
  ( shortPath,
  )
where

import System.FilePath (isRelative, makeRelative, (</>))

-- | @shortPath cwd home path@: @path@ relative to @cwd@ when it is under
-- it, else @~\/...@ when it is under @home@, else as is.
shortPath :: FilePath -> Maybe FilePath -> FilePath -> FilePath
shortPath cwd home path
  | isRelative path = path
  | Just rel <- under cwd = rel
  | Just h <- home, Just rel <- under h = "~" </> rel
  | otherwise = path
  where
    under dir
      | null dir = Nothing
      | otherwise =
          let rel = makeRelative dir path
           in if isRelative rel then Just rel else Nothing
