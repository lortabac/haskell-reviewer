{-# LANGUAGE CPP #-}

-- | The few Cabal-syntax fields whose types changed between 3.12 and
-- 3.14 (paths became typed). The only module with CPP for Cabal-syntax.
module HaskellReviewer.Core.CabalCompat
  ( exeMainIs,
    testMainIs,
    benchMainIs,
    includeDirPaths,
  )
where

import Distribution.Types.Benchmark (Benchmark (..))
import Distribution.Types.BenchmarkInterface (BenchmarkInterface (..))
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.Executable (Executable (..))
import Distribution.Types.TestSuite (TestSuite (..))
import Distribution.Types.TestSuiteInterface (TestSuiteInterface (..))
#if MIN_VERSION_Cabal_syntax(3,14,0)
import Distribution.Utils.Path (SymbolicPathX, getSymbolicPath)
#endif

-- | @main-is@ of an executable, if set.
exeMainIs :: Executable -> Maybe FilePath
exeMainIs e = nonBlank (path (modulePath e))

-- | @main-is@ of an exitcode-stdio test suite, if set.
testMainIs :: TestSuite -> Maybe FilePath
testMainIs t = case testInterface t of
  TestSuiteExeV10 _ p -> nonBlank (path p)
  _ -> Nothing

-- | @main-is@ of an exitcode-stdio benchmark, if set.
benchMainIs :: Benchmark -> Maybe FilePath
benchMainIs b = case benchmarkInterface b of
  BenchmarkExeV10 _ p -> nonBlank (path p)
  _ -> Nothing

-- | Cabal-syntax uses an empty path for "not set".
nonBlank :: FilePath -> Maybe FilePath
nonBlank p = if null p then Nothing else Just p

-- | @include-dirs@, as written.
includeDirPaths :: BuildInfo -> [FilePath]
includeDirPaths bi = map path (includeDirs bi)

#if MIN_VERSION_Cabal_syntax(3,14,0)
path :: SymbolicPathX allowAbsolute from to -> FilePath
path = getSymbolicPath
#else
path :: FilePath -> FilePath
path = id
#endif
