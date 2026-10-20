module HaskellReviewer.Effects.HelperTest (tests) where

import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.Helper (findHelperIn)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Helper"
    [ testCase "found in a dir outside ROOT" $
        withDirs $ \root outside -> do
          exe <- makeTool outside True
          found <- findHelperIn [outside] root "tool"
          found @?= Just exe,
      testCase "first match wins" $
        withDirs $ \root outside -> do
          let second = outside </> "second"
          Dir.createDirectory second
          _ <- makeTool second True
          exe <- makeTool outside True
          found <- findHelperIn [outside, second] root "tool"
          found @?= Just exe,
      testCase "relative PATH entry skipped" $
        withDirs $ \root outside -> do
          _ <- makeTool outside True
          Dir.withCurrentDirectory outside $ do
            found <- findHelperIn [".", "", "rel"] root "tool"
            found @?= Nothing,
      testCase "dir inside ROOT skipped" $
        withDirs $ \root _ -> do
          let bin = root.path </> "bin"
          Dir.createDirectory bin
          _ <- makeTool bin True
          found <- findHelperIn [bin, root.path] root "tool"
          found @?= Nothing,
      testCase "symlinked dir pointing into ROOT skipped" $
        withDirs $ \root outside -> do
          let bin = root.path </> "bin"
              link = outside </> "link"
          Dir.createDirectory bin
          _ <- makeTool bin True
          Dir.createDirectoryLink bin link
          found <- findHelperIn [link] root "tool"
          found @?= Nothing,
      testCase "symlinked tool pointing into ROOT skipped" $
        withDirs $ \root outside -> do
          planted <- makeTool root.path True
          Dir.createFileLink planted (outside </> "tool")
          found <- findHelperIn [outside] root "tool"
          found @?= Nothing,
      testCase "non-executable file skipped" $
        withDirs $ \root outside -> do
          let other = outside </> "other"
          Dir.createDirectory other
          _ <- makeTool outside False
          exe <- makeTool other True
          found <- findHelperIn [outside, other] root "tool"
          found @?= Just exe,
      testCase "missing dir skipped" $
        withDirs $ \root outside -> do
          found <- findHelperIn [outside </> "nope"] root "tool"
          found @?= Nothing
    ]

-- | A canonical ROOT and a canonical dir outside it.
withDirs :: (Root -> FilePath -> IO a) -> IO a
withDirs action =
  withSystemTempDirectory "haskell-reviewer-test" $ \tmp -> do
    base <- Dir.canonicalizePath tmp
    let root = base </> "root"
        outside = base </> "outside"
    Dir.createDirectory root
    Dir.createDirectory outside
    action (Root root) outside

-- | Write a script called @tool@ in the dir. Gives back its path.
makeTool :: FilePath -> Bool -> IO FilePath
makeTool dir executable = do
  let file = dir </> "tool"
  writeFile file "#!/bin/sh\n"
  perms <- Dir.getPermissions file
  Dir.setPermissions file (Dir.setOwnerExecutable executable perms)
  pure file
