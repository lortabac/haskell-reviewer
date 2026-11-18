module HaskellReviewer.Effects.RevisionsTest (tests) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileList (FileList (..))
import HaskellReviewer.Core.Git (History (..), ObjectId (..))
import HaskellReviewer.Core.RelPath (relPathText)
import HaskellReviewer.Core.Revspec
  ( BaseFrom (..),
    CommitsInfo (..),
    DiffInfo (..),
    Range (..),
    RangeForm (..),
    Revspec (..),
    WorkTreeInfo (..),
    parseRevspec,
  )
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Effects.Git
  ( GitError,
    GitSetup (..),
    gitListTimeLimit,
    gitProbeTimeLimit,
    repoInfo,
  )
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import HaskellReviewer.Effects.Revisions (DiffError (..), renderDiffError, resolveDiff)
import HaskellReviewer.Effects.Source
  ( Listed (..),
    Lister (..),
    Probe (..),
    ProbeError (..),
    probeFilesWith,
    renderProbeError,
  )
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport (fixtureRepo, git, gitOut, gitSetup, realGitAt)

tests :: TestTree
tests =
  -- One fixture for the group: Note [Shared values in tests] in
  -- "HaskellReviewer.Core.PackageGlobTest".
  withResource makeRepos Dir.removeDirectoryRecursive $ \get ->
    testGroup
      "Revisions"
      [ testCase "BASE..HEAD: both ends as typed" $ do
          t <- get
          info <- resolveOk t "light..feature"
          commitOf t "light" >>= (@?= info.sides.base)
          commitOf t "feature" >>= (@?= info.sides.head)
          info.baseFrom @?= BaseTyped,
        testCase "BASE...HEAD: the merge base" $ do
          t <- get
          info <- resolveOk t "main...feature"
          commitOf t "feature~1" >>= (@?= info.sides.base)
          commitOf t "feature" >>= (@?= info.sides.head)
          info.baseFrom @?= MergeBase 1,
        testCase "an empty end is HEAD" $ do
          t <- get
          info <- resolveOk t "light.."
          commitOf t "main" >>= (@?= info.sides.head),
        testCase "two merge bases: the first one, and the count" $ do
          t <- get
          info <- resolveOk t "cross-a...cross-b"
          info.baseFrom @?= MergeBase 2
          bases <- mapM (commitOf t) ["a1", "b1"]
          assertBool (show info) (info.sides.base `elem` bases),
        testCase "an annotated tag is peeled to its commit" $ do
          t <- get
          info <- resolveOk t "v1..main"
          commitOf t "v1^{commit}" >>= (@?= info.sides.base),
        testCase "unknown, not a commit: the base first" $ do
          t <- get
          resolveErr t "nope..HEAD" >>= (@?= DiffUnknownRev "nope" FullHistory)
          resolveErr t "HEAD..nope" >>= (@?= DiffUnknownRev "nope" FullHistory)
          resolveErr t "nope..nada" >>= (@?= DiffUnknownRev "nope" FullHistory)
          resolveErr t "blobtag..HEAD" >>= (@?= DiffUnknownRev "blobtag" FullHistory)
          resolveErr t "HEAD:pkg..HEAD" >>= (@?= DiffUnknownRev "HEAD:pkg" FullHistory)
          resolveErr t "--output=x..HEAD"
            >>= (@?= DiffUnknownRev "--output=x" FullHistory)
          -- rev-parse exits 0 here, and prints ^<id>.
          resolveErr t "^main..HEAD" >>= (@?= DiffUnknownRev "^main" FullHistory),
        testCase "messages: one line, no hint in a full clone" $ do
          renderDiffError (DiffUnknownRev "nope" FullHistory)
            @?= "--diff: unknown revision or not a commit: nope"
          renderDiffError (DiffUnknownRev "a\nb\ESC" FullHistory)
            @?= "--diff: unknown revision or not a commit: a\\nb\\ESC"
          renderDiffError (DiffNoMergeBase (Range ThreeDot "a\tb" "c") FullHistory)
            @?= "--diff: no merge base for a\\tb...c"
          renderProbeError (ProbeNotWorkTree "/x\ny")
            @?= "--diff needs ROOT in a git work tree: /x\\ny",
        testCase "no merge base" $ do
          t <- get
          resolveErr t "main...lonely"
            >>= (@?= DiffNoMergeBase (Range ThreeDot "main" "lonely") FullHistory),
        testCase "shallow clone: the hint" $ do
          t <- get
          e1 <- resolveIn t "shallow" "HEAD~1..HEAD"
          e1 @?= Right (Left (DiffUnknownRev "HEAD~1" ShallowClone))
          renderDiffError (DiffUnknownRev "HEAD~1" ShallowClone)
            @?= "--diff: unknown revision or not a commit: HEAD~1; \
                \this is a shallow clone, its history may be cut"
          e2 <- resolveIn t "shallow" "main...origin/feature"
          let r = Range ThreeDot "main" "origin/feature"
          e2 @?= Right (Left (DiffNoMergeBase r ShallowClone))
          renderDiffError (DiffNoMergeBase r ShallowClone)
            @?= "--diff: no merge base for main...origin/feature; \
                \this is a shallow clone: fetch more history",
        testCase "probe: no git, not a work tree" $ do
          t <- get
          let none =
                GitSetup
                  { searchPath = [],
                    inherited = [],
                    probeTimeLimit = gitProbeTimeLimit,
                    listTimeLimit = gitListTimeLimit
                  }
          probeErr none (t </> "repo") "a..b" >>= (@?= ProbeNoGit)
          setup <- gitSetup (t </> "home") gitProbeTimeLimit
          probeErr setup (t </> "plain") "a..b"
            >>= (@?= ProbeNotWorkTree (t </> "plain"))
          probeErr setup (t </> "repo" </> ".git") "a..b"
            >>= (@?= ProbeNotWorkTree (t </> "repo" </> ".git"))
          probeErr none (t </> "repo") "HEAD" >>= (@?= ProbeNoGit)
          probeErr setup (t </> "plain") "HEAD"
            >>= (@?= ProbeNotWorkTree (t </> "plain"))
          probeErr setup (t </> "repo") "nope..HEAD"
            >>= (@?= ProbeDiff (DiffUnknownRev "nope" FullHistory)),
        testCase "probe REV: unknown, not a commit, shallow" $ do
          t <- get
          setup <- gitSetup (t </> "home") gitProbeTimeLimit
          let unknown h rev = ProbeDiff (DiffUnknownRev rev h)
          mapM_
            (\rev -> probeErr setup (t </> "repo") rev >>= (@?= unknown FullHistory rev))
            ["nope", "blobtag", "HEAD:pkg", "--output=x", "^main"]
          probeErr setup (t </> "shallow") "HEAD~1"
            >>= (@?= unknown ShallowClone "HEAD~1"),
        testCase "probe REV: base the commit, head the working tree" $ do
          t <- get
          setup <- gitSetup (t </> "home") gitProbeTimeLimit
          let root = Root (t </> "wt" </> "pkg")
          probeFilesWith setup quiet root (Just (AgainstWorkTree "v1")) >>= \case
            Left e -> assertFailure (show e)
            Right (DiffProbe (WorkTreeDiff info) with) -> do
              info.rev @?= "v1"
              commitOf t "v1^{commit}" >>= (@?= info.base)
              with $ \p -> do
                h <- names p.head
                b <- names p.base
                let main = ["pkg.cabal", "src/A.hs", "src/B.hs", "src/C.hs", "src/F.hs"]
                h @?= main <> ["src/G.hs", "src/New.hs"]
                b @?= ["pkg.cabal", "src/A.hs", "src/B.hs"]
            Right (DiffProbe info _) -> assertFailure (show info)
            Right (BrowseProbe _) -> assertFailure "browse probe",
        testCase "probe: two commit sides, under ROOT only" $ do
          t <- get
          setup <- gitSetup (t </> "home") gitProbeTimeLimit
          let spec = RangeSpec Range {form = ThreeDot, base = "main", head = "feature"}
          probeFilesWith setup quiet (Root (t </> "repo" </> "pkg")) (Just spec) >>= \case
            Left e -> assertFailure (show e)
            Right (DiffProbe (CommitsDiff info) with) -> do
              info.baseFrom @?= MergeBase 1
              with $ \p -> do
                h <- names p.head
                b <- names p.base
                let f2 = ["pkg.cabal", "src/A.hs", "src/B.hs", "src/F.hs", "src/G.hs"]
                h @?= f2 <> ["src/H.hs"]
                b @?= f2
            Right (DiffProbe info _) -> assertFailure (show info)
            Right (BrowseProbe _) -> assertFailure "browse probe"
      ]

-- | The fixture, a plain dir, an empty HOME, a shallow clone with
-- every branch tip, and a clone of main with an untracked file.
makeRepos :: IO FilePath
makeRepos = do
  tmp <- getCanonicalTemporaryDirectory
  t <- createTempDirectory tmp "revisions" >>= Dir.canonicalizePath
  fixtureRepo "revspecs" [t </> "repo"]
  mapM_ (Dir.createDirectory . (t </>)) ["home", "plain"]
  let url = "file://" <> t </> "repo"
  git t ["clone", "-q", "--depth", "1", "--no-single-branch", url, "shallow"]
  git t ["clone", "-q", url, "wt"]
  writeFile (t </> "wt" </> "pkg" </> "src" </> "New.hs") "module New where\n"
  pure t

quiet :: Logger
quiet = newLogger LevelInfo (\_ -> pure ())

-- | Resolve in a repo of the fixture (repo or shallow), ROOT at pkg/.
resolveIn ::
  FilePath -> FilePath -> Text -> IO (Either GitError (Either DiffError CommitsInfo))
resolveIn t repo spec = do
  g <- realGitAt (t </> "home") (t </> repo </> "pkg")
  info <- repoInfo g >>= either (assertFailure . show) pure
  case parseRevspec spec of
    Right (RangeSpec r) -> resolveDiff g info r
    other -> assertFailure ("not a range: " <> show other)

resolveOk :: FilePath -> Text -> IO CommitsInfo
resolveOk t spec =
  resolveIn t "repo" spec >>= \case
    Right (Right info) -> pure info
    other -> assertFailure (show other)

resolveErr :: FilePath -> Text -> IO DiffError
resolveErr t spec =
  resolveIn t "repo" spec >>= \case
    Right (Left e) -> pure e
    other -> assertFailure ("not a revision error: " <> show other)

probeErr :: GitSetup -> FilePath -> Text -> IO ProbeError
probeErr setup root spec = do
  r <- either (assertFailure . show) pure (parseRevspec spec)
  probeFilesWith setup quiet (Root root) (Just r) >>= \case
    Left e -> pure e
    Right _ -> assertFailure "probe worked"

-- | The commit a rev names, from plain git.
commitOf :: FilePath -> String -> IO ObjectId
commitOf t rev =
  ObjectId . Text.strip . Text.pack <$> gitOut (t </> "repo") ["rev-parse", rev]

-- | The paths a lister lists.
names :: Lister -> IO [Text]
names l =
  l.list >>= \case
    Left e -> assertFailure (show e)
    Right listed -> pure (map relPathText (Map.keys listed.files.entries))
