-- | Entry sources: commit reads on the kinds repo, and the same tree
-- read from disk and from a commit.
module HaskellReviewer.Effects.SourceTest (tests, parsingTests) where

import ApiSupport (ApiResult (..), TestApi (..), getApi, indexed, searchApi)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import HaskellReviewer.Core.CommitFiles (linkTargetCap)
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    LinkInfo (..),
    TooLargeInfo (..),
    textLimit,
  )
import HaskellReviewer.Core.FileList (CommitId (..), EntryKind (..), FileList (..))
import HaskellReviewer.Core.Git (ObjectId)
import HaskellReviewer.Core.Project (FileProblem (..), hostPlatform)
import HaskellReviewer.Core.RelPath (RelPath, parseRelPath)
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Core.Sides (Side (..), Sides (..))
import HaskellReviewer.Effects.FileSystem (ReadError (..), canonicalRoot)
import HaskellReviewer.Effects.Git (Git, Listing (..), repoInfo, resolveCommit)
import HaskellReviewer.Effects.GitObjects
  ( ObjectReader,
    defaultReaderSettings,
    withObjectReader,
  )
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import HaskellReviewer.Effects.Source
  ( Listed (..),
    Lister (..),
    commitLister,
    diskLister,
    readSourceEntry,
  )
import HaskellReviewer.Server.Api (newSnapshot)
import HaskellReviewer.Session.Index (newPool)
import HaskellReviewer.Session.Project (readProject, readProjectFile)
import HaskellReviewer.Session.Snapshot (Snapshot (..))
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import TestSupport (fixtureRepo, git, gitOut, realGitAt, withCanonicalTemp)

-- | Commit reads. No parse here.
tests :: TestTree
tests =
  -- Built once (Note [Shared values in tests] in
  -- "HaskellReviewer.Core.PackageGlobTest").
  withResource (makeRepos "kinds") (Dir.removeDirectoryRecursive . (.dir)) $ \get ->
    testGroup
      "Source"
      [ testCase "commit reads: every kind (sha1)" $ do
          r <- get
          commitReads r r.sha1 40,
        testCase "commit reads: every kind (sha256)" $ do
          r <- get
          commitReads r r.sha256 64,
        testCase "commit reads: not listed, reader stopped" $ do
          r <- get
          g <- realGitAt r.home (r.sha1 </> "proj")
          source <- withObjectReader quiet defaultReaderSettings g $ \reader -> do
            l <- headListed g reader
            readSourceEntry l.source (rel "nope.hs") EntryFile >>= (@?= Left ReadGone)
            pure l.source
          readSourceEntry source (rel "a.hs") EntryFile
            >>= (@?= Left (ReadFailed "the object reader is stopped")),
        testCase "commit reads: a project path that is a submodule is gone" $ do
          r <- get
          g <- realGitAt r.home (r.sha1 </> "proj")
          withObjectReader quiet defaultReaderSettings g $ \reader -> do
            l <- headListed g reader
            readProjectFile l.source l.files (rel "sub") >>= (@?= Left Gone),
        testCase "commit reads: a symlink target over 4096 bytes is cut" $
          withCanonicalTemp $ \tmp -> do
            let repo = tmp </> "r"
                long = replicate 5000 'a'
            Dir.createDirectory repo
            Dir.createDirectory (tmp </> "home")
            git repo ["init", "-q"]
            writeFile (tmp </> "target") long
            let blob f = filter (/= '\n') <$> gitOut repo ["hash-object", "-w", tmp </> f]
                addLink oid name =
                  git repo ["update-index", "--add", "--cacheinfo", link oid name]
                link oid name = "120000," <> oid <> "," <> name
            oid <- blob "target"
            writeFile (tmp </> "short") "a.hs"
            short <- blob "short"
            writeFile (repo </> "a.hs") "module A where\n"
            git repo ["add", "a.hs"]
            addLink oid "long"
            addLink short "short"
            git repo ["commit", "-qm", "links"]
            g <- realGitAt (tmp </> "home") repo
            withObjectReader quiet defaultReaderSettings g $ \reader -> do
              l <- headListed g reader
              let cut = Text.replicate linkTargetCap "a"
                  toA = LinkInfo {target = "a.hs", resolved = Just (rel "a.hs")}
              readSourceEntry l.source (rel "long") EntrySymlink
                >>= (@?= Right (Link LinkInfo {target = cut, resolved = Nothing}))
              readSourceEntry l.source (rel "short") EntrySymlink
                >>= (@?= Right (Link toA))
      ]

-- | The sides repo from disk and from its HEAD: the same module tree
-- and module pages. These parse: they run with the API tests.
parsingTests :: TestTree
parsingTests =
  withResource (makeRepos "sides") (Dir.removeDirectoryRecursive . (.dir)) $ \get ->
    testGroup
      "Source: disk vs commit"
      [ testCase "clean checkout: same file list, module tree, pages" $ do
          r <- get
          g <- realGitAt r.home r.sha1
          canon <- canonicalRoot (Root r.sha1)
          withObjectReader quiet defaultReaderSettings g $ \reader -> do
            disk <- snapshotFrom (diskLister quiet canon (ListFromGit g))
            commit <- snapshotFrom =<< headLister g reader
            disk.fileListJson @?= commit.fileListJson
            disk.moduleTreeJson @?= commit.moduleTreeJson
            let paths =
                  [ ["api", "module", "tree", "A"],
                    ["api", "module", "tree", "C"],
                    ["api", "source", "tree", "src", "A.hs"],
                    ["api", "source", "tree", "sides.cabal"],
                    ["api", "file", "tree", "src", "C.hs"],
                    ["api", "file", "tree", "readme-link"],
                    ["api", "file", "tree", "README.md"]
                  ]
            forM_ paths $ \path -> do
              d <- getApi (apiOn disk) path
              c <- getApi (apiOn commit) path
              (path, d.status) @?= (path, 200)
              (path, c.status, c.body) @?= (path, d.status, d.body)
      ]
  where
    snapshotFrom lister =
      lister.list >>= \case
        Left e -> assertFailure (show e)
        Right l -> do
          project <- readProject l.source l.files hostPlatform Nothing
          pool <- newPool
          newSnapshot TreeSide pool l.source l.files project
    apiOn snap =
      indexed
        searchApi
          { sides = BrowseSides (pure (Right snap)),
            readEntry = \s -> readSourceEntry s.source
          }

-- | Every kind of entry of the kinds repo, read from its HEAD.
commitReads :: Repos -> FilePath -> Int -> Assertion
commitReads r repo idLength = do
  g <- realGitAt r.home (repo </> "proj")
  withObjectReader quiet defaultReaderSettings g $ \reader -> do
    l <- headListed g reader
    let read' p = case Map.lookup (rel p) l.files.entries of
          Nothing -> assertFailure ("not listed: " <> show p)
          Just kind -> readSourceEntry l.source (rel p) kind
    read' "a.hs" >>= (@?= Right (TextContent "module A where\n\nx :: Int\nx = 1\n"))
    read' "empty.txt" >>= (@?= Right (TextContent ""))
    read' "data.dat" >>= (@?= Right (Binary (FileSize 10)))
    let tooLarge = TooLargeInfo {size = FileSize 1_200_000, limit = textLimit}
    read' "big.txt" >>= (@?= Right (TooLarge tooLarge))
    read' "link"
      >>= (@?= Right (Link LinkInfo {target = "a.hs", resolved = Just (rel "a.hs")}))
    read' "sub" >>= (@?= Right (Submodule (CommitId (Text.replicate idLength "1"))))
    read' "lfs.bin" >>= \case
      Right (TextContent t) -> do
        let pointer = "version https://git-lfs.github.com/spec/v1\n"
        assertBool (show t) (pointer `Text.isPrefixOf` t)
      other -> assertFailure (show other)

-- | HEAD's lister.
headLister :: Git -> ObjectReader -> IO Lister
headLister g reader = do
  info <- repoInfo g >>= either (assertFailure . show) pure
  commit <- headCommit g
  pure (commitLister quiet g info reader commit)

-- | HEAD's list.
headListed :: Git -> ObjectReader -> IO Listed
headListed g reader = do
  lister <- headLister g reader
  lister.list >>= either (assertFailure . show) pure

headCommit :: Git -> IO ObjectId
headCommit g =
  resolveCommit g "HEAD" >>= \case
    Right (Just c) -> pure c
    other -> assertFailure (show other)

-- | A fixture repo in both formats, and an empty HOME.
data Repos = Repos
  { dir :: FilePath,
    home :: FilePath,
    sha1 :: FilePath,
    sha256 :: FilePath
  }

makeRepos :: String -> IO Repos
makeRepos name = do
  tmp <- getCanonicalTemporaryDirectory
  dir <- createTempDirectory tmp name >>= Dir.canonicalizePath
  let r =
        Repos
          { dir = dir,
            home = dir </> "home",
            sha1 = dir </> "sha1",
            sha256 = dir </> "sha256"
          }
  fixtureRepo name [r.sha1]
  fixtureRepo name [r.sha256, "sha256"]
  Dir.createDirectory r.home
  pure r

rel :: ByteString -> RelPath
rel p = either (error . show) id (parseRelPath p)

quiet :: Logger
quiet = newLogger LevelInfo (\_ -> pure ())
