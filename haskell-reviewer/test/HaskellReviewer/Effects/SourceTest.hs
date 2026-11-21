-- | Entry sources: commit reads on the kinds repo, and the same tree
-- read from disk and from a commit.
module HaskellReviewer.Effects.SourceTest (tests, parsingTests) where

import ApiSupport (ApiResult (..), TestApi (..), getApi, indexed, searchApi)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import HaskellReviewer.Core.CommitFiles (BlobRef (..), linkTargetCap)
import HaskellReviewer.Core.Eol (CleanEol (..), Filtering (..))
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    LinkInfo (..),
    TooLargeInfo (..),
    textLimit,
  )
import HaskellReviewer.Core.FileList (CommitId (..), EntryKind (..), FileList (..))
import HaskellReviewer.Core.Git (ObjectFormat (..), ObjectId (..))
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyReason (..),
    textOnlyModule,
  )
import HaskellReviewer.Core.Modules (buildModuleIndex)
import HaskellReviewer.Core.Project (FileProblem (..), hostPlatform)
import HaskellReviewer.Core.RelPath (RelPath, parseRelPath)
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Core.Sides (Side (..), Sides (..))
import HaskellReviewer.Core.WorkTree (DiskFile (..), WorkFile (..))
import HaskellReviewer.Effects.FileSystem (ReadError (..), canonicalRoot)
import HaskellReviewer.Effects.Git (Git (..), Listing (..), repoInfo, resolveCommit)
import HaskellReviewer.Effects.GitObjects
  ( ObjectReader,
    defaultReaderSettings,
    withObjectReader,
  )
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import HaskellReviewer.Effects.Source
  ( CommitSource (..),
    EntrySource (..),
    Listed (..),
    Lister (..),
    WorkTreeSource (..),
    commitLister,
    diskLister,
    readSourceEntry,
    sharedBlob,
    sourceBlob,
  )
import HaskellReviewer.Server.Api (newSnapshot)
import HaskellReviewer.Session.Index
  ( IndexDeps (..),
    IndexJob (..),
    Indexed (..),
    ShareCounts (..),
    SlotState (..),
    readStates,
    runWorkers,
    shareCounts,
  )
import HaskellReviewer.Session.Project (readProject, readProjectFile)
import HaskellReviewer.Session.Snapshot (Snapshot (..), newSessionShare)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import TestSupport
  ( fixtureRepo,
    git,
    gitOut,
    realGitAt,
    withCanonicalTemp,
    writeScript,
  )

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
        testCase "commit reads: a blob not in the repo is ReadMissing (D-331)" $ do
          r <- get
          g <- realGitAt r.home (r.sha1 </> "proj")
          let gone = ObjectId (Text.replicate 40 "e")
          withObjectReader quiet defaultReaderSettings g $ \reader -> do
            l <- headListed g reader
            src <- withBlob (rel "a.hs") gone l.source
            readSourceEntry src (rel "a.hs") EntryFile >>= (@?= Left (ReadMissing gone)),
        testCase "commit reads: git 2.44.1's promisor death is ReadMissing" $
          withCanonicalTemp $ \tmp -> do
            r <- get
            real <- realGitAt r.home (r.sha1 </> "proj")
            let missing = replicate 40 'd'
            exe <- writeScript tmp "git" (dyingCatFile real.exe missing)
            let g = real {exe = exe}
                gone = ObjectId (Text.pack missing)
            withObjectReader quiet defaultReaderSettings g $ \reader -> do
              l <- headListed g reader
              src <- withBlob (rel "a.hs") gone l.source
              readSourceEntry src (rel "a.hs") EntryFile >>= (@?= Left (ReadMissing gone))
              -- The reader starts again for the next read.
              readSourceEntry src (rel "empty.txt") EntryFile
                >>= (@?= Right (TextContent "")),
        testCase "commit reads: a project path that is a submodule is gone" $ do
          r <- get
          g <- realGitAt r.home (r.sha1 </> "proj")
          withObjectReader quiet defaultReaderSettings g $ \reader -> do
            l <- headListed g reader
            readProjectFile l.source l.files (rel "sub") >>= (@?= Left Gone),
        testCase "blobs: shared only when read from a blob (D-320)" $ do
          r <- get
          let proj = r.sha1 </> "proj"
          g <- realGitAt r.home proj
          canon <- canonicalRoot (Root proj)
          withObjectReader quiet defaultReaderSettings g $ \reader -> do
            l <- headListed g reader
            let both src p = (sourceBlob src (rel p), sharedBlob src (rel p))
                known = fst (both l.source "a.hs")
            assertBool "a.hs has a blob" (known /= Nothing)
            both l.source "a.hs" @?= (known, known)
            both l.source "nope.hs" @?= (Nothing, Nothing)
            both (DiskEntries canon) "a.hs" @?= (Nothing, Nothing)
            let o = ObjectId "0123"
                disk b =
                  FromDisk DiskFile {blob = b, eol = KeepRaw, filtered = Unfiltered}
                wt =
                  WorkTreeEntries
                    WorkTreeSource
                      { root = canon,
                        reader = reader,
                        format = Sha1,
                        entries = Map.empty,
                        hashable = Set.empty,
                        outside = Nothing,
                        files =
                          Map.fromList
                            [ (rel "clean.hs", FromIndex o),
                              (rel "dirty.hs", disk (Just o)),
                              (rel "new.hs", disk Nothing)
                            ]
                      }
            both wt "clean.hs" @?= (Just o, Just o)
            both wt "dirty.hs" @?= (Just o, Nothing)
            both wt "new.hs" @?= (Nothing, Nothing)
            both wt "nope.hs" @?= (Nothing, Nothing),
        testCase "a symlink module with a file's blob gets no shared result" $
          withCanonicalTemp $ \tmp -> do
            -- L.hs links to A.hs; B.hs holds the text "A.hs": the same
            -- blob. L has no slot (we never follow links), so no hit.
            let repo = tmp </> "r"
            Dir.createDirectory repo
            Dir.createDirectory (tmp </> "home")
            git repo ["init", "-q"]
            writeFile (repo </> "p.cabal") $
              unlines
                [ "cabal-version: 3.0",
                  "name: p",
                  "version: 0.1",
                  "library",
                  "  exposed-modules: B L",
                  "  build-depends: base",
                  "  default-language: Haskell2010"
                ]
            writeFile (repo </> "A.hs") "module A where\n"
            writeFile (repo </> "B.hs") "A.hs"
            Dir.createFileLink "A.hs" (repo </> "L.hs")
            git repo ["add", "-A"]
            git repo ["commit", "-qm", "links"]
            g <- realGitAt (tmp </> "home") repo
            withObjectReader quiet defaultReaderSettings g $ \reader -> do
              l <- headListed g reader
              sourceBlob l.source (rel "L.hs") @?= sourceBlob l.source (rel "B.hs")
              project <- readProject l.source l.files hostPlatform Nothing
              share <- newSessionShare
              let modules = buildModuleIndex project l.files
              snap <- newSnapshot TreeSide share l.source l.files modules Map.empty
              let deps =
                    IndexDeps
                      { readModule = \p -> readSourceEntry l.source p EntryFile,
                        parse = \_ _ -> pure (TextOnly (textOnlyModule TooSlow [])),
                        logger = quiet,
                        label = ""
                      }
              runWorkers 1 [IndexJob deps snap.index]
              -- Slots: A (a loose module) and B.
              states <- atomically (readStates snap.index)
              let ready = Ready (IndexedTextOnly (textOnlyModule TooSlow []))
              Map.elems states @?= [ready, ready]
              assertBool (show (Map.keys states)) $
                not (any (Text.isInfixOf "\"L\"" . Text.pack . show) (Map.keys states))
              atomically (shareCounts snap.index)
                >>= (@?= ShareCounts {parses = 2, shares = 0}),
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
          share <- newSessionShare
          let modules = buildModuleIndex project l.files
          newSnapshot TreeSide share l.source l.files modules Map.empty
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

-- | The source, with this blob for one path.
withBlob :: RelPath -> ObjectId -> EntrySource -> IO EntrySource
withBlob p o = \case
  CommitEntries c ->
    let swap b = BlobRef {oid = o, size = b.size}
     in pure (CommitEntries c {blobs = Map.adjust swap p c.blobs})
  _ -> assertFailure "not a commit source"

-- | A git whose cat-file dies on one id, as git 2.44.1 does on a blob
-- missing from a partial clone. The rest goes to the real git.
dyingCatFile :: FilePath -> String -> [String]
dyingCatFile real missing =
  [ "case \" $* \" in",
    "  *' cat-file '*)",
    "    while read -r cmd oid; do",
    "      if [ \"$oid\" = " <> missing <> " ]; then",
    "        echo \"fatal: could not fetch $oid from promisor remote\" >&2",
    "        exit 128",
    "      fi",
    "      printf '%s %s\\n' \"$cmd\" \"$oid\"",
    "    done | '" <> real <> "' \"$@\"",
    "    exit ;;",
    "esac",
    "exec '" <> real <> "' \"$@\""
  ]

rel :: ByteString -> RelPath
rel p = either (error . show) id (parseRelPath p)

quiet :: Logger
quiet = newLogger LevelInfo (\_ -> pure ())
