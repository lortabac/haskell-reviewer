-- | The working-tree side: the worktree.sh cases, a cross-check with
-- @git status@, the new git calls, lstat through a dir fd and reads
-- that find a swapped file.
module HaskellReviewer.Effects.WorkTreeTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time.Clock.POSIX (POSIXTime)
import HaskellReviewer.Core.CommitFiles
  ( CommitFiles (..),
    OutsideChanges (..),
    outsideChanges,
  )
import HaskellReviewer.Core.Eol
  ( CleanEol (..),
    Filtering (..),
    attrNames,
    defaultCoreConfig,
    parseCheckAttr,
    parseCoreConfig,
  )
import HaskellReviewer.Core.FileContent (FileContent (..), FileNotes (..), LinkInfo (..))
import HaskellReviewer.Core.FileList (CommitId (..), EntryKind (..), FileList (..))
import HaskellReviewer.Core.Git (Exec (..), ObjectId (..), RepoInfo (..))
import HaskellReviewer.Core.RelPath (RelPath, parseRelPath, relPathText)
import HaskellReviewer.Core.Renames (Blob (..), DiffEntry (File, Symlink))
import HaskellReviewer.Core.Renames qualified as Renames
import HaskellReviewer.Core.Root (CanonicalRoot (..), Root (..))
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Core.WorkTree
  ( DiskFile (..),
    FullStat (..),
    Look (..),
    StatTime (..),
    WorkFile (..),
  )
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.FileSystem
  ( ListError (..),
    ReadError (..),
    canonicalRoot,
    listCommit,
  )
import HaskellReviewer.Effects.Git
  ( Git,
    GitError (..),
    GitSetup (..),
    Listing (..),
    checkAttr,
    coreConfig,
    probeGitWith,
    repoInfo,
  )
import HaskellReviewer.Effects.GitObjects (defaultReaderSettings, withObjectReader)
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import HaskellReviewer.Effects.PosixAt (lstatAt)
import HaskellReviewer.Effects.Source
  ( EntrySource (..),
    WorkTreeSource (..),
    fileNotes,
    readForPairing,
    readSourceEntry,
  )
import HaskellReviewer.Effects.WorkTree
  ( ReadResult (..),
    WorkTreeFiles (..),
    hashCap,
    listWorkTree,
    lookPath,
    readForHash,
    stripPrefixLevels,
    withWalker,
  )
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Posix.Files qualified as Files
import System.Posix.IO
  ( OpenFileFlags (..),
    OpenMode (ReadOnly),
    closeFd,
    defaultFileFlags,
    openFd,
  )
import System.Posix.Types (Fd)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import TestSupport
  ( GitRun (..),
    fixtureRepo,
    git,
    gitBytes,
    gitOut,
    gitSetup,
    isRootUser,
    realGitAt,
    withCanonicalTemp,
  )

tests :: TestTree
tests =
  testGroup
    "WorkTree"
    [ -- Built once (Note [Shared values in tests] in
      -- "HaskellReviewer.Core.PackageGlobTest"). git status runs in the
      -- same test, after our lister.
      withResource makeFixtures Dir.removeDirectoryRecursive $ \get ->
        testGroup
          "worktree.sh"
          [ testCase (name <> ", then git status agrees (" <> format <> ")") $ do
              t <- get
              check t (t </> format)
          | format <- ["sha1", "sha256"],
            (name, check) <-
              [ ("w: every case", mainRepo),
                ("ac: core.autocrlf", autocrlf),
                ("lit: literal values, ident, a filter", litRepo),
                ("sl: core.symlinks off", slRepo),
                ("fm: core.filemode off", fmRepo),
                ("si: split index, clean by stat", siRepo),
                ("sd: ROOT in a subdir", sdRepo),
                ("bn: binary and -crlf, touched", binRepo "bn"),
                ("bnc: binary and -crlf, fresh clone", binRepo "bnc")
              ]
          ],
      testCase "config: no core key is empty" $ withCanonicalTemp $ \tmp -> do
        let repo = tmp </> "r"
        git tmp ["init", "-q", "r"]
        -- git init writes some of these (core.filemode; on macOS
        -- core.ignorecase too). Unset each one the regexp asks for; a
        -- key that isn't set is exit 5, fine here.
        forM_ coreKeys $ \k -> gitBytes repo ["config", "--unset-all", k] ""
        Dir.createDirectory (tmp </> "home")
        g <- realGitAt (tmp </> "home") repo
        coreConfig g >>= (@?= Right "")
        -- As on macOS: a key we read but don't use is skipped.
        git repo ["config", "core.ignorecase", "true"]
        out0 <- either (assertFailure . show) pure =<< coreConfig g
        out0 @?= "core.ignorecase\ntrue\0"
        parseCoreConfig out0 @?= Right defaultCoreConfig
        git repo ["config", "--unset-all", "core.ignorecase"]
        git repo ["config", "core.symlinks", "false"]
        out <- either (assertFailure . show) pure =<< coreConfig g
        parseCoreConfig out @?= parseCoreConfig "core.symlinks\nfalse\0",
      testCase "check-attr: 10,000 long paths through the pipe" $
        withCanonicalTemp $ \tmp -> do
          let repo = tmp </> "r"
          git tmp ["init", "-q", "r"]
          writeFile (repo </> ".gitattributes") "*.t text\n"
          Dir.createDirectory (tmp </> "home")
          g <- realGitAt (tmp </> "home") repo
          let paths =
                [ Char8.pack ("d/" <> replicate 120 'x' <> show i <> ".t")
                | i <- [1 .. 10_000 :: Int]
                ]
          out <- either (assertFailure . show) pure =<< checkAttr g attrNames paths
          case parseCheckAttr out of
            Right m -> Map.size m @?= 10_000
            Left e -> assertFailure (show e),
      testCase "a FIFO .gitattributes: check-attr and the listing time out" $
        withCanonicalTemp $ \tmp -> do
          let repo = tmp </> "r"
          git tmp ["init", "-q", "r"]
          writeFile (repo </> "a.hs") "x\n"
          git repo ["add", "a.hs"]
          -- An untracked file: check-attr runs even if a.hs is clean by
          -- stat (only check-attr reads .gitattributes).
          writeFile (repo </> "new.hs") "y\n"
          Files.createNamedPipe (repo </> ".gitattributes") 0o644
          Dir.createDirectory (tmp </> "home")
          setup <- gitSetup (tmp </> "home") (Microseconds 1_000_000)
          g <- gitWith setup repo
          checkAttr g attrNames ["a.hs"] >>= \case
            Left (GitTimedOut _) -> pure ()
            other -> assertFailure ("check-attr: " <> show other)
          info <- either (assertFailure . show) pure =<< repoInfo g
          canon <- canonicalRoot (Root repo)
          withObjectReader quiet defaultReaderSettings g $ \r ->
            listWorkTree quiet g info r canon >>= \case
              Left (ListGitFailed (GitTimedOut _)) -> pure ()
              Left e -> assertFailure ("listing: " <> show e)
              Right _ -> assertFailure "listing: no time out",
      testCase "stripPrefixLevels: one level, two levels, the top" $ do
        let root r = CanonicalRoot (Char8.unpack r) r
            top prefix r = (.bytes) <$> stripPrefixLevels prefix (root r)
        top "pkg/" "/pkg" @?= Just "/"
        top "pkg/" "/r/pkg" @?= Just "/r"
        top "a/b/" "/r/a/b" @?= Just "/r"
        top "a/b/" "/r/x/a/b" @?= Just "/r/x"
        top "a/b/" "/r/a/c" @?= Nothing
        top "" "/r" @?= Just "/r"
        stripPrefixLevels "a/b/" (root "/r/a/b") @?= Just (root "/r"),
      testCase "outside ROOT: what counts, with a two-level prefix" outsideCounts,
      testCase "outside ROOT: a FIFO .gitignore there: not known, the listing goes on" $
        withCanonicalTemp $ \tmp -> do
          let repo = tmp </> "r"
          git tmp ["init", "-q", "r"]
          Dir.createDirectoryIfMissing True (repo </> "o")
          Dir.createDirectoryIfMissing True (repo </> "root")
          writeFile (repo </> "o/f") "x\n"
          writeFile (repo </> "root/a.hs") "y\n"
          git repo ["add", "-A"]
          Files.createNamedPipe (repo </> "o/.gitignore") 0o644
          Dir.createDirectory (tmp </> "home")
          setup <- gitSetup (tmp </> "home") (Microseconds 1_000_000)
          g <- gitWith setup (repo </> "root")
          info <- either (assertFailure . show) pure =<< repoInfo g
          canon <- canonicalRoot (Root (repo </> "root"))
          logs <- newIORef []
          let logger = newLogger LevelVerbose (\l -> modifyIORef' logs (l :))
          withObjectReader quiet defaultReaderSettings g $ \r ->
            listWorkTree logger g info r canon >>= \case
              Left e -> assertFailure ("listing: " <> show e)
              Right wt -> do
                Map.size wt.files.entries @?= 1
                assertBool "outside is not known" (isNothing wt.outside)
                ls <- readIORef logs
                assertBool (show ls) (any ("outside ROOT not known" `Text.isInfixOf`) ls),
      testCase "outside ROOT: check-attr fails there: not known, the listing goes on" $
        withCanonicalTemp $ \tmp -> do
          let repo = tmp </> "r"
          git tmp ["init", "-q", "r"]
          Dir.createDirectoryIfMissing True (repo </> "root")
          Dir.createDirectoryIfMissing True (repo </> "o")
          writeFile (repo </> "o/f") "x\n"
          writeFile (repo </> "root/a.hs") "y\n"
          git repo ["add", "-A"]
          -- Racy: the outside step needs check-attr for it. Only that
          -- reads o/.gitattributes, not ROOT's files.
          Files.setFileTimes (repo </> "o/f") 4102444800 4102444800
          git repo ["add", "-A"]
          Files.createNamedPipe (repo </> "o/.gitattributes") 0o644
          Dir.createDirectory (tmp </> "home")
          setup <- gitSetup (tmp </> "home") (Microseconds 1_000_000)
          g <- gitWith setup (repo </> "root")
          info <- either (assertFailure . show) pure =<< repoInfo g
          canon <- canonicalRoot (Root (repo </> "root"))
          logs <- newIORef []
          let logger = newLogger LevelVerbose (\l -> modifyIORef' logs (l :))
          withObjectReader quiet defaultReaderSettings g $ \r ->
            listWorkTree logger g info r canon >>= \case
              Left e -> assertFailure ("listing: " <> show e)
              Right wt -> do
                assertBool "outside is not known" (isNothing wt.outside)
                ls <- readIORef logs
                assertBool (show ls) (any ("outside ROOT not known" `Text.isInfixOf`) ls),
      testCase "lstatAt: same as lstat, never follows" $ withCanonicalTemp $ \tmp -> do
        writeFile (tmp </> "f") "hello"
        Files.createSymbolicLink "f" (tmp </> "l")
        Dir.createDirectory (tmp </> "d")
        withDir tmp $ \fd ->
          forM_ ["f", "l", "d"] $ \n -> do
            ours <- lstatAt fd (Char8.pack n)
            st <- Files.getSymbolicLinkStatus (tmp </> n)
            ours.mode @?= fromIntegral (Files.fileMode st)
            ours.size @?= fromIntegral (Files.fileSize st)
            ours.ino @?= fromIntegral (Files.fileID st)
            ours.dev @?= fromIntegral (Files.deviceID st)
            ours.uid @?= fromIntegral (Files.fileOwner st)
            ours.gid @?= fromIntegral (Files.fileGroup st)
            ours.mtime @?= split (Files.modificationTimeHiRes st)
            ours.ctime @?= split (Files.statusChangeTimeHiRes st),
      testCase "walk: a symlink or file on the way is gone" $
        withCanonicalTemp $ \tmp -> do
          Dir.createDirectoryIfMissing True (tmp </> "real/sub")
          writeFile (tmp </> "real/sub/a") "a"
          writeFile (tmp </> "file") "f"
          Files.createSymbolicLink "real" (tmp </> "link")
          canon <- canonicalRoot (Root tmp)
          looks <- withWalker canon $ \w ->
            mapM
              (lookPath w . rel)
              [ "file/x",
                "link/sub/a",
                "missing/a",
                "real/missing",
                "real/sub/a",
                "link"
              ]
          map kindOfLook looks @?= ["gone", "gone", "gone", "gone", "found", "found"]
          root <- isRootUser
          unless root $ do
            Files.setFileMode (tmp </> "real") 0o000
            l <- withWalker canon (\w -> lookPath w (rel "real/sub/a"))
            Files.setFileMode (tmp </> "real") 0o755
            kindOfLook l @?= "failed",
      testCase "read for hashing: a swapped file is changed" $
        withCanonicalTemp $ \tmp -> do
          let f = tmp </> "f"
              fresh = writeFile f "one\n" >> withDir tmp (\fd -> lstatAt fd "f")
              readNow st = withDir tmp (\fd -> readForHash fd "f" st)
          st0 <- fresh
          readNow st0 >>= (@?= ReadBytes "one\n")
          -- A new file under the same name: another inode.
          writeFile (tmp </> "g") "one\n"
          Dir.renameFile (tmp </> "g") f
          readNow st0 >>= assertChanged
          st1 <- fresh
          Dir.removeFile f >> Files.createSymbolicLink "elsewhere" f
          readNow st1 >>= assertChanged
          st2 <- fresh
          Dir.removeFile f >> Files.createNamedPipe f 0o644
          readNow st2 >>= assertChanged
          Dir.removeFile f
          st3 <- fresh
          Dir.removeFile f
          readNow st3 >>= assertChanged
          -- A sparse file over the cap is not read.
          _ <- fresh
          Files.setFileSize f (fromIntegral hashCap + 1)
          st4 <- withDir tmp (\fd -> lstatAt fd "f")
          readNow st4 >>= (@?= ReadTooBig)
          Dir.removeFile f >> Files.createSymbolicLink "target" f
          st5 <- withDir tmp (\fd -> lstatAt fd "f")
          readNow st5 >>= (@?= ReadLinkTarget "target")
    ]
  where
    -- Seconds cut to 32 bits, as lstatAt gives them.
    split :: POSIXTime -> StatTime
    split t =
      let ns = floor (toRational t * 1_000_000_000) :: Integer
       in StatTime
            { sec = fromInteger (ns `div` 1_000_000_000),
              nsec = fromInteger (ns `mod` 1_000_000_000)
            }
    kindOfLook :: Look -> String
    kindOfLook = \case
      LookGone -> "gone"
      LookFailed _ -> "failed"
      LookFound _ -> "found"
    assertChanged = \case
      ReadChanged _ -> pure ()
      other -> assertFailure ("not changed: " <> show other)

makeFixtures :: IO FilePath
makeFixtures = do
  tmp <- getCanonicalTemporaryDirectory
  t <- createTempDirectory tmp "worktree" >>= Dir.canonicalizePath
  fixtureRepo "worktree" [t </> "sha1"]
  fixtureRepo "worktree" [t </> "sha256", "sha256"]
  Dir.createDirectory (t </> "home")
  pure t

-- | The cases in w/. See fixtures/repos/worktree.sh.
mainRepo :: FilePath -> FilePath -> Assertion
mainRepo t dir = do
  let repo = dir </> "w"
  g <- realGitAt (t </> "home") repo
  info <- either (assertFailure . show) pure =<< repoInfo g
  canon <- canonicalRoot (Root repo)
  withObjectReader quiet defaultReaderSettings g $ \r -> do
    wt <- either (assertFailure . show) pure =<< listWorkTree quiet g info r canon
    -- Few git calls: each one forks the test process.
    staged <- gitOut repo ["ls-files", "-s"]
    let indexIds =
          Map.fromList
            [ (path, ObjectId (Text.pack o))
            | l <- lines staged,
              (meta, '\t' : path) <- [break (== '\t') l],
              [_, o, "0"] <- [words meta]
            ]
        indexId p =
          maybe (assertFailure ("not in the index: " <> p)) pure (Map.lookup p indexIds)
    let work p = Map.lookup (rel p) wt.work
        kind p = Map.lookup (rel p) wt.files.entries
        source =
          WorkTreeEntries
            WorkTreeSource
              { root = canon,
                reader = r,
                format = info.format,
                entries = wt.diffEntries,
                hashable = wt.hashable,
                outside = wt.outside,
                files = wt.work
              }
        readP p = case kind p of
          Just k -> readSourceEntry source (rel p) k
          Nothing -> assertFailure ("not listed: " <> p)
        hashOf p =
          ObjectId . Text.pack . filter (/= '\n') <$> gitOut repo ["hash-object", p]
        rawHashOf p =
          ObjectId . Text.pack . filter (/= '\n')
            <$> gitOut repo ["hash-object", "--no-filters", p]
        clean p = do
          o <- indexId p
          (p, work p) @?= (p, Just (FromIndex o))
        absent p = (p, kind p, work p) @?= (p, Nothing, Nothing)
        disk p blob eol f =
          (p, work p)
            @?= (p, Just (FromDisk DiskFile {blob = blob, eol = eol, filtered = f}))
    mapM_
      clean
      [ ".gitattributes",
        "plain.hs",
        "eol.e",
        "racy.hs",
        "staged.hs",
        "skip.hs",
        "link"
      ]
    (commit, headStaged) <-
      lines <$> gitOut repo ["rev-parse", "HEAD", "HEAD:staged.hs"] >>= \case
        [c, h] -> pure (c, h)
        other -> assertFailure ("rev-parse: " <> show other)
    let headBlob = FromIndex (ObjectId (Text.pack headStaged))
    assertBool "staged.hs is the index blob" (work "staged.hs" /= Just headBlob)
    mapM_ absent ["ignored.log", "deleted.hs", "dir/a.hs", "gone-sub"]
    kind "sub" @?= Just (EntrySubmodule (CommitId (Text.pack commit)))
    kind "dir" @?= Just EntrySymlink
    kind "link2" @?= Just EntrySymlink
    kind "fifo.hs" @?= Just EntryFile
    -- Dirty, with git's id for the bytes.
    -- The index blob has CRLF, so git add keeps them: the raw bytes'
    -- id. (hash-object doesn't read the index, so it converts.)
    crlfAuto <- rawHashOf "crlf-auto.a"
    disk "crlf-auto.a" (Just crlfAuto) KeepRaw Unfiltered
    binA <- hashOf "bin.a"
    disk "bin.a" (Just binA) AutoOnRead Unfiltered
    modeId <- indexId "mode.sh"
    disk "mode.sh" (Just modeId) KeepRaw Unfiltered
    disk "lfs-dirty.bin" Nothing KeepRaw Filtered
    -- Touched only, but we can't be sure: text (check-attr prints the
    -- same for text=set), and a filter we can't run.
    disk "crlf.t" Nothing DropCrBeforeLf Unfiltered
    disk "lfs.bin" Nothing KeepRaw Filtered
    disk "ident.id" Nothing KeepRaw Filtered
    forM_ ["new.hs", "ita.hs", "conflict.txt", "dir2/a.hs", "fifo.hs"] $ \p ->
      disk p Nothing KeepRaw Unfiltered
    linkId <- linkHash repo "dir2/a.hs"
    disk "link2" (Just linkId) KeepRaw Unfiltered
    -- Reads.
    readP "crlf.t" >>= (@?= Right (TextContent "a\nb\n"))
    readP "crlf-auto.a" >>= (@?= Right (TextContent "x\r\ny\r\nz\r\n"))
    readP "bin.a" >>= \case
      Right (TextContent tx) ->
        assertBool "bin.a kept its CRs" ("\r\nmore\r\n" `Text.isInfixOf` tx)
      other -> assertFailure ("bin.a: " <> show other)
    readP "conflict.txt" >>= \case
      Right (TextContent tx) -> assertBool "markers" ("<<<<<<<" `Text.isInfixOf` tx)
      other -> assertFailure ("conflict.txt: " <> show other)
    readP "lfs-dirty.bin" >>= (@?= Right (TextContent "big data\n"))
    readP "fifo.hs" >>= (@?= Left ReadGone)
    readP "link" >>= \case
      Right (Link l) -> l.target @?= "plain.hs"
      other -> assertFailure ("link: " <> show other)
    readP "new.hs" >>= (@?= Right (TextContent "module New where\n"))
    -- For pairing: kinds, exec bits from disk, blobs from work.
    let entry p = Map.lookup (rel p) wt.diffEntries
    entry "mode.sh" @?= Just (File Executable (pairingBlob wt "mode.sh"))
    entry "plain.hs" @?= Just (File NotExecutable (pairingBlob wt "plain.hs"))
    entry "new.hs" @?= Just (File NotExecutable (pairingBlob wt "new.hs"))
    entry "link" @?= Just (Symlink (pairingBlob wt "link"))
    entry "sub" @?= Just (Renames.Submodule (ObjectId (Text.pack commit)))
    entry "deleted.hs" @?= Nothing
    Map.keysSet wt.diffEntries
      @?= Map.keysSet (Map.filter (/= EntryNestedRepo) wt.files.entries)
    -- Pairing hashes only files with no index blob to tell the line
    -- ends; a doubt about the rule (crlf.t) stays unknown.
    forM_ ["new.hs", "ita.hs"] $ \p ->
      assertBool p (Set.member (rel p) wt.hashable)
    -- Unsure: a doubt (crlf.t), untracked under text (new.t), unmerged
    -- (conflict.txt).
    forM_ ["crlf.t", "new.t", "conflict.txt"] $ \p ->
      assertBool p (Set.notMember (rel p) wt.hashable)
    let pairingRead p = case entry p of
          Just e -> readForPairing hashCap source (rel p) e
          Nothing -> assertFailure ("no entry: " <> p)
    pairingRead "crlf.t" >>= (@?= Nothing)
    pairingRead "new.t" >>= (@?= Nothing)
    pairingRead "conflict.txt" >>= (@?= Nothing)
    pairingRead "lfs-dirty.bin" >>= (@?= Nothing)
    pairingRead "new.hs" >>= (@?= Just "module New where\n")
    readSourceEntry source (rel "ignored.log") EntryFile >>= (@?= Left ReadGone)
    fileNotes source (rel "lfs-dirty.bin") @?= FileNotes {filtered = Filtered}
    fileNotes source (rel "ident.id") @?= FileNotes {filtered = Filtered}
    fileNotes source (rel "plain.hs") @?= FileNotes {filtered = Unfiltered}
    -- Last: git status may look at the files in its own way. It says
    -- clean for the two we can't be sure of.
    crossCheck repo "" wt ["crlf.t", "lfs.bin"]

-- | A path's blob in the work map, as pairing sees it.
pairingBlob :: WorkTreeFiles -> String -> Blob
pairingBlob wt p = Blob {oid = oid, size = Nothing}
  where
    oid = case Map.lookup (rel p) wt.work of
      Just (FromIndex o) -> Just o
      Just (FromDisk d) -> d.blob
      Nothing -> Nothing

-- | core.autocrlf=true in ac/.
autocrlf :: FilePath -> FilePath -> Assertion
autocrlf t dir = do
  let repo = dir </> "ac"
  g <- realGitAt (t </> "home") repo
  info <- either (assertFailure . show) pure =<< repoInfo g
  canon <- canonicalRoot (Root repo)
  withObjectReader quiet defaultReaderSettings g $ \r -> do
    wt <- either (assertFailure . show) pure =<< listWorkTree quiet g info r canon
    let source =
          WorkTreeEntries
            WorkTreeSource
              { root = canon,
                reader = r,
                format = info.format,
                entries = wt.diffEntries,
                hashable = wt.hashable,
                outside = wt.outside,
                files = wt.work
              }
    f <- filter (/= '\n') <$> gitOut repo ["rev-parse", ":f.txt"]
    Map.lookup (rel "f.txt") wt.work @?= Just (FromIndex (ObjectId (Text.pack f)))
    gId <- filter (/= '\n') <$> gitOut repo ["hash-object", "g.txt"]
    Map.lookup (rel "g.txt") wt.work
      @?= Just
        ( FromDisk
            DiskFile
              { blob = Just (ObjectId (Text.pack gId)),
                eol = AutoOnRead,
                filtered = Unfiltered
              }
        )
    readSourceEntry source (rel "g.txt") EntryFile
      >>= (@?= Right (TextContent "a\nb\nc\n"))
    crossCheck repo "" wt []

-- | lit/: git says each of these is modified, and we may not call
-- them clean. See fixtures/repos/worktree.sh.
litRepo :: FilePath -> FilePath -> Assertion
litRepo t dir =
  listed (t </> "home") (dir </> "lit") $ \wt _ -> do
    states wt [("plain.hs", "clean"), ("sz.e", "dirty")]
    forM_ ["set.txt", "u.dat", "i.c", "f.u", "x.fu"] $ \p ->
      fmap blobOf (Map.lookup (rel p) wt.work) @?= Just Nothing
    crossCheck (dir </> "lit") "" wt []
  where
    blobOf = \case
      FromDisk d -> d.blob
      FromIndex o -> Just o

-- | sl/: core.symlinks=false.
slRepo :: FilePath -> FilePath -> Assertion
slRepo t dir =
  listed (t </> "home") (dir </> "sl") $ \wt _ -> do
    states wt [("link", "clean"), ("link2", "dirty"), ("target.txt", "clean")]
    Map.lookup (rel "link") wt.files.entries @?= Just EntrySymlink
    Map.lookup (rel "link2") wt.files.entries @?= Just EntryFile
    crossCheck (dir </> "sl") "" wt []

-- | fm/: core.filemode=false.
fmRepo :: FilePath -> FilePath -> Assertion
fmRepo t dir =
  listed (t </> "home") (dir </> "fm") $ \wt _ -> do
    states wt [("x.sh", "clean"), ("y.sh", "dirty")]
    -- The index's exec bit, clean or not.
    forM_ ["x.sh", "y.sh"] $ \p ->
      Map.lookup (rel p) wt.diffEntries @?= Just (File NotExecutable (pairingBlob wt p))
    crossCheck (dir </> "fm") "" wt []

-- | si/: a split index. Only b.hs is hashed: the others are clean by
-- stat.
siRepo :: FilePath -> FilePath -> Assertion
siRepo t dir =
  listed (t </> "home") (dir </> "si") $ \wt logs -> do
    states wt [("a.hs", "clean"), ("b.hs", "dirty"), ("c.hs", "clean")]
    assertBool (show logs) (any (" 1 hashed" `Text.isInfixOf`) logs)
    crossCheck (dir </> "si") "" wt []

-- | sd/: ROOT is sd/pkg.
sdRepo :: FilePath -> FilePath -> Assertion
sdRepo t dir =
  listed (t </> "home") (dir </> "sd" </> "pkg") $ \wt logs -> do
    states wt [("a.hs", "clean"), ("b.hs", "dirty")]
    Map.keys wt.files.entries @?= map rel ["a.hs", "b.hs"]
    assertBool (show logs) (any (" 1 hashed" `Text.isInfixOf`) logs)
    crossCheck (dir </> "sd" </> "pkg") "pkg/" wt []

-- | bn/ and bnc/: binary and -crlf files with CRLF bytes, touched or
-- racy. git calls them clean, and so do we: -text and a literal
-- text=unset clean them the same way.
binRepo :: FilePath -> FilePath -> FilePath -> Assertion
binRepo name t dir =
  listed (t </> "home") (dir </> name) $ \wt _ -> do
    states wt [("a.bat", "clean"), ("a.png", "clean")]
    crossCheck (dir </> name) "" wt []

-- | The keys 'coreConfig' asks for, but the filter.* ones (git init
-- never writes those).
coreKeys :: [String]
coreKeys =
  map
    ("core." <>)
    ["autocrlf", "filemode", "symlinks", "ignorecase", "trustctime", "checkstat"]

-- | The states of these paths are the given ones.
states :: WorkTreeFiles -> [(String, String)] -> Assertion
states wt want = [(p, stateOf wt p) | (p, _) <- want] @?= want

-- | List ROOT, with the verbose log lines.
listed :: FilePath -> FilePath -> (WorkTreeFiles -> [Text] -> IO a) -> IO a
listed home root k = do
  logs <- newIORef []
  let logger = newLogger LevelVerbose (\l -> modifyIORef' logs (l :))
  g <- realGitAt home root
  info <- either (assertFailure . show) pure =<< repoInfo g
  canon <- canonicalRoot (Root root)
  withObjectReader quiet defaultReaderSettings g $ \r -> do
    wt <- either (assertFailure . show) pure =<< listWorkTree logger g info r canon
    readIORef logs >>= k wt

-- | "clean", "dirty", "absent", or "submodule".
stateOf :: WorkTreeFiles -> String -> String
stateOf wt p = case (Map.lookup (rel p) wt.files.entries, Map.lookup (rel p) wt.work) of
  (Just (EntrySubmodule _), _) -> "submodule"
  (_, Just (FromIndex _)) -> "clean"
  (_, Just (FromDisk _)) -> "dirty"
  _ -> "absent"

-- | Run after our lister: git status must never call dirty what we
-- call clean, and every tracked path git calls clean is clean here
-- too, except the given ones (documented differences). git runs in
-- ROOT; the prefix is ROOT from the top (git status gives paths from
-- there).
crossCheck :: FilePath -> String -> WorkTreeFiles -> [String] -> Assertion
crossCheck root prefix wt known = do
  out <-
    gitOut
      root
      ["--no-optional-locks", "status", "--porcelain=v2", "-z", "--untracked-files=all"]
  tracked <- filter (not . null) . splitNul <$> gitOut root ["ls-files", "-z"]
  let records =
        [(p, d) | (full, d) <- statusRecords (splitNul out), Just p <- [strip full]]
      gitDirty = [p | (p, dirty) <- records, dirty]
      oursClean = [Text.unpack (relPathText p) | (p, FromIndex _) <- Map.toList wt.work]
      gitClean = [p | p <- uniq tracked, p `notElem` gitDirty]
      clean p = stateOf wt p `elem` ["clean", "submodule"]
  [p | p <- oursClean, p `elem` gitDirty] @?= []
  [p | p <- gitClean, not (clean p)] @?= known
  where
    strip p = List.stripPrefix prefix p
    splitNul = splitOn '\0'
    uniq = map NonEmpty.head . NonEmpty.group . sort

-- | Path and "dirty in the work tree" for each porcelain v2 record.
statusRecords :: [String] -> [(String, Bool)]
statusRecords = \case
  [] -> []
  r : rest -> case r of
    '1' : ' ' : _ : y : more -> (pathAfter 6 more, y /= '.') : statusRecords rest
    -- A rename: the old path is the next record.
    '2' : ' ' : _ : y : more -> (pathAfter 7 more, y /= '.') : statusRecords (drop 1 rest)
    'u' : ' ' : _ : _ : more -> (pathAfter 8 more, True) : statusRecords rest
    '?' : ' ' : p -> (p, True) : statusRecords rest
    _ -> statusRecords rest
  where
    -- Drop n space-separated fields; the path is the rest.
    pathAfter :: Int -> String -> String
    pathAfter n s = iterate (drop 1 . dropWhile (/= ' ')) (drop 1 s) !! n

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest

-- | git's id for a symlink with this target.
linkHash :: FilePath -> String -> IO ObjectId
linkHash repo target = do
  run <-
    gitBytes repo ["hash-object", "--stdin", "--no-filters"] (Char8.pack target)
  pure (ObjectId (Text.pack (Char8.unpack (BS.takeWhile (/= 10) run.out))))

gitWith :: GitSetup -> FilePath -> IO Git
gitWith setup root =
  probeGitWith setup quiet (Root root) >>= \case
    Right (ListFromGit g) -> pure g
    other -> assertFailure ("git: " <> either show (const "not a repo") other)

withDir :: FilePath -> (Fd -> IO a) -> IO a
withDir d = bracket (openFd d ReadOnly defaultFileFlags {directory = True}) closeFd

rel :: String -> RelPath
rel p = either (error . show) id (parseRelPath (Text.Encoding.encodeUtf8 (Text.pack p)))

quiet :: Logger
quiet = newLogger LevelInfo (\_ -> pure ())

-- | A repo with ROOT at a/b. Against the first commit: the edit,
-- deletion, chmod, untracked, intent-to-add and unmerged paths outside
-- ROOT count. Clean files, a racy one with the same content, a
-- symlink, a gitlink, a non-UTF-8 name, and a file added then deleted
-- (not in the base) do not.
outsideCounts :: Assertion
outsideCounts = withCanonicalTemp $ \tmp -> do
  let repo = tmp </> "r"
      at p = repo </> p
      future = fromIntegral (4102444800 :: Int)
  git tmp ["init", "-q", "r"]
  forM_ ["o", "a/b"] $ \d -> Dir.createDirectoryIfMissing True (at d)
  forM_
    [ "top.txt",
      "o/same",
      "o/racy",
      "o/edit",
      "o/del",
      "o/mode.sh",
      "o/conf",
      "a/other",
      "a/b/in.hs"
    ]
    $ \f -> writeFile (at f) (f <> "\n")
  Files.createSymbolicLink "target" (at "o/link")
  Dir.createDirectory (at "o/sub")
  -- Racy: the mtime is in the future, so not older than the index.
  Files.setFileTimes (at "o/racy") future future
  git repo ["add", "-A"]
  -- A non-UTF-8 name: index only (macOS can't have it on disk).
  blob <- takeWhile (/= '\n') <$> gitOut repo ["hash-object", "-w", "o/same"]
  let odd' = "100644 " <> Char8.pack blob <> "\t" <> "o/caf\xe9\0"
  _ <- gitBytes repo ["update-index", "-z", "--index-info"] odd'
  let gitlink = "160000," <> replicate 40 '1' <> ",o/sub"
  git repo ["update-index", "--add", "--cacheinfo", gitlink]
  git repo ["commit", "-q", "-m", "base"]
  base <- takeWhile (/= '\n') <$> gitOut repo ["rev-parse", "HEAD"]
  -- An unmerged o/conf.
  git repo ["checkout", "-q", "-b", "side"]
  writeFile (at "o/conf") "side\n"
  git repo ["commit", "-q", "-a", "-m", "side"]
  git repo ["checkout", "-q", "-"]
  writeFile (at "o/conf") "ours\n"
  git repo ["commit", "-q", "-a", "-m", "ours"]
  _ <- gitBytes repo ["merge", "side"] ""
  -- Then the working tree.
  appendFile (at "o/edit") "more\n"
  Dir.removeFile (at "o/del")
  Files.setFileMode (at "o/mode.sh") 0o755
  writeFile (at "o/untracked") "u\n"
  writeFile (at "o/ita") "i\n"
  git repo ["add", "-N", "o/ita"]
  writeFile (at "o/ghost") "g\n"
  git repo ["add", "o/ghost"]
  Dir.removeFile (at "o/ghost")
  Dir.createDirectory (tmp </> "home")
  g <- realGitAt (tmp </> "home") (at "a/b")
  info <- either (assertFailure . show) pure =<< repoInfo g
  info.prefix @?= "a/b/"
  canon <- canonicalRoot (Root (at "a/b"))
  withObjectReader quiet defaultReaderSettings g $ \r -> do
    wt <- either (assertFailure . show) pure =<< listWorkTree quiet g info r canon
    let baseId = ObjectId (Text.pack base)
    cf <- either (assertFailure . show) pure =<< listCommit quiet g info baseId
    side <- maybe (assertFailure "outside is not known") pure wt.outside
    let c = outsideChanges Pair {head = side, base = cf.outside}
    c.paths @?= ["o/conf", "o/del", "o/edit", "o/ita", "o/mode.sh", "o/untracked"]
    c.total @?= 6
