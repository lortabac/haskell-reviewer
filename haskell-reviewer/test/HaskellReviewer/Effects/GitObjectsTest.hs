-- | The object reader and the commit listing: real git on the kinds
-- repo, and fake gits for the failure paths.
module HaskellReviewer.Effects.GitObjectsTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, forConcurrently_, wait)
import Control.Exception (IOException, try)
import Control.Monad (forM_, replicateM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import HaskellReviewer.Core.CommitFiles
  ( BlobRef (..),
    BlobSize (..),
    CommitFiles (..),
    OutsideRows (..),
  )
import HaskellReviewer.Core.FileContent (FileSize (..))
import HaskellReviewer.Core.FileList (CommitId (..), EntryKind (..), FileList (..))
import HaskellReviewer.Core.Git
  ( History (..),
    ObjectFormat (..),
    ObjectId (..),
    ObjectType (..),
    RepoInfo (..),
  )
import HaskellReviewer.Core.RelPath (RelPath, parseRelPath)
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.FileSystem (ListError (..), listCommit)
import HaskellReviewer.Effects.Git
  ( Failure (code),
    Git (..),
    GitError (..),
    Listing (..),
    gitProbeTimeLimit,
    hardenedArgs,
    hardenedEnv,
    mergeBase,
    probeGitWith,
    repoInfo,
    resolveCommit,
  )
import HaskellReviewer.Effects.GitObjects
  ( BlobBytes (..),
    ObjectError (..),
    ObjectInfo (..),
    ReadFailure (..),
    ReadProblem (..),
    ReaderSettings (..),
    defaultReaderSettings,
    objectInfo,
    readBlob,
    withObjectReader,
  )
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Process
  ( CreateProcess (..),
    StdStream (CreatePipe),
    createProcess,
    proc,
    readProcess,
    waitForProcess,
  )
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import TestSupport
  ( assertFds,
    baselineFds,
    fixtureRepo,
    git,
    gitOut,
    gitSetup,
    listFdsTo,
    withCanonicalTemp,
    withFdLock,
    writeScript,
  )

tests :: TestTree
tests =
  testGroup
    "Git objects"
    [ -- Built once (Note [Shared values in tests] in
      -- "HaskellReviewer.Core.PackageGlobTest").
      withResource makeKinds (Dir.removeDirectoryRecursive . (.dir)) $ \get ->
        testGroup "real git" (realTests get),
      testGroup "fake git" fakeTests
    ]

-- * Real git

-- | The kinds repo, in both formats.
data Kinds = Kinds
  { dir :: FilePath,
    sha1 :: FilePath,
    sha256 :: FilePath
  }

makeKinds :: IO Kinds
makeKinds = do
  tmp <- getCanonicalTemporaryDirectory
  dir <- createTempDirectory tmp "kinds" >>= Dir.canonicalizePath
  let kinds = Kinds {dir = dir, sha1 = dir </> "sha1", sha256 = dir </> "sha256"}
  fixtureRepo "kinds" [kinds.sha1]
  fixtureRepo "kinds" [kinds.sha256, "sha256"]
  Dir.createDirectory (dir </> "home")
  pure kinds

realTests :: IO Kinds -> [TestTree]
realTests get =
  [ testCase "listCommit: kinds, sizes, outside rows, skipped name" $ do
      k <- get
      checkKinds k k.sha1 Sha1 40,
    testCase "listCommit: sha256" $ do
      k <- get
      checkKinds k k.sha256 Sha256 64,
    testCase "readBlob: text, LFS pointer, binary, big, symlink" $ do
      k <- get
      (g, cf) <- listed k k.sha1
      withObjectReader quiet defaultReaderSettings g $ \r -> do
        let read' p = readBlob r cap (oidOf cf p) >>= either (assertFailure . show) pure
        a <- read' "a.hs"
        a @?= BlobBytes (FileSize 31) "module A where\n\nx :: Int\nx = 1\n"
        lfs <- read' "lfs.bin"
        let pointer = "version https://git-lfs.github.com/spec/v1\n"
        assertBool "pointer" (pointer `BS.isPrefixOf` lfs.bytes)
        bin <- read' "data.dat"
        bin.bytes @?= "bin\0ary\1\2\n"
        big <- read' "big.txt"
        big.size @?= FileSize 1_200_000
        BS.length big.bytes @?= cap
        link <- read' "link"
        link.bytes @?= "a.hs"
        -- The reader is still in sync after a capped read.
        read' "empty.txt" >>= (@?= BlobBytes (FileSize 0) ""),
    testCase "objectInfo, missing id, a commit is not a blob" $ do
      k <- get
      (g, cf) <- listed k k.sha1
      Just commit <- resolveCommit g "HEAD" >>= either (assertFailure . show) pure
      -- One process, so the read after the commit lands on the same pipe.
      withObjectReader quiet defaultReaderSettings {handles = 1} g $ \r -> do
        objectInfo r (oidOf cf "a.hs") >>= (@?= Right (ObjectInfo TypeBlob (FileSize 31)))
        objectInfo r commit >>= \case
          Right i -> i.kind @?= TypeCommit
          Left e -> assertFailure (show e)
        let gone = ObjectId (Text.replicate 39 "0" <> "1")
        objectInfo r gone >>= (@?= Left (ObjectMissing gone))
        readBlob r cap gone >>= (@?= Left (ObjectMissing gone))
        readBlob r cap commit >>= \case
          Left (ObjectNotABlob _) -> pure ()
          other -> assertFailure (show other)
        readBlob r cap (oidOf cf "a.hs") >>= \case
          Right b -> b.size @?= FileSize 31
          Left e -> assertFailure (show e),
    testCase "8 threads read every blob: same as git cat-file" $ do
      k <- get
      (g, cf) <- listed k k.sha1
      let oids = [b.oid | b <- Map.elems cf.blobs]
      let catFile (ObjectId o) = gitBytes k.sha1 ["cat-file", "blob", Text.unpack o]
      expected <- traverse catFile oids
      withObjectReader quiet defaultReaderSettings g $ \r ->
        forConcurrently_ [1 :: Int .. 8] $ \_ ->
          forM_ (zip oids expected) $ \(o, want) -> do
            got <- readBlob r (2 * 1024 * 1024) o
            fmap (.bytes) got @?= Right want,
    testCase "resolveCommit: unknown, range, tree" $ do
      k <- get
      g <- realGitIn (k.dir </> "home") k.sha1
      resolveCommit g "nope" >>= (@?= Right Nothing)
      resolveCommit g "HEAD..HEAD" >>= (@?= Right Nothing)
      resolveCommit g "HEAD^{tree}" >>= (@?= Right Nothing)
      resolveCommit g "-dash" >>= (@?= Right Nothing),
    testCase "mergeBase: linear history, none" $
      withCanonicalTemp $ \tmp -> do
        let repo = tmp </> "r"
        Dir.createDirectory repo
        Dir.createDirectory (tmp </> "home")
        git repo ["init", "-q"]
        writeFile (repo </> "a") "1\n"
        git repo ["add", "a"]
        git repo ["commit", "-qm", "c0"]
        writeFile (repo </> "a") "2\n"
        git repo ["commit", "-qam", "c1"]
        c0 <- oidLine <$> gitOut repo ["rev-parse", "HEAD~1"]
        c1 <- oidLine <$> gitOut repo ["rev-parse", "HEAD"]
        tree <- filter (/= '\n') <$> gitOut repo ["rev-parse", "HEAD^{tree}"]
        orphan <- oidLine <$> gitOut repo ["commit-tree", tree, "-m", "orphan"]
        g <- realGitIn (tmp </> "home") repo
        mergeBase g c0 c1 >>= (@?= Right [c0])
        mergeBase g c1 orphan >>= (@?= Right []),
    testCase "mergeBase: criss-cross, two bases" $
      withCanonicalTemp $ \tmp -> do
        let repo = tmp </> "r"
            commit msg = git repo ["commit", "-q", "--allow-empty", "-m", msg]
            rev r = oidLine <$> gitOut repo ["rev-parse", r]
        Dir.createDirectory repo
        Dir.createDirectory (tmp </> "home")
        git repo ["init", "-q", "-b", "main"]
        commit "a"
        git repo ["checkout", "-q", "-b", "x"]
        commit "b"
        git repo ["checkout", "-q", "main"]
        commit "c"
        b <- rev "x"
        c <- rev "main"
        git repo ["merge", "-q", "--no-ff", "-m", "m1", "x"]
        git repo ["checkout", "-q", "x"]
        let ObjectId cText = c
        git repo ["merge", "-q", "--no-ff", "-m", "m2", Text.unpack cText]
        m1 <- rev "main"
        m2 <- rev "x"
        g <- realGitIn (tmp </> "home") repo
        bases <- mergeBase g m1 m2 >>= either (assertFailure . show) pure
        sort bases @?= sort [b, c]
  ]
  where
    oidLine = ObjectId . Text.pack . filter (/= '\n')

-- | The text limit + 1, as the API will pass.
cap :: Int
cap = 1_048_577

-- | List the kinds repo at HEAD, ROOT = proj.
listed :: Kinds -> FilePath -> IO (Git, CommitFiles)
listed k repo = do
  g <- realGitIn (k.dir </> "home") (repo </> "proj")
  info <- repoInfo g >>= either (assertFailure . show) pure
  Just commit <- resolveCommit g "HEAD" >>= either (assertFailure . show) pure
  cf <- listCommit quiet g info commit >>= either (assertFailure . show) pure
  pure (g, cf)

checkKinds :: Kinds -> FilePath -> ObjectFormat -> Int -> Assertion
checkKinds k repo format idLength = do
  g <- realGitIn (k.dir </> "home") (repo </> "proj")
  info <- repoInfo g >>= either (assertFailure . show) pure
  info @?= RepoInfo {prefix = "proj/", format = format, history = FullHistory}
  (_, cf) <- listed k repo
  let sub = Text.replicate idLength "1"
  cf.files.entries
    @?= Map.fromList
      [ (rel ".gitattributes", EntryFile),
        (rel "a.hs", EntryFile),
        (rel "big.txt", EntryFile),
        (rel "data.dat", EntryFile),
        (rel "empty.txt", EntryFile),
        (rel "lfs.bin", EntryFile),
        (rel "link", EntrySymlink),
        (rel "run.sh", EntryFile),
        (rel "sub", EntrySubmodule (CommitId sub))
      ]
  cf.files.skippedNames @?= 1
  cf.files.truncated @?= False
  Map.map (.size) cf.blobs
    @?= Map.fromList
      [ (rel ".gitattributes", BlobSize (FileSize 42)),
        (rel "a.hs", BlobSize (FileSize 31)),
        (rel "big.txt", BlobSize (FileSize 1_200_000)),
        (rel "data.dat", BlobSize (FileSize 10)),
        (rel "empty.txt", BlobSize (FileSize 0)),
        (rel "lfs.bin", BlobSize (FileSize 127)),
        (rel "link", BlobSize (FileSize 4)),
        (rel "run.sh", BlobSize (FileSize 18))
      ]
  forM_ (Map.elems cf.blobs) $ \b ->
    let ObjectId o = b.oid in Text.length o @?= idLength
  Map.keys cf.outside.rows @?= ["other/x.hs", "outside.txt"]

oidOf :: CommitFiles -> ByteString -> ObjectId
oidOf cf p = maybe (error ("no blob " <> show p)) (.oid) (Map.lookup (rel p) cf.blobs)

rel :: ByteString -> RelPath
rel p = either (error . show) id (parseRelPath p)

-- | The real git, found as the program finds it, with ROOT at the dir
-- and HOME in the given (empty) dir.
realGitIn :: FilePath -> FilePath -> IO Git
realGitIn home root = do
  setup <- gitSetup home gitProbeTimeLimit
  probeGitWith setup quiet (Root root) >>= \case
    Right (ListFromGit g) -> pure g
    Right (ListFromDisk _) -> assertFailure "git not used"
    Left e -> assertFailure ("git error (git 2.44.1 or newer needed): " <> show e)

-- | Run the git on PATH in a dir; its stdout as bytes.
gitBytes :: FilePath -> [String] -> IO ByteString
gitBytes dir args = do
  (_, Just out, Just err, ph) <-
    createProcess
      (proc "git" args)
        { cwd = Just dir,
          std_out = CreatePipe,
          std_err = CreatePipe,
          close_fds = True
        }
  bytes <- BS.hGetContents out
  hClose err
  _ <- waitForProcess ph
  pure bytes

quiet :: Logger
quiet = newLogger LevelInfo (\_ -> pure ())

-- * Fake git

{- The fake cat-file answers by the first digit of the id:

a  a 3-byte blob "abc"
b  hangs (exec sleep, so the pid stays the same)
c  a junk header
d  a header for another id
e  a short body, then exit
f  no LF after the body
1  dies as git 2.44.1 does on a blob missing from a partial clone
2  1 MiB on stderr first, then "abc"
3  exits (a crash)
4  "abc", then exits while idle
5  a promisor line for another id on stderr, then "abc"
6  "missing" for another id

It logs its pid to starts and each request to requests.
-}

fakeCatFile :: FilePath -> [String]
fakeCatFile dir =
  [ "echo $$ >> '" <> dir </> "starts" <> "'",
    "while read -r cmd oid; do",
    "  echo \"$cmd $oid\" >> '" <> dir </> "requests" <> "'",
    "  case $oid in",
    "    a*) printf '%s blob 3\\nabc\\n' \"$oid\" ;;",
    "    b*) exec /bin/sleep 60 ;;",
    "    c*) echo 'junk header' ;;",
    "    d*) printf '%s blob 3\\nabc\\n' " <> show (idOf 'a') <> " ;;",
    "    e*) printf '%s blob 10\\nabc' \"$oid\"; exit 1 ;;",
    "    f*) printf '%s blob 3\\nabcX' \"$oid\" ;;",
    "    1*) echo 'warning: lazy fetching disabled; some objects may not be' \\",
    "          'available' >&2",
    "        echo \"fatal: could not fetch $oid from promisor remote\" >&2",
    "        exit 128 ;;",
    "    2*) dd if=/dev/zero bs=1024 count=1024 1>&2 2>/dev/null",
    "        printf '%s blob 3\\nabc\\n' \"$oid\" ;;",
    "    3*) exit 3 ;;",
    "    4*) printf '%s blob 3\\nabc\\n' \"$oid\"; exit 0 ;;",
    "    5*) echo 'fatal: could not fetch " <> idOf '9' <> " from promisor remote' >&2",
    "        printf '%s blob 3\\nabc\\n' \"$oid\" ;;",
    "    6*) printf '%s missing\\n' " <> idOf 'a' <> " ;;",
    "  esac",
    "done"
  ]

idOf :: Char -> String
idOf c = c : replicate 39 '0'

oid :: Char -> ObjectId
oid = ObjectId . Text.pack . idOf

-- | A fake cat-file in a temp dir, as a 'Git'. The extra lines run first.
withFake :: [String] -> (FilePath -> Git -> IO a) -> IO a
withFake extra = withScript (\dir -> extra <> fakeCatFile dir)

-- | A fake git in a temp dir, from its body (given the dir), as a 'Git'.
withScript :: (FilePath -> [String]) -> (FilePath -> Git -> IO a) -> IO a
withScript body action =
  withCanonicalTemp $ \dir -> do
    exe <- writeScript dir "git" (body dir)
    let g =
          Git
            { exe = exe,
              cwd = dir,
              env = hardenedEnv [] ["/usr/bin", "/bin"],
              probeTimeLimit = gitProbeTimeLimit,
              listTimeLimit = gitProbeTimeLimit
            }
    action dir g

settings :: Int -> ReaderSettings
settings seconds =
  ReaderSettings {handles = 1, readTimeLimit = Microseconds (seconds * 1_000_000)}

-- | A function, not a shared value: see Note [Shared values in tests]
-- in "HaskellReviewer.Core.PackageGlobTest".
isAbc :: Either ObjectError BlobBytes -> Assertion
isAbc got = got @?= Right (BlobBytes (FileSize 3) "abc")

-- | A fake that logs each call to calls and runs the line for
-- @ls-tree --long@. Anything else fails.
lsTreeFake :: String -> FilePath -> [String]
lsTreeFake line dir =
  [ "echo \"$*\" >> '" <> dir </> "calls" <> "'",
    "case \" $* \" in",
    "  *' ls-tree '*' --long '*) " <> line <> " ;;",
    "esac",
    "exit 1"
  ]

-- | Wait up to 10 s for our child to exit (and stay unreaped).
waitForZombie :: Int -> IO ()
waitForZombie pid = go (500 :: Int)
  where
    go 0 = assertFailure ("pid " <> show pid <> " never exited")
    go n = do
      state <- readProcess "ps" ["-o", "stat=", "-p", show pid] ""
      unless ("Z" `isPrefixOf` dropWhile (== ' ') state) $
        threadDelay 20_000 >> go (n - 1)

-- | The pids the fake logged.
startsIn :: FilePath -> IO [Int]
startsIn dir = do
  let f = dir </> "starts"
  there <- Dir.doesFileExist f
  if there then map read . lines <$> readFile f else pure []

-- | None of the pids runs any more.
pidsGone :: [Int] -> Assertion
pidsGone pids = forM_ pids $ \p -> do
  alive <- try @IOException (signalProcess nullSignal (fromIntegral p))
  assertBool ("pid " <> show p <> " still running") (isLeft alive)

-- | Wait up to 10 s for a file to hold a line with the text.
waitForLine :: FilePath -> String -> IO ()
waitForLine file text = go (500 :: Int)
  where
    go 0 = assertFailure ("never saw " <> text <> " in " <> file)
    go n = do
      there <- Dir.doesFileExist file
      found <- if there then (text `isInfixOf`) <$> readFile file else pure False
      unless found (threadDelay 20_000 >> go (n - 1))

failure :: ObjectId -> ReadProblem -> Either ObjectError a
failure o p = Left (ObjectReadFailed ReadFailure {oid = o, problem = p})

fakeTests :: [TestTree]
fakeTests =
  [ testCase "starts on the first read, then reuses the process" $
      withFake [] $ \dir g ->
        withObjectReader quiet (settings 10) g $ \r -> do
          startsIn dir >>= (@?= [])
          replicateM_ 3 (readBlob r 10 (oid 'a') >>= isAbc)
          length <$> startsIn dir >>= (@?= 1),
    testCase "cap: bytes cut, size full" $
      withFake [] $ \_ g ->
        withObjectReader quiet (settings 10) g $ \r -> do
          readBlob r 2 (oid 'a') >>= (@?= Right (BlobBytes (FileSize 3) "ab"))
          readBlob r 10 (oid 'a') >>= isAbc,
    testCase "a hang is killed at the limit, the next read restarts" $
      withFake [] $ \dir g ->
        -- 5 s, not 2: the read after the kill starts a fresh shell, and
        -- under load that alone took over 2 s.
        withObjectReader quiet (settings 5) g $ \r -> do
          readBlob r 10 (oid 'b') >>= (@?= failure (oid 'b') TimedOut)
          startsIn dir >>= pidsGone
          readBlob r 10 (oid 'a') >>= isAbc
          length <$> startsIn dir >>= (@?= 2),
    testCase "a crash: read error, then a restart" $
      withFake [] $ \dir g ->
        withObjectReader quiet (settings 10) g $ \r -> do
          readBlob r 10 (oid 'a') >>= isAbc
          readBlob r 10 (oid 'a') >>= isAbc
          readBlob r 10 (oid '3') >>= (@?= failure (oid '3') (Died ""))
          readBlob r 10 (oid 'a') >>= isAbc
          length <$> startsIn dir >>= (@?= 2),
    testCase "bad output: junk, another id, short body, no LF" $
      withFake [] $ \dir g ->
        withObjectReader quiet (settings 10) g $ \r -> do
          let badOutput c = do
                got <- readBlob r 10 (oid c)
                case got of
                  Left (ObjectReadFailed f) | BadAnswer _ <- f.problem -> pure ()
                  _ -> assertFailure (c : ": " <> show got)
          badOutput 'c'
          badOutput 'd'
          readBlob r 10 (oid 'e') >>= \case
            Left (ObjectReadFailed f) | Died _ <- f.problem -> pure ()
            other -> assertFailure ("e: " <> show other)
          badOutput 'f'
          readBlob r 10 (oid 'a') >>= isAbc
          starts <- startsIn dir
          length starts @?= 5
          pidsGone (take 4 starts),
    testCase "2.44.1 promisor death: missing, then a restart" $
      withFake [] $ \dir g ->
        withObjectReader quiet (settings 10) g $ \r -> do
          readBlob r 10 (oid '1') >>= (@?= Left (ObjectMissing (oid '1')))
          objectInfo r (oid '1') >>= (@?= Left (ObjectMissing (oid '1')))
          readBlob r 10 (oid 'a') >>= isAbc
          length <$> startsIn dir >>= (@?= 3),
    testCase "1 MiB on stderr doesn't block" $
      withFake [] $ \_ g ->
        withObjectReader quiet (settings 10) g $ \r -> do
          readBlob r 10 (oid '2') >>= (@?= Right (BlobBytes (FileSize 3) "abc"))
          readBlob r 10 (oid '2') >>= (@?= Right (BlobBytes (FileSize 3) "abc")),
    testCase "a bad id is never written" $
      withFake [] $ \dir g ->
        withObjectReader quiet (settings 10) g $ \r -> do
          let bad =
                [ ObjectId (Text.pack (idOf 'a') <> "\nflush"),
                  ObjectId (Text.toUpper (Text.pack (idOf 'a'))),
                  ObjectId "HEAD",
                  ObjectId (Text.pack (take 39 (idOf 'a')) <> " ")
                ]
          forM_ bad $ \b@(ObjectId t) -> readBlob r 10 b >>= (@?= Left (ObjectBadId t))
          startsIn dir >>= (@?= [])
          readBlob r 10 (oid 'a') >>= isAbc
          requests <- lines <$> readFile (dir </> "requests")
          requests @?= ["contents " <> idOf 'a'],
    testCase "a read cancelled half way: killed, the next read restarts" $
      withFake [] $ \dir g ->
        withObjectReader quiet (settings 30) g $ \r -> do
          reading <- async (readBlob r 10 (oid 'b'))
          waitForLine (dir </> "requests") (idOf 'b')
          cancel reading
          startsIn dir >>= pidsGone
          readBlob r 10 (oid 'a') >>= isAbc
          length <$> startsIn dir >>= (@?= 2),
    testCase "stop while a read is in flight: every process gone" $
      withFake [] $ \dir g -> do
        (reader, pending) <-
          withObjectReader quiet defaultReaderSettings g $ \r -> do
            -- One slot busy, one idle with a live process.
            readBlob r 10 (oid 'a') >>= isAbc
            reading <- async (readBlob r 10 (oid 'b'))
            waitForLine (dir </> "requests") (idOf 'b')
            pure (r, reading)
        starts <- startsIn dir
        pidsGone starts
        wait pending >>= (@?= Left ObjectReaderClosed)
        readBlob reader 10 (oid 'a') >>= (@?= Left ObjectReaderClosed)
        startsIn dir >>= (@?= starts),
    testCase "died while idle: the write fails, one retry on a new process" $
      withFake [] $ \dir g ->
        withObjectReader quiet (settings 10) g $ \r -> do
          readBlob r 10 (oid '4') >>= isAbc
          -- Wait until it has exited (a zombie: only we reap it).
          [pid] <- startsIn dir
          waitForZombie pid
          readBlob r 10 (oid 'a') >>= isAbc
          length <$> startsIn dir >>= (@?= 2),
    testCase "a promisor line for another id is not 'missing'" $
      withFake [] $ \dir g ->
        withObjectReader quiet (settings 10) g $ \r -> do
          readBlob r 10 (oid '5') >>= isAbc
          readBlob r 10 (oid '3') >>= \case
            Left (ObjectReadFailed f)
              | Died err <- f.problem ->
                  assertBool (show err) ("promisor remote" `Text.isInfixOf` err)
            other -> assertFailure (show other)
          length <$> startsIn dir >>= (@?= 1),
    testCase "'missing' for another id is a bad answer" $
      withFake [] $ \_ g ->
        withObjectReader quiet (settings 10) g $ \r ->
          readBlob r 10 (oid '6') >>= \case
            Left (ObjectReadFailed f) | BadAnswer _ <- f.problem -> pure ()
            other -> assertFailure (show other),
    testCase "listCommit: a time out is an error, not the fallback" $
      withScript (lsTreeFake "exec /bin/sleep 60") $ \dir g -> do
        let quick = g {listTimeLimit = Microseconds 2_000_000}
        r <- listCommit quiet quick (RepoInfo "" Sha1 FullHistory) (oid 'a')
        case r of
          Left (ListGitFailed (GitTimedOut _)) -> pure ()
          other -> assertFailure (show other)
        calls <- lines <$> readFile (dir </> "calls")
        length calls @?= 1,
    testCase "listCommit: too much output is an error, not the fallback" $
      withScript (lsTreeFake "exec head -c 270000000 /dev/zero") $ \dir g -> do
        r <- listCommit quiet g (RepoInfo "" Sha1 FullHistory) (oid 'a')
        case r of
          Left (ListGitFailed (GitOutputTooLarge _)) -> pure ()
          other -> assertFailure (show (() <$ other))
        calls <- lines <$> readFile (dir </> "calls")
        length calls @?= 1,
    testCase "rev-parse junk: GitBadOutput; --verify junk: no commit" $
      withScript (const ["echo junk", "echo more"]) $ \_ g -> do
        repoInfo g >>= \case
          Left (GitBadOutput _) -> pure ()
          other -> assertFailure (show other)
        -- Exit 0 with anything but one id: no commit (D-316).
        resolveCommit g "HEAD" >>= (@?= Right Nothing)
        mergeBase g (oid 'a') (oid 'b') >>= \case
          Left (GitBadOutput _) -> pure ()
          other -> assertFailure (show other),
    testCase "rev-parse --verify and merge-base exit 128: GitFailed" $
      withScript (const ["echo 'fatal: bad' >&2", "exit 128"]) $ \_ g -> do
        resolveCommit g "HEAD" >>= \case
          Left (GitFailed f) -> f.code @?= 128
          other -> assertFailure (show other)
        mergeBase g (oid 'a') (oid 'b') >>= \case
          Left (GitFailed f) -> f.code @?= 128
          other -> assertFailure (show other),
    testCase "hardened: args, env, no inherited fds" $
      withCanonicalTemp $ \tmp -> do
        let fds = tmp </> "cat-file.fds"
        withFake
          [ "printf '%s\\n' \"$@\" > '" <> tmp </> "args" <> "'",
            "env > '" <> tmp </> "env" <> "'",
            listFdsTo ("'" <> fds <> "'")
          ]
          $ \dir g -> do
            withFdLock $
              withObjectReader quiet (settings 10) g $ \r ->
                readBlob r 10 (oid 'a') >>= isAbc
            args <- lines <$> readFile (tmp </> "args")
            args @?= hardenedArgs ["cat-file", "--batch-command"]
            names <- map (takeWhile (/= '=')) . lines <$> readFile (tmp </> "env")
            let allowed = map fst g.env <> ["PWD", "SHLVL", "_", "OLDPWD"]
                -- macOS adds __CF_USER_TEXT_ENCODING.
                ok n = n `elem` allowed || "__CF" `isPrefixOf` n
            assertBool (show names) (all ok names)
            baseline <- baselineFds tmp dir
            assertFds baseline fds,
    testCase "fallback: listing without sizes, a missing blob at first read" $
      withCanonicalTemp $ \tmp -> do
        fixtureRepo "kinds" [tmp </> "repo"]
        Dir.createDirectory (tmp </> "home")
        real <- realGitIn (tmp </> "home") (tmp </> "repo" </> "proj")
        missing <-
          filter (/= '\n') <$> gitOut (tmp </> "repo") ["rev-parse", "HEAD:proj/a.hs"]
        exe <- writeScript tmp "git" (wrapper tmp real.exe missing)
        let g = real {exe = exe}
        info <- repoInfo g >>= either (assertFailure . show) pure
        Just commit <- resolveCommit g "HEAD" >>= either (assertFailure . show) pure
        cf <- listCommit quiet g info commit >>= either (assertFailure . show) pure
        Map.size cf.files.entries @?= 9
        assertBool "sizes unknown" (all ((== BlobSizeUnknown) . (.size)) cf.blobs)
        Map.size cf.blobs @?= 8
        startsIn tmp >>= (@?= [])
        withObjectReader quiet (settings 10) g $ \r -> do
          let a = oidOf cf "a.hs"
          readBlob r cap a >>= (@?= Left (ObjectMissing a))
          big <- readBlob r cap (oidOf cf "big.txt")
          fmap (.size) big @?= Right (FileSize 1_200_000)
          objectInfo r (oidOf cf "lfs.bin")
            >>= (@?= Right (ObjectInfo TypeBlob (FileSize 127)))
        length <$> startsIn tmp >>= (@?= 2)
  ]

-- | A git that fails ls-tree --long as 2.44.1 does in a partial clone,
-- and whose cat-file dies on one id. The rest goes to the real git.
wrapper :: FilePath -> FilePath -> String -> [String]
wrapper tmp real missing =
  [ "case \" $* \" in",
    "  *' ls-tree '*' --long '*)",
    "    echo 'warning: lazy fetching disabled' >&2",
    "    echo 'fatal: could not fetch " <> missing <> " from promisor remote' >&2",
    "    exit 128 ;;",
    "  *' cat-file '*)",
    "    echo $$ >> '" <> tmp </> "starts" <> "'",
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
