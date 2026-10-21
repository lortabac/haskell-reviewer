-- | Tests that run the real binary.
--
-- A normal run serves until a signal, so those tests start the binary,
-- wait for the URL line on stderr, poke it, and stop it with a signal.
-- Every run passes --no-open, except the tests of the opener, which
-- control PATH.
module ExeTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Char (isDigit, toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import HaskellReviewer.App.Version (toolVersion)
import HaskellReviewer.Effects.Browser (openerName)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket.BS
import Numeric qualified
import System.Directory qualified as Dir
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, hGetContents', hGetLine, hIsEOF, readFile')
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Resource
  ( Resource (ResourceOpenFiles),
    ResourceLimit (..),
    ResourceLimits (..),
    getResourceLimit,
  )
import System.Posix.Signals (Signal, sigINT, sigKILL, sigTERM, signalProcess)
import System.Process
  ( CmdSpec (..),
    CreateProcess (..),
    ProcessHandle,
    StdStream (CreatePipe),
    createProcess,
    getPid,
    proc,
    readCreateProcessWithExitCode,
    waitForProcess,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    testCaseInfo,
    (@?=),
  )
import TestSupport (git, withCanonicalTemp, writeScript)

tests :: TestTree
tests =
  testGroup
    "Executable"
    [ quickExitTests,
      serverTests,
      filesTests,
      gitTests,
      signalTests,
      openerTests
    ]

-- * Runs that exit at once

quickExitTests :: TestTree
quickExitTests =
  testGroup
    "quick exit"
    [ testCase "--help: exit 0, flags on stderr" $ do
        (code, err) <- runExe ["--help"]
        code @?= ExitSuccess
        assertContains "--port" err,
      testCase "--version: exit 0, same version as the library" $ do
        (code, err) <- runExe ["--version"]
        code @?= ExitSuccess
        -- Catches a stale binary on PATH.
        assertContains ("haskell-reviewer " <> Text.unpack toolVersion) err,
      testCase "--bogus: exit 2" $ do
        (code, _) <- runExe ["--bogus"]
        code @?= ExitFailure 2,
      testCase "--port abc: exit 2" $ do
        (code, _) <- runExe ["--port", "abc"]
        code @?= ExitFailure 2,
      testCase "missing ROOT: exit 2" $ do
        (code, err) <- runExe ["/no/such/dir"]
        code @?= ExitFailure 2
        assertContains "ROOT does not exist" err,
      testCase "file as ROOT: exit 2" $
        withTemp $ \dir -> do
          let file = dir </> "file"
          writeFile file ""
          (code, err) <- runExe [file]
          code @?= ExitFailure 2
          assertContains "ROOT is not a directory" err,
      testCase "unreadable ROOT: exit 2" $
        withTemp $ \dir -> do
          let sub = dir </> "locked"
              -- Give the rights back, so the temp dir can be removed.
              unlock =
                Dir.setPermissions sub $
                  Dir.setOwnerReadable True $
                    Dir.setOwnerWritable True $
                      Dir.setOwnerSearchable True Dir.emptyPermissions
          Dir.createDirectory sub
          flip finally unlock $ do
            Dir.setPermissions sub Dir.emptyPermissions
            perms <- Dir.getPermissions sub
            -- Root can read anything: nothing to test then.
            unless (Dir.readable perms) $ do
              (code, err) <- runExe [sub]
              code @?= ExitFailure 2
              assertContains "cannot read ROOT" err,
      testCase "--trace-file in a missing dir: exit 2" $
        withTemp $ \dir -> do
          (code, err) <- runExe ["--trace-file", dir </> "missing" </> "t.jsonl", dir]
          code @?= ExitFailure 2
          assertContains "cannot open trace file" err,
      testCase "non-ASCII ROOT with LC_ALL=C: exit 2, not 1" $ do
        -- "dïr" as raw UTF-8 bytes (C3 AF), written as GHC's escapes for
        -- undecodable bytes, so the test works whatever our own locale is.
        -- The binary prints them as U+FFFD.
        let root = "/no/such/d\xDCC3\xDCAFr"
        env <- getEnvironment
        let cEnv = ("LC_ALL", "C") : filter ((/= "LC_ALL") . fst) env
        (code, err) <- runExeWith (\p -> p {env = Just cEnv}) [root]
        code @?= ExitFailure 2
        assertContains "ROOT does not exist: /no/such/d" err,
      testCase "busy --port: exit 2" $
        withTemp $ \dir ->
          bracket busySocket Socket.close $ \s -> do
            port <- show <$> Socket.socketPort s
            (code, err) <- runExe ["--no-open", "--port", port, dir]
            code @?= ExitFailure 2
            assertContains
              ("haskell-reviewer: cannot listen on 127.0.0.1:" <> port <> ": ")
              err
    ]

-- * HTTP

serverTests :: TestTree
serverTests =
  testGroup
    "server"
    [ testCaseInfo "start-up: URL printed and accepting connections" $
        withTemp $ \dir -> do
          t0 <- getMonotonicTime
          withServer id [dir] $ \server -> do
            -- Connected means accepting: the URL line comes after listen.
            r <- httpGet server (goodHost server) (tokenPath server)
            t1 <- getMonotonicTime
            r.status @?= 200
            let ms = round ((t1 - t0) * 1000) :: Int
            -- The target is checked by eye (and by benchmarks later):
            -- test machines are too noisy for a hard limit. Only a hang
            -- fails.
            assertBool ("start-up took " <> show ms <> " ms") (ms < 5000)
            pure ("URL + accepting in " <> show ms <> " ms (target < 300 ms)"),
      testCase "right token: 200 with the security headers" $
        withTemp $ \dir -> withServer id [dir] $ \server -> do
          r <- httpGet server (goodHost server) (tokenPath server)
          r.status @?= 200
          lookup "content-type" r.headers @?= Just "text/html; charset=utf-8"
          assertSecurityHeaders r,
      testCase "an asset of the shell: 200, cached as immutable" $
        withTemp $ \dir -> withServer id [dir] $ \server -> do
          shell <- httpGet server (goodHost server) (tokenPath server)
          let prefix = Char8.pack (tokenPath server <> "assets/")
              (_, rest) = BS.breakSubstring prefix shell.body
              path = Char8.unpack (Char8.takeWhile (/= '"') rest)
          assertBool "no asset ref in the shell" (not (BS.null rest))
          r <- httpGet server (goodHost server) path
          r.status @?= 200
          lookup "cache-control" r.headers
            @?= Just "private, max-age=31536000, immutable"
          lookup "x-content-type-options" r.headers @?= Just "nosniff"
          lookup "content-length" r.headers
            @?= Just (Char8.pack (show (BS.length r.body)))
          h <- httpRequest server "HEAD" (goodHost server) path
          lookup "content-length" h.headers @?= lookup "content-length" r.headers
          h.body @?= "",
      testCase "localhost Host: 200" $
        withTemp $ \dir -> withServer id [dir] $ \server -> do
          r <- httpGet server (Just ("localhost:" <> show server.port)) (tokenPath server)
          r.status @?= 200,
      testCase "HEAD: 200, no body" $
        withTemp $ \dir -> withServer id [dir] $ \server -> do
          r <- httpRequest server "HEAD" (goodHost server) (tokenPath server)
          r.status @?= 200
          r.body @?= "",
      testCase "no token, wrong token, wrong length: 404" $
        withTemp $ \dir -> withServer id [dir] $ \server -> do
          let wrong = map (\c -> if c == 'A' then 'B' else 'A') server.token
          mapM_
            ( \path -> do
                r <- httpGet server (goodHost server) path
                (path, r.status) @?= (path, 404)
                r.body @?= "not found"
                assertSecurityHeaders r
            )
            [ "/",
              "/" <> wrong <> "/",
              "/" <> take 31 server.token <> "/",
              "/" <> server.token <> "A/",
              "/" <> server.token <> "/nope"
            ],
      testCase "bad Host, missing Host, other port: 403" $
        withTemp $ \dir -> withServer id [dir] $ \server -> do
          let other = show (if server.port == 65535 then 1 else server.port + 1)
          mapM_
            ( \host -> do
                r <- httpGet server host (tokenPath server)
                (host, r.status) @?= (host, 403)
                assertSecurityHeaders r
            )
            [ Just ("evil.com:" <> show server.port),
              Nothing,
              Just ("127.0.0.1:" <> other),
              Just "127.0.0.1"
            ],
      testCase "event stream: headers and hello within 5 s" $
        withTemp $ \dir -> withServer id [dir] $ \server -> do
          -- No Connection: close, like a browser. The stream never ends,
          -- so read only up to the first event.
          let req =
                "GET "
                  <> Char8.pack (tokenPath server)
                  <> "api/events HTTP/1.1\r\nHost: 127.0.0.1:"
                  <> Char8.pack (show server.port)
                  <> "\r\n\r\n"
          -- Up to the end of the first chunk.
          bytes <- rawHttpUntil server.port req "data: {}\n\n\r\n"
          -- Add the last chunk, so the body parses.
          case parseResponse (bytes <> "0\r\n\r\n") of
            Nothing -> assertFailure ("bad HTTP answer: " <> show bytes)
            Just r -> do
              r.status @?= 200
              lookup "content-type" r.headers @?= Just "text/event-stream"
              lookup "transfer-encoding" r.headers @?= Just "chunked"
              assertSecurityHeaders r
              r.body @?= "id: 1\nevent: hello\ndata: {}\n\n",
      testCase "warp's own error response has the security headers" $
        withTemp $ \dir -> withServer id [dir] $ \server -> do
          -- Headers over warp's 50 KiB limit: warp answers by itself.
          -- Exactly one byte over, no line end: warp reads all we send
          -- before it answers. Unread bytes would make the kernel reset
          -- the connection on close, and the reset can beat the answer
          -- (see Note [Loopback MSS]). Depends on warp's default limit
          -- (settingsMaxTotalHeaderLength, 50 KiB): if it changes,
          -- change the size here.
          let start =
                Char8.pack
                  ( "GET "
                      <> tokenPath server
                      <> " HTTP/1.1\r\nHost: 127.0.0.1:"
                      <> show server.port
                      <> "\r\nX-Big: "
                  )
              huge = start <> BS.replicate (50 * 1024 + 1 - BS.length start) 97
          r <- rawHttp server.port huge
          r.status @?= 400
          assertSecurityHeaders r
    ]

-- * File routes

filesTests :: TestTree
filesTests =
  testGroup
    "files"
    [ testCase "path escapes and unlisted files: 404, no-store, nosniff" $
        withCanonicalTemp $ \dir -> do
          git dir ["init", "-q"]
          writeFile (dir </> ".gitignore") ".env\n"
          writeFile (dir </> ".env") "SECRET=1\n"
          Dir.createDirectory (dir </> "a")
          writeFile (dir </> "a" </> "b") "b\n"
          withServer id [dir] $ \server -> do
            let base = tokenPath server <> "api/file/tree/"
            ok <- httpGet server (goodHost server) (base <> "a/b")
            ok.status @?= 200
            mapM_
              ( \path -> do
                  r <- httpGet server (goodHost server) (base <> path)
                  (path, r.status) @?= (path, 404)
                  r.body @?= "not found"
                  assertSecurityHeaders r
              )
              [ "..%2F..%2Fetc%2Fpasswd",
                "../../etc/passwd",
                "%2e%2e/%2e%2e/etc/passwd",
                "a/%2e%2e/a/b",
                "a%2Fb",
                "a%00b",
                "a/b%00",
                "/etc/passwd",
                "a//b",
                "a/b/",
                "a",
                "",
                "not/listed",
                ".env",
                ".git/config"
              ],
      testCase "a <script> file: JSON text, nosniff" $
        withCanonicalTemp $ \dir -> do
          writeFile (dir </> "x.html") "<script>alert(1)</script>\n"
          withServer id [dir] $ \server -> do
            r <- getPath server "api/file/tree/x.html"
            r.status @?= 200
            lookup "content-type" r.headers @?= Just "application/json"
            r.body
              @?= "{\"generation\":1,\"kind\":\"text\",\"text\":\"<script>alert(1)</script>\\n\"}"
            assertSecurityHeaders r,
      testCase "file list: ignored files are not in it" $
        withCanonicalTemp $ \dir -> do
          git dir ["init", "-q"]
          writeFile (dir </> ".gitignore") ".env\n"
          writeFile (dir </> ".env") ""
          writeFile (dir </> "a.hs") ""
          withServer id [dir] $ \server -> do
            r <- getPath server "api/files/tree"
            r.status @?= 200
            r.body
              @?= "{\"generation\":1,\"source\":\"git\",\"entries\":[\
                  \{\"path\":\".gitignore\",\"kind\":\"file\"},\
                  \{\"path\":\"a.hs\",\"kind\":\"file\"}],\
                  \\"skippedNames\":0,\"truncated\":false}"
    ]

-- * git

gitTests :: TestTree
gitTests =
  testGroup
    "git"
    [ testCase "git planted in ROOT, only one on PATH: not run, from disk" $
        withCanonicalTemp $ \dir -> do
          let marker = dir </> "marker"
              root = dir </> "root"
          Dir.createDirectoryIfMissing True (root </> "bin")
          plantGit (root </> "bin") marker
          env <- envWith [("PATH", root </> "bin")] []
          withServer (withEnv env) [root] $ \server -> do
            r <- getPath server "api/files/tree"
            assertContains "\"source\":\"disk\"" (Char8.unpack r.body)
          assertNoMarker marker,
      testCase "git planted in ROOT, real git after it: real one used" $
        withCanonicalTemp $ \dir -> do
          let marker = dir </> "marker"
              root = dir </> "root"
          Dir.createDirectoryIfMissing True (root </> "bin")
          git root ["init", "-q"]
          plantGit (root </> "bin") marker
          gitDir <- realGitDir
          env <- envWith [("PATH", root </> "bin" <> ":" <> gitDir)] []
          withServer (withEnv env) [root] $ \server -> do
            r <- getPath server "api/files/tree"
            assertContains "\"source\":\"git\"" (Char8.unpack r.body)
          assertNoMarker marker,
      testCase "git too old: exit 2" $
        withCanonicalTemp $ \dir -> do
          let bin = dir </> "bin"
          Dir.createDirectory bin
          exe <- writeScript bin "git" ["echo 'git version 2.43.0'"]
          env <- envWith [("PATH", bin)] []
          Dir.createDirectory (dir </> "root")
          (code, err) <- runExeWith (withEnv env) ["--no-open", dir </> "root"]
          code @?= ExitFailure 2
          assertContains
            ( "haskell-reviewer: git at "
                <> exe
                <> " is version 2.43.0; need 2.44 or newer"
            )
            err,
      testCase "git version unreadable: exit 2" $
        withCanonicalTemp $ \dir -> do
          let bin = dir </> "bin"
          Dir.createDirectory bin
          exe <- writeScript bin "git" ["echo 'not git'"]
          env <- envWith [("PATH", bin)] []
          Dir.createDirectory (dir </> "root")
          (code, err) <- runExeWith (withEnv env) ["--no-open", dir </> "root"]
          code @?= ExitFailure 2
          assertContains
            ("haskell-reviewer: cannot read the version of git at " <> exe <> ": not git")
            err,
      testCase "hardened env and flags reach git" $
        withCanonicalTemp $ \dir -> do
          let bin = dir </> "bin"
              root = dir </> "root"
              envLog = dir </> "env.log"
              argLog = dir </> "args.log"
          Dir.createDirectory bin
          Dir.createDirectory root
          git root ["init", "-q"]
          writeFile (root </> "a.hs") ""
          real <- realGit
          _ <-
            writeScript
              bin
              "git"
              [ "/usr/bin/env >> '" <> envLog <> "'",
                "echo \"$*\" >> '" <> argLog <> "'",
                "exec '" <> real <> "' \"$@\""
              ]
          gitDir <- realGitDir
          env <-
            envWith
              [("PATH", bin <> ":" <> gitDir)]
              [ ("GIT_DIR", "/nope"),
                ("GIT_WORK_TREE", "/nope"),
                ("GIT_CONFIG_PARAMETERS", "'core.fsmonitor=touch /tmp/x'"),
                ("GIT_CONFIG_COUNT", "1"),
                ("GIT_CONFIG_KEY_0", "core.pager"),
                ("GIT_CONFIG_VALUE_0", "touch /tmp/x"),
                ("GIT_INDEX_FILE", "/nope"),
                ("LANG", "fr_FR.UTF-8"),
                ("PAGER", "touch /tmp/x")
              ]
          withServer (withEnv env) [root] $ \server -> do
            r <- getPath server "api/files/tree"
            assertContains "\"path\":\"a.hs\"" (Char8.unpack r.body)
          envs <- lines <$> readFile envLog
          args <- lines <$> readFile argLog
          let names = map (takeWhile (/= '=')) envs
          mapM_
            (\bad -> assertBool (bad <> " reached git") (bad `notElem` names))
            [ "GIT_DIR",
              "GIT_WORK_TREE",
              "GIT_CONFIG_PARAMETERS",
              "GIT_CONFIG_COUNT",
              "GIT_CONFIG_KEY_0",
              "GIT_INDEX_FILE",
              "LANG",
              "PAGER"
            ]
          mapM_
            (\good -> assertBool (good <> " missing") (good `elem` envs))
            [ "LC_ALL=C",
              "GIT_OPTIONAL_LOCKS=0",
              "GIT_NO_LAZY_FETCH=1",
              "GIT_NO_REPLACE_OBJECTS=1",
              "GIT_TERMINAL_PROMPT=0"
            ]
          -- --version, rev-parse, two ls-files.
          length args @?= 4
          mapM_
            ( \a ->
                assertBool
                  ("flags missing: " <> a)
                  ( "--no-pager -c core.fsmonitor=false -c core.untrackedCache=false \
                    \-c core.hooksPath=/dev/null -c safe.bareRepository=explicit "
                      `isPrefixOf` a
                  )
            )
            args,
      -- See Note [No inherited fds] in "HaskellReviewer.Effects.Spawn".
      -- We start the binary through bash, with close_fds off, a soft fd
      -- limit of 8192 and a plain fd 5000 open (above the cap). Its gits
      -- must still see only 0-2 (plus ls's own /dev/fd, 3) and a soft
      -- limit of at most 4096. Fails if main skips prepareSpawning.
      -- bash, not sh: dash (Ubuntu's sh) only takes one-digit fds in
      -- redirections.
      testCaseInfo "git inherits no fds and a capped limit" $ do
        lim <- getResourceLimit ResourceOpenFiles
        hasBash <- Dir.doesFileExist "/bin/bash"
        let room = case lim.hardLimit of
              ResourceLimit h -> h >= 8192
              ResourceLimitInfinity -> True
              ResourceLimitUnknown -> False
        if not hasBash || not room
          then
            pure $
              if hasBash
                then "skipped: hard fd limit below 8192"
                else "skipped: no /bin/bash"
          else withCanonicalTemp $ \dir -> do
            let bin = dir </> "bin"
                root = dir </> "root"
                -- One file per call: the two ls-files run at once.
                fdDir = dir </> "fds"
            Dir.createDirectory bin
            Dir.createDirectory fdDir
            Dir.createDirectory root
            git root ["init", "-q"]
            writeFile (root </> "a.hs") ""
            real <- realGit
            _ <-
              writeScript
                bin
                "git"
                -- One command per line, each with its own redirect: no
                -- group redirect, so the shell itself holds no extra fd
                -- while ls runs. lsof (if there) shows what the shell
                -- holds, for the failure message.
                [ "log='" <> fdDir <> "'/$$",
                  "ulimit -n > \"$log.limit\"",
                  "/bin/ls -l /dev/fd/ > \"$log.fds\"",
                  "for l in /usr/sbin/lsof /usr/bin/lsof; do",
                  "  if [ -x \"$l\" ]; then \"$l\" -p $$ > \"$log.lsof\" 2>&1; break; fi",
                  "done",
                  "exec '" <> real <> "' \"$@\""
                ]
            gitDir <- realGitDir
            env <- envWith [("PATH", bin <> ":" <> gitDir)] []
            let shell = "ulimit -S -n 8192; exec 5000</dev/null; exec \"$0\" \"$@\""
                viaShell p = case p.cmdspec of
                  RawCommand exe given ->
                    p {cmdspec = RawCommand "/bin/bash" (["-c", shell, exe] <> given)}
                  ShellCommand _ -> p
                tweak p = viaShell (withEnv env p) {close_fds = False}
                args = ["--trace-file", dir </> "trace.jsonl", root]
            withServer tweak args $ \server -> do
              r <- getPath server "api/files/tree"
              assertContains "\"path\":\"a.hs\"" (Char8.unpack r.body)
            files <- Dir.listDirectory fdDir
            let logs = [fdDir </> takeWhile (/= '.') f | f <- files, ".fds" `isSuffixOf` f]
            -- --version, rev-parse, two ls-files.
            length logs @?= 4
            mapM_ checkFdLog logs
            pure "",
      testCase "hostile repo config: nothing runs, .git unchanged" $
        withCanonicalTemp $ \dir -> do
          let root = dir </> "root"
              hooks = dir </> "hooks"
              marker = dir </> "marker"
          Dir.createDirectory root
          Dir.createDirectory hooks
          touch <- writeScript dir "touch-marker" ["echo \"$0 $*\" >> '" <> marker <> "'"]
          mapM_
            (\h -> writeScript hooks h ["echo hook " <> h <> " >> '" <> marker <> "'"])
            [ "pre-commit",
              "post-checkout",
              "post-index-change",
              "fsmonitor-watchman",
              "reference-transaction",
              "post-merge"
            ]
          git root ["init", "-q"]
          writeFile (root </> "a.hs") "a\n"
          writeFile (root </> "b.hs") "b\n"
          git root ["add", "a.hs", "b.hs"]
          mapM_
            (\(k, v) -> git root ["config", k, v])
            [ ("core.fsmonitor", touch),
              ("core.hooksPath", hooks),
              ("core.pager", touch),
              ("pager.ls-files", touch),
              ("alias.ls-files", "!" <> touch),
              ("diff.external", touch),
              ("core.untrackedCache", "true"),
              ("core.splitIndex", "true"),
              ("core.sshCommand", touch),
              ("core.askPass", touch)
            ]
          -- A stale index: git status would refresh it.
          threadDelay 1_100_000
          writeFile (root </> "a.hs") "changed\n"
          writeFile (root </> "new.hs") ""
          before <- snapshotDir (root </> ".git")
          withServer id [root] $ \server -> do
            r <- getPath server "api/files/tree"
            r.status @?= 200
            assertContains "\"path\":\"new.hs\"" (Char8.unpack r.body)
            f <- getPath server "api/file/tree/a.hs"
            f.status @?= 200
          after <- snapshotDir (root </> ".git")
          assertNoMarker marker
          after @?= before
          lock <- Dir.doesPathExist (root </> ".git" </> "index.lock")
          assertBool "index.lock left" (not lock)
    ]

-- | Check one call of the fake git in "git inherits no fds and a capped
-- limit": soft fd limit at most 4096, only fds 0-3 (3 is ls's own
-- /dev/fd). On failure, show the full ls -l and lsof output.
checkFdLog :: FilePath -> Assertion
checkFdLog logFile = do
  limit <- filter (/= '\n') <$> readFile (logFile <> ".limit")
  listing <- readFile (logFile <> ".fds")
  lsof <- either (const "(no lsof)") id <$> try @IOException (readFile' (logFile <> ".lsof"))
  assertBool ("fd limit " <> limit) (read limit <= (4096 :: Int))
  let fds = sort (map fdOf (filter (not . ("total" `isPrefixOf`)) (lines listing)))
  unless (fds == [0, 1, 2, 3]) $
    assertFailure $
      "fds seen by git: "
        <> show fds
        <> "\nls -l /dev/fd/:\n"
        <> listing
        <> "lsof of the shell:\n"
        <> lsof
  where
    -- The name is before "->" if it's a link (Linux), else last (macOS).
    fdOf :: String -> Int
    fdOf line = case break (== "->") (words line) of
      (before, _ : _) | not (null before) -> read (last before)
      (ws, _) -> read (last ws)

-- | A git script that writes the marker if run.
plantGit :: FilePath -> FilePath -> IO ()
plantGit bin marker = do
  _ <-
    writeScript
      bin
      "git"
      [ "echo planted >> '" <> marker <> "'",
        "echo 'git version 2.99.0'"
      ]
  pure ()

assertNoMarker :: FilePath -> Assertion
assertNoMarker marker = do
  ran <- Dir.doesPathExist marker
  unless (not ran) $ do
    what <- readFile marker
    assertFailure ("something ran: " <> what)

-- | The real git, from our PATH.
realGit :: IO FilePath
realGit =
  Dir.findExecutable "git"
    >>= maybe (assertFailure "git not found on PATH") Dir.canonicalizePath

realGitDir :: IO FilePath
realGitDir = takeDirectory <$> realGit

-- | Our environment, plus the given variables. Both lists win over
-- ours; they are two only to keep the call sites readable (what we
-- replace, what we add).
envWith :: [(String, String)] -> [(String, String)] -> IO [(String, String)]
envWith set extra = do
  env <- getEnvironment
  let names = map fst (set <> extra)
  pure (set <> extra <> filter ((`notElem` names) . fst) env)

-- | One file, as seen by 'snapshotDir'.
data FileState = FileState
  { path :: FilePath,
    size :: Integer,
    mtime :: String,
    contents :: ByteString
  }
  deriving (Eq, Show)

-- | Every file under a dir.
snapshotDir :: FilePath -> IO [FileState]
snapshotDir dir = do
  entries <- Dir.listDirectory dir
  concat
    <$> mapM
      ( \name -> do
          let p = dir </> name
          isLink <- Dir.pathIsSymbolicLink p
          isDir <- Dir.doesDirectoryExist p
          if isDir && not isLink
            then snapshotDir p
            else do
              size <- Dir.getFileSize p
              mtime <- show <$> Dir.getModificationTime p
              bytes <- BS.readFile p
              pure [FileState {path = p, size = size, mtime = mtime, contents = bytes}]
      )
      (sort entries)

-- * Signals and trace file

signalTests :: TestTree
signalTests =
  testGroup
    "signals"
    [ testCase "SIGTERM: exit 0" $
        withTemp $ \dir -> withServer id [dir] $ \server -> do
          (code, _) <- stopServer server sigTERM
          code @?= ExitSuccess,
      testCase "SIGINT: exit 0" $
        withTemp $ \dir -> withServer id [dir] $ \server -> do
          (code, _) <- stopServer server sigINT
          code @?= ExitSuccess,
      testCase "two quick SIGTERMs: exit 0" $
        withTemp $ \dir -> withServer id [dir] $ \server -> do
          -- The second one may hit the forced exit path. Both exit 0.
          getPid server.process >>= mapM_ (signalProcess sigTERM)
          (code, _) <- stopServer server sigTERM
          code @?= ExitSuccess,
      testCase "--trace-file: startup and files spans" $
        withTemp $ \dir -> do
          let file = dir </> "t.jsonl"
              root = dir </> "root"
          Dir.createDirectory root
          withServer id ["--trace-file", file, root] $ \server -> do
            r <- getPath server "api/files/tree"
            r.status @?= 200
            -- The span is written just after the list is ready.
            _ <- waitForFile' file ((== 2) . length . lines)
            (code, _) <- stopServer server sigTERM
            code @?= ExitSuccess
          contents <- readFile file
          case lines contents of
            [startup, files] -> do
              assertContains "\"name\":\"startup\"" startup
              assertContains "\"name\":\"files\"" files
            ls -> assertFailure ("expected two lines, got " <> show ls),
      testCase "--trace-file /dev/full: exit 0 with a warning" $ do
        -- Linux only: elsewhere, nothing to do.
        hasDevFull <- Dir.doesPathExist "/dev/full"
        when hasDevFull $
          withTemp $ \dir ->
            withServer id ["--trace-file", "/dev/full", dir] $ \server -> do
              (code, err) <- stopServer server sigTERM
              code @?= ExitSuccess
              assertContains "haskell-reviewer: warning: trace file:" err
    ]

-- * Browser opener

openerTests :: TestTree
openerTests =
  testGroup
    "opener"
    [ testCase "opens the URL, opener output not on our stdout" $
        withTemp $ \dir -> do
          let bin = dir </> "bin"
              root = dir </> "root"
              marker = dir </> "marker"
          Dir.createDirectory bin
          Dir.createDirectory root
          writeOpener bin marker
          withServerOpen bin [root] $ \server -> do
            opened <- waitForFile marker server.url
            opened @?= server.url
            (code, _) <- stopServer server sigTERM
            code @?= ExitSuccess,
      testCase "opener inside ROOT is not run" $
        withTemp $ \root -> do
          let bin = root </> "bin"
              marker = root </> "marker"
          Dir.createDirectory bin
          writeOpener bin marker
          withServerOpen bin [root] $ \server -> do
            waitForLine
              server
              ("haskell-reviewer: warning: cannot open the browser:" `isPrefixOf`)
            (code, _) <- stopServer server sigTERM
            code @?= ExitSuccess
          ran <- Dir.doesPathExist marker
          assertBool "planted opener ran" (not ran)
    ]

-- | An opener script that writes junk on stdout and stderr, and its
-- first argument to the marker file.
writeOpener :: FilePath -> FilePath -> IO ()
writeOpener bin marker =
  void $
    writeScript
      bin
      openerName
      [ "echo opener stdout",
        "echo opener stderr >&2",
        "printf '%s' \"$1\" > '" <> marker <> "'"
      ]

-- | Run without --no-open, with only the given dir on PATH.
withServerOpen :: FilePath -> [String] -> (Server -> IO a) -> IO a
withServerOpen bin args action = do
  env <- getEnvironment
  let env' = ("PATH", bin) : filter ((/= "PATH") . fst) env
  withServerRaw (\p -> p {env = Just env'}) args action

-- | Wait up to 5 s for the file's text to pass the check. Gives back
-- what it holds at the end.
waitForFile' :: FilePath -> (String -> Bool) -> IO String
waitForFile' file ok = go (50 :: Int)
  where
    go n = do
      exists <- Dir.doesPathExist file
      contents <- if exists then Char8.unpack <$> BS.readFile file else pure ""
      if ok contents || n == 0
        then pure contents
        else threadDelay 100000 >> go (n - 1)

-- | Wait up to 5 s for the file to hold the expected text. Gives back
-- what it holds at the end.
waitForFile :: FilePath -> String -> IO String
waitForFile file expected = waitForFile' file (== expected)

-- * Running the binary

-- | A running server.
data Server = Server
  { process :: ProcessHandle,
    port :: Int,
    token :: String,
    url :: String,
    stdout :: Handle,
    stderr :: Handle,
    -- | stderr lines read so far, oldest first.
    seen :: IORef [String]
  }

-- | Start the binary with --no-open, wait for the URL line, run the
-- action. The process is killed at the end if still there. The tweak
-- goes last, so it can turn close_fds off (it must keep the pipes).
withServer :: (CreateProcess -> CreateProcess) -> [String] -> (Server -> IO a) -> IO a
withServer tweak args = withServerRaw tweak ("--no-open" : args)

withServerRaw :: (CreateProcess -> CreateProcess) -> [String] -> (Server -> IO a) -> IO a
withServerRaw tweak args action = do
  exe <- findExe
  let cp =
        tweak
          (proc exe args)
            { std_out = CreatePipe,
              std_err = CreatePipe,
              close_fds = True
            }
  bracket (createProcess cp) kill $ \case
    (_, Just out, Just err, ph) -> do
      seen <- newIORef []
      found <- timeout 10000000 (readUrl err seen)
      url <- case found of
        Just (Just url) -> pure url
        Just Nothing -> do
          ls <- readIORef seen
          assertFailure ("exited before the URL line; stderr: " <> show ls)
        Nothing -> assertFailure "no URL line within 10 s"
      (port, token) <- case parseUrl url of
        Just pt -> pure pt
        Nothing -> assertFailure ("bad URL: " <> url)
      action
        Server
          { process = ph,
            port = port,
            token = token,
            url = url,
            stdout = out,
            stderr = err,
            seen = seen
          }
    _ -> assertFailure "no pipes"
  where
    kill (_, _, _, ph) = do
      pid <- getPid ph
      mapM_ (\p -> try @IOException (signalProcess sigKILL p)) pid
      void (waitForProcess ph)

-- | Read stderr up to the URL line. 'Nothing' at end of file.
readUrl :: Handle -> IORef [String] -> IO (Maybe String)
readUrl h seen = do
  line <- nextLine h seen
  case line of
    Nothing -> pure Nothing
    Just l -> case stripPrefix' urlPrefix l of
      Just url -> pure (Just url)
      Nothing -> readUrl h seen

urlPrefix :: String
urlPrefix = "haskell-reviewer: serving at "

-- | Read stderr until a line matches, for up to 10 s.
waitForLine :: Server -> (String -> Bool) -> Assertion
waitForLine server match = do
  found <- timeout 10000000 go
  case found of
    Just True -> pure ()
    _ -> do
      ls <- readIORef server.seen
      assertFailure ("line not found on stderr: " <> show ls)
  where
    go = do
      line <- nextLine server.stderr server.seen
      case line of
        Nothing -> pure False
        Just l | match l -> pure True
        Just _ -> go

nextLine :: Handle -> IORef [String] -> IO (Maybe String)
nextLine h seen = do
  eof <- hIsEOF h
  if eof
    then pure Nothing
    else do
      l <- hGetLine h
      modifyIORef' seen (<> [l])
      pure (Just l)

-- | Send a signal, wait for the exit (10 s at most). Checks that stdout
-- is empty. Gives back the exit code and all of stderr.
stopServer :: Server -> Signal -> IO (ExitCode, String)
stopServer server sig = do
  pid <- getPid server.process
  case pid of
    Just p -> signalProcess sig p
    Nothing -> assertFailure "process already gone"
  code <- timeout 10000000 (waitForProcess server.process)
  case code of
    Nothing -> assertFailure "no exit within 10 s of the signal"
    Just c -> do
      rest <- hGetContents' server.stderr
      out <- hGetContents' server.stdout
      out @?= ""
      before <- readIORef server.seen
      pure (c, unlines before <> rest)

-- | Split the session URL (@http:\/\/127.0.0.1:PORT\/TOKEN\/@) into port
-- and token.
parseUrl :: String -> Maybe (Int, String)
parseUrl url = do
  rest <- stripPrefix' "http://127.0.0.1:" url
  let (digits, afterPort) = span isDigit rest
  path <- stripPrefix' "/" afterPort
  let (token, end) = break (== '/') path
  if null digits || end /= "/" then Nothing else Just (read digits, token)

stripPrefix' :: String -> String -> Maybe String
stripPrefix' prefix s
  | prefix `isPrefixOf` s = Just (drop (length prefix) s)
  | otherwise = Nothing

-- | GET a path under the token.
getPath :: Server -> String -> IO HttpResponse
getPath server p = httpGet server (goodHost server) (tokenPath server <> p)

-- | Run with this environment.
withEnv :: [(String, String)] -> CreateProcess -> CreateProcess
withEnv env p = p {env = Just env}

tokenPath :: Server -> String
tokenPath server = "/" <> server.token <> "/"

goodHost :: Server -> Maybe String
goodHost server = Just ("127.0.0.1:" <> show server.port)

-- | A socket listening on a free port of 127.0.0.1.
busySocket :: IO Socket.Socket
busySocket = do
  s <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.bind s (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  Socket.listen s 1
  pure s

-- * Raw HTTP

data HttpResponse = HttpResponse
  { status :: Int,
    -- | Names in lower case.
    headers :: [(String, ByteString)],
    body :: ByteString
  }

httpGet :: Server -> Maybe String -> String -> IO HttpResponse
httpGet server = httpRequest server "GET"

httpRequest :: Server -> ByteString -> Maybe String -> String -> IO HttpResponse
httpRequest server method host path =
  rawHttp server.port (requestBytes method host path "")

-- | An HTTP/1.1 request with @Connection: close@, plus extra header lines.
requestBytes :: ByteString -> Maybe String -> String -> ByteString -> ByteString
requestBytes method host path extra =
  method
    <> " "
    <> Char8.pack path
    <> " HTTP/1.1\r\n"
    <> maybe "" (\h -> "Host: " <> Char8.pack h <> "\r\n") host
    <> extra
    <> "Connection: close\r\n\r\n"

-- | Send the bytes, read the answer up to EOF (10 s at most).
rawHttp :: Int -> ByteString -> IO HttpResponse
rawHttp port req = do
  result <- timeout 10000000 $
    bracket open Socket.close $ \s -> do
      Socket.BS.sendAll s req
      readAll s mempty
  case result of
    Nothing -> assertFailure "no HTTP answer within 10 s"
    Just bytes -> case parseResponse bytes of
      Just r -> pure r
      Nothing -> assertFailure ("bad HTTP answer: " <> show bytes)
  where
    open = do
      s <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.connect s addr
      pure s
    addr =
      Socket.SockAddrInet
        (fromIntegral port)
        (Socket.tupleToHostAddress (127, 0, 0, 1))
    readAll s acc = do
      chunk <- Socket.BS.recv s 65536
      if BS.null chunk then pure acc else readAll s (acc <> chunk)

-- | Send the bytes, read until the answer holds the text (5 s at most).
-- For answers that don't end, like the event stream.
rawHttpUntil :: Int -> ByteString -> ByteString -> IO ByteString
rawHttpUntil port req needle = do
  result <- timeout 5000000 $
    bracket open Socket.close $ \s -> do
      Socket.BS.sendAll s req
      readUntil s mempty
  case result of
    Nothing -> assertFailure ("not seen within 5 s: " <> show needle)
    Just bytes -> pure bytes
  where
    open = do
      s <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.connect s addr
      pure s
    addr =
      Socket.SockAddrInet
        (fromIntegral port)
        (Socket.tupleToHostAddress (127, 0, 0, 1))
    readUntil s acc
      | needle `BS.isInfixOf` acc = pure acc
      | otherwise = do
          chunk <- Socket.BS.recv s 65536
          if BS.null chunk
            then assertFailure ("closed before " <> show needle <> ": " <> show acc)
            else readUntil s (acc <> chunk)

-- | Status line, headers and body (chunked or not).
parseResponse :: ByteString -> Maybe HttpResponse
parseResponse bytes = do
  let (head', rest) = BS.breakSubstring "\r\n\r\n" bytes
  statusLine : headerLines <- Just (splitCrlf head')
  _ : code : _ <- Just (Char8.words statusLine)
  (status, _) <- Char8.readInt code
  let headers = map header headerLines
      raw = BS.drop 4 rest
  body <- case lookup "transfer-encoding" headers of
    Just "chunked" -> dechunk raw
    _ -> Just raw
  pure HttpResponse {status = status, headers = headers, body = body}
  where
    dechunk s = do
      let (sizeLine, afterSize) = BS.breakSubstring "\r\n" s
      size <- readHex (Char8.unpack sizeLine)
      let chunk = BS.take size (BS.drop 2 afterSize)
      if size == 0
        then Just ""
        else (chunk <>) <$> dechunk (BS.drop (size + 4) afterSize)
    readHex str = case Numeric.readHex str of
      [(n, "")] -> Just n
      _ -> Nothing
    header l =
      let (name, value) = Char8.break (== ':') l
       in (map toLower (Char8.unpack name), Char8.dropWhile (== ' ') (BS.drop 1 value))
    splitCrlf s
      | BS.null s = []
      | otherwise =
          let (l, r) = BS.breakSubstring "\r\n" s
           in l : splitCrlf (BS.drop 2 r)

assertSecurityHeaders :: HttpResponse -> Assertion
assertSecurityHeaders r = do
  lookup "content-security-policy" r.headers
    @?= Just
      "default-src 'self'; img-src 'self' data:; connect-src 'self'; \
      \script-src 'self'; object-src 'none'; base-uri 'none'; \
      \form-action 'none'; frame-ancestors 'none'"
  lookup "referrer-policy" r.headers @?= Just "no-referrer"
  lookup "x-content-type-options" r.headers @?= Just "nosniff"
  lookup "cache-control" r.headers @?= Just "no-store"

-- * Helpers

withTemp :: (FilePath -> IO a) -> IO a
withTemp = withSystemTempDirectory "haskell-reviewer-test"

runExe :: [String] -> IO (ExitCode, String)
runExe = runExeWith id

-- | Run the binary, check that stdout is empty, give back the exit code
-- and stderr. Fails (and kills it) after 10 s.
runExeWith :: (CreateProcess -> CreateProcess) -> [String] -> IO (ExitCode, String)
runExeWith tweak args = do
  exe <- findExe
  let cp = (tweak (proc exe args)) {close_fds = True}
  result <- timeout 10000000 (readCreateProcessWithExitCode cp "")
  case result of
    Nothing -> assertFailure ("no exit within 10 s: " <> show args)
    Just (code, out, err) -> do
      out @?= ""
      pure (code, err)

-- | @HASKELL_REVIEWER_EXE@ if set, else the binary on PATH (cabal puts it
-- there through build-tool-depends).
findExe :: IO FilePath
findExe = do
  override <- lookupEnv "HASKELL_REVIEWER_EXE"
  case override of
    Just exe -> pure exe
    Nothing -> do
      found <- Dir.findExecutable "haskell-reviewer"
      case found of
        Just exe -> pure exe
        Nothing ->
          assertFailure
            "haskell-reviewer not found: run through cabal test, \
            \or set HASKELL_REVIEWER_EXE"

assertContains :: String -> String -> Assertion
assertContains needle haystack =
  assertBool
    (show needle <> " not found in " <> show haystack)
    (needle `isInfixOf` haystack)
