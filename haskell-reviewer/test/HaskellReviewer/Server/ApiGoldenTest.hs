-- | Writes the API fixtures (D-50). See Note [API fixtures] in
-- "HaskellReviewer.Server.Wire".
--
-- Tests run from the package dir, so fixtures/api is one level up.
module HaskellReviewer.Server.ApiGoldenTest (tests) where

import ApiSupport
  ( ApiResult (..),
    DiffFile (..),
    RequestBody (..),
    TestApi (..),
    diffReady,
    diffRouteApi,
    fixtureApi,
    gatedOn,
    getApi,
    getApiQuery,
    goodOrigin,
    implicitApi,
    indexed,
    pageApi,
    projectApi,
    refsApi,
    refsGatedOn,
    searchApi,
    sendApi,
    sourceApi,
    textSearchApi,
  )
import Control.Concurrent.MVar (newEmptyMVar)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.CommitFiles (OutsideChanges (..))
import HaskellReviewer.Core.Config
  ( DiffLayout (..),
    SidebarView (..),
    Theme (..),
    UiDefaults (..),
  )
import HaskellReviewer.Core.Eol (Filtering (..))
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileNotes (..),
    FileSize (..),
    LinkInfo (..),
    TooLargeInfo (..),
    textLimit,
  )
import HaskellReviewer.Core.FileList (CommitId (..), EntryKind (..))
import HaskellReviewer.Core.Git (ObjectId (..))
import HaskellReviewer.Core.Markers (Marker (..), ModuleChange (..), SettingsChange (..))
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyReason (..),
    textOnlyModule,
  )
import HaskellReviewer.Core.Modules (ModuleIndex (..))
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments)
import HaskellReviewer.Core.Renames
  ( Change (..),
    ChangeNote (..),
    ExecChange (..),
    Limit (..),
    ModifiedFile (..),
    Pairing (..),
    RenameStep (..),
    RenamedFile (..),
  )
import HaskellReviewer.Core.ResolveWarmUp (resolveWarmUpIndex)
import HaskellReviewer.Core.Revspec
  ( BaseFrom (..),
    CommitsInfo (..),
    DiffInfo (..),
    Range (..),
    RangeForm (..),
    WorkTreeInfo (..),
  )
import HaskellReviewer.Core.Sides (Pair (..), Side (..), Sides (..))
import HaskellReviewer.Core.Similarity (Score (..), maxScore)
import HaskellReviewer.Core.Xdg (XdgProblem (..))
import HaskellReviewer.Effects.FileSystem (ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Server.CborJson (cborToJson, renderCborJsonError)
import HaskellReviewer.Server.Diff (GuardedDiff (..))
import HaskellReviewer.Server.Wire
  ( DiffEnd (..),
    DiffFiles (..),
    SessionInfo (..),
    encodeDiffFiles,
    encodeGeneration,
    encodeIndexing,
    encodeModuleTree,
    helloPayload,
  )
import HaskellReviewer.Server.Wire.Source (coveringView, encodeSource)
import HaskellReviewer.Session.Index (Progress (..))
import HaskellReviewer.Session.Prefs (newPrefsStore)
import HaskellReviewer.Session.Reload (ReloadOutcome (..))
import HaskellReviewer.Session.Snapshot (Generation (..), Snapshot (..))
import System.Directory qualified as Dir
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup "API fixtures" $
    testCase "every fixture file has a test, and the other way round" noStale
      : concatMap golden cases

-- | One fixture: its name and how it's written.
data Case = Case
  { name :: String,
    fixture :: Fixture
  }

-- | How a fixture is written.
data Fixture
  = -- | Compact JSON and a newline, to <name>.json.
    Json (IO LBS.ByteString)
  | -- | CBOR bytes to <name>.cbor, their JSON dump and a newline to
    -- <name>.cbor.json.
    Cbor (IO LBS.ByteString)

-- | The golden tests of one case: one per file it writes.
golden :: Case -> [TestTree]
golden c = case c.fixture of
  Json body -> [goldenVsString c.name (path ".json") body]
  Cbor body ->
    [ goldenVsString c.name (path ".cbor") body,
      goldenVsString (c.name <> " (dump)") (path ".cbor.json") $ do
        bytes <- body
        either
          (assertFailure . Text.unpack . renderCborJsonError)
          (pure . (<> "\n"))
          (cborToJson bytes)
    ]
  where
    path ext = fixtureDir <> c.name <> ext

cases :: [Case]
cases =
  [ json "events-hello" (pure (LBS.fromStrict helloPayload <> "\n")),
    -- A diff-mode event: head's progress.
    json
      "events-indexing"
      (pure (event (encodeIndexing (Generation 1) HeadSide halfway))),
    -- After a reload (D-351).
    json "events-generation" (pure (event (encodeGeneration (Generation 2)))),
    json "reload" okReload,
    -- Done, with a failed module (a loose file readEntry doesn't have).
    json "indexing" (okJson (indexed projectApi) ["api", "indexing", "tree"]),
    json "files" (okJson fixtureApi ["api", "files", "tree"]),
    cbor "file-text" (okCbor fixtureApi (file ["app", "Main.hs"])),
    cbor "file-binary" (okCbor fixtureApi (file ["assets", "logo.png"])),
    cbor "file-too-large" (okCbor fixtureApi (file ["data", "big.csv"])),
    cbor "file-symlink" (okCbor fixtureApi (file ["docs", "guide.md"])),
    cbor "file-submodule" (okCbor fixtureApi (file ["vendor", "dep"])),
    -- Read from disk, and git would filter it (LFS, say).
    cbor "file-filtered" (okCbor filteredApi (file ["app", "Main.hs"])),
    -- A blob missing from a partial clone (D-343).
    cbor "file-not-local" (okCbor (missingApi fixtureApi) (file ["app", "Main.hs"])),
    json "modules" (okJson projectApi ["api", "modules", "tree"]),
    json "modules-implicit" (okJson implicitApi ["api", "modules", "tree"]),
    -- Diff mode: every marker value, on every kind of module.
    json "modules-diff" modulesDiff,
    -- Commits: every status and note, outside ROOT. The working tree:
    -- no head commit, filtered, outside unknown. Over the limit.
    json "diff-files" (diffFiles commitsDiff),
    json "diff-files-worktree" (diffFiles workTreeDiff),
    json "diff-files-limit" (diffFiles limitDiff),
    -- One file's diff: every status, side kind, diff kind and note, the
    -- BOM and CRLF notes, ws=ignore.
    cbor "diff-modified" (okDiff diffFixtureApi "src/A.hs" ""),
    cbor "diff-renamed" (okDiff diffFixtureApi "new/R.hs" ""),
    cbor "diff-added" (okDiff diffFixtureApi "added.txt" ""),
    cbor "diff-removed" (okDiff diffFixtureApi "removed.txt" ""),
    cbor "diff-unchanged" (okDiff diffFixtureApi "same.txt" ""),
    cbor "diff-binary" (okDiff diffFixtureApi "logo.png" ""),
    cbor "diff-too-large" (okDiff diffFixtureApi "big.csv" ""),
    -- No pairing entry (as past the cut): same path, compared by kind.
    cbor "diff-submodule" (okDiff diffFixtureApi "vendor/dep" ""),
    cbor "diff-symlink" (okDiff diffFixtureApi "link" ""),
    cbor "diff-type-changed" (okDiff diffFixtureApi "tc" ""),
    cbor "diff-eol" (okDiff diffFixtureApi "eol.txt" ""),
    cbor "diff-ws-ignore" (okDiff diffFixtureApi "ws.hs" "?ws=ignore"),
    cbor "diff-too-slow" $
      okDiff diffFixtureApi {lineDiff = \_ _ -> pure DiffTimedOut} "src/A.hs" "",
    cbor "diff-not-local" (okDiff diffFixtureApi "remote.bin" ""),
    cbor "diff-filtered" (okDiff diffFixtureApi "data.bin" ""),
    -- One per reason a module is text only, a parsed one and one after
    -- CPP.
    cbor "module-parsed" (okCbor projectApi (modulePath "Foo")),
    cbor "module-parse-error" (okCbor projectApi (modulePath "pkg:exe:app:Main")),
    cbor "module-pragma-error" (okCbor projectApi (modulePath "Tests")),
    cbor "module-preprocessed" (okCbor projectApi (modulePath "pkg:test:spec:Main")),
    cbor "module-cpp" (okCbor projectApi (modulePath "pkg:bench:slow:Main")),
    cbor "module-crashed" (okCbor crashingApi (modulePath "Foo")),
    cbor "module-too-slow" (okCbor slowApi (modulePath "Foo")),
    cbor "module-generated" (okCbor projectApi (modulePath "Lexer")),
    cbor "module-no-source" (okCbor projectApi (modulePath "Gone")),
    cbor "module-not-text" (okCbor implicitApi (modulePath "Main")),
    cbor "module-too-large" (okCbor implicitApi (modulePath "Data.Deep.Mod")),
    -- The blob is missing from a partial clone (D-359).
    cbor "module-not-local" (okCbor (indexed (missingApi sourceApi)) (modulePath "C")),
    -- The page: every kind of declaration and doc markup; sections.
    cbor "module-page" (okCbor pageApi (modulePath "Page")),
    cbor "module-page-sections" (okCbor pageApi (modulePath "Sections")),
    -- Links out of the module, in code and docs; partial while an
    -- import isn't parsed.
    cbor "module-page-links" (okCbor (indexed sourceApi) (modulePath "C")),
    cbor "module-page-partial" $ do
      gate <- newEmptyMVar
      okCbor (gatedOn "s/src/A.hs" gate) {workers = 2} (modulePath "C"),
    json "module-not-found" (notFoundJson projectApi (modulePath "Nope")),
    json "prefs-defaults" (okJson fixtureApi ["api", "prefs"]),
    -- The mode and revisions: browse, two dots, three dots with two
    -- merge bases, against the working tree.
    json "session-browse" (okJson fixtureApi ["api", "session"]),
    json "session-diff" (sessionOf (commits TwoDot BaseTyped)),
    json "session-three-dot" (sessionOf (commits ThreeDot (MergeBase 2))),
    json "session-worktree" $
      sessionOf (WorkTreeDiff WorkTreeInfo {rev = "HEAD~1", base = hex 'c'}),
    -- The source view: linked, CPP, partial (an import that never gets
    -- parsed), text only, plain, not found, and the warm-up's covering
    -- view (every value).
    cbor "source-parsed" (okCbor (indexed sourceApi) (source ["s", "src", "B.hs"])),
    cbor "source-cpp" (okCbor (indexed sourceApi) (source ["s", "src", "Cpp.hs"])),
    cbor "source-partial" $ do
      gate <- newEmptyMVar
      okCbor (gatedOn "s/src/A.hs" gate) {workers = 2} (source ["s", "src", "B.hs"]),
    cbor "source-text-only" (okCbor (indexed sourceApi) (source ["s", "src", "Bad.hs"])),
    cbor "source-plain" (okCbor sourceApi {workers = 0} (source ["s", "notes.txt"])),
    cbor "source-markdown" (okCbor sourceApi (source ["s", "docs", "All.md"])),
    -- Markdown, rendered: every node; not text.
    cbor "markdown-page" (okCbor sourceApi (markdown ["s", "docs", "All.md"])),
    cbor "markdown-not-text" (okCbor sourceApi (markdown ["s", "blob.md"])),
    cbor "markdown-not-local" $
      okCbor (missingApi sourceApi) (markdown ["s", "docs", "All.md"]),
    cbor "markdown-filtered" (okCbor filteredSource (markdown ["s", "docs", "All.md"])),
    -- A module's blob is missing: the slot fails, the view says so.
    cbor "source-not-local" $
      okCbor (indexed (missingApi sourceApi)) (source ["s", "src", "B.hs"]),
    json "source-not-found" $
      notFoundJson sourceApi {workers = 0} (source ["s", "nope.hs"]),
    cbor "source-warm-up" $
      pure (encodeSource (Generation 1) resolveWarmUpIndex coveringView),
    -- Every kind of name, a private one and a field.
    json "search" (okSearch (indexed searchApi) "?q=map"),
    json "search-qualified" (okSearch (indexed searchApi) "?q=Data.Map.insert"),
    -- Nothing indexed yet: modules only.
    json "search-partial" (okSearch searchApi {workers = 0} "?q=map"),
    -- Find references: every kind, a guess; partial while a module
    -- isn't parsed; not found.
    json "refs" (okRefs (indexed refsApi) "?module=R.Shape&decl=val:area"),
    json "refs-partial" $ do
      gate <- newEmptyMVar
      okRefs
        (refsGatedOn "r/src/R/Use.hs" gate) {workers = 2}
        "?module=R.Shape&decl=con:Square",
    json "refs-not-local" $
      okRefs (indexed (missingApi refsApi)) "?module=R.Shape&decl=val:area",
    json "refs-not-found" $ do
      r <-
        getApiQuery
          (indexed refsApi)
          ["api", "refs", "tree"]
          "?module=R.Shape&decl=val:nope"
      r.status @?= 404
      lookup "Content-Type" r.headers @?= Just "application/json"
      pure (r.body <> "\n"),
    -- Text search: one file of each kind, a BOM, CRLF, a long line, 25
    -- hits on a line (20 sent), non-ASCII; and capped.
    json "text-search" (okTextSearch textSearchApi "?q=main"),
    json "text-search-capped" (okTextSearch textSearchApi "?q=main&limit=2"),
    -- Every value other than the built-in one.
    json "prefs" (okJson fixtureApi {prefs = configured} ["api", "prefs"])
  ]
  where
    json n body = Case {name = n, fixture = Json body}
    cbor n body = Case {name = n, fixture = Cbor body}
    file p = ["api", "file", "tree"] <> p
    modulePath ref = ["api", "module", "tree", ref]
    source p = ["api", "source", "tree"] <> p
    markdown p = ["api", "markdown", "tree"] <> p
    missingApi api = api {readEntry = \_ _ _ -> pure (Left (ReadMissing (hex 'e')))}
    filteredSource = sourceApi {fileNotes = \_ _ -> FileNotes {filtered = Filtered}}
    filteredApi = fixtureApi {fileNotes = \_ _ -> FileNotes {filtered = Filtered}}
    crashingApi = answering (ParserCrashed "boom")
    slowApi = answering TooSlow
    halfway = Progress {total = 358, ready = 120, failed = 2}
    event e = LBS.fromStrict e <> "\n"
    configured =
      newPrefsStore
        (newLogger LevelInfo (\_ -> pure ()))
        UiDefaults
          { theme = ThemeDark,
            exportedOnly = True,
            sidebar = SidebarModules,
            singleKeyShortcuts = False,
            changedOnly = True,
            diffLayout = LayoutSplit
          }
        (Left HomeUnset)
    answering r =
      projectApi
        { parse = \_ _ -> pure (TextOnly (textOnlyModule r []))
        }

-- | GET api/session in diff mode.
sessionOf :: DiffInfo -> IO LBS.ByteString
sessionOf info = okJson fixtureApi {session = DiffSession info} ["api", "session"]

-- | main and feature, as commits.
commits :: RangeForm -> BaseFrom -> DiffInfo
commits form from =
  CommitsDiff
    CommitsInfo
      { range = Range {form = form, base = "main", head = "feature"},
        sides = Pair {base = hex 'a', head = hex 'b'},
        baseFrom = from
      }

-- | GET api/diff-files with this body in the slot.
diffFiles :: DiffFiles -> IO LBS.ByteString
diffFiles d =
  okJson
    fixtureApi
      { diff = Just (Right (diffReady (encodeDiffFiles (Generation 1) d) [])),
        workers = 0
      }
    ["api", "diff-files"]

-- | Commits: every status and note but filtered, outside ROOT.
commitsDiff :: DiffFiles
commitsDiff =
  DiffFiles
    { base = DiffEnd {label = "main", commit = Just (hex 'a')},
      head = DiffEnd {label = "feature", commit = Just (hex 'b')},
      pairing =
        Pairing
          { changes =
              [ Added (relPath "copy/dup.txt"),
                Renamed (renamed "exact/a.txt" "exact/b.txt" maxScore ExactStep SameExec),
                modified "mode/x.sh" (Just ModeChanged),
                Renamed
                  (renamed "run/a.sh" "run/b.sh" (Score 45000) MatrixStep ExecChanged),
                Removed (relPath "sim40/old.txt"),
                Modified ModifiedFile {path = relPath "src/A.hs", note = Nothing},
                Modified ModifiedFile {path = relPath "tc/f", note = Just TypeChanged}
              ],
            limit = WithinLimits
          },
      filtered = Set.empty,
      outside =
        Just
          OutsideChanges
            { total = 3,
              paths = ["other/moved.hs", "other/new", "other/x"],
              truncated = False
            },
      truncated = False
    }

-- | The working tree: no head commit, filtered, outside not known.
workTreeDiff :: DiffFiles
workTreeDiff =
  DiffFiles
    { base = DiffEnd {label = "HEAD", commit = Just (hex 'a')},
      head = DiffEnd {label = "working tree", commit = Nothing},
      pairing =
        Pairing
          { changes =
              [ Modified ModifiedFile {path = relPath "data.bin", note = Nothing},
                Renamed (renamed "wt/a.txt" "wt/b.txt" maxScore ExactStep SameExec)
              ],
            limit = WithinLimits
          },
      filtered = Set.fromList [relPath "data.bin"],
      outside = Nothing,
      truncated = False
    }

-- | Over the rename limit, a list cut at the cap.
limitDiff :: DiffFiles
limitDiff =
  DiffFiles
    { base = DiffEnd {label = "v1", commit = Just (hex 'a')},
      head = DiffEnd {label = "v2", commit = Just (hex 'b')},
      pairing =
        Pairing
          { changes =
              [ Renamed
                  (renamed "d1/Same.hs" "d2/Same.hs" (Score 48000) BasenameStep SameExec),
                Added (relPath "lim/q1.txt"),
                Removed (relPath "lim/r1.txt")
              ],
            limit = OverRenameLimit
          },
      filtered = Set.empty,
      outside = Just OutsideChanges {total = 100001, paths = ["a"], truncated = True},
      truncated = True
    }

-- | A rename of these paths.
renamed :: Text -> Text -> Score -> RenameStep -> ExecChange -> RenamedFile
renamed a b score step exec =
  RenamedFile {from = relPath a, to = relPath b, score = score, step = step, exec = exec}

-- | A path, split at @/@.
relPath :: Text -> RelPath
relPath t =
  fromMaybe (error ("bad path: " <> show t)) (relPathFromSegments (Text.splitOn "/" t))

-- | A modified file with this note.
modified :: Text -> Maybe ChangeNote -> Change
modified p n = Modified ModifiedFile {path = relPath p, note = n}

-- | Two sides for api/diff (Note [Diff view]): a file per golden.
diffFixtureApi :: TestApi
diffFixtureApi =
  (diffRouteApi headFiles baseFiles changes)
    { fileNotes = \snap p -> case snap.side of
        HeadSide | p == relPath "data.bin" -> FileNotes {filtered = Filtered}
        _ -> FileNotes {filtered = Unfiltered}
    }
  where
    headFiles =
      [ text "src/A.hs" "module A where\n\na = 2\nb = 3\n",
        text "new/R.hs" "module R where\nr = 2\n",
        text "added.txt" "one\ntwo\n",
        text "same.txt" "same\n",
        file "logo.png" EntryFile (Right (Binary (FileSize 300))),
        file "big.csv" EntryFile (Right tooLarge),
        file "vendor/dep" EntryNestedRepo (Right NestedRepo),
        link "link" "../new",
        link "tc" "elsewhere",
        text "eol.txt" "\xFEFF\&a\nb\nc\n",
        text "ws.hs" "x = 1\n",
        file "remote.bin" EntryFile (Left (ReadMissing (hex 'e'))),
        text "data.bin" "big data\n"
      ]
    baseFiles =
      [ text "src/A.hs" "module A where\n\na = 1\n",
        text "old/R.hs" "module R where\nr = 1\n",
        text "removed.txt" "gone\n",
        text "same.txt" "same\n",
        file "logo.png" EntryFile (Right (Binary (FileSize 200))),
        text "big.csv" "a,b\n",
        file "vendor/dep" (EntrySubmodule sub) (Right (Submodule sub)),
        link "link" "../old",
        text "tc" "one\ntwo\n",
        text "eol.txt" "a\r\nb\r\n",
        text "ws.hs" "\xFEFFx  =  1\n",
        text "remote.bin" "r\n",
        text "data.bin" "version https://git-lfs.github.com/spec/v1\n"
      ]
    changes =
      [ Added (relPath "added.txt"),
        modified "big.csv" Nothing,
        modified "data.bin" Nothing,
        modified "eol.txt" Nothing,
        modified "link" Nothing,
        modified "logo.png" Nothing,
        Renamed (renamed "old/R.hs" "new/R.hs" (Score 36000) MatrixStep ExecChanged),
        modified "remote.bin" Nothing,
        Removed (relPath "removed.txt"),
        modified "src/A.hs" Nothing,
        modified "tc" (Just TypeChanged),
        modified "ws.hs" Nothing
      ]
    text p t = file p EntryFile (Right (TextContent t))
    file p k r = DiffFile {path = p, kind = k, answer = r}
    sub = CommitId (Text.replicate 40 "c")
    link p t =
      file p EntrySymlink (Right (Link LinkInfo {target = t, resolved = Nothing}))
    tooLarge = TooLarge TooLargeInfo {size = FileSize 2_000_000, limit = textLimit}

-- | GET api/diff/<path…> with this query: a 200 with CBOR.
okDiff :: TestApi -> Text -> ByteString -> IO LBS.ByteString
okDiff api p query = do
  r <- getApiQuery api (["api", "diff"] <> Text.splitOn "/" p) query
  r.status @?= 200
  lookup "Content-Type" r.headers @?= Just "application/cbor"
  pure r.body

-- | A made-up SHA-1 id.
hex :: Char -> ObjectId
hex c = ObjectId (Text.replicate 40 (Text.singleton c))

-- | The project fixture's tree, with markers: each value in turn.
modulesDiff :: IO LBS.ByteString
modulesDiff = do
  snap <- case projectApi.sides of
    BrowseSides get -> get >>= either (assertFailure . show) pure
    DiffSides _ -> assertFailure "a diff api"
  let changes =
        cycle
          [ Nothing,
            Just ModuleAdded,
            Just ModuleRemoved,
            Just ModuleModified,
            Just ModuleRenamed
          ]
      settings = cycle [SettingsSame, SettingsSame, SettingsChanged]
      markers =
        Map.fromList
          (zip (Set.toList snap.modules.ids) (zipWith Marker changes settings))
  pure (encodeModuleTree (Generation 1) markers snap.modules <> "\n")

fixtureDir :: FilePath
fixtureDir = "../fixtures/api/"

-- | A fixture no test writes would never be checked again.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort files @?= sort (concatMap fileNames cases)
  where
    fileNames c = case c.fixture of
      Json _ -> [c.name <> ".json"]
      Cbor _ -> [c.name <> ".cbor", c.name <> ".cbor.json"]

-- | GET through the real app; must be a 200 with JSON.
okJson :: TestApi -> [Text] -> IO LBS.ByteString
okJson api path = (<> "\n") <$> answer 200 "application/json" api path

-- | POST api/reload, answered with generation 2; must be a 200 with
-- JSON.
okReload :: IO LBS.ByteString
okReload = do
  let api = fixtureApi {reload = Just (pure (Reloaded (Generation 2))), workers = 0}
  r <-
    sendApi
      "POST"
      [goodOrigin, ("Content-Type", "application/json")]
      (Sized "{}")
      api
      ["api", "reload"]
  r.status @?= 200
  lookup "Content-Type" r.headers @?= Just "application/json"
  pure (r.body <> "\n")

-- | GET api/search/tree with this query string; must be a 200 with
-- JSON.
okSearch :: TestApi -> ByteString -> IO LBS.ByteString
okSearch api query = do
  r <- getApiQuery api ["api", "search", "tree"] query
  r.status @?= 200
  lookup "Content-Type" r.headers @?= Just "application/json"
  pure (r.body <> "\n")

-- | GET api/refs/tree with this query string; must be a 200 with JSON.
okRefs :: TestApi -> ByteString -> IO LBS.ByteString
okRefs api query = do
  r <- getApiQuery api ["api", "refs", "tree"] query
  r.status @?= 200
  lookup "Content-Type" r.headers @?= Just "application/json"
  pure (r.body <> "\n")

-- | GET api/text-search/tree with this query string; must be a 200
-- with JSON.
okTextSearch :: TestApi -> ByteString -> IO LBS.ByteString
okTextSearch api query = do
  r <- getApiQuery api ["api", "text-search", "tree"] query
  r.status @?= 200
  lookup "Content-Type" r.headers @?= Just "application/json"
  pure (r.body <> "\n")

-- | GET through the real app; must be a 200 with CBOR. The bytes as
-- they come.
okCbor :: TestApi -> [Text] -> IO LBS.ByteString
okCbor = answer 200 "application/cbor"

-- | GET through the real app; must be a 404 with JSON.
notFoundJson :: TestApi -> [Text] -> IO LBS.ByteString
notFoundJson api path = (<> "\n") <$> answer 404 "application/json" api path

-- | GET through the real app: check the status and the Content-Type,
-- give back the body.
answer :: Int -> ByteString -> TestApi -> [Text] -> IO LBS.ByteString
answer status contentType api path = do
  r <- getApi api path
  r.status @?= status
  lookup "Content-Type" r.headers @?= Just contentType
  pure r.body
