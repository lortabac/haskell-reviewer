-- | The benchmark harness (SPEC §10, §11.9; D-76, D-215, D-303, D-357).
-- Runs the product binary against the pinned reference projects (D-82),
-- in browse mode and in diff mode against each project's base commit,
-- and reports the SPEC §10.1 targets, memory, GC pauses and the
-- fallback census.
--
-- Exit codes: 0 when every run worked, even if a target was missed
-- (§10.3: being slow never fails this); 1 for a broken run (a refs
-- query that 404s at the pinned sha is one) or a fallback count that
-- is wrong for a reason other than time; 2 for a usage error.
module Main (main) where

import Bench.Fallbacks
  ( Category (..),
    Diff (..),
    Expected (..),
    Hit (..),
    Lookup (..),
    Strictness (..),
    compareCensus,
    expectedTextOnly,
    failsBench,
    fallbackVerdict,
    lookupExpected,
    parseFallbacksFile,
    renderFallbacksError,
  )
import Bench.Http (Gzip (..), HttpResponse (..))
import Bench.IndexLog (IndexDone (..))
import Bench.Machine (LoadAvg (..), getLoadAvg, getMachineInfo)
import Bench.Mode
  ( Mode (..),
    ModeRuns (..),
    allModes,
    diffSpec,
    modeNames,
    modeRuns,
    modeSides,
    modeText,
    onSide,
    parseModes,
    projectModes,
    sideText,
  )
import Bench.ModuleList
  ( Queued (..),
    SideModules (..),
    Sized (..),
    modeQueueOrder,
    parseModuleList,
    pickDiffSamples,
    pickSamples,
    queueOrder,
  )
import Bench.Path (shortPath)
import Bench.Projects
  ( Project (..),
    Role (..),
    parseProjectsFile,
    renderProjectsError,
    selectProjects,
  )
import Bench.Queries (refsQueries)
import Bench.Refs (refsWarnings)
import Bench.RefsDuring (runRefsDuring)
import Bench.Report
  ( FallbackCheck (..),
    ProjectReport (..),
    Report (..),
    Settings (..),
    SideCount (..),
    ToolInfo (..),
    renderTable,
  )
import Bench.Report.Json (encodeReportJson)
import Bench.RtsStats (checkUserRtsOpts)
import Bench.Run
  ( RunError,
    SessionConfig (..),
    StatsFile (..),
    Timed (..),
    awaitDone,
    failWith,
    getApi,
    renderRunError,
    withSession,
  )
import Bench.SearchDuring (runSearchDuring)
import Bench.Session (checkSession, parseSession)
import Bench.Startup (StartupRun (..), runStartup)
import Bench.Steady
  ( CensusPass (..),
    SteadyPlan (..),
    SteadyRun (..),
    TextTime (..),
    TimedPass (..),
    runSteady,
  )
import Bench.Url (modulesPath, sessionPath)
import Control.Exception (Exception, IOException, throwIO, try)
import Control.Monad (forM, mfilter, unless, when)
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Time.Clock (getCurrentTime)
import Options.Applicative qualified as Opt
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    getCurrentDirectory,
    getFileSize,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (proc, readCreateProcessWithExitCode)
import Text.Printf (printf)

main :: IO ()
main = do
  opts <- Opt.execParser parserInfo
  listText <- Text.IO.readFile opts.listFile
  fallbacksText <- Text.IO.readFile opts.fallbacksFile
  case parseProjectsFile listText of
    Left e -> usageError (renderProjectsError e)
    Right allProjects -> case selectProjects opts.names allProjects of
      Left e -> usageError (renderProjectsError e)
      Right picked -> case parseFallbacksFile fallbacksText of
        Left e -> usageError (renderFallbacksError opts.fallbacksFile e)
        Right rows -> case checkUserRtsOpts opts.rts of
          Left e -> usageError (Text.pack e)
          Right () -> runBench opts rows picked
  where
    usageError e = do
      hPutStrLn stderr (Text.unpack e)
      exitWith (ExitFailure 2)

-- * CLI

data Options = Options
  { exe :: FilePath,
    modes :: [Mode],
    listFile :: FilePath,
    fallbacksFile :: FilePath,
    runs :: Int,
    warmup :: Int,
    steadyRuns :: Int,
    searchRuns :: Int,
    refsRuns :: Int,
    navIdle :: Int,
    rts :: [String],
    censusOnly :: Bool,
    jsonOut :: Maybe FilePath,
    reportOut :: Maybe FilePath,
    label :: Maybe Text,
    names :: [Text]
  }

parserInfo :: Opt.ParserInfo Options
parserInfo =
  Opt.info
    (optionsParser Opt.<**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.header "haskell-reviewer-bench - SPEC §10 benchmark harness"
        <> Opt.failureCode 2
    )

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> Opt.strOption
      ( Opt.long "exe"
          <> Opt.metavar "PATH"
          <> Opt.help "The haskell-reviewer binary to benchmark"
      )
    <*> Opt.option
      (Opt.eitherReader parseModes)
      ( Opt.long "modes"
          <> Opt.metavar "LIST"
          <> Opt.value allModes
          <> Opt.showDefaultWith (const (Text.unpack modeNames))
          <> Opt.help
            "Modes, comma separated: browse, range (--diff BASE..HEAD), \
            \worktree (--diff BASE). The diff modes need a base in the project list"
      )
    <*> Opt.strOption
      ( Opt.long "list"
          <> Opt.metavar "FILE"
          <> Opt.value "bench/projects.txt"
          <> Opt.showDefault
          <> Opt.help "Reference project list"
      )
    <*> Opt.strOption
      ( Opt.long "fallbacks"
          <> Opt.metavar "FILE"
          <> Opt.value "bench/fallbacks.txt"
          <> Opt.showDefault
          <> Opt.help "Expected fallback counts per project and sha"
      )
    <*> count "runs" 10 "Timed start-up runs per project"
    <*> count "warmup" 1 "Warm-up start-up runs per project (discarded)"
    <*> count "steady-runs" 3 "Runs after indexing: pages, search, refs, GC (+ census)"
    <*> count "search-runs" 3 "Runs searching while indexing"
    <*> count "refs-runs" 3 "Runs finding references while indexing"
    <*> count "nav-idle" 50 "Pages with 0.2-1 s gaps, per steady run"
    <*> ( maybe [] words
            <$> Opt.optional
              ( Opt.strOption $
                  Opt.long "rts"
                    <> Opt.metavar "OPTS"
                    <> Opt.help "Extra +RTS options for the product, e.g. \"--nonmoving-gc\""
              )
        )
    <*> Opt.switch
      ( Opt.long "census-only"
          <> Opt.help "Only the fallback census (one run per project), no timings"
      )
    <*> Opt.optional
      ( Opt.strOption $
          Opt.long "json"
            <> Opt.metavar "FILE"
            <> Opt.help "Also write the report as JSON"
      )
    <*> Opt.optional
      ( Opt.strOption $
          Opt.long "report"
            <> Opt.metavar "FILE"
            <> Opt.help "Also write the text report (no progress lines)"
      )
    <*> Opt.optional
      ( Text.pack
          <$> Opt.strOption
            ( Opt.long "label"
                <> Opt.metavar "TEXT"
                <> Opt.help "A short label, in the JSON report"
            )
      )
    <*> Opt.many
      ( Text.pack
          <$> Opt.strArgument
            ( Opt.metavar "NAME..."
                <> Opt.help "Project names (default: small, typical, large)"
            )
      )
  where
    count name def help =
      Opt.option
        (Opt.eitherReader (readCount ("--" <> name)))
        (Opt.long name <> Opt.metavar "N" <> Opt.value def <> Opt.showDefault <> Opt.help help)

readCount :: String -> String -> Either String Int
readCount flag s = case reads s of
  [(n, "")] | n >= 0 -> Right n
  _ -> Left ("invalid " <> flag <> ": " <> s)

-- * Running

runBench :: Options -> [Expected] -> [Project] -> IO ()
runBench opts rows projects = do
  -- Only diff modes asked, for projects with no base: nothing to run.
  when (null pairs) $ do
    mapM_ (hPutStrLn stderr . ("haskell-reviewer-bench: " <>) . Text.unpack) notes
    hPutStrLn stderr "haskell-reviewer-bench: nothing to run"
    exitWith (ExitFailure 2)
  started <- getCurrentTime
  tool <- getToolInfo opts.exe
  machine <- getMachineInfo
  load <- getLoadAvg
  withSystemTempDirectory "haskell-reviewer-bench" $ \tmp -> do
    outcomes <- forM pairs $ \(p, m) -> (p,m,) <$> runProject opts rows tmp p m
    let succeeded = [pr | (_, _, Just pr) <- outcomes]
        failedNames = [runLabel p m | (p, m, Nothing) <- outcomes]
        badFallbacks =
          [ runLabel pr.project pr.mode
          | pr <- succeeded,
            Just fc <- [pr.fallbacks],
            failsBench fc.strictness fc.verdict
          ]
        report =
          Report
            { started = started,
              label = opts.label,
              settings =
                Settings
                  { modes = asked,
                    runs = opts.runs,
                    warmup = opts.warmup,
                    steadyRuns = opts.steadyRuns,
                    searchRuns = opts.searchRuns,
                    refsRuns = opts.refsRuns,
                    navIdle = opts.navIdle,
                    rtsOpts = opts.rts,
                    censusOnly = opts.censusOnly,
                    fallbacksFile = opts.fallbacksFile
                  },
              tool = tool,
              machine = machine,
              loadAtStart = load,
              projects = succeeded,
              notes = notes
            }
    mapM_ (hPutStrLn stderr . ("haskell-reviewer-bench: " <>) . Text.unpack) notes
    Text.IO.putStr (renderTable report)
    maybe (pure ()) (`LBS.writeFile` encodeReportJson report) opts.jsonOut
    maybe (pure ()) (`Text.IO.writeFile` renderTable report) opts.reportOut
    unless (null failedNames) $
      hPutStrLn stderr $
        "haskell-reviewer-bench: failed: " <> show (map Text.unpack failedNames)
    unless (null badFallbacks) $
      hPutStrLn stderr $
        "haskell-reviewer-bench: fallback counts wrong or not checkable against "
          <> opts.fallbacksFile
          <> ": "
          <> show (map Text.unpack badFallbacks)
    exitWith (if null failedNames && null badFallbacks then ExitSuccess else ExitFailure 1)
  where
    -- The census is a browse thing (D-357).
    asked = if opts.censusOnly then [Browse] else opts.modes
    pairs = [(p, m) | p <- projects, m <- projectModes asked p]
    notes =
      [ p.name <> ": no base commit, so no " <> Text.intercalate ", " (map modeText s)
      | p <- projects,
        let s = filter (`notElem` projectModes asked p) asked,
        not (null s)
      ]

-- | "pandoc", "pandoc (range)": for messages.
runLabel :: Project -> Mode -> Text
runLabel p Browse = p.name
runLabel p m = p.name <> " (" <> modeText m <> ")"

-- | One project in one mode: check its pin (and base), read its module
-- trees, then the runs. 'Nothing' on any failure (already reported on
-- stderr).
runProject ::
  Options -> [Expected] -> FilePath -> Project -> Mode -> IO (Maybe ProjectReport)
runProject opts rows tmp project mode = do
  result <- try @BenchFailure go
  case result of
    Right pr -> pure (Just pr)
    Left (BenchFailure err) -> do
      hPutStrLn stderr (prefix <> err)
      pure Nothing
  where
    prefix = "haskell-reviewer-bench: " <> Text.unpack (runLabel project mode) <> ": "
    progress msg = hPutStrLn stderr (prefix <> msg)
    orFail = either (throwBench . renderRunError) pure
    spec = diffSpec mode project
    rules = modeRuns mode
    sides = modeSides mode
    firstSide = NE.head sides
    go = do
      dir <- projectDir opts project >>= either throwBench pure
      checkPin dir project >>= either throwBench pure
      when rules.needsBase $
        mapM_ (\b -> checkBase dir b >>= either throwBench pure) project.base
      when rules.needsClean $ checkClean dir >>= either throwBench pure
      let cfg tag stats =
            SessionConfig
              { exe = opts.exe,
                projectDir = dir,
                tmpDir = tmp,
                tag = Text.unpack (project.name <> "-" <> modeText mode) <> "-" <> tag,
                rtsOpts = opts.rts,
                stats = stats,
                diff = Text.unpack <$> spec
              }
      progress "reading the module list"
      trees <- listModules (cfg "list" TStats) mode project >>= orFail
      let queue = modeQueueOrder trees
          headQueue = [q | q <- queue, q.side == firstSide]
      sizes <- forM headQueue $ \q -> do
        size <- fileSize (dir </> q.path)
        -- Unreadable: it then can't be the largest.
        pure Sized {side = q.side, ref = q.ref, bytes = fromMaybe 0 size}
      let samples = case mode of
            Browse -> pickSamples sizes
            _ -> pickDiffSamples sizes queue
          sampleFor i = case samples of
            [] -> Nothing
            _ -> Just (samples !! (i `mod` length samples))
          expected = lookupExpected project.name project.sha rows
          refs = refsQueries project.name
          steadyRuns = maybe id min rules.maxSteadyRuns opts.steadyRuns
          duringRuns n = if rules.duringRuns then n else 0
          startup tag i =
            runStartup (cfg tag TStats) mode (sampleFor (i - 1)) >>= orFail
          loop :: String -> Int -> (Int -> IO a) -> IO [a]
          loop what n f =
            forM [1 .. n] $ \i -> do
              progress (what <> " " <> show i <> "/" <> show n)
              f i
      (startups, steadies, during, refsDuring) <-
        if opts.censusOnly
          then do
            -- One round of refs: a stale list shows up at once.
            let plan =
                  SteadyPlan
                    { census = CensusPass,
                      timed = NoTimedPass,
                      idlePages = 0,
                      searchRounds = 0,
                      refs = refs,
                      refsRounds = 1,
                      textRounds = 0,
                      seed = 1
                    }
            s <- loop "census run" 1 $ \_ ->
              runSteady (cfg "census" TStats) trees plan >>= orFail
            pure ([], s, [], [])
          else do
            _ <- loop "warm-up run" opts.warmup $ \i ->
              startup ("warmup-" <> show i) i
            st <- loop "start-up run" opts.runs $ \i ->
              startup ("startup-" <> show i) i
            sd <- loop "steady run" steadyRuns $ \i ->
              let plan =
                    SteadyPlan
                      { census =
                          if i == 1 && rules.census then CensusPass else NoCensusPass,
                        timed = TimedPass,
                        idlePages = opts.navIdle,
                        searchRounds = 5,
                        refs = refs,
                        refsRounds = 5,
                        textRounds = 5,
                        seed = i
                      }
               in runSteady (cfg ("steady-" <> show i) GcStats) trees plan >>= orFail
            -- While indexing: the first side only (head in diff mode).
            sr <- loop "search-during run" (duringRuns opts.searchRuns) $ \i ->
              runSearchDuring (cfg ("search-" <> show i) TStats) firstSide >>= orFail
            rd <- case NE.nonEmpty refs of
              Nothing -> pure []
              Just qs -> loop "refs-during run" (duringRuns opts.refsRuns) $ \i ->
                runRefsDuring (cfg ("refs-" <> show i) TStats) firstSide qs >>= orFail
            pure (st, sd, sr, rd)
      let census = case steadies of
            s : _ -> s.census
            [] -> Nothing
          diffs = case (census, expected) of
            (Just c, Found e) -> compareCensus e.counts c
            _ -> []
          strictness
            | opts.censusOnly || project.role == Extra = RowOptional
            | otherwise = RowRequired
          counts =
            NE.map
              ( \t ->
                  SideCount
                    { side = t.side,
                      listed = length t.items,
                      parsed = length (queueOrder t.side t.items)
                    }
              )
              trees
          parsed = sum (NE.map (.parsed) counts)
          runKind = if opts.censusOnly then CensusRunKind else SteadyRunKind
          -- The text-only count is checked against the census row,
          -- which is for browse mode.
          expectedForDone = if rules.census then expected else NoRow
      pure
        ProjectReport
          { project = project,
            mode = mode,
            diffSpec = spec,
            sides = counts,
            modulesListed = sum (NE.map (.listed) counts),
            modulesParsed = parsed,
            samples = samples,
            startup = startups,
            steady = if opts.censusOnly then [] else steadies,
            searchDuring = during,
            refsQueries = refs,
            refsDuring = refsDuring,
            fallbacks =
              if rules.census
                then
                  Just
                    FallbackCheck
                      { got = census,
                        expected = expected,
                        diffs = diffs,
                        verdict = fallbackVerdict census expected diffs,
                        strictness = strictness
                      }
                else Nothing,
            warnings =
              projectWarnings
                expectedForDone
                parsed
                ( [(StartupRunKind, r.done, r.loadAvg) | r <- startups]
                    <> [(runKind, r.done, r.loadAvg) | r <- steadies]
                )
                diffs
                <> refsWarnings (concatMap (.refsTimes) steadies)
                <> textWarnings steadies
          }

-- | Text searches that hit the deadline (stopped): once per query.
textWarnings :: [SteadyRun] -> [Text]
textWarnings runs =
  [ "text search \"" <> q <> "\"" <> onSide s <> ": stopped at the deadline"
  | (s, q) <- nub [(t.side, t.query) | t <- ts, t.stopped]
  ]
  where
    ts = concat [r.textFirst <> r.textTimes <> r.textPages | r <- runs]

-- | Which kind of run a done line came from, for the warnings.
data RunKind = StartupRunKind | SteadyRunKind | CensusRunKind

runKindText :: RunKind -> Text
runKindText StartupRunKind = "start-up"
runKindText SteadyRunKind = "steady"
runKindText CensusRunKind = "census"

-- | What to point out below a project's numbers: done lines that don't
-- fit the module tree or the expected text-only count, and modules
-- too slow to parse.
projectWarnings ::
  Lookup -> Int -> [(RunKind, IndexDone, Maybe LoadAvg)] -> [Diff] -> [Text]
projectWarnings expected parsed doneLines diffs =
  concat (zipWith doneWarnings (numbered doneLines) doneLines) <> tooSlow
  where
    -- Runs are numbered per kind.
    numbered = go []
      where
        go _ [] = []
        go seen ((k, _, _) : rest) =
          let n = 1 + length [() | k' <- seen, sameKind k k']
           in n : go (k : seen) rest
    sameKind a b = runKindText a == runKindText b
    doneWarnings :: Int -> (RunKind, IndexDone, Maybe LoadAvg) -> [Text]
    doneWarnings i (kind, d, load) =
      [ runName
          <> ": done line says "
          <> showT d.modules
          <> " modules, the tree lists "
          <> showT parsed
          <> " to parse"
      | d.modules /= parsed
      ]
        <> case expected of
          Found e
            | d.textOnly /= expectedTextOnly e.counts ->
                [ runName
                    <> ": "
                    <> showT d.textOnly
                    <> " text only, expected "
                    <> showT (expectedTextOnly e.counts)
                    <> " (load average "
                    <> loadText load
                    <> (if d.textOnly > expectedTextOnly e.counts then "; slow?" else "")
                    <> ")"
                ]
          _ -> []
      where
        runName = runKindText kind <> " run " <> showT i
    tooSlow =
      [ "too-slow: "
          <> showT d.got
          <> " modules ("
          <> Text.intercalate ", " (map (.ref) (take 10 d.hits))
          <> "), expected "
          <> showT d.expected
          <> ": rerun on an idle machine; if it stays, raise the limit or \
             \measure CPU time (§10.3)"
      | d <- diffs,
        d.category == TooSlow,
        d.got > d.expected
      ]
    loadText Nothing = "unknown"
    loadText (Just l) = Text.pack (printf "%.2f" l.one)
    showT :: (Show a) => a -> Text
    showT = Text.pack . show

-- | Read the module trees: spawn, a few requests, stop. Checks
-- @api\/session@ first: the mode and commits we asked for (D-357).
-- Waits for the done line first, so the stop reaches an idle server: on
-- macOS a busy one can miss it (Note [Second SIGTERM] in "Bench.Run").
listModules ::
  SessionConfig -> Mode -> Project -> IO (Either RunError (NonEmpty SideModules))
listModules cfg mode project = fmap fst <$> withSession cfg steps
  where
    steps s = do
      sess <- getApi s Gzip sessionPath
      unless (sess.response.status == 200) $
        failWith ("session: expected 200, got " <> show sess.response.status)
      answer <-
        either (failWith . ("session: " <>) . show) pure (parseSession sess.response.body)
      either (failWith . Text.unpack) pure (checkSession mode project answer)
      trees <- forM (modeSides mode) $ \side -> do
        let what = "modules " <> Text.unpack (sideText side)
        t <- getApi s Gzip (modulesPath side)
        unless (t.response.status == 200) $
          failWith (what <> ": expected 200, got " <> show t.response.status)
        items <-
          either
            (failWith . ((what <> ": ") <>) . show)
            pure
            (parseModuleList t.response.body)
        pure SideModules {side = side, items = items}
      _ <- awaitDone s
      pure trees

-- | 'Nothing' if we can't read it.
fileSize :: FilePath -> IO (Maybe Integer)
fileSize p = either (const Nothing) Just <$> try @IOException (getFileSize p)

-- | Ends one project's runs, caught in 'runProject'. An exception, not
-- an 'Either': a failure can come from deep inside the run loops, and
-- the only thing to do with it is to skip to the next project.
newtype BenchFailure = BenchFailure String
  deriving (Show)

instance Exception BenchFailure

throwBench :: String -> IO a
throwBench = throwIO . BenchFailure

-- | Where a project is expected to be checked out (D-82).
projectDir :: Options -> Project -> IO (Either String FilePath)
projectDir opts project = do
  override <- lookupEnv "REFERENCE_PROJECTS_DIR"
  let dir = case override of
        Just base -> base </> Text.unpack project.name
        Nothing -> takeDirectory opts.listFile </> "projects" </> Text.unpack project.name
  exists <- doesDirectoryExist dir
  pure $
    if exists
      then Right dir
      else Left (dir <> ": not found; run bench/fetch-projects.sh")

-- | Is the project checked out at its pinned commit?
checkPin :: FilePath -> Project -> IO (Either String ())
checkPin dir project = do
  (code, out, _) <-
    readCreateProcessWithExitCode (proc "git" ["-C", dir, "rev-parse", "HEAD"]) ""
  let headSha = Text.strip (Text.pack out)
  pure $ case code of
    ExitSuccess
      | headSha == project.sha -> Right ()
      | otherwise ->
          Left
            ( dir
                <> ": at "
                <> Text.unpack headSha
                <> ", expected "
                <> Text.unpack project.sha
                <> "; run bench/fetch-projects.sh"
            )
    ExitFailure _ -> Left (dir <> ": git rev-parse failed; run bench/fetch-projects.sh")

-- | Is the base commit there (D-357)? A depth-1 fetch has it only if
-- fetch-projects.sh fetched it.
checkBase :: FilePath -> Text -> IO (Either String ())
checkBase dir sha = do
  let base = Text.unpack sha
  (code, _, _) <-
    readCreateProcessWithExitCode
      (proc "git" ["-C", dir, "cat-file", "-e", base <> "^{commit}"])
      ""
  pure $ case code of
    ExitSuccess -> Right ()
    ExitFailure _ ->
      Left (dir <> ": no base commit " <> base <> "; run bench/fetch-projects.sh")

-- | Worktree mode compares the working tree: it must be the clean
-- checkout, else we'd measure local edits.
checkClean :: FilePath -> IO (Either String ())
checkClean dir = do
  (code, out, _) <-
    readCreateProcessWithExitCode (proc "git" ["-C", dir, "status", "--porcelain"]) ""
  pure $ case code of
    ExitSuccess
      | null out -> Right ()
      | otherwise -> Left (dir <> ": not clean; run bench/fetch-projects.sh")
    ExitFailure _ -> Left (dir <> ": git status failed; run bench/fetch-projects.sh")

-- | What the product binary says about itself: its version (printed on
-- stderr, by design: see "HaskellReviewer.App.Run"), and its RTS build
-- info (@+RTS --info@, always allowed, even without @-rtsopts@).
getToolInfo :: FilePath -> IO ToolInfo
getToolInfo exePath = do
  (_, verOut, verErr) <- readCreateProcessWithExitCode (proc exePath ["--version"]) ""
  (_, infoOut, _) <-
    readCreateProcessWithExitCode (proc exePath ["+RTS", "--info", "-RTS"]) ""
  let rtsInfo = readRtsPairs infoOut
  gitSha <- tryGit ["rev-parse", "HEAD"]
  dirtyOut <- tryGit ["status", "--porcelain"]
  -- Resolve symlinks like getCurrentDirectory does, else the match fails.
  cwd <- getCurrentDirectory
  home <- traverse canonicalizePath . mfilter (not . null) =<< lookupEnv "HOME"
  exeFull <- canonicalizePath exePath
  pure
    ToolInfo
      { -- Short, so a committed report has no home directory in it.
        exe = shortPath cwd home exeFull,
        version = Text.pack (firstLine (verOut <> verErr)),
        gitSha = Text.pack <$> gitSha,
        gitDirty = not . null <$> dirtyOut,
        ghc = Text.pack <$> lookup "GHC version" rtsInfo,
        rtsWay = Text.pack <$> lookup "RTS way" rtsInfo
      }
  where
    firstLine s = case lines s of
      (l : _) -> l
      [] -> "unknown"
    tryGit args = do
      (code, out, _) <- readCreateProcessWithExitCode (proc "git" args) ""
      pure $ case code of
        ExitSuccess -> Just (trimEnd out)
        ExitFailure _ -> Nothing
    trimEnd = reverse . dropWhile (== '\n') . reverse

-- | The RTS's own @[(String, String)]@ dump (same shape as the @-t@
-- file, see "Bench.RtsStats", but without the echoed-command line in
-- front, since @--info@ never runs our own @main@).
readRtsPairs :: String -> [(String, String)]
readRtsPairs raw = case break (== '[') raw of
  (_, rest@('[' : _)) -> case reads rest of
    [(xs, _)] -> xs
    _ -> []
  _ -> []
