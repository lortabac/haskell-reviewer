-- | The benchmark harness (SPEC §10, §11.9; D-76). Runs the product
-- binary against the pinned reference projects (D-82) and reports the
-- SPEC §10 numbers and peak memory.
--
-- Being slow never fails this (SPEC §10.3): only a run that's actually
-- broken does (bad pin, a crash, a bad response, memory out of a sane
-- range).
module Main (main) where

import Bench.Machine (getMachineInfo)
import Bench.Projects
  ( Project (..),
    parseProjectsFile,
    renderProjectsError,
    selectProjects,
  )
import Bench.Report
  ( ProjectRun (..),
    Report (..),
    ToolInfo (..),
    encodeReportJson,
    renderTable,
  )
import Bench.Run (RunConfig (..), renderRunError, runOnce)
import Control.Monad (forM, unless)
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Time.Clock (getCurrentTime)
import Options.Applicative qualified as Opt
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (proc, readCreateProcessWithExitCode)

main :: IO ()
main = do
  opts <- Opt.execParser parserInfo
  listText <- Text.IO.readFile opts.listFile
  case parseProjectsFile listText of
    Left e -> usageError e
    Right allProjects -> case selectProjects opts.names allProjects of
      Left e -> usageError e
      Right picked -> runBench opts picked
  where
    usageError e = do
      hPutStrLn stderr (Text.unpack (renderProjectsError e))
      exitWith (ExitFailure 2)

-- * CLI

data Options = Options
  { exe :: FilePath,
    listFile :: FilePath,
    runs :: Int,
    warmup :: Int,
    jsonOut :: Maybe FilePath,
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
    <*> Opt.strOption
      ( Opt.long "list"
          <> Opt.metavar "FILE"
          <> Opt.value "bench/projects.txt"
          <> Opt.showDefault
          <> Opt.help "Reference project list"
      )
    <*> Opt.option
      (Opt.eitherReader (readBound "--runs" (>= 1)))
      ( Opt.long "runs"
          <> Opt.metavar "N"
          <> Opt.value 10
          <> Opt.showDefault
          <> Opt.help "Timed runs per project"
      )
    <*> Opt.option
      (Opt.eitherReader (readBound "--warmup" (>= 0)))
      ( Opt.long "warmup"
          <> Opt.metavar "N"
          <> Opt.value 1
          <> Opt.showDefault
          <> Opt.help "Warm-up runs per project (discarded)"
      )
    <*> Opt.optional
      ( Opt.strOption $
          Opt.long "json"
            <> Opt.metavar "FILE"
            <> Opt.help "Also write the report as JSON"
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

readBound :: String -> (Int -> Bool) -> String -> Either String Int
readBound flag ok s = case reads s of
  [(n, "")] | ok n -> Right n
  _ -> Left ("invalid " <> flag <> ": " <> s)

-- * Running

runBench :: Options -> [Project] -> IO ()
runBench opts projects = do
  started <- getCurrentTime
  tool <- getToolInfo opts.exe
  machine <- getMachineInfo
  withSystemTempDirectory "haskell-reviewer-bench" $ \tmp -> do
    outcomes <- forM projects (runProject opts tmp)
    let succeeded = [pr | Just pr <- outcomes]
        failedNames = [p.name | (p, Nothing) <- zip projects outcomes]
        report =
          Report
            { started = started,
              label = opts.label,
              runs = opts.runs,
              warmup = opts.warmup,
              tool = tool,
              machine = machine,
              projects = succeeded
            }
    Text.IO.putStr (renderTable report)
    maybe (pure ()) (`LBS.writeFile` encodeReportJson report) opts.jsonOut
    unless (null failedNames) $
      hPutStrLn stderr $
        "haskell-reviewer-bench: failed: " <> show (map Text.unpack failedNames)
    exitWith (if null failedNames then ExitSuccess else ExitFailure 1)

-- | One project: check its pin, warm up, run the timed runs. 'Nothing'
-- on any failure (already reported on stderr).
runProject :: Options -> FilePath -> Project -> IO (Maybe ProjectRun)
runProject opts tmp project = do
  dirResult <- projectDir opts project
  case dirResult of
    Left err -> failProject err
    Right dir -> do
      pinResult <- checkPin dir project
      case pinResult of
        Left err -> failProject err
        Right () -> do
          warmupResult <- warmupLoop dir opts.warmup
          case warmupResult of
            Left err -> failProject err
            Right () -> do
              timedResult <- timedLoop dir opts.runs
              case timedResult of
                Left err -> failProject err
                Right rs -> pure (Just ProjectRun {project = project, runResults = rs})
  where
    failProject err = do
      hPutStrLn stderr (label <> err)
      pure Nothing
    progress msg = hPutStrLn stderr (label <> msg)
    label = "haskell-reviewer-bench: " <> Text.unpack project.name <> ": "
    config dir i = RunConfig {exe = opts.exe, projectDir = dir, tmpDir = tmp, index = i}
    warmupLoop _ 0 = pure (Right ())
    warmupLoop dir n = go 1
      where
        go i
          | i > n = pure (Right ())
          | otherwise = do
              progress ("warm-up run " <> show i <> "/" <> show n)
              result <- runOnce (config dir i)
              case result of
                Left e -> pure (Left (renderRunError e))
                Right _ -> go (i + 1)
    timedLoop dir n = go 1 []
      where
        -- acc is never empty here: --runs is at least 1 (readBound
        -- "--runs" (>= 1) in the CLI parser), so the loop always runs
        -- at least once before n is reached.
        go i acc
          | i > n = pure (Right (NE.fromList (reverse acc)))
          | otherwise = do
              progress ("run " <> show i <> "/" <> show n)
              result <- runOnce (config dir i)
              case result of
                Left e -> pure (Left (renderRunError e))
                Right r -> go (i + 1) (r : acc)

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
  pure
    ToolInfo
      { exe = exePath,
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
