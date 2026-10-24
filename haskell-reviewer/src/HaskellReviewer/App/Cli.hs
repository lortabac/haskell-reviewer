-- | Command line parsing (SPEC §2.1). Pure: nothing here prints.
module HaskellReviewer.App.Cli
  ( -- * Types
    Command (..),
    Options (..),
    Browser (..),
    IdleExit (..),
    Minutes (..),
    CliResult (..),

    -- * Parsing
    parseArgs,
    parserInfo,
    defaultIdleExit,
    maxIdleExit,
  )
where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Address (Port (..))
import HaskellReviewer.Core.Project
  ( GhcVersion (..),
    parseGhcVersionNumbers,
    parserGhcVersion,
    renderGhcVersion,
  )
import HaskellReviewer.Effects.Log (LogLevel (..))
import Options.Applicative qualified as Opt
import System.Exit (ExitCode (..))
import Text.Read (readMaybe)

-- | What the user asked for.
data Command
  = -- | @--version@.
    ShowVersion
  | -- | Normal run.
    Run Options
  deriving (Eq, Show)

-- | Options for a normal run.
data Options = Options
  { -- | ROOT as given, not checked yet.
    root :: FilePath,
    -- | 'Nothing': pick a free port.
    port :: Maybe Port,
    browser :: Browser,
    idleExit :: IdleExit,
    logLevel :: LogLevel,
    -- | Where to write timing spans, if anywhere.
    traceFile :: Maybe FilePath,
    -- | GHC version the project targets. 'Nothing': with-compiler, else
    -- the parser's own version (§7.1).
    ghc :: Maybe GhcVersion
  }
  deriving (Eq, Show)

-- | Open the browser at start-up or not (@--no-open@).
data Browser = OpenBrowser | NoBrowser
  deriving (Eq, Show)

-- | When to exit if no browser tab is connected.
data IdleExit
  = -- | @--idle-exit 0@: never.
    IdleExitOff
  | IdleExitAfter Minutes
  deriving (Eq, Show)

-- | A number of minutes, from 1 to 'maxIdleExit'.
--
-- Like 'Port', the range is checked by the CLI parser, not by the type.
newtype Minutes = Minutes Int
  deriving (Eq, Show)

-- | What 'parseArgs' gives back.
data CliResult
  = -- | Go on and run this.
    CliCommand Command
  | -- | Stop now: print the text on stderr and exit with the code. Used for
    -- @--help@ (code 0) and usage errors (code 2).
    CliExit ExitCode Text
  deriving (Eq, Show)

-- | @--idle-exit@ when not given.
defaultIdleExit :: Minutes
defaultIdleExit = Minutes 5

-- | Upper limit for @--idle-exit@: one week. Keeps the timer far from any
-- overflow: a week in microseconds (about 6e11) fits easily in a 64-bit
-- 'Int'. We only support 64-bit platforms.
maxIdleExit :: Minutes
maxIdleExit = Minutes 10080

-- | Parse the command line arguments.
--
-- We don't use optparse's @handleParseResult@: it prints @--help@ on
-- stdout, and stdout must stay unused.
parseArgs :: [String] -> CliResult
parseArgs args =
  case Opt.execParserPure Opt.defaultPrefs parserInfo args of
    Opt.Success cmd -> CliCommand cmd
    Opt.Failure failure ->
      let (msg, code) = Opt.renderFailure failure "haskell-reviewer"
       in CliExit code (Text.pack msg)
    Opt.CompletionInvoked _ ->
      CliExit (ExitFailure 2) "shell completion is not supported"

-- | The full parser, with help text. Usage errors exit with code 2.
parserInfo :: Opt.ParserInfo Command
parserInfo =
  Opt.info
    (commandParser Opt.<**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.header "haskell-reviewer - review Haskell code in the browser"
        <> Opt.failureCode 2
    )

-- * Internal

commandParser :: Opt.Parser Command
commandParser =
  Opt.flag'
    ShowVersion
    (Opt.long "version" <> Opt.help "Print the version and exit")
    Opt.<|> (Run <$> optionsParser)

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> rootArg
    <*> Opt.optional portOption
    <*> browserFlag
    <*> idleExitOption
    <*> logLevelFlags
    <*> Opt.optional traceFileOption
    <*> Opt.optional ghcOption

rootArg :: Opt.Parser FilePath
rootArg =
  Opt.strArgument
    ( Opt.metavar "ROOT"
        <> Opt.value "."
        <> Opt.showDefault
        <> Opt.help "Project directory to review"
    )

portOption :: Opt.Parser Port
portOption =
  Opt.option
    (Opt.eitherReader readPort)
    ( Opt.long "port"
        <> Opt.metavar "N"
        <> Opt.help "Port to listen on (default: a free one)"
    )

browserFlag :: Opt.Parser Browser
browserFlag =
  Opt.flag
    OpenBrowser
    NoBrowser
    (Opt.long "no-open" <> Opt.help "Don't open the browser")

idleExitOption :: Opt.Parser IdleExit
idleExitOption =
  Opt.option
    (Opt.eitherReader readIdleExit)
    ( Opt.long "idle-exit"
        <> Opt.metavar "MINUTES"
        <> Opt.value (IdleExitAfter defaultIdleExit)
        <> Opt.showDefaultWith (const (showMinutes defaultIdleExit))
        <> Opt.help "Exit after this long with no browser tab (0: never)"
    )

-- | Both flags may be given; the higher level wins.
logLevelFlags :: Opt.Parser LogLevel
logLevelFlags = max <$> verbose <*> debug
  where
    verbose =
      Opt.flag
        LevelInfo
        LevelVerbose
        (Opt.long "verbose" <> Opt.help "Log more on stderr")
    debug =
      Opt.flag
        LevelInfo
        LevelDebug
        (Opt.long "debug" <> Opt.help "Log even more on stderr")

traceFileOption :: Opt.Parser FilePath
traceFileOption =
  Opt.strOption
    ( Opt.long "trace-file"
        <> Opt.metavar "PATH"
        <> Opt.help "Write timing spans to PATH, as JSON lines"
    )

ghcOption :: Opt.Parser GhcVersion
ghcOption =
  Opt.option
    (Opt.eitherReader readGhcVersion)
    ( Opt.long "ghc"
        <> Opt.metavar "X.Y[.Z]"
        <> Opt.help
          ( "GHC version the project targets (default: with-compiler, else "
              <> Text.unpack (renderGhcVersion parserGhcVersion)
              <> ")"
          )
    )

readPort :: String -> Either String Port
readPort s = case readNatural s of
  Just n | n >= 1 && n <= 65535 -> Right (Port (fromInteger n))
  _ -> Left ("invalid port: " <> s <> " (expected 1 to 65535)")

-- | @X.Y@ or @X.Y.Z@, each part at most 9 digits (D-98).
readGhcVersion :: String -> Either String GhcVersion
readGhcVersion s = case parseGhcVersionNumbers (Text.pack s) of
  Just ns | length ns == 2 || length ns == 3 -> Right (GhcVersion ns)
  _ -> Left ("invalid GHC version: " <> s <> " (expected X.Y or X.Y.Z)")

readIdleExit :: String -> Either String IdleExit
readIdleExit s = case readNatural s of
  Just 0 -> Right IdleExitOff
  Just n
    | n <= toInteger maxMinutes ->
        Right (IdleExitAfter (Minutes (fromInteger n)))
  _ ->
    Left
      ( "invalid minutes: "
          <> s
          <> " (expected 0 to "
          <> showMinutes maxIdleExit
          <> ")"
      )
  where
    Minutes maxMinutes = maxIdleExit

showMinutes :: Minutes -> String
showMinutes (Minutes n) = show n

-- | Digits only: no sign, no spaces, no hex.
readNatural :: String -> Maybe Integer
readNatural s
  | not (null s) && all isDigit s = readMaybe s
  | otherwise = Nothing
