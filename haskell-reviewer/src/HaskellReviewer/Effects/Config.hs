-- | Read the user config file at start-up. See Note [Config file] in
-- "HaskellReviewer.Core.Config".
module HaskellReviewer.Effects.Config
  ( -- * Loading
    loadConfig,
    configSizeLimit,

    -- * Warm-up
    warmUpConfig,
  )
where

import Control.Exception (IOException, evaluate, finally, onException, try)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Config
  ( ConfigWarning (..),
    ParsedConfig (..),
    UiDefaults,
    builtinDefaults,
    parseConfig,
    renderConfigWarning,
  )
import HaskellReviewer.Core.Xdg (configFile, renderXdgProblem)
import HaskellReviewer.Effects.Log (Logger, ioErrorMessage, logInfo, logVerbose)
import System.IO (hClose)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (getFdStatus, getFileStatus, isRegularFile)
import System.Posix.IO
  ( OpenFileFlags (..),
    OpenMode (ReadOnly),
    closeFd,
    defaultFileFlags,
    fdToHandle,
    openFd,
  )

-- | Find config.toml from the environment, read it, log its problems
-- (one line each, default level) and give back the UI defaults. Never
-- fails: a missing file is the built-in defaults, silently; any other
-- problem is a warning and the defaults.
--
-- Only a regular file is read: a FIFO or a device could block start-up
-- (it is never opened; and the open doesn't block either, in case it
-- changes in between).
--
-- With --verbose, says which file it read.
loadConfig :: Logger -> (String -> Maybe String) -> IO UiDefaults
loadConfig logger env = case configFile env of
  Left problem -> do
    logVerbose logger ("config: none (" <> renderXdgProblem problem <> ")")
    pure builtinDefaults
  Right path -> do
    result <- try @IOException (readRegular path)
    case result of
      Left e
        | isDoesNotExistError e -> do
            logVerbose logger ("config: " <> Text.pack path <> " (not found)")
            pure builtinDefaults
        | otherwise -> badFile path ("cannot read: " <> ioErrorMessage e)
      Right Nothing -> badFile path "not a regular file; ignored"
      Right (Just bytes)
        | BS.length bytes > configSizeLimit ->
            badFile path "over 64 KiB; ignored"
        | otherwise -> case Text.Encoding.decodeUtf8' bytes of
            Left _ -> badFile path "not UTF-8; ignored"
            Right text -> do
              logVerbose logger ("config: " <> Text.pack path <> " (found)")
              let parsed = parseConfig text
              mapM_ (logInfo logger . prefixed . renderConfigWarning path) parsed.warnings
              pure parsed.defaults
  where
    badFile path msg = do
      logVerbose logger ("config: " <> Text.pack path <> " (found)")
      logInfo logger (prefixed (Text.pack path <> ": " <> msg))
      pure builtinDefaults

-- | The biggest config file we read, in bytes: 64 KiB.
configSizeLimit :: Int
configSizeLimit = 64 * 1024

-- | Parse a config with every kind of warning, so the shared values of
-- toml-parser and of our walk are built. For tests that parse in
-- parallel (Q-RTS-BLACKHOLE); the program parses on one thread.
warmUpConfig :: IO ()
warmUpConfig =
  mapM_
    (\t -> let p = parseConfig t in void (evaluate (sum (map size p.warnings))))
    [ "[ui]\ntheme = \"dark\"\nexported-only = true\nsidebar = \"files\"\n\
      \x = 1\n[ui.y]\n[llm]\nz = 2\n",
      "[ui]\ntheme = 1\nsidebar = \"nope\"\nexported-only = \"x\"\n",
      "ui = 1\n",
      "[ui]\ntheme = \"a\"\ntheme = \"b\"\n",
      "[ui]\ntheme = \x01\n",
      "\xFEFF"
    ]
  where
    size w = Text.length w.message

-- * Internal

-- | The first bytes of the file (one past the limit), or 'Nothing' if
-- it isn't a regular file.
readRegular :: FilePath -> IO (Maybe BS.ByteString)
readRegular path = do
  status <- getFileStatus path
  if not (isRegularFile status)
    then pure Nothing
    else do
      fd <- openFd path ReadOnly defaultFileFlags {nonBlock = True}
      -- It may have changed since the first look.
      again <- getFdStatus fd `onException` closeFd fd
      if not (isRegularFile again)
        then Nothing <$ closeFd fd
        else do
          h <- fdToHandle fd `onException` closeFd fd
          (Just <$> BS.hGet h (configSizeLimit + 1)) `finally` hClose h

prefixed :: Text -> Text
prefixed = ("haskell-reviewer: " <>)
