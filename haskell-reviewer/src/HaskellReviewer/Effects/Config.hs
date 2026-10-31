-- | Read the user config file at start-up. See Note [Config file] in
-- "HaskellReviewer.Core.Config".
module HaskellReviewer.Effects.Config
  ( -- * Loading
    loadConfig,
    configSizeLimit,

    -- * Warm-up
    warmUpConfig,

    -- * Reading small files
    readRegularFile,
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
    result <- try @IOException (readRegularFile configSizeLimit path)
    case result of
      Left e
        | isDoesNotExistError e -> do
            logVerbose logger ("config: " <> Text.pack path <> " (not found)")
            pure builtinDefaults
        | otherwise -> badFile path "cannot read" ("cannot read: " <> ioErrorMessage e)
      Right Nothing -> badFile path "not a regular file" "not a regular file; ignored"
      Right (Just bytes)
        | BS.length bytes > configSizeLimit ->
            badFile path "found" "over 64 KiB; ignored"
        | otherwise -> case Text.Encoding.decodeUtf8' bytes of
            Left _ -> badFile path "found" "not UTF-8; ignored"
            Right text -> do
              logVerbose logger ("config: " <> Text.pack path <> " (found)")
              let parsed = parseConfig text
              mapM_ (logInfo logger . prefixed . renderConfigWarning path) parsed.warnings
              pure parsed.defaults
  where
    -- The verbose word, then the warning.
    badFile path state msg = do
      logVerbose logger ("config: " <> Text.pack path <> " (" <> state <> ")")
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

-- | The first bytes of the file, one past the limit (so a caller can
-- tell it's too big), or 'Nothing' if it isn't a regular file. A FIFO
-- or a device is never opened, so it can't block us; the open is
-- non-blocking anyway, in case the file changes in between. Throws IO
-- errors (missing, permissions).
readRegularFile :: Int -> FilePath -> IO (Maybe BS.ByteString)
readRegularFile limit path = do
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
          (Just <$> BS.hGet h (limit + 1)) `finally` hClose h

-- * Internal

prefixed :: Text -> Text
prefixed = ("haskell-reviewer: " <>)
