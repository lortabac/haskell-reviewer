-- | Open the user's browser at start-up.
module HaskellReviewer.Effects.Browser
  ( -- * Operations
    openBrowser,
    openerName,
  )
where

import Control.Concurrent (forkIO)
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    bracketOnError,
    fromException,
    throwIO,
    try,
  )
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Root (Root)
import HaskellReviewer.Effects.Helper (findHelper)
import HaskellReviewer.Effects.Log (Logger, exceptionMessage, logInfo)
import System.Exit (ExitCode (..))
import System.IO (IOMode (ReadWriteMode), hClose, openFile)
import System.Info qualified
import System.Process
  ( CreateProcess (..),
    StdStream (UseHandle),
    createProcess,
    proc,
    waitForProcess,
  )

-- | The program that opens a URL: @open@ on macOS, @xdg-open@ elsewhere.
openerName :: String
openerName = if System.Info.os == "darwin" then "open" else "xdg-open"

-- | Open the URL in the browser, in the background. Never throws, never
-- blocks. If it fails, we log a warning: the user can still copy the URL
-- from stderr.
--
-- The opener is looked up like any helper (Note [Helper lookup] in
-- "HaskellReviewer.Effects.Helper"). Its stdin, stdout and stderr go to
-- @\/dev\/null@: our stdout must stay unused. It runs in a new session, so
-- Ctrl-C in our terminal doesn't reach it (or the browser it starts). We
-- never wait for it in the main path and never kill it.
openBrowser :: Logger -> Root -> Text -> IO ()
openBrowser logger root url = void $ forkIO $ do
  result <- try @SomeException run
  case result of
    Right (Left why) -> warn why
    Right (Right ()) -> pure ()
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> warn (exceptionMessage e)
  where
    run = do
      found <- findHelper root openerName
      case found of
        Nothing -> pure (Left (Text.pack openerName <> " not found on PATH"))
        Just exe -> do
          -- createProcess closes the handle in the parent for us. If it
          -- throws, bracketOnError does.
          (_, _, _, ph) <-
            bracketOnError (openFile "/dev/null" ReadWriteMode) hClose $ \devNull ->
              createProcess
                (proc exe [Text.unpack url])
                  { std_in = UseHandle devNull,
                    std_out = UseHandle devNull,
                    std_err = UseHandle devNull,
                    close_fds = True,
                    new_session = True
                  }
          code <- waitForProcess ph
          pure $ case code of
            ExitSuccess -> Right ()
            ExitFailure n ->
              Left (Text.pack openerName <> " exited with code " <> Text.pack (show n))
    warn why =
      logInfo logger $
        "haskell-reviewer: warning: cannot open the browser: "
          <> why
          <> "; open the URL by hand"
