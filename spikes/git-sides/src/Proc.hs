-- | git with the lib's hardened args and env, plus the hgit.sh knobs.
module Proc
  ( GitCfg (..),
    gitCfg,
    gitProc,
    runGit,
    runGitOk,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, bracket, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import HaskellReviewer.Effects.Git (hardenedArgs, hardenedEnv)
import System.Directory (findExecutablesInDirectories)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (splitSearchPath)
import System.IO (hClose, hPutStrLn, stderr)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process
import System.Timeout (timeout)

data GitCfg = GitCfg
  { exe :: FilePath,
    env :: [(String, String)],
    repo :: FilePath
  }

-- | GIT, HGIT_PATH, HGIT_ENV and HGIT_TRACE as in hgit.sh.
gitCfg :: FilePath -> IO GitCfg
gitCfg repo = do
  inherited <- getEnvironment
  let dirs = splitSearchPath (fromMaybe "/usr/bin:/bin" (lookup "HGIT_PATH" inherited))
      extra =
        [ (k, drop 1 v)
        | w <- words (fromMaybe "" (lookup "HGIT_ENV" inherited)),
          let (k, v) = break (== '=') w
        ]
      trace = [("GIT_TRACE2_EVENT", t) | Just t <- [lookup "HGIT_TRACE" inherited], not (null t)]
  exe <- case fromMaybe "git" (lookup "GIT" inherited) of
    g | '/' `elem` g -> pure g
    g ->
      findExecutablesInDirectories dirs g >>= \case
        (x : _) -> pure x
        [] -> hPutStrLn stderr ("no " <> g <> " in HGIT_PATH") >> exitWith (ExitFailure 2)
  pure GitCfg {exe = exe, env = hardenedEnv inherited dirs <> extra <> trace, repo = repo}

gitProc :: GitCfg -> [String] -> CreateProcess
gitProc g args =
  (proc g.exe (hardenedArgs args))
    { cwd = Just g.repo,
      env = Just g.env,
      close_fds = True
    }

-- | One call, 120 s limit, killed past it. Stdin given or /dev/null.
runGit :: GitCfg -> [String] -> Maybe ByteString -> IO (ExitCode, ByteString, ByteString)
runGit g args input = do
  let cp = (gitProc g args) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  bracket (createProcess cp) cleanUp $ \case
    (Just hIn, Just hOut, Just hErr, ph) -> do
      r <- timeout 120_000_000 $ do
        ((), (o, e)) <-
          concurrently
            (mapM_ (BS.hPut hIn) input >> hClose hIn)
            (concurrently (BS.hGetContents hOut) (BS.hGetContents hErr))
        c <- waitForProcess ph
        pure (c, o, e)
      maybe (ioError (userError ("git timed out: " <> unwords args))) pure r
    _ -> ioError (userError "no pipes")
  where
    cleanUp (hIn, hOut, hErr, ph) = do
      getPid ph >>= mapM_ (\p -> void (try @IOException (signalProcess sigKILL p)))
      mapM_ (void . try @IOException . hClose) hIn
      mapM_ hClose hOut
      mapM_ hClose hErr
      void (try @IOException (waitForProcess ph))

-- | Like 'runGit', and a non-zero exit is an error.
runGitOk :: GitCfg -> [String] -> Maybe ByteString -> IO ByteString
runGitOk g args input = do
  (c, o, e) <- runGit g args input
  case c of
    ExitSuccess -> pure o
    _ -> ioError (userError ("git " <> unwords args <> ": " <> show c <> " " <> show e))
