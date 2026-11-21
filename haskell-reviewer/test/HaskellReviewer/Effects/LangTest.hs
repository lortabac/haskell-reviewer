-- | Reading the highlighting rules from disk, in temp dirs. The parsing
-- itself is in the lang fixtures (LangGoldenTest).
module HaskellReviewer.Effects.LangTest (tests) where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Lang (Lang (..), Langs, langCount, langFor, noLangs)
import HaskellReviewer.Core.RelPath (relPathFromSegments)
import HaskellReviewer.Effects.Lang (loadLangs)
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import System.Directory qualified as Dir
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createNamedPipe)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Lang load"
    [ testCase "no dir: no rules, nothing logged at the default level" $
        withRules Nothing $ \_ env -> do
          (langs, logged) <- load LevelInfo env
          langs @?= noLangs
          logged @?= [],
      testCase "no dir, --verbose: says so" $
        withRules Nothing $ \dir env -> do
          (_, logged) <- load LevelVerbose env
          logged @?= ["highlight: " <> Text.pack dir <> " (not found)"],
      testCase "no HOME: no rules" $ do
        (langs, logged) <- load LevelInfo (const Nothing)
        langs @?= noLangs
        logged @?= [],
      testCase "the dir is a file: one line, no rules" $
        withRules Nothing $ \dir env -> do
          Dir.createDirectoryIfMissing True (takeDirectory dir)
          BS.writeFile dir "x"
          (langs, logged) <- load LevelInfo env
          langs @?= noLangs
          let start = "haskell-reviewer: " <> Text.pack dir <> ": cannot read: "
          map (Text.isPrefixOf start) logged @?= [True],
      testCase "only *.toml is read, in name order; the first claim wins"
        $ withRules
          ( Just
              [ ("b.toml", "extensions = [\"x\", \"y\"]\n"),
                ("a.toml", "extensions = [\"x\"]\nnumbers = true\n"),
                ("a.toml.bak", "extensions = [\"bak\"]\n"),
                ("notes.txt", "not TOML [")
              ]
          )
        $ \dir env -> do
          (langs, logged) <- load LevelVerbose env
          langCount langs @?= 2
          (.numbers) <$> langFor langs (rel "f.x") @?= Just True
          (.numbers) <$> langFor langs (rel "f.y") @?= Just False
          isJust (langFor langs (rel "f.bak")) @?= False
          logged
            @?= [ "haskell-reviewer: "
                    <> Text.pack (dir </> "b.toml")
                    <> ": extension \"x\" already has rules; ignored",
                  "highlight: " <> Text.pack dir <> " (2 extensions)"
                ],
      testCase "a bad key: a line with the place, the rest is kept" $
        withRules (Just [("a.toml", "extensions = [\"x\"]\nnumbers = 1\n")]) $ \dir env -> do
          (langs, logged) <- load LevelInfo env
          langCount langs @?= 1
          logged
            @?= [ "haskell-reviewer: "
                    <> Text.pack (dir </> "a.toml")
                    <> ":2:11: numbers: expected a boolean, got an integer"
                ],
      testCase "not UTF-8, too large: one line, the other files are read"
        $ forM_
          [ ("extensions = [\"\xff\"]\n", "not UTF-8; ignored"),
            ("extensions = [\"z\"]\n" <> BS.replicate (64 * 1024) 32, "over 64 KiB; ignored")
          ]
        $ \(bytes, why) ->
          withRules (Just [("a.toml", bytes), ("b.toml", "extensions = [\"x\"]\n")]) $
            \dir env -> do
              (langs, logged) <- load LevelInfo env
              langCount langs @?= 1
              let path = Text.pack (dir </> "a.toml")
              logged @?= ["haskell-reviewer: " <> path <> ": " <> why],
      testCase "a dir or a FIFO named *.toml: not opened, one line" $
        forM_ [Dir.createDirectory, \p -> createNamedPipe p 0o600] $ \make ->
          withRules (Just []) $ \dir env -> do
            make (dir </> "a.toml")
            -- A FIFO with no writer would block an open: bounded.
            r <- timeout 5_000_000 (load LevelInfo env)
            r
              @?= Just
                ( noLangs,
                  [ "haskell-reviewer: "
                      <> Text.pack (dir </> "a.toml")
                      <> ": not a regular file; ignored"
                  ]
                )
    ]
  where
    rel name = maybe (error "bad path") id (relPathFromSegments [name])

-- | A temp XDG config dir, with these files in its highlight dir (no
-- dir if 'Nothing'). Gives the highlight dir.
withRules ::
  Maybe [(FilePath, BS.ByteString)] ->
  (FilePath -> (String -> Maybe String) -> IO a) ->
  IO a
withRules files k =
  withSystemTempDirectory "haskell-reviewer-lang" $ \tmp -> do
    let dir = tmp </> "haskell-reviewer" </> "highlight"
    forM_ files $ \fs -> do
      Dir.createDirectoryIfMissing True dir
      forM_ fs $ \(name, bytes) -> BS.writeFile (dir </> name) bytes
    k dir (\name -> if name == "XDG_CONFIG_HOME" then Just tmp else Nothing)

load :: LogLevel -> (String -> Maybe String) -> IO (Langs, [Text])
load level env = do
  ref <- newIORef []
  let logger = newLogger level (\l -> atomicModifyIORef' ref (\ls -> (ls <> [l], ())))
  langs <- loadLangs logger env
  (langs,) <$> readIORef ref
