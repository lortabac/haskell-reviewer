-- | Reading config.toml from disk, in temp dirs. The parsing itself is
-- in the config fixtures (ConfigGoldenTest).
module HaskellReviewer.Effects.ConfigTest (tests) where

import Control.Exception (finally)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Config
  ( SidebarView (..),
    Theme (..),
    UiDefaults (..),
    builtinDefaults,
  )
import HaskellReviewer.Effects.Config (loadConfig)
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
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
    "Config load"
    [ testCase "no file: built-in defaults, nothing logged at the default level" $
        withConfig Nothing $ \_ env -> do
          (d, logged) <- load LevelInfo env
          d @?= builtinDefaults
          logged @?= [],
      testCase "no file, --verbose: says so" $
        withConfig Nothing $ \path env -> do
          (_, logged) <- load LevelVerbose env
          logged @?= ["config: " <> Text.pack path <> " (not found)"],
      testCase "a good file with a bad key: its values, one line" $
        withConfig (Just "[ui]\ntheme = \"dark\"\nsidebar = 1\n") $ \path env -> do
          (d, logged) <- load LevelInfo env
          d @?= builtinDefaults {theme = ThemeDark}
          logged
            @?= [ "haskell-reviewer: "
                    <> Text.pack path
                    <> ":3:11: ui.sidebar: expected a string, got an integer"
                ],
      testCase "not UTF-8, too large: one line, built-in defaults" $
        mapM_
          ( \(bytes, why) ->
              withConfig (Just bytes) $ \path env -> do
                (d, logged) <- load LevelInfo env
                d @?= builtinDefaults
                logged @?= ["haskell-reviewer: " <> Text.pack path <> ": " <> why]
          )
          [ ("[ui]\ntheme = \"\xff\"\n", "not UTF-8; ignored"),
            ( "[ui]\nsidebar = \"files\"\n" <> BS.replicate (64 * 1024) 32,
              "over 64 KiB; ignored"
            )
          ],
      testCase "a dir or a FIFO: not opened, one line, built-in defaults" $
        mapM_
          ( \make -> withConfig Nothing $ \path env -> do
              Dir.createDirectoryIfMissing True (takeDirectory path)
              make path
              -- A FIFO with no writer would block an open: bounded.
              r <- timeout 5_000_000 (load LevelInfo env)
              r
                @?= Just
                  ( builtinDefaults,
                    [ "haskell-reviewer: "
                        <> Text.pack path
                        <> ": not a regular file; ignored"
                    ]
                  )
          )
          [Dir.createDirectory, \p -> createNamedPipe p 0o600],
      testCase "unreadable: one line, the path said once" $
        withConfig (Just "[ui]\n") $ \path env ->
          flip finally (Dir.setPermissions path readable) $ do
            Dir.setPermissions path Dir.emptyPermissions
            now <- Dir.getPermissions path
            -- Root can read anyway: nothing to test then.
            unless (Dir.readable now) $ do
              (d, logged) <- load LevelVerbose env
              d @?= builtinDefaults
              -- Not "found": we couldn't even look.
              take 1 logged @?= ["config: " <> Text.pack path <> " (cannot read)"]
              let start = "haskell-reviewer: " <> Text.pack path <> ": cannot read: "
              map (Text.isPrefixOf start) (drop 1 logged) @?= [True]
              map (Text.count (Text.pack path)) (drop 1 logged) @?= [1],
      testCase "no HOME: built-in defaults, a verbose line" $ do
        (d, logged) <- load LevelVerbose (const Nothing)
        d @?= builtinDefaults
        logged @?= ["config: none (HOME is not set)"],
      testCase "the sidebar key" $
        withConfig (Just "[ui]\nsidebar = \"modules\"\n") $ \_ env -> do
          (d, _) <- load LevelInfo env
          d.sidebar @?= SidebarModules
    ]

readable :: Dir.Permissions
readable = Dir.setOwnerReadable True Dir.emptyPermissions

-- | A temp XDG config dir, with config.toml if given.
withConfig ::
  Maybe BS.ByteString -> (FilePath -> (String -> Maybe String) -> IO a) -> IO a
withConfig contents k =
  withSystemTempDirectory "haskell-reviewer-config" $ \dir -> do
    let path = dir </> "haskell-reviewer" </> "config.toml"
    case contents of
      Nothing -> pure ()
      Just bytes -> do
        Dir.createDirectoryIfMissing True (takeDirectory path)
        BS.writeFile path bytes
    k path (\name -> if name == "XDG_CONFIG_HOME" then Just dir else Nothing)

load :: LogLevel -> (String -> Maybe String) -> IO (UiDefaults, [Text])
load level env = do
  ref <- newIORef []
  d <- loadConfig (logTo level ref) env
  (d,) <$> readIORef ref

logTo :: LogLevel -> IORef [Text] -> Logger
logTo level ref = newLogger level (\l -> atomicModifyIORef' ref (\ls -> (ls <> [l], ())))
