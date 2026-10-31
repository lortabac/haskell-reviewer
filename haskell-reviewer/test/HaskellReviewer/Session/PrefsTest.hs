-- | The preferences store on a real ui.json, in temp dirs.
module HaskellReviewer.Session.PrefsTest (tests) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (finally)
import Control.Monad (unless, void)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Config
  ( SidebarView (..),
    Theme (..),
    UiDefaults (..),
  )
import HaskellReviewer.Core.Prefs (Effective (..), PrefsPatch, decodePatch)
import HaskellReviewer.Core.Xdg (XdgProblem (..))
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import HaskellReviewer.Session.Prefs (PrefsStore (..), newPrefsStore)
import System.Directory qualified as Dir
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createNamedPipe, fileMode, getFileStatus)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prefs store"
    [ testCase "no file: the defaults; nothing written by a GET" $
        withState $ \dir path -> do
          (store, _) <- fileStore path
          store.current >>= (@?= effectiveOf config)
          Dir.listDirectory dir >>= (@?= []),
      testCase "a save makes the dir and the file, with only the set keys" $
        withState $ \_ path -> do
          (store, _) <- fileStore path
          r <- store.update =<< patch "{\"theme\":\"light\"}"
          r @?= eff ThemeLight True SidebarFiles
          BS.readFile path >>= (@?= "{\"version\":1,\"theme\":\"light\"}\n")
          store.current >>= (@?= eff ThemeLight True SidebarFiles),
      testCase "two stores on one file: both keys survive, each sees the other" $
        withState $ \_ path -> do
          (a, _) <- fileStore path
          (b, _) <- fileStore path
          _ <- a.update =<< patch "{\"theme\":\"dark\"}"
          _ <- b.update =<< patch "{\"sidebar\":\"modules\"}"
          _ <- a.update =<< patch "{\"exportedOnly\":false}"
          let both = eff ThemeDark False SidebarModules
          a.current >>= (@?= both)
          b.current >>= (@?= both)
          BS.readFile path
            >>= ( @?=
                    "{\"version\":1,\"theme\":\"dark\",\"exportedOnly\":false,\
                    \\"sidebar\":\"modules\"}\n"
                ),
      testCase "many saves at once in one store: none lost" $
        withState $ \_ path -> do
          (store, logged) <- fileStore path
          ps <-
            mapM
              patch
              [ "{\"theme\":\"dark\"}",
                "{\"exportedOnly\":false}",
                "{\"sidebar\":\"files\"}"
              ]
          let save p = void (store.update p)
          mapConcurrently_ save (concat (replicate 20 ps))
          store.current >>= (@?= eff ThemeDark False SidebarFiles)
          -- On disk, not in memory after a failed save.
          BS.readFile path
            >>= ( @?=
                    "{\"version\":1,\"theme\":\"dark\",\"exportedOnly\":false,\
                    \\"sidebar\":\"files\"}\n"
                )
          readIORef logged >>= (@?= []),
      testCase "a FIFO as ui.json: not opened; defaults, a save goes to memory" $
        withState $ \_ path -> do
          Dir.createDirectoryIfMissing True (takeDirectory path)
          createNamedPipe path 0o600
          (store, logged) <- fileStore path
          -- With no writer, an open would block: bounded.
          r <- timeout 5_000_000 $ do
            g <- store.current
            u <- store.update =<< patch "{\"theme\":\"dark\"}"
            pure (g, u)
          r @?= Just (effectiveOf config, eff ThemeDark True SidebarFiles)
          lines' <- readIORef logged
          map ("not a regular file" `Text.isInfixOf`) lines' @?= [True, True],
      testCase "unknown keys and odd values are kept; odd values logged once" $
        withState $ \_ path -> do
          writeState
            path
            "{\"version\":1,\"zoom\":2,\"sidebar\":\"wide\",\"theme\":\"dark\"}"
          (store, logged) <- fileStore path
          store.current >>= (@?= eff ThemeDark True SidebarFiles)
          _ <- store.update =<< patch "{\"exportedOnly\":true}"
          _ <- store.update =<< patch "{\"exportedOnly\":false}"
          BS.readFile path
            >>= ( @?=
                    "{\"version\":1,\"theme\":\"dark\",\"exportedOnly\":false,\
                    \\"sidebar\":\"wide\",\"zoom\":2}\n"
                )
          readIORef logged
            >>= (@?= ["haskell-reviewer: " <> Text.pack path <> ": " <> oddSidebar])
          -- Setting the key replaces the odd value.
          _ <- store.update =<< patch "{\"sidebar\":\"modules\"}"
          BS.readFile path
            >>= ( @?=
                    "{\"version\":1,\"theme\":\"dark\",\"exportedOnly\":false,\
                    \\"sidebar\":\"modules\",\"zoom\":2}\n"
                ),
      testCase "a newer version: never written or moved; changes in memory" $
        withState $ \dir path -> do
          let newer = "{\"version\":2,\"theme\":\"light\",\"zoom\":3}"
          writeState path newer
          (store, logged) <- fileStore path
          -- The keys we understand in it count.
          store.current >>= (@?= eff ThemeLight True SidebarFiles)
          r <- store.update =<< patch "{\"theme\":\"dark\"}"
          r @?= eff ThemeDark True SidebarFiles
          store.current >>= (@?= eff ThemeDark True SidebarFiles)
          _ <- store.update =<< patch "{\"exportedOnly\":false}"
          store.current >>= (@?= eff ThemeDark False SidebarFiles)
          BS.readFile path >>= (@?= newer)
          Dir.listDirectory (dir </> "haskell-reviewer") >>= (@?= ["ui.json"])
          readIORef logged
            >>= ( @?=
                    [ "haskell-reviewer: "
                        <> Text.pack path
                        <> " is from a newer version; not saving preferences"
                    ]
                ),
      testCase "bad file: defaults, logged once; a save moves it to ui.json.bad" $
        withState $ \dir path -> do
          writeState path "{\"theme\":\"dark\"}"
          (store, logged) <- fileStore path
          store.current >>= (@?= effectiveOf config)
          store.current >>= (@?= effectiveOf config)
          r <- store.update =<< patch "{\"sidebar\":\"files\"}"
          r @?= eff ThemeSystem True SidebarFiles
          BS.readFile (path <> ".bad") >>= (@?= "{\"theme\":\"dark\"}")
          BS.readFile path >>= (@?= "{\"version\":1,\"sidebar\":\"files\"}\n")
          files <- Dir.listDirectory (dir </> "haskell-reviewer")
          sort files @?= ["ui.json", "ui.json.bad"]
          let p = "haskell-reviewer: " <> Text.pack path <> ": "
          readIORef logged
            >>= ( @?=
                    [ p <> "no \"version\": 1; using the defaults",
                      p <> "no \"version\": 1; moved to " <> Text.pack path <> ".bad"
                    ]
                ),
      testCase "an odd value the patch replaces: not logged" $
        withState $ \_ path -> do
          writeState path "{\"version\":1,\"sidebar\":\"wide\"}"
          (store, logged) <- fileStore path
          _ <- store.update =<< patch "{\"sidebar\":\"files\"}"
          BS.readFile path >>= (@?= "{\"version\":1,\"sidebar\":\"files\"}\n")
          readIORef logged >>= (@?= []),
      testCase "a bad file behind a symlink: the file it points to is moved" $
        withState $ \dir path -> do
          let real = dir </> "elsewhere" </> "prefs.json"
          Dir.createDirectoryIfMissing True (takeDirectory real)
          BS.writeFile real "[]"
          Dir.createDirectoryIfMissing True (takeDirectory path)
          Dir.createFileLink real path
          (store, logged) <- fileStore path
          _ <- store.update =<< patch "{\"theme\":\"dark\"}"
          Dir.pathIsSymbolicLink path >>= (@?= True)
          BS.readFile (real <> ".bad") >>= (@?= "[]")
          BS.readFile real >>= (@?= "{\"version\":1,\"theme\":\"dark\"}\n")
          readIORef logged
            >>= ( @?=
                    [ "haskell-reviewer: "
                        <> Text.pack path
                        <> ": not a JSON object; moved to "
                        <> Text.pack real
                        <> ".bad"
                    ]
                ),
      testCase "an unreadable file: defaults; a save goes to memory, file untouched" $
        withState $ \_ path -> do
          writeState path "{\"version\":1}"
          let readable = Dir.setOwnerReadable True Dir.emptyPermissions
          flip finally (Dir.setPermissions path readable) $ do
            Dir.setPermissions path Dir.emptyPermissions
            now <- Dir.getPermissions path
            -- Root can read anyway: nothing to test then.
            unless (Dir.readable now) $ do
              (store, logged) <- fileStore path
              store.current >>= (@?= effectiveOf config)
              r <- store.update =<< patch "{\"theme\":\"dark\"}"
              r @?= eff ThemeDark True SidebarFiles
              _ <- store.update =<< patch "{\"exportedOnly\":false}"
              store.current >>= (@?= eff ThemeDark False SidebarFiles)
              lines' <- readIORef logged
              -- One for the GET, one when it went to memory; the path
              -- once in each.
              length lines' @?= 2
              mapM_ (\l -> Text.count (Text.pack path) l @?= 1) lines'
              assertBool (show lines') (any (forgetting `Text.isPrefixOf`) lines')
          BS.readFile path >>= (@?= "{\"version\":1}"),
      testCase "dirs made for ui.json get mode 0700" $
        withState $ \_ path -> do
          (store, _) <- fileStore path
          _ <- store.update =<< patch "{\"theme\":\"dark\"}"
          mode <- fileMode <$> getFileStatus (takeDirectory path)
          (mode .&. 0o777) @?= 0o700,
      testCase "not JSON, too large: bad too" $
        withState $ \_ path ->
          mapM_
            ( \bytes -> do
                writeState path bytes
                (store, _) <- fileStore path
                store.current >>= (@?= effectiveOf config)
                _ <- store.update =<< patch "{\"theme\":\"dark\"}"
                BS.readFile (path <> ".bad") >>= (@?= bytes)
            )
            ["{\"version\":1,", BS.replicate (64 * 1024 + 1) 32],
      testCase "a read-only state dir: saves go to memory, one line" $
        readOnly "{\"version\":1,\"theme\":\"dark\"}\n" $ \path -> do
          (store, logged) <- fileStore path
          store.current >>= (@?= eff ThemeDark True SidebarFiles)
          r <- store.update =<< patch "{\"exportedOnly\":false}"
          r @?= eff ThemeDark False SidebarFiles
          _ <- store.update =<< patch "{\"sidebar\":\"modules\"}"
          _ <- store.update =<< patch "{\"theme\":\"light\"}"
          store.current >>= (@?= eff ThemeLight False SidebarModules)
          BS.readFile path >>= (@?= "{\"version\":1,\"theme\":\"dark\"}\n")
          lines' <- readIORef logged
          map (forgetting `Text.isPrefixOf`) lines' @?= [True]
          map (Text.count (Text.pack path)) lines' @?= [1],
      testCase "a read-only state dir with a bad file: memory, file untouched" $
        readOnly "[]" $ \path -> do
          (store, logged) <- fileStore path
          store.current >>= (@?= effectiveOf config)
          r <- store.update =<< patch "{\"theme\":\"dark\"}"
          r @?= eff ThemeDark True SidebarFiles
          _ <- store.update =<< patch "{\"theme\":\"light\"}"
          store.current >>= (@?= eff ThemeLight True SidebarFiles)
          BS.readFile path >>= (@?= "[]")
          Dir.doesFileExist (path <> ".bad") >>= (@?= False)
          lines' <- readIORef logged
          -- The GET's "bad file" line, then the one switch to memory.
          map (forgetting `Text.isPrefixOf`) lines' @?= [False, True],
      testCase "a state dir we can't make: memory, one line" $
        withState $ \dir _ -> do
          let parent = dir </> "ro"
              path = parent </> "haskell-reviewer" </> "ui.json"
          Dir.createDirectory parent
          withReadOnly parent $ do
            (store, logged) <- fileStore path
            _ <- store.update =<< patch "{\"theme\":\"dark\"}"
            _ <- store.update =<< patch "{\"theme\":\"light\"}"
            store.current >>= (@?= eff ThemeLight True SidebarFiles)
            Dir.doesDirectoryExist (takeDirectory path) >>= (@?= False)
            lines' <- readIORef logged
            map (forgetting `Text.isPrefixOf`) lines' @?= [True],
      testCase "no state dir: in memory, one warning on the first save" $ do
        logged <- newIORef []
        store <- newPrefsStore (logTo logged) config (Left HomeUnset)
        store.current >>= (@?= effectiveOf config)
        _ <- store.update =<< patch "{\"theme\":\"dark\"}"
        _ <- store.update =<< patch "{\"exportedOnly\":false}"
        store.current
          >>= (@?= eff ThemeDark False SidebarFiles)
        readIORef logged
          >>= (@?= ["haskell-reviewer: preferences won't be remembered: HOME is not set"])
    ]

oddSidebar :: Text
oddSidebar = "sidebar: expected one of \"auto\", \"modules\", \"files\"; kept as is"

forgetting :: Text
forgetting = "haskell-reviewer: preferences won't be remembered: "

-- | ui.json with these bytes, in a state dir we can't write. Skipped as
-- root (root writes anyway).
readOnly :: ByteString -> (FilePath -> IO ()) -> IO ()
readOnly bytes k =
  withState $ \_ path -> do
    writeState path bytes
    withReadOnly (takeDirectory path) (k path)

-- | Run with the dir read-only, then give the rights back (so the temp
-- dir can be removed). Skipped as root.
withReadOnly :: FilePath -> IO () -> IO ()
withReadOnly dir action =
  flip finally (Dir.setPermissions dir (perms True)) $ do
    Dir.setPermissions dir (perms False)
    now <- Dir.getPermissions dir
    unless (Dir.writable now) action
  where
    perms w =
      Dir.setOwnerWritable w $
        Dir.setOwnerSearchable True $
          Dir.setOwnerReadable True Dir.emptyPermissions

-- | Not the built-in defaults, so precedence shows.
config :: UiDefaults
config = UiDefaults {theme = ThemeSystem, exportedOnly = True, sidebar = SidebarFiles}

effectiveOf :: UiDefaults -> Effective
effectiveOf d = eff d.theme d.exportedOnly d.sidebar

eff :: Theme -> Bool -> SidebarView -> Effective
eff t e s = Effective {theme = t, exportedOnly = e, sidebar = s}

-- | A temp state dir and where ui.json goes in it.
withState :: (FilePath -> FilePath -> IO a) -> IO a
withState k =
  withSystemTempDirectory "haskell-reviewer-state" $ \dir ->
    k dir (dir </> "haskell-reviewer" </> "ui.json")

fileStore :: FilePath -> IO (PrefsStore, IORef [Text])
fileStore path = do
  logged <- newIORef []
  store <- newPrefsStore (logTo logged) config (Right path)
  pure (store, logged)

logTo :: IORef [Text] -> Logger
logTo ref = newLogger LevelInfo (\l -> atomicModifyIORef' ref (\ls -> (ls <> [l], ())))

writeState :: FilePath -> ByteString -> IO ()
writeState path bytes = do
  Dir.createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path bytes

patch :: ByteString -> IO PrefsPatch
patch body = either (assertFailure . show) pure (decodePatch body)
