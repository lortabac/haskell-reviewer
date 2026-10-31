-- | The preferences of the page, kept in ui.json (D-68, D-200).
module HaskellReviewer.Session.Prefs
  ( -- * Types
    PrefsStore (..),
    PrefsError (..),

    -- * Handles
    newPrefsStore,

    -- * Messages
    renderPrefsError,

    -- * Limits
    prefsSizeLimit,
  )
where

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (IOException, evaluate, try)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Config (UiDefaults)
import HaskellReviewer.Core.Prefs
  ( BadPrefsFile (..),
    BadValue (..),
    DecodedPrefs (..),
    Effective,
    Prefs,
    PrefsFile (..),
    PrefsPatch,
    PrefsRead (..),
    applyPatch,
    applyPatchFile,
    decodePrefsFile,
    effective,
    emptyPrefsFile,
    encodePrefsFile,
    noPrefs,
    overridePrefs,
    patchKeys,
    renderBadPrefsFile,
    renderBadValue,
  )
import HaskellReviewer.Core.Xdg (XdgProblem, renderXdgProblem)
import HaskellReviewer.Effects.AtomicFile
  ( AtomicWriteError,
    renderAtomicWriteError,
    resolveLink,
    writeFileAtomic,
  )
import HaskellReviewer.Effects.Log (Logger, ioErrorMessage, logInfo)
import System.Directory qualified as Dir
import System.IO (IOMode (ReadMode), withBinaryFile)
import System.IO.Error (isDoesNotExistError)

{- Note [Saving preferences]

D-200. Each GET reads ui.json again, so a change made by another
instance shows up on reload. Each POST reads the file, applies the
change and writes it back atomically, all under an MVar, so two
requests of this process never lose each other's keys. See
Note [Atomic write] in "HaskellReviewer.Effects.AtomicFile".

Two processes can still race: there is no file lock. The last writer
wins, per key: each one only changes the keys it was sent, on top of
what it just read. The window is a few milliseconds, and the file is
never torn. In that window a newer tool's file can also be moved
aside as bad, and a newer file over 64 KiB counts as too large and is
moved too: accepted, like the rest of the race.

What ui.json can be (Note [ui.json] in "HaskellReviewer.Core.Prefs"):

- good: as above. Values we don't understand are logged once, on
  save, and kept;
- bad: on read, we log it once and use the defaults; on save, we
  rename it to ui.json.bad (replacing an older one), log that, and
  write a fresh file with just the new keys;
- from a newer version: never written or moved. We log once that we
  won't save, and keep this session's changes in memory, over the keys
  of the file we understand;
- unreadable (permissions, ...): reads use the defaults, saves fail.
  Overwriting it could lose what's in it.

With no state dir (no HOME), preferences live in memory: the first
save logs that they won't be remembered. See
Note [XDG dirs] in "HaskellReviewer.Core.Xdg".
-}

-- | Read and change the preferences. Safe to use from several threads.
data PrefsStore = PrefsStore
  { -- | The preferences now, defaults filled in.
    current :: IO Effective,
    -- | Apply a change and save it. Gives back the new preferences.
    update :: PrefsPatch -> IO (Either PrefsError Effective)
  }

-- | A change that could not be saved. It is logged.
data PrefsError
  = -- | The old file, with its message.
    CannotRead FilePath Text
  | -- | Moving a bad file aside: the file, the message.
    CannotMoveAside FilePath Text
  | CannotWrite AtomicWriteError
  deriving (Eq, Show)

-- | A store on ui.json, or in memory when there is no state dir. See
-- Note [Saving preferences].
newPrefsStore :: Logger -> UiDefaults -> Either XdgProblem FilePath -> IO PrefsStore
newPrefsStore logger defaults = \case
  Left problem -> memoryStore logger defaults problem
  Right path -> fileStore logger defaults path

-- | For the user.
renderPrefsError :: PrefsError -> Text
renderPrefsError = \case
  CannotRead path e -> "cannot read " <> Text.pack path <> ": " <> e
  CannotMoveAside path e -> "cannot move " <> Text.pack path <> " aside: " <> e
  CannotWrite e -> renderAtomicWriteError e

-- | The biggest ui.json we read, in bytes: 64 KiB. A bigger one is bad.
prefsSizeLimit :: Int
prefsSizeLimit = 64 * 1024

-- * Internal

memoryStore :: Logger -> UiDefaults -> XdgProblem -> IO PrefsStore
memoryStore logger defaults problem = do
  ref <- newIORef noPrefs
  warned <- newIORef False
  -- Built here, not first on a request thread (Q-RTS-BLACKHOLE).
  message <-
    evaluate $
      "haskell-reviewer: preferences won't be remembered: "
        <> renderXdgProblem problem
  pure
    PrefsStore
      { current = effective defaults <$> readIORef ref,
        update = \patch -> do
          already <- atomicModifyIORef' warned (True,)
          unless already (logInfo logger message)
          new <- atomicModifyIORef' ref (\p -> dup (applyPatch patch p))
          pure (Right (effective defaults new))
      }

-- | What reading ui.json gave.
data Loaded
  = Loaded DecodedPrefs
  | -- | From a newer version: the keys we understand in it.
    Newer Prefs
  | Missing
  | Bad BadPrefsFile
  | Unreadable Text

fileStore :: Logger -> UiDefaults -> FilePath -> IO PrefsStore
fileStore logger defaults path = do
  lock <- newMVar ()
  -- Lines already logged: a bad or newer file would say the same thing
  -- on every request. Reset after a good save.
  said <- newIORef Set.empty
  -- Odd values: once per store, saves don't change them.
  saidOdd <- newIORef Set.empty
  -- This session's changes, when ui.json is from a newer version.
  inMemory <- newIORef noPrefs
  -- Built here, not first on a request thread (Q-RTS-BLACKHOLE). Text
  -- is strict: this forces the path too.
  pathText <- evaluate (Text.pack path)
  newer <- evaluate (pathText <> " is from a newer version; not saving preferences")
  let onceIn ref msg = do
        already <- atomicModifyIORef' ref (\s -> (Set.insert msg s, Set.member msg s))
        unless already (logInfo logger ("haskell-reviewer: " <> msg))
      sayOnce = onceIn said
      current = do
        loaded <- load
        effective defaults <$> case loaded of
          Loaded d -> pure d.file.prefs
          Missing -> pure noPrefs
          Newer fromFile -> do
            sayOnce newer
            (`overridePrefs` fromFile) <$> readIORef inMemory
          Bad bad -> noPrefs <$ sayOnce (badLine bad "using the defaults")
          Unreadable e -> noPrefs <$ sayOnce ("cannot read " <> pathText <> ": " <> e)
      update patch = withMVar lock $ \() -> do
        loaded <- load
        case loaded of
          Newer fromFile -> do
            sayOnce newer
            mine <- atomicModifyIORef' inMemory (\p -> dup (applyPatch patch p))
            pure (Right (effective defaults (overridePrefs mine fromFile)))
          Loaded d -> do
            -- Not those the patch replaces.
            let kept = [b | b <- d.oddValues, b.key `notElem` patchKeys patch]
                odd' b = pathText <> ": " <> renderBadValue b <> "; kept as is"
            mapM_ (onceIn saidOdd . odd') kept
            save patch d.file
          Missing -> save patch emptyPrefsFile
          Bad bad ->
            moveAside >>= \case
              Left err -> failed err
              Right moved -> do
                logInfo logger $
                  "haskell-reviewer: "
                    <> badLine bad ("moved to " <> Text.pack moved)
                save patch emptyPrefsFile
          Unreadable e -> failed (CannotRead path e)
      save patch file = do
        let new = applyPatchFile patch file
        writeFileAtomic path (encodePrefsFile new) >>= \case
          Left e -> failed (CannotWrite e)
          Right () -> do
            writeIORef said Set.empty
            pure (Right (effective defaults new.prefs))
      failed err = do
        logInfo logger $
          "haskell-reviewer: cannot save preferences: " <> renderPrefsError err
        pure (Left err)
      badLine bad what = pathText <> ": " <> renderBadPrefsFile bad <> "; " <> what
      -- Next to the file a symlink points to, like the write. Gives the
      -- new name.
      moveAside = do
        target <- resolveLink path
        let moved = target <> ".bad"
        try @IOException (Dir.renameFile target moved) >>= \case
          Left e -> pure (Left (CannotMoveAside target (ioErrorMessage e)))
          Right () -> pure (Right moved)
      load = do
        result <- try @IOException (withBinaryFile path ReadMode readCapped)
        pure $ case result of
          Left e
            | isDoesNotExistError e -> Missing
            | otherwise -> Unreadable (ioErrorMessage e)
          Right bytes
            | BS.length bytes > prefsSizeLimit -> Bad PrefsTooLarge
            | otherwise -> case decodePrefsFile bytes of
                Left bad -> Bad bad
                Right (PrefsCurrent d) -> Loaded d
                Right (PrefsNewer p) -> Newer p
  pure PrefsStore {current = current, update = update}
  where
    readCapped h = BS.hGet h (prefsSizeLimit + 1)

dup :: a -> (a, a)
dup x = (x, x)
