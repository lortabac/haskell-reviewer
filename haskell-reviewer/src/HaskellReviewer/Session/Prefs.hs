-- | The preferences of the page, kept in ui.json (D-68, D-200).
module HaskellReviewer.Session.Prefs
  ( -- * Types
    PrefsStore (..),

    -- * Handles
    newPrefsStore,

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
  ( AtomicWriteError (..),
    renderAtomicWriteError,
    resolveLink,
    writeFileAtomic,
  )
import HaskellReviewer.Effects.Config (readRegularFile)
import HaskellReviewer.Effects.Log (Logger, ioErrorMessage, logInfo)
import System.Directory qualified as Dir
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
- unreadable (permissions, ...), or not a regular file (a FIFO or a
  device is never opened): reads use the defaults (logged once).
  A save doesn't overwrite it (that could lose what's in it): see
  below.

A save never fails (D-200). The tool must work in a read-only or
restricted sandbox. If a save can't read ui.json, can't make the dir,
can't move a bad file aside, or can't write or rename (EACCES, EROFS,
ENOSPC, ...), the store switches to memory for the rest of the
session: it starts from the preferences it knew plus the change, logs
one line ("preferences won't be remembered: <path>: <why>"), and never
touches the file again. The POST still answers 200.

With no state dir (no HOME), preferences live in memory from the
start: the first save logs that they won't be remembered. See
Note [XDG dirs] in "HaskellReviewer.Core.Xdg".
-}

-- | Read and change the preferences. Safe to use from several threads.
data PrefsStore = PrefsStore
  { -- | The preferences now, defaults filled in.
    current :: IO Effective,
    -- | Apply a change and save it, if we can. Gives back the new
    -- preferences. Never fails: see Note [Saving preferences].
    update :: PrefsPatch -> IO Effective
  }

-- | A store on ui.json, or in memory when there is no state dir. See
-- Note [Saving preferences].
newPrefsStore :: Logger -> UiDefaults -> Either XdgProblem FilePath -> IO PrefsStore
newPrefsStore logger defaults = \case
  Left problem -> memoryStore logger defaults problem
  Right path -> fileStore logger defaults path

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
          pure (effective defaults new)
      }

-- | Why a store stopped saving.
data SaveProblem
  = -- | The old file, with its message.
    CannotRead Text
  | -- | Moving a bad file aside: the file, the message.
    CannotMoveAside FilePath Text
  | CannotWrite AtomicWriteError

-- | For the user, after "<path>: ". The other file is named only when
-- it isn't the path (a symlink's target).
renderSaveProblem :: FilePath -> SaveProblem -> Text
renderSaveProblem path = \case
  CannotRead e -> "cannot read it: " <> e
  CannotMoveAside target e
    | target == path -> "cannot move it aside: " <> e
    | otherwise -> "cannot move " <> Text.pack target <> " aside: " <> e
  CannotWrite e
    | e.target == path -> e.reason
    | otherwise -> renderAtomicWriteError e

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
  -- 'Just' once a save failed: the preferences from then on, in memory
  -- only.
  fallback <- newIORef Nothing
  -- Built here, not first on a request thread (Q-RTS-BLACKHOLE). Text
  -- is strict: this forces the path too.
  pathText <- evaluate (Text.pack path)
  newer <- evaluate (pathText <> " is from a newer version; not saving preferences")
  let onceIn ref msg = do
        already <- atomicModifyIORef' ref (\s -> (Set.insert msg s, Set.member msg s))
        unless already (logInfo logger ("haskell-reviewer: " <> msg))
      sayOnce = onceIn said
      current =
        readIORef fallback >>= \case
          Just p -> pure (effective defaults p)
          Nothing -> readFromFile
      readFromFile = do
        loaded <- load
        effective defaults <$> case loaded of
          Loaded d -> pure d.file.prefs
          Missing -> pure noPrefs
          Newer filePrefs -> do
            sayOnce newer
            (`overridePrefs` filePrefs) <$> readIORef inMemory
          Bad bad -> noPrefs <$ sayOnce (badLine bad "using the defaults")
          Unreadable e -> noPrefs <$ sayOnce ("cannot read " <> pathText <> ": " <> e)
      update patch = withMVar lock $ \() ->
        readIORef fallback >>= \case
          Just p -> do
            -- Forced: GETs read it without the lock (Q-RTS-BLACKHOLE).
            new <- evaluate (applyPatch patch p)
            writeIORef fallback (Just new)
            pure (effective defaults new)
          Nothing -> toFile patch
      toFile patch = do
        loaded <- load
        case loaded of
          Newer filePrefs -> do
            sayOnce newer
            mine <- atomicModifyIORef' inMemory (\p -> dup (applyPatch patch p))
            pure (effective defaults (overridePrefs mine filePrefs))
          Loaded d -> do
            -- Not those the patch replaces.
            let kept = [b | b <- d.oddValues, b.key `notElem` patchKeys patch]
                odd' b = pathText <> ": " <> renderBadValue b <> "; kept as is"
            mapM_ (onceIn saidOdd . odd') kept
            save patch d.file
          Missing -> save patch emptyPrefsFile
          Bad bad ->
            moveAside >>= \case
              Left err -> toMemory patch noPrefs err
              Right moved -> do
                -- Nothing: another instance moved it first.
                mapM_
                  ( \m ->
                      logInfo logger $
                        "haskell-reviewer: " <> badLine bad ("moved to " <> Text.pack m)
                  )
                  moved
                save patch emptyPrefsFile
          -- What GET showed: the defaults.
          Unreadable e -> toMemory patch noPrefs (CannotRead e)
      save patch file = do
        let new = applyPatchFile patch file
        writeFileAtomic path (encodePrefsFile new) >>= \case
          Left e -> toMemory patch file.prefs (CannotWrite e)
          Right () -> do
            writeIORef said Set.empty
            pure (effective defaults new.prefs)
      -- From now on, memory only. See Note [Saving preferences].
      -- Starts from what we knew, with this session's changes kept for a
      -- newer file (if any) on top.
      toMemory patch known why = do
        mine <- readIORef inMemory
        new <- evaluate (applyPatch patch (overridePrefs mine known))
        writeIORef fallback (Just new)
        logInfo logger $
          "haskell-reviewer: preferences won't be remembered: "
            <> pathText
            <> ": "
            <> renderSaveProblem path why
        pure (effective defaults new)
      badLine bad what = pathText <> ": " <> renderBadPrefsFile bad <> "; " <> what
      -- Next to the file a symlink points to, like the write. Gives the
      -- new name, or 'Nothing' if the file is gone (another instance
      -- moved it first).
      moveAside = do
        target <- resolveLink path
        let moved = target <> ".bad"
        try @IOException (Dir.renameFile target moved) >>= \case
          Left e
            | isDoesNotExistError e -> pure (Right Nothing)
            | otherwise -> pure (Left (CannotMoveAside target (ioErrorMessage e)))
          Right () -> pure (Right (Just moved))
      -- Only a regular file, like config.toml: a FIFO would block.
      load = do
        result <- try @IOException (readRegularFile prefsSizeLimit path)
        pure $ case result of
          Left e
            | isDoesNotExistError e -> Missing
            | otherwise -> Unreadable (ioErrorMessage e)
          Right Nothing -> Unreadable "not a regular file"
          Right (Just bytes)
            | BS.length bytes > prefsSizeLimit -> Bad PrefsTooLarge
            | otherwise -> case decodePrefsFile bytes of
                Left bad -> Bad bad
                Right (PrefsCurrent d) -> Loaded d
                Right (PrefsNewer p) -> Newer p
  pure PrefsStore {current = current, update = update}

dup :: a -> (a, a)
dup x = (x, x)
