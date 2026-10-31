-- | Replace a file in one step: readers see the old file or the new
-- one, never half of it.
module HaskellReviewer.Effects.AtomicFile
  ( -- * Types
    AtomicWriteError (..),

    -- * Writing
    writeFileAtomic,
    resolveLink,
    renderAtomicWriteError,

    -- * Internal, for tests
    writeFileAtomicWith,
    discardTemp,
  )
where

import Control.Exception (IOException, bracketOnError, finally, try)
import Control.Monad (unless, void)
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Effects.Log (ioErrorMessage)
import System.Directory qualified as Dir
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hClose, hFlush, openBinaryTempFile)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Directory qualified as Posix
import System.Posix.IO
  ( OpenMode (ReadOnly),
    closeFd,
    defaultFileFlags,
    handleToFd,
    openFd,
  )
import System.Posix.Unistd (fileSynchronise)

{- Note [Atomic write]

D-200. 'writeFileAtomic' writes a temp file in the same dir (so the
rename never crosses file systems), flushes it to disk, renames it
over the target, then syncs the dir (best effort: some file systems
refuse). A crash leaves the old file or the new one.

The temp file is created with mode 0600 (openBinaryTempFile). On any
error, or an async exception, it is removed, even when closing it
fails too (a full disk fails the write and then the close). There is
no fallback to a plain write: a target we can't rename over (a
bind-mounted file) is an error.

Dirs we have to make get mode 0700, as the XDG spec asks for the state
dir. Dirs that are there already are left alone.

If the target is a symlink, we write next to the file it points to
and keep the link.
-}

-- | A write that failed. Nothing changed on disk, except maybe new
-- dirs.
data AtomicWriteError = AtomicWriteError
  { -- | The file we were writing (after links).
    target :: FilePath,
    reason :: Text
  }
  deriving (Eq, Show)

-- | Write the bytes to the file. Makes the parent dirs. See Note
-- [Atomic write].
writeFileAtomic :: FilePath -> LBS.ByteString -> IO (Either AtomicWriteError ())
writeFileAtomic = writeFileAtomicWith LBS.hPut

-- | The file a symlink points to, all links followed. Any other path
-- (or a link we can't follow) as is. Never throws.
resolveLink :: FilePath -> IO FilePath
resolveLink path = do
  isLink <- fromRight False <$> try @IOException (Dir.pathIsSymbolicLink path)
  if isLink
    then fromRight path <$> try @IOException (Dir.canonicalizePath path)
    else pure path

-- | @\<file>: \<reason>@.
renderAtomicWriteError :: AtomicWriteError -> Text
renderAtomicWriteError e = Text.pack e.target <> ": " <> e.reason

-- | 'writeFileAtomic' with the given way to write the bytes. Tests make
-- it fail.
writeFileAtomicWith ::
  (Handle -> LBS.ByteString -> IO ()) ->
  FilePath ->
  LBS.ByteString ->
  IO (Either AtomicWriteError ())
writeFileAtomicWith write path bytes = do
  target <- resolveLink path
  let dir = takeDirectory target
  result <- try @IOException $ do
    makeDirs dir
    bracketOnError
      (openBinaryTempFile dir (takeFileName target <> ".tmp"))
      (\(tmp, h) -> discardTemp tmp h)
      $ \(tmp, h) -> do
        write h bytes
        hFlush h
        -- Closes the handle; we close the fd.
        fd <- handleToFd h
        fileSynchronise fd `finally` closeFd fd
        Dir.renameFile tmp target
    syncDir dir
  pure $ case result of
    Right () -> Right ()
    Left e -> Left AtomicWriteError {target = target, reason = ioErrorMessage e}

-- | Close and remove a temp file after a failure. The removal happens
-- even if the close throws (it does, after a failed write: it flushes
-- again). Never throws.
discardTemp :: FilePath -> Handle -> IO ()
discardTemp tmp h =
  void (try @IOException (hClose h)) `finally` removeQuietly tmp

-- * Internal

-- | Like createDirectoryIfMissing True, but new dirs get mode 0700.
makeDirs :: FilePath -> IO ()
makeDirs dir = do
  there <- Dir.doesDirectoryExist dir
  unless there $ do
    let parent = takeDirectory dir
    unless (parent == dir) (makeDirs parent)
    -- Someone else may make it meanwhile: fine.
    try @IOException (Posix.createDirectory dir 0o700) >>= \case
      Left e | not (isAlreadyExistsError e) -> ioError e
      _ -> pure ()

removeQuietly :: FilePath -> IO ()
removeQuietly p = void (try @IOException (Dir.removeFile p))

-- | So the rename itself is on disk. Some file systems can't: ignored.
syncDir :: FilePath -> IO ()
syncDir dir = void . try @IOException $ do
  fd <- openFd dir ReadOnly defaultFileFlags
  fileSynchronise fd `finally` closeFd fd
