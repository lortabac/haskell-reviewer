{-# LANGUAGE MultiWayIf #-}

-- | Reading git objects through long-lived @git cat-file
-- --batch-command@ processes. See Note [Object reader].
module HaskellReviewer.Effects.GitObjects
  ( -- * Types
    ObjectReader,
    ReaderSettings (..),
    BlobBytes (..),
    ObjectInfo (..),
    ObjectError (..),
    NotABlob (..),
    ReadFailure (..),
    ReadProblem (..),

    -- * Operations
    defaultReaderSettings,
    withObjectReader,
    readBlob,
    objectInfo,
    renderObjectError,

    -- * Warm-up
    warmUpReader,
  )
where

import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, waitCatch)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception
  ( IOException,
    SomeException,
    bracket,
    evaluate,
    finally,
    mask,
    mask_,
    throwIO,
    try,
    uninterruptibleMask_,
  )
import Control.Monad (forM, forM_, replicateM_, unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import GHC.IO.Exception (IOException (..))
import HaskellReviewer.Core.FileContent (FileSize (..))
import HaskellReviewer.Core.Git
  ( CatFileHeader (..),
    FoundHeader (..),
    ObjectId (..),
    ObjectType (..),
    parseCatFileHeader,
    parseObjectId,
    renderObjectType,
  )
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.Git (CannotRun (..), Git (..), gitProcess)
import HaskellReviewer.Effects.Log (Logger, logInfo)
import System.IO
  ( BufferMode (BlockBuffering),
    Handle,
    hClose,
    hFlush,
    hSetBinaryMode,
    hSetBuffering,
  )
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Types (CPid)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (CreatePipe),
    createProcess,
    getPid,
    waitForProcess,
  )
import System.Timeout (timeout)

{- Note [Object reader]

Commit sides read blobs through a small pool of @git cat-file
--batch-command@ processes (D-311: 2 of them), hardened as every git
call (Note [Git hardening] in "HaskellReviewer.Effects.Git"). We only
send @contents <id>@ and @info <id>@, with an id that is 40 or 64
lowercase hex digits: we check it before writing, so a crafted id can't
add a command (@flush@, @remote-object-info@). The check is in the
reader, not only in the parsers: ids from ls-files are taken as they
are, and 'ObjectId' has a public constructor.

The pool:

- Each slot holds at most one process. A free slot waits in a 'Chan':
  holding it means you own its process. Threads blocked on the Chan are
  served in FIFO order.
- A process starts on the first read of its slot, so nothing runs if
  nothing reads.
- A read has a time limit (30 s by default), counting a start and the
  drain of a big blob.
- A blob over the caller's cap is read in 64 KiB chunks and dropped, so
  memory stays bounded and the pipe stays in sync.
- stderr is drained by its own thread, so a full pipe can't block git.
  We keep the last 4 KiB: git's fatal line is at the end.

Any failure in a read kills and reaps the process and empties the slot:
a crash, the time limit, a bad header, a header for another id, a short
body, no LF after it, or an exception (warp cancelling a request). The
pipe may be out of sync after that, so we never reuse it. The next read
starts a fresh process. We don't retry the read that failed, with one
exception: if the write fails (the process died while idle), we start a
new one and try once more.

On git 2.44.1, cat-file dies on a blob missing from a partial clone
("could not fetch <id> from promisor remote"). A death with that line
for the id we asked is 'ObjectMissing', not a read error (D-311,
D-312). A line for another id doesn't count. git 2.56 just says
@<id> missing@.

Kill and reap. A pid is signalled only while it is in the registry, and
only its owner reaps it, after taking it out of the registry under the
registry lock. So we never signal a pid that was reaped and maybe
reused.

Stop ('withObjectReader' returns):

1. mark the reader closed and SIGKILL every live process, under the
   registry lock (no new process can start after that);
2. take every slot: quick now, a read in flight gets EOF. We wait at
   most 5 s: a read that still holds its slot has its process killed
   already, and its owner reaps it;
3. reap and close what is left, then give the slots back, so any later
   read gets 'ObjectReaderClosed' instead of waiting for ever.

Stop must handle slots in use: request threads can outlive the server.

A slot is given back under 'uninterruptibleMask_'. 'writeChan' can
block for a moment on the Chan's lock, and an exception there (warp
cancelling a request, a caller's timeout) would lose the slot: lose
them all and every read waits for ever. We have no test that hits that
moment for sure.
-}

-- | The pool. See Note [Object reader].
data ObjectReader = ObjectReader
  { git :: Git,
    settings :: ReaderSettings,
    free :: Chan Slot,
    slots :: [Slot],
    registry :: MVar Registry
  }

-- | How big the pool is and how long a read may take.
data ReaderSettings = ReaderSettings
  { -- | Processes in the pool, at least 1.
    handles :: Int,
    -- | Per read, a start included.
    readTimeLimit :: Microseconds
  }

-- | 2 processes (D-311), 30 s per read.
defaultReaderSettings :: ReaderSettings
defaultReaderSettings =
  ReaderSettings {handles = 2, readTimeLimit = Microseconds 30_000_000}

-- | A blob, cut at the caller's cap.
data BlobBytes = BlobBytes
  { -- | The full size.
    size :: FileSize,
    -- | At most the cap.
    bytes :: ByteString
  }
  deriving (Eq, Show)

-- | What @info@ says.
data ObjectInfo = ObjectInfo
  { kind :: ObjectType,
    size :: FileSize
  }
  deriving (Eq, Show)

-- | Why an object can't be read.
data ObjectError
  = -- | Not in the local repo (partial clone).
    ObjectMissing ObjectId
  | -- | 'readBlob' on a tree, commit or tag.
    ObjectNotABlob NotABlob
  | -- | Not 40 or 64 lowercase hex digits. Never sent to git.
    ObjectBadId Text
  | ObjectReadFailed ReadFailure
  | -- | The reader was stopped.
    ObjectReaderClosed
  deriving (Eq, Show)

-- | An object of another type.
data NotABlob = NotABlob
  { oid :: ObjectId,
    kind :: ObjectType
  }
  deriving (Eq, Show)

-- | A read that failed. The process was killed; the next read starts
-- a new one.
data ReadFailure = ReadFailure
  { oid :: ObjectId,
    problem :: ReadProblem
  }
  deriving (Eq, Show)

-- | What went wrong in a read.
data ReadProblem
  = CannotStart CannotRun
  | -- | It exited. The end of its stderr, at most 4 KiB.
    Died Text
  | TimedOut
  | -- | An answer we can't read, or for another id.
    BadAnswer Text
  deriving (Eq, Show)

-- | Message for the user.
renderObjectError :: ObjectError -> Text
renderObjectError = \case
  ObjectMissing o -> "object " <> hex o <> " is not in the local repo"
  ObjectNotABlob n -> "object " <> hex n.oid <> " is a " <> renderObjectType n.kind
  ObjectBadId t -> "bad object id: " <> t
  ObjectReadFailed f -> "cannot read object " <> hex f.oid <> ": " <> problem f.problem
  ObjectReaderClosed -> "the object reader is stopped"
  where
    hex (ObjectId o) = o
    problem = \case
      CannotStart c -> "cannot run git at " <> Text.pack c.exe <> ": " <> c.reason
      Died err
        | Text.null err -> "git cat-file died"
        | otherwise -> "git cat-file died: " <> err
      TimedOut -> "git cat-file took too long"
      BadAnswer t -> "bad answer from git cat-file: " <> t

-- | Run with a pool, then kill every process left. See Note [Object
-- reader]. The logger gets a line if stop can't get every slot back.
withObjectReader :: Logger -> ReaderSettings -> Git -> (ObjectReader -> IO a) -> IO a
withObjectReader logger settings git =
  bracket (newReader settings git) (stopReader logger)

-- | A blob's bytes, at most the given number (the caller passes its
-- text limit + 1), and its full size.
readBlob :: ObjectReader -> Int -> ObjectId -> IO (Either ObjectError BlobBytes)
readBlob r cap oid = ask r oid (\c -> talkContents c oid cap)

-- | An object's type and size.
objectInfo :: ObjectReader -> ObjectId -> IO (Either ObjectError ObjectInfo)
objectInfo r oid = ask r oid (\c -> talkInfo c oid)

-- | Build the shared values of a read before workers and request
-- threads read at once (Q-RTS-BLACKHOLE): the header parser on every
-- arm, the id check, the death check and every message. Run before
-- warp. Throws if a sample doesn't parse as it should (a bug).
--
-- Not warmed: the talk path (the verb literals, talkContents,
-- talkInfo). It needs a live process, so a read before the URL. A
-- process start is serialised under the registry lock anyway.
warmUpReader :: IO ()
warmUpReader = do
  forM_ headers $ \(line, ok) ->
    case parseCatFileHeader line of
      Right h | ok -> () <$ evaluate (length (show h))
      Left e | not ok -> () <$ evaluate (Text.length e)
      other -> ioError (userError ("warmUpReader: " <> show other))
  forM_ [sha1, sha256] $ \o -> () <$ evaluate (length (show (parseObjectId o)))
  _ <- evaluate (promisorDeath oid "fatal: could not fetch x from promisor remote")
  forM_ errors $ \e -> evaluate (Text.length (renderObjectError e))
  where
    sha1 = BS.replicate 40 0x61
    sha256 = BS.replicate 64 0x62
    oid = ObjectId (Text.Encoding.decodeUtf8Lenient sha1)
    headers =
      [ (sha1 <> " blob 12", True),
        (sha1 <> " tree 30", True),
        (sha1 <> " commit 200", True),
        (sha1 <> " tag 140", True),
        (sha256 <> " blob 0", True),
        (sha1 <> " missing", True),
        (sha1 <> " ambiguous", False),
        ("junk", False)
      ]
    cannot = CannotRun {exe = "/usr/bin/git", reason = "no such file"}
    errors =
      [ObjectMissing oid, ObjectBadId "x", ObjectReaderClosed]
        <> [ ObjectNotABlob NotABlob {oid = oid, kind = k}
           | k <- [TypeBlob, TypeTree, TypeCommit, TypeTag]
           ]
        <> [ failed oid p
           | p <- [CannotStart cannot, Died "", Died "fatal: x", TimedOut, BadAnswer "x"]
           ]

-- * Internal

-- | One place in the pool.
data Slot = Slot
  { number :: Int,
    process :: IORef (Maybe CatFile)
  }

-- | The processes we may signal. See Note [Object reader].
data Registry = Registry
  { state :: ReaderState,
    -- | By slot number.
    live :: Map Int CPid
  }

data ReaderState = Open | Closed

-- | One running cat-file.
data CatFile = CatFile
  { input :: Handle,
    output :: Input,
    errors :: Handle,
    handle :: ProcessHandle,
    pid :: Maybe CPid,
    -- | The last 4 KiB of stderr.
    stderr :: Async ByteString
  }

-- | git's stdout with our own buffer, so a line can be read with a cap.
data Input = Input
  { from :: Handle,
    buffer :: IORef ByteString
  }

-- | The process is no use any more.
data Trouble
  = -- | It was gone before we wrote.
    WriteFailed
  | -- | EOF: it died.
    Ended
  | Garbled Text

-- | Whether a failed write may be tried again on a new process.
data Retry = MayRetry | NoRetry

newReader :: ReaderSettings -> Git -> IO ObjectReader
newReader settings git = do
  slots <- forM [1 .. max 1 settings.handles] $ \n -> Slot n <$> newIORef Nothing
  free <- newChan
  mapM_ (writeChan free) slots
  registry <- newMVar Registry {state = Open, live = Map.empty}
  pure
    ObjectReader
      { git = git,
        settings = settings,
        free = free,
        slots = slots,
        registry = registry
      }

stopReader :: Logger -> ObjectReader -> IO ()
stopReader logger r = do
  uninterruptibleMask_ $ modifyMVar_ r.registry $ \reg -> do
    mapM_ kill (Map.elems reg.live)
    pure reg {state = Closed}
  -- A slot taken here is kept, even if the time limit hits right after.
  taken <- newIORef []
  _ <-
    timeout stopTimeLimit . replicateM_ (length r.slots) . mask_ $ do
      slot <- readChan r.free
      modifyIORef' taken (slot :)
  got <- readIORef taken
  unless (length got == length r.slots) $
    logInfo logger "git cat-file: a read still held its slot at stop (its git was killed)"
  mapM_ (void . dropSlot r) got
  mapM_ (uninterruptibleMask_ . writeChan r.free) got

-- | How long stop waits for slots in use. The kills are sent already.
stopTimeLimit :: Int
stopTimeLimit = 5_000_000

-- | Send one request, in a slot, within the time limit. The id is
-- checked here, not only at parse: see Note [Object reader].
ask ::
  ObjectReader ->
  ObjectId ->
  (CatFile -> IO (Either Trouble (Either ObjectError a))) ->
  IO (Either ObjectError a)
ask r oid@(ObjectId t) talk
  | Nothing <- parseObjectId (Text.Encoding.encodeUtf8 t) = pure (Left (ObjectBadId t))
  | otherwise = do
      reg <- readMVar r.registry
      case reg.state of
        Closed -> pure (Left ObjectReaderClosed)
        Open -> withSlot r $ \slot -> do
          let Microseconds limit = r.settings.readTimeLimit
          outcome <- timeout limit (attempt slot MayRetry)
          case outcome of
            Just a -> pure a
            Nothing -> do
              _ <- dropSlot r slot
              closedOr (failed oid TimedOut)
  where
    attempt slot retry = do
      started <- catFileOf r oid slot
      case started of
        Left e -> pure (Left e)
        Right c -> do
          talked <- try @IOException (talk c)
          case (talked, retry) of
            (Right (Right answer), _) -> pure answer
            (Right (Left WriteFailed), MayRetry) -> do
              _ <- dropSlot r slot
              attempt slot NoRetry
            (Right (Left (Garbled g)), _) -> do
              _ <- dropSlot r slot
              closedOr (failed oid (BadAnswer g))
            -- It died: before the write, at EOF, or an IO error.
            _ -> do
              err <- dropSlot r slot
              closedOr $
                if promisorDeath oid err
                  then ObjectMissing oid
                  else failed oid (Died err)
    -- A read cut short by stop is not a read error.
    closedOr e = do
      reg <- readMVar r.registry
      pure . Left $ case reg.state of
        Closed -> ObjectReaderClosed
        Open -> e

-- | git 2.44.1 died on this blob, missing from a partial clone.
promisorDeath :: ObjectId -> Text -> Bool
promisorDeath (ObjectId t) err =
  ("could not fetch " <> t <> " from promisor remote") `Text.isInfixOf` err

failed :: ObjectId -> ReadProblem -> ObjectError
failed oid p = ObjectReadFailed ReadFailure {oid = oid, problem = p}

-- | Take a free slot, use it, give it back. On an exception the pipe
-- may be out of sync: the process is dropped. Giving the slot back
-- can't be interrupted: see Note [Object reader].
withSlot :: ObjectReader -> (Slot -> IO a) -> IO a
withSlot r act = mask $ \restore -> do
  slot <- readChan r.free
  result <- try @SomeException (restore (act slot))
  case result of
    Left _ -> void (try @SomeException (dropSlot r slot))
    Right _ -> pure ()
  uninterruptibleMask_ (writeChan r.free slot)
  either throwIO pure result

-- | The slot's process, started if there is none.
catFileOf :: ObjectReader -> ObjectId -> Slot -> IO (Either ObjectError CatFile)
catFileOf r oid slot = do
  current <- readIORef slot.process
  case current of
    Just c -> pure (Right c)
    Nothing -> start
  where
    start = uninterruptibleMask_ $ modifyMVar r.registry $ \reg -> case reg.state of
      Closed -> pure (reg, Left ObjectReaderClosed)
      Open -> do
        let cp =
              (gitProcess r.git ["cat-file", "--batch-command"])
                { std_in = CreatePipe,
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
        made <- try @IOException (createProcess cp)
        case made of
          Left e -> pure (reg, Left (cannotStart (reasonOf e)))
          Right (mi, mo, me, ph) -> do
            pid <- getPid ph
            -- If any of this fails, git is killed here: it is in
            -- neither the registry nor the slot yet.
            setUp <- try @SomeException (setUpPipes mi mo me ph pid)
            case setUp of
              Right c -> do
                writeIORef slot.process (Just c)
                let live = maybe reg.live (\p -> Map.insert slot.number p reg.live) pid
                pure (reg {live = live}, Right c)
              Left e -> do
                mapM_ kill pid
                void (try @IOException (waitForProcess ph))
                forM_ (catMaybes [mi, mo, me]) $ \h -> void (try @IOException (hClose h))
                pure (reg, Left (cannotStart (Text.pack (show e))))
    setUpPipes mi mo me ph pid = case (mi, mo, me) of
      (Just i, Just o, Just e) -> do
        mapM_ (`hSetBinaryMode` True) [i, o, e]
        hSetBuffering i (BlockBuffering Nothing)
        buffer <- newIORef BS.empty
        drain <- asyncWithUnmask (\unmask -> unmask (drainErrors e))
        pure
          CatFile
            { input = i,
              output = Input {from = o, buffer = buffer},
              errors = e,
              handle = ph,
              pid = pid,
              stderr = drain
            }
      _ -> ioError (userError "no pipes")
    cannotStart why = failed oid (CannotStart CannotRun {exe = r.git.exe, reason = why})
    reasonOf e
      | null e.ioe_description = Text.pack (show e)
      | otherwise = Text.pack e.ioe_description

-- | Kill and reap the slot's process, if any, and empty the slot.
-- Gives back what it printed on stderr. See Note [Object reader].
dropSlot :: ObjectReader -> Slot -> IO Text
dropSlot r slot = mask_ $ do
  current <- readIORef slot.process
  case current of
    Nothing -> pure ""
    Just c -> do
      uninterruptibleMask_ $ do
        modifyMVar_ r.registry $ \reg -> do
          when (Map.member slot.number reg.live) (mapM_ kill c.pid)
          pure reg {live = Map.delete slot.number reg.live}
        void (try @IOException (waitForProcess c.handle))
        writeIORef slot.process Nothing
      -- Close stdin and stdout first: a child of git that holds stderr
      -- then sees EOF and goes. git itself is gone, so stderr is at EOF
      -- now, unless such a child still holds it.
      err <-
        ( do
            forM_ [c.input, c.output.from] $ \h -> void (try @IOException (hClose h))
            timeout 1_000_000 (waitCatch c.stderr)
        )
          `finally` (cancel c.stderr >> void (try @IOException (hClose c.errors)))
      pure $ case err of
        Just (Right e) -> Text.stripEnd (Text.Encoding.decodeUtf8Lenient e)
        _ -> ""

kill :: CPid -> IO ()
kill p = void (try @IOException (signalProcess sigKILL p))

-- | Most header bytes we take.
maxHeader :: Int
maxHeader = 256

-- | Most stderr we keep: the last bytes, where git's fatal line is.
maxStderr :: Int
maxStderr = 4096

drainErrors :: Handle -> IO ByteString
drainErrors h = go BS.empty
  where
    go kept = do
      chunk <- BS.hGetSome h 4096
      if BS.null chunk
        then pure kept
        else go (BS.takeEnd maxStderr (kept <> chunk))

-- | Send one line, read the header. The header must be for this id.
request :: CatFile -> ByteString -> ObjectId -> IO (Either Trouble CatFileHeader)
request c verb oid@(ObjectId t) = do
  wrote <-
    try @IOException $ do
      BS.hPut c.input (verb <> " " <> Text.Encoding.encodeUtf8 t <> "\n")
      hFlush c.input
  case wrote of
    Left _ -> pure (Left WriteFailed)
    Right () -> do
      header <- getHeader c.output
      pure $ case header of
        LineEnded -> Left Ended
        LineTooLong -> Left (Garbled "header too long")
        Line l -> case parseCatFileHeader l of
          Left e -> Left (Garbled e)
          Right h@(HeaderMissing o) | o == oid -> Right h
          Right h@(HeaderFound f) | f.oid == oid -> Right h
          Right _ -> Left (Garbled ("answer for another object: " <> lenient l))

-- | @info <id>@.
talkInfo :: CatFile -> ObjectId -> IO (Either Trouble (Either ObjectError ObjectInfo))
talkInfo c oid = do
  header <- request c "info" oid
  pure $ case header of
    Left trouble -> Left trouble
    Right (HeaderMissing _) -> Right (Left (ObjectMissing oid))
    Right (HeaderFound h) -> Right (Right ObjectInfo {kind = h.kind, size = h.size})

-- | @contents <id>@, keeping at most cap bytes. A non-blob is drained
-- too, so the pipe stays in sync.
talkContents ::
  CatFile -> ObjectId -> Int -> IO (Either Trouble (Either ObjectError BlobBytes))
talkContents c oid cap = do
  header <- request c "contents" oid
  case header of
    Left trouble -> pure (Left trouble)
    Right (HeaderMissing _) -> pure (Right (Left (ObjectMissing oid)))
    Right (HeaderFound h) -> do
      let FileSize n = h.size
          keep = if h.kind == TypeBlob then min n (max 0 cap) else 0
      bytes <- getExactly c.output keep
      skipped <- skipBytes c.output (n - BS.length bytes)
      lf <- getExactly c.output 1
      pure $
        if
          | BS.length bytes + skipped /= n || BS.null lf -> Left Ended
          | lf /= "\n" -> Left (Garbled "no LF after the object")
          | h.kind /= TypeBlob ->
              Right (Left (ObjectNotABlob NotABlob {oid = oid, kind = h.kind}))
          | otherwise -> Right (Right BlobBytes {size = h.size, bytes = bytes})

data Line = Line ByteString | LineTooLong | LineEnded

-- | A line without its LF, at most 'maxHeader' bytes.
getHeader :: Input -> IO Line
getHeader i = readIORef i.buffer >>= go
  where
    go buf = case BS.elemIndex 10 buf of
      Just k -> do
        writeIORef i.buffer (BS.drop (k + 1) buf)
        pure (Line (BS.take k buf))
      Nothing
        | BS.length buf > maxHeader -> pure LineTooLong
        | otherwise -> do
            more <- BS.hGetSome i.from 65536
            if BS.null more
              then writeIORef i.buffer buf >> pure LineEnded
              else go (buf <> more)

-- | Exactly n bytes, or fewer at EOF.
getExactly :: Input -> Int -> IO ByteString
getExactly i n = do
  buf <- readIORef i.buffer
  if BS.length buf >= n
    then do
      writeIORef i.buffer (BS.drop n buf)
      pure (BS.take n buf)
    else do
      writeIORef i.buffer BS.empty
      rest <- BS.hGet i.from (n - BS.length buf)
      pure (buf <> rest)

-- | Read and drop n bytes, in chunks. Gives back how many there were.
skipBytes :: Input -> Int -> IO Int
skipBytes i n = do
  buf <- readIORef i.buffer
  let fromBuf = min n (BS.length buf)
  writeIORef i.buffer (BS.drop fromBuf buf)
  go fromBuf
  where
    go done
      | done >= n = pure done
      | otherwise = do
          chunk <- BS.hGetSome i.from (min 65536 (n - done))
          if BS.null chunk then pure done else go (done + BS.length chunk)

lenient :: ByteString -> Text
lenient = Text.Encoding.decodeUtf8Lenient
