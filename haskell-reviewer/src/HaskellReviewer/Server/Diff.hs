-- | GET api\/diff\/\<path…>: one file's line diff, base against head
-- (D-331). See Note [Diff route].
module HaskellReviewer.Server.Diff
  ( -- * Types
    DiffRoutes (..),
    GuardedDiff (..),

    -- * Route
    diffApp,

    -- * Line diff
    guardedLineDiff,
    diffTimeLimit,

    -- * Warm-up
    warmUpDiff,
  )
where

import Control.Exception
  ( SomeAsyncException,
    SomeException,
    evaluate,
    fromException,
    throwIO,
    try,
  )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import HaskellReviewer.Core.DiffTarget
  ( Target (..),
    findTarget,
    targetPath,
    targetPaths,
  )
import HaskellReviewer.Core.Eol (Filtering (..))
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileNotes (..),
    FileSize (..),
    LinkInfo (..),
    TooLargeInfo (..),
  )
import HaskellReviewer.Core.FileList (CommitId (..), EntryKind (..), FileList (..))
import HaskellReviewer.Core.LineDiff
  ( BomChange (..),
    LineDiff (..),
    Op (..),
    Run (..),
    Whitespace (..),
    lineDiff,
    warmUpLineDiff,
  )
import HaskellReviewer.Core.RelPath
  ( RelPath,
    parseRelPath,
    relPathFromSegments,
    relPathText,
  )
import HaskellReviewer.Core.Renames
  ( Change (..),
    ChangeNote (..),
    ExecChange (..),
    ModifiedFile (..),
    RenamedFile (..),
  )
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Core.Similarity (Score (..))
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.FileSystem (ReadError (..))
import HaskellReviewer.Effects.Log (exceptionMessage, logInfo)
import HaskellReviewer.Effects.Source (sourceBlob)
import HaskellReviewer.Server.Api (FilesApi (..), cbor, jsonWith, readFailed, serverError)
import HaskellReviewer.Server.Security (methodNotAllowed, notFound)
import HaskellReviewer.Server.Wire (encodeFileNotFound)
import HaskellReviewer.Server.Wire.Diff
  ( DiffBody (..),
    DiffNote (..),
    DiffStatus (..),
    DiffView (..),
    RenameInfo (..),
    SideView (..),
    encodeDiffView,
  )
import HaskellReviewer.Session.Diff
  ( DiffProblem (..),
    DiffReady (..),
    awaitDiff,
  )
import HaskellReviewer.Session.Index (withRequest)
import HaskellReviewer.Session.Live (DiffLive (..))
import HaskellReviewer.Session.Snapshot
  ( Snapshot (..),
    awaitSnapshot,
    firstGeneration,
  )
import Network.HTTP.Types
  ( hContentType,
    methodGet,
    parseQueryReplacePlus,
    status400,
    status404,
  )
import Network.Wai (Application, Request (..), Response, responseLBS)
import System.Timeout (timeout)

{- Note [Diff route]

GET api/diff/<path…>[?ws=ignore]: the diff of one file, as CBOR (Note
[Diff view] in "HaskellReviewer.Server.Wire.Diff").

- Not GET: 405 (HEAD too). Then browse mode: a plain 404.
- Then we wait for the pairing. Not listed: 500 "cannot list files".
  Pairing failed: 500 "cannot pair files". The wait comes first, so
  nothing below runs before the warm-up (Q-RTS-BLACKHOLE).
- ws: none keeps whitespace, "ignore" is git's -w. Anything else, or ws
  twice: a plain 400.
- The path, head or base: Note [Diff target] in
  "HaskellReviewer.Core.DiffTarget". A bad path, or one on neither
  side: a 404 with the source route's JSON body. Status and note come
  from the pairing, as in api/diff-files. So with ws=ignore a
  whitespace-only edit is still modified, with only equal runs.
- A path on both sides that the pairing doesn't list: unchanged if both
  sides have a blob id (or a submodule commit) and they are equal.
  Without ids (a disk side) a text or a symlink target is compared;
  binary and too large count as modified. Kinds that differ: modified,
  type-changed. A mode-only change shows as unchanged (the list has no
  exec bit).
- Base is read, then head, each from its own snapshot; the first
  problem stops. The pairing and both snapshots come from one read of
  the current generation, so they always match (Note [Live] in
  "HaskellReviewer.Session.Live"). Gone: the 404. A read error:
  logged, 500 "cannot read file". A blob missing from a partial clone:
  the side is not-local.
- Text is diffed: a file's text, a symlink's target, "" for a side
  without the file. Anything else (binary, too large, submodule, not
  local) on either side: not-text.
- The line diff runs with a time limit (Note [Diff time limit]). If it
  throws: logged, 500 "cannot diff file".

The line diff and the encode run inside 'withRequest' on head's index:
a request takes a core, as the module page does. The reads don't, as
for Markdown.

No cache: each request diffs again (about 50 ms for a big file, D-321).
The client keeps answers by generation. Nothing lazy is kept between
requests (Q-RTS-BLACKHOLE).
-}

{- Note [Diff time limit]

A line diff that takes over 2 s ('diffTimeLimit') gives up: the answer
is "too-slow". Normal files take a few ms. Hostile input is slow: 1 MiB
of 500k lines of 8 letters takes 13.9 s (git: 2.2 s, D-321).

System.Timeout throws an async exception at the next heap check. The
line diff allocates in its loops, so it stops soon after the limit; a
test checks that on the hostile input. The result is forced inside the
limit. A stopped diff leaves only its own thunks: nothing else holds
them.
-}

-- | What the diff route needs, in diff mode.
data DiffRoutes = DiffRoutes
  { -- | The current generation's pairing and sides, read at once: a
    -- request never mixes two generations (Note [Live] in
    -- "HaskellReviewer.Session.Live").
    current :: IO DiffLive,
    -- | Production: 'guardedLineDiff' 'diffTimeLimit'.
    lineDiff :: Whitespace -> Pair Text -> IO GuardedDiff
  }

-- | A line diff with a time limit.
data GuardedDiff
  = DiffDone LineDiff
  | DiffTimedOut

-- | 2 s. See Note [Diff time limit].
diffTimeLimit :: Microseconds
diffTimeLimit = Microseconds 2_000_000

-- | 'lineDiff', forced, within this time. Exceptions go through. See
-- Note [Diff time limit].
guardedLineDiff :: Microseconds -> Whitespace -> Pair Text -> IO GuardedDiff
guardedLineDiff (Microseconds limit) ws p =
  maybe DiffTimedOut DiffDone <$> timeout limit run
  where
    run = do
      let d = lineDiff ws p
      _ <- evaluate (forceLineDiff d)
      pure d

-- | See Note [Diff route]. 'Nothing': browse mode.
diffApp :: FilesApi -> Maybe DiffRoutes -> [Text] -> Application
diffApp api routes segs req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | Nothing <- routes = respond notFound
  | Just r <- routes = do
      live <- r.current
      awaitDiff live.pairing >>= \case
        Left DiffNotListed -> respond cannotList
        Left (DiffNotPaired _) -> respond (serverError "cannot pair files")
        Right ready -> case whitespaceParam req.rawQueryString of
          Nothing -> respond (badRequest "bad ws")
          Just ws -> do
            h <- awaitSnapshot live.sides.head
            b <- awaitSnapshot live.sides.base
            case (h, b) of
              (Right hs, Right bs) ->
                respond =<< answer api r ws ready Pair {head = hs, base = bs} segs
              _ -> respond cannotList

-- | Built before the fill: every value of the body, the line diff, the
-- ws parse and the 404 body (Q-RTS-BLACKHOLE). Run before the slot is
-- filled, in diff mode.
warmUpDiff :: IO ()
warmUpDiff = do
  warmUpLineDiff
  _ <- guardedLineDiff diffTimeLimit KeepWhitespace Pair {head = "a\nb\n", base = "a\n"}
  mapM_ (evaluate . whitespaceParam) ["", "ws=ignore", "ws=x", "ws=ignore&ws=ignore"]
  _ <- evaluate (LBS.length (encodeFileNotFound firstGeneration))
  path <- either (ioError . userError . show) pure (parseRelPath "a/b.hs")
  let view st n ws s d =
        DiffView
          { path = path,
            status = st,
            note = n,
            whitespace = ws,
            sides = Pair {head = s, base = s},
            diff = d
          }
      statuses =
        [ StatusAdded,
          StatusRemoved,
          StatusModified,
          StatusRenamed RenameInfo {from = path, score = Score 1},
          StatusUnchanged
        ]
      views =
        [ view st n ws SideText DiffNotText
        | st <- statuses,
          n <- Nothing : map Just [minBound .. maxBound],
          ws <- [KeepWhitespace, IgnoreWhitespace]
        ]
          <> [ view StatusModified Nothing KeepWhitespace s d
             | s <- sampleSides,
               d <- sampleBodies
             ]
  mapM_ (evaluate . LBS.length . encodeDiffView firstGeneration) views
  where
    sampleSides =
      [ SideAbsent,
        SideText,
        SideBinary (FileSize 3),
        SideTooLarge TooLargeInfo {size = FileSize 3, limit = FileSize 2},
        SideSymlink "x",
        SideSubmodule (CommitId "0"),
        SideNestedRepo,
        SideNotLocal
      ]
    sampleBodies =
      [ DiffLines
          LineDiff
            { runs = [Run Equal 1, Run Delete 1, Run Insert 2],
              crlfLines = 1,
              bom = b
            }
      | b <- [Nothing, Just BomAdded, Just BomRemoved]
      ]
        <> [DiffTooSlow, DiffNotText]

-- * Internal

-- | Every run, the counts and the BOM note.
forceLineDiff :: LineDiff -> Int
forceLineDiff d =
  foldl' (\n r -> n + r.count) 0 d.runs + d.crlfLines + maybe 0 (const 1) d.bom

-- | Find the target, read both sides, diff, encode.
answer ::
  FilesApi ->
  DiffRoutes ->
  Whitespace ->
  DiffReady ->
  Pair Snapshot ->
  [Text] ->
  IO Response
answer api r ws ready snaps segs =
  case relPathFromSegments segs >>= findTarget ready.changes on of
    Nothing -> pure missing
    Just target -> do
      let paths = targetPaths target
      readSide api snaps.base paths.base >>= \case
        Left problem -> failed snaps.base problem
        Right bc ->
          readSide api snaps.head paths.head >>= \case
            Left problem -> failed snaps.head problem
            Right hc ->
              withRequest snaps.head.index $
                diffed target Pair {head = hc, base = bc}
  where
    on = (\snap p -> Map.member p snap.files.entries) <$> snaps
    missing = jsonWith status404 (encodeFileNotFound snaps.head.generation)
    failed snap = \case
      Gone -> pure missing
      Failed p why -> readFailed api snap p why
    diffed target got = do
      result <- try @SomeException $ do
        d <- diffOf r ws got
        let view = viewOf api snaps ws target got d
            body = encodeDiffView snaps.head.generation view
        _ <- evaluate (LBS.length body)
        pure (cbor body)
      case result of
        Right resp -> pure resp
        Left e
          | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
          | otherwise -> do
              logInfo api.logger $
                "haskell-reviewer: cannot diff "
                  <> relPathText (targetPath target)
                  <> ": "
                  <> exceptionMessage e
              pure (serverError "cannot diff file")

-- | What one side has at its path.
data SideRead
  = Absent
  | Present FileContent
  | -- | Not in the local repo.
    NotLocal

-- | Why a side wasn't read.
data ReadProblem
  = Gone
  | -- | A read error: the path and why. Logged, then a 500.
    Failed RelPath Text

-- | Read one side's file, if it has one.
readSide :: FilesApi -> Snapshot -> Maybe RelPath -> IO (Either ReadProblem SideRead)
readSide api snap = \case
  Nothing -> pure (Right Absent)
  Just p -> case Map.lookup p snap.files.entries of
    Nothing -> pure (Left Gone)
    Just kind ->
      api.readEntry snap p kind >>= \case
        Left ReadGone -> pure (Left Gone)
        Left (ReadFailed why) -> pure (Left (Failed p why))
        Left (ReadMissing _) -> pure (Right NotLocal)
        Right c -> pure (Right (Present c))

-- | The line diff, if both sides have text.
diffOf :: DiffRoutes -> Whitespace -> Pair SideRead -> IO DiffBody
diffOf r ws got = case traverse sideText got of
  Nothing -> pure DiffNotText
  Just texts ->
    r.lineDiff ws texts >>= \case
      DiffDone d -> pure (DiffLines d)
      DiffTimedOut -> pure DiffTooSlow

-- | What gets diffed. See Note [Diff route].
sideText :: SideRead -> Maybe Text
sideText = \case
  Absent -> Just ""
  Present (TextContent t) -> Just t
  Present (Link info) -> Just info.target
  _ -> Nothing

-- | The body, from the target and what was read.
viewOf ::
  FilesApi ->
  Pair Snapshot ->
  Whitespace ->
  Target ->
  Pair SideRead ->
  DiffBody ->
  DiffView
viewOf api snaps ws target got d =
  DiffView
    { path = targetPath target,
      status = status,
      note = note,
      whitespace = ws,
      sides = sideView <$> got,
      diff = d
    }
  where
    status = case target of
      Changed (Added _) -> StatusAdded
      Changed (Removed _) -> StatusRemoved
      Changed (Modified _) -> StatusModified
      Changed (Renamed rn) -> StatusRenamed RenameInfo {from = rn.from, score = rn.score}
      SamePath p
        | sameKind p && sameContents p -> StatusUnchanged
        | otherwise -> StatusModified
    -- The change's own note.
    own = case target of
      Changed (Modified m) -> noteOf <$> m.note
      Changed (Renamed rn) -> execNote rn.exec
      SamePath p | not (sameKind p) -> Just NoteTypeChanged
      _ -> Nothing
    -- A modified file with no other note: filtered, as api/diff-files.
    note
      | Nothing <- own,
        status == StatusModified,
        (api.fileNotes snaps.head (targetPath target)).filtered == Filtered =
          Just NoteFiltered
      | otherwise = own
    noteOf = \case
      TypeChanged -> NoteTypeChanged
      ModeChanged -> NoteModeChanged
    execNote = \case
      ExecChanged -> Just NoteModeChanged
      SameExec -> Nothing
    entries p = (\snap -> Map.lookup p snap.files.entries) <$> snaps
    sameKind p = let k = fmap kindTag <$> entries p in k.head == k.base
    -- See Note [Diff route].
    sameContents p =
      case (\snap -> sourceBlob snap.source p) <$> snaps of
        Pair {head = Just h, base = Just b} -> h == b
        _ -> case (entries p, got) of
          (Pair {head = Just (EntrySubmodule h), base = Just (EntrySubmodule b)}, _) ->
            h == b
          (_, Pair {head = Present hc, base = Present bc}) -> sameRead hc bc
          _ -> False
    sameRead hc bc = case (hc, bc) of
      (TextContent h, TextContent b) -> h == b
      (Link h, Link b) -> h.target == b.target
      (NestedRepo, NestedRepo) -> True
      _ -> False

-- | File, symlink or submodule (a nested repo counts as one).
data KindTag = TagFile | TagSymlink | TagSubmodule
  deriving (Eq)

-- | An entry's kind, as pairing sees it.
kindTag :: EntryKind -> KindTag
kindTag = \case
  EntryFile -> TagFile
  EntrySymlink -> TagSymlink
  EntrySubmodule _ -> TagSubmodule
  EntryNestedRepo -> TagSubmodule

-- | A side on the wire.
sideView :: SideRead -> SideView
sideView = \case
  Absent -> SideAbsent
  NotLocal -> SideNotLocal
  Present c -> case c of
    TextContent _ -> SideText
    Binary size -> SideBinary size
    TooLarge info -> SideTooLarge info
    Link info -> SideSymlink info.target
    Submodule commit -> SideSubmodule commit
    NestedRepo -> SideNestedRepo

-- | ws: see Note [Diff route]. 'Nothing': a 400.
whitespaceParam :: ByteString -> Maybe Whitespace
whitespaceParam query =
  case [v | (k, v) <- parseQueryReplacePlus False query, k == "ws"] of
    [] -> Just KeepWhitespace
    [Just "ignore"] -> Just IgnoreWhitespace
    _ -> Nothing

-- | A plain-text 400.
badRequest :: LBS.ByteString -> Response
badRequest = responseLBS status400 [(hContentType, "text/plain; charset=utf-8")]

-- | The listing failed. The reason is on stderr.
cannotList :: Response
cannotList = serverError "cannot list files"
