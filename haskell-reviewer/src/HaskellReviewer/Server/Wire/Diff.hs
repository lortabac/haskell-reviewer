-- | One file's diff: GET api\/diff\/\<path…> (D-331). See Note [Diff
-- view].
module HaskellReviewer.Server.Wire.Diff
  ( -- * Types
    DiffView (..),
    DiffStatus (..),
    RenameInfo (..),
    DiffNote (..),
    SideView (..),
    DiffBody (..),

    -- * Body
    encodeDiffView,
  )
where

import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import HaskellReviewer.Core.FileContent (FileSize (..), TooLargeInfo (..))
import HaskellReviewer.Core.FileList (CommitId (..))
import HaskellReviewer.Core.LineDiff
  ( BomChange (..),
    LineDiff (..),
    Op (..),
    Run (..),
    Whitespace (..),
  )
import HaskellReviewer.Core.RelPath (RelPath, relPathText)
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Core.Similarity (Score, scorePercent)
import HaskellReviewer.Server.Cbor qualified as Cbor
import HaskellReviewer.Session.Snapshot (Generation (..))

{- Note [Diff view]

GET api/diff/<path…>[?ws=ignore], as CBOR. In JSON terms:

> {"generation":1,"path":"b.hs","oldPath":"a.hs"|null,
>  "status":"added"|"removed"|"modified"|"renamed"|"unchanged",
>  "similarity":60|null,
>  "note":null|"type-changed"|"mode-changed"|"filtered",
>  "ignoreWhitespace":false,
>  "base":<side>,"head":<side>,"diff":<diff>}

path, oldPath, similarity and note are as in api/diff-files (path: the
head path, the base path for removed; oldPath and similarity only for
renamed). A side is one of

> {"kind":"absent"}                       not on this side
> {"kind":"text"}
> {"kind":"binary","size":123}
> {"kind":"too-large","size":123,"limit":1048576}
> {"kind":"symlink","target":"../x"}
> {"kind":"submodule","commit":"<hex>"|null}   null: a nested repo
> {"kind":"not-local"}                    a blob not in a partial clone

and the diff one of

> {"kind":"lines","runs":[["equal",3],["delete",1],["insert",2]],
>  "crlfLines":0,"bom":null|"added"|"removed"}
> {"kind":"too-slow"}    over the time limit (Note [Diff time limit] in
>                        "HaskellReviewer.Server.Diff")
> {"kind":"not-text"}    a side has no text; the side kinds say why

Runs as in "HaskellReviewer.Core.LineDiff": base lines are equal +
delete, head lines equal + insert. An absent side is "" (so all insert
or all delete), a symlink is its target, one line. The texts are on
api/file and api/source.
-}

-- | What GET api\/diff\/\<path…> says, besides the generation.
data DiffView = DiffView
  { -- | The head path, the base path for removed.
    path :: RelPath,
    status :: DiffStatus,
    note :: Maybe DiffNote,
    whitespace :: Whitespace,
    sides :: Pair SideView,
    diff :: DiffBody
  }

-- | As in api\/diff-files, plus unchanged.
data DiffStatus
  = StatusAdded
  | StatusRemoved
  | StatusModified
  | StatusRenamed RenameInfo
  | StatusUnchanged
  deriving (Eq, Show)

-- | A rename's base path and score: oldPath and similarity on the
-- wire.
data RenameInfo = RenameInfo
  { from :: RelPath,
    score :: Score
  }
  deriving (Eq, Show)

-- | As in api\/diff-files.
data DiffNote = NoteTypeChanged | NoteModeChanged | NoteFiltered
  deriving (Eq, Show, Enum, Bounded)

-- | What one side has at its path.
data SideView
  = SideAbsent
  | SideText
  | SideBinary FileSize
  | SideTooLarge TooLargeInfo
  | SideSymlink Text
  | SideSubmodule CommitId
  | -- | On the wire: a submodule with commit null.
    SideNestedRepo
  | SideNotLocal
  deriving (Eq, Show)

-- | The diff, or why there is none.
data DiffBody
  = DiffLines LineDiff
  | DiffTooSlow
  | DiffNotText
  deriving (Eq, Show)

-- | See Note [Diff view].
encodeDiffView :: Generation -> DiffView -> LBS.ByteString
encodeDiffView (Generation g) v =
  Cbor.cborBody . Cbor.obj $
    [ ("generation", Cbor.word64 g),
      ("path", path v.path),
      ("oldPath", Cbor.nullable (path . (.from)) renamed),
      ("status", Cbor.text (statusName v.status)),
      ("similarity", Cbor.nullable (Cbor.int . scorePercent . (.score)) renamed),
      ("note", Cbor.nullable (Cbor.text . noteName) v.note),
      ("ignoreWhitespace", Cbor.bool (v.whitespace == IgnoreWhitespace)),
      ("base", side v.sides.base),
      ("head", side v.sides.head),
      ("diff", body v.diff)
    ]
  where
    renamed = case v.status of
      StatusRenamed r -> Just r
      _ -> Nothing
    path = Cbor.text . relPathText
    kind k = ("kind", Cbor.text k)
    bytes (FileSize n) = Cbor.int n
    side = \case
      SideAbsent -> Cbor.obj [kind "absent"]
      SideText -> Cbor.obj [kind "text"]
      SideBinary size -> Cbor.obj [kind "binary", ("size", bytes size)]
      SideTooLarge info ->
        Cbor.obj
          [kind "too-large", ("size", bytes info.size), ("limit", bytes info.limit)]
      SideSymlink t -> Cbor.obj [kind "symlink", ("target", Cbor.text t)]
      SideSubmodule c -> Cbor.obj [kind "submodule", ("commit", commit c)]
      SideNestedRepo -> Cbor.obj [kind "submodule", ("commit", Cbor.null_)]
      SideNotLocal -> Cbor.obj [kind "not-local"]
    commit (CommitId c) = Cbor.text c
    body = \case
      DiffLines d ->
        Cbor.obj
          [ kind "lines",
            ("runs", Cbor.list run d.runs),
            ("crlfLines", Cbor.int d.crlfLines),
            ("bom", Cbor.nullable (Cbor.text . bomName) d.bom)
          ]
      DiffTooSlow -> Cbor.obj [kind "too-slow"]
      DiffNotText -> Cbor.obj [kind "not-text"]
    run r = Cbor.list id [Cbor.text (opName r.op), Cbor.int r.count]

-- * Internal

-- | The status on the wire.
statusName :: DiffStatus -> Text
statusName = \case
  StatusAdded -> "added"
  StatusRemoved -> "removed"
  StatusModified -> "modified"
  StatusRenamed _ -> "renamed"
  StatusUnchanged -> "unchanged"

-- | The note on the wire.
noteName :: DiffNote -> Text
noteName = \case
  NoteTypeChanged -> "type-changed"
  NoteModeChanged -> "mode-changed"
  NoteFiltered -> "filtered"

opName :: Op -> Text
opName = \case
  Equal -> "equal"
  Delete -> "delete"
  Insert -> "insert"

bomName :: BomChange -> Text
bomName = \case
  BomAdded -> "added"
  BomRemoved -> "removed"
