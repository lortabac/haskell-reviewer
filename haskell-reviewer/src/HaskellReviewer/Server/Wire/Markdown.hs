-- | A Markdown file, rendered (SPEC §4.2): GET api\/markdown\/\<side>\/\<path>.
-- See Note [Markdown page].
module HaskellReviewer.Server.Wire.Markdown
  ( MarkdownAnswer (..),
    encodeMarkdownPage,
    warmUpMarkdown,
    markdownSource,
  )
where

import Control.Exception (evaluate)
import Data.ByteString.Lazy qualified as LBS
import HaskellReviewer.Core.Eol (Filtering (..))
import HaskellReviewer.Core.FileContent (FileNotes (..), unfiltered)
import HaskellReviewer.Core.Highlight (linkedRunsFromList)
import HaskellReviewer.Core.Markdown (Markdown (..))
import HaskellReviewer.Core.RichText (Doc (..))
import HaskellReviewer.Server.Cbor qualified as Cbor
import HaskellReviewer.Server.Wire (filteredValue)
import HaskellReviewer.Server.Wire.RichText (encodeDoc, noNames)
import HaskellReviewer.Server.Wire.Source (LexedSource (..), SourceBody (..))
import HaskellReviewer.Session.Snapshot (Generation (..))

{- Note [Markdown page]

> {generation, kind: "doc", doc, filtered}

doc is rich text as on the module page (Note [Module page] in
"HaskellReviewer.Server.Wire.Page"), with the Markdown nodes: blocks
quote {blocks}, rule, listing {lang, pieces: [[class, text]]} (class a
display class number, as the source view's runs, or null), tasks
{items: [{done, blocks}]}; a heading has an anchor; inlines strike
{inlines}, file {path, fragment, label} (a file of the snapshot) and
anchor {anchor, label} (a heading of this page). Names and module links
never occur. filtered is as on api/file, on every kind (D-343).

A file that isn't text (binary, too large, a symlink, ...):

> {generation, kind: "not-text", filtered}

Its file view says why. A blob missing from a partial clone (D-343):

> {generation, kind: "not-local", filtered}

A file that took too long to parse (Note [Parse time limit] in
"HaskellReviewer.Effects.Parser"):

> {generation, kind: "too-slow", filtered}
-}

-- | What we got for a Markdown file.
data MarkdownAnswer
  = MarkdownDoc Markdown
  | MarkdownNotText
  | MarkdownTooSlow
  | -- | The blob is not in the local repo (partial clone).
    MarkdownNotLocal

-- | The rendered file, or why not. See Note [Markdown page].
encodeMarkdownPage :: Generation -> FileNotes -> MarkdownAnswer -> LBS.ByteString
encodeMarkdownPage (Generation g) notes md =
  Cbor.cborBody . Cbor.obj $
    ("generation", Cbor.word64 g)
      : case md of
        MarkdownDoc m ->
          [("kind", Cbor.text "doc"), ("doc", encodeDoc noNames m.doc), filtered]
        MarkdownNotText -> [("kind", Cbor.text "not-text"), filtered]
        MarkdownNotLocal -> [("kind", Cbor.text "not-local"), filtered]
        MarkdownTooSlow -> [("kind", Cbor.text "too-slow"), filtered]
  where
    filtered = ("filtered", filteredValue notes)

-- | Encode every kind of page, filtered or not (Q-RTS-BLACKHOLE). The
-- doc is empty, so the Markdown-only doc nodes are not covered. See
-- Note [Page warm-up] in "HaskellReviewer.Server.Wire.Page".
warmUpMarkdown :: IO ()
warmUpMarkdown =
  sequence_
    [ evaluate (LBS.length (encodeMarkdownPage (Generation 1) n a))
    | n <- [unfiltered, FileNotes {filtered = Filtered}],
      a <-
        [ MarkdownDoc Markdown {doc = Doc {blocks = [], since = Nothing}, runs = []},
          MarkdownNotText,
          MarkdownNotLocal,
          MarkdownTooSlow
        ]
    ]

-- | The source view of a Markdown file: its runs, no links. Plain if
-- there are none.
markdownSource :: MarkdownAnswer -> SourceBody
markdownSource = \case
  MarkdownDoc m -> lexed m
  MarkdownNotText -> PlainBody
  MarkdownTooSlow -> PlainBody
  MarkdownNotLocal -> NotLocalBody

lexed :: Markdown -> SourceBody
lexed m =
  LexedBody
    LexedSource
      { runs = linkedRunsFromList [(r, 0) | r <- m.runs],
        regions = [],
        targets = [],
        outline = []
      }
