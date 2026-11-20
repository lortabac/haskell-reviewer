-- | A Markdown file, rendered (SPEC §4.2): GET api\/markdown\/\<side>\/\<path>.
-- See Note [Markdown page].
module HaskellReviewer.Server.Wire.Markdown
  ( MarkdownAnswer (..),
    encodeMarkdownPage,
    markdownSource,
  )
where

import Data.ByteString.Lazy qualified as LBS
import HaskellReviewer.Core.Highlight (linkedRunsFromList)
import HaskellReviewer.Core.Markdown (Markdown (..))
import HaskellReviewer.Server.Cbor qualified as Cbor
import HaskellReviewer.Server.Wire.RichText (encodeDoc, noNames)
import HaskellReviewer.Server.Wire.Source (LexedSource (..), SourceBody (..))
import HaskellReviewer.Session.Snapshot (Generation (..))

{- Note [Markdown page]

> {generation, kind: "doc", doc}

doc is rich text as on the module page (Note [Module page] in
"HaskellReviewer.Server.Wire.Page"), with the Markdown nodes: blocks
quote {blocks}, rule, listing {lang, pieces: [[class, text]]} (class a
display class number, as the source view's runs, or null), tasks
{items: [{done, blocks}]}; a heading has an anchor; inlines strike
{inlines}, file {path, fragment, label} (a file of the snapshot) and
anchor {anchor, label} (a heading of this page). Names and module links
never occur.

A file that isn't text (binary, too large, a symlink, ...):

> {generation, kind: "not-text"}

Its file view says why. A file that took too long to parse (Note [Parse
time limit] in "HaskellReviewer.Effects.Parser"):

> {generation, kind: "too-slow"}
-}

-- | What we got for a Markdown file.
data MarkdownAnswer
  = MarkdownDoc Markdown
  | MarkdownNotText
  | MarkdownTooSlow

-- | The rendered file, or why not. See Note [Markdown page].
encodeMarkdownPage :: Generation -> MarkdownAnswer -> LBS.ByteString
encodeMarkdownPage (Generation g) md =
  Cbor.cborBody . Cbor.obj $
    ("generation", Cbor.word64 g) : case md of
      MarkdownDoc m -> [("kind", Cbor.text "doc"), ("doc", encodeDoc noNames m.doc)]
      MarkdownNotText -> [("kind", Cbor.text "not-text")]
      MarkdownTooSlow -> [("kind", Cbor.text "too-slow")]

-- | The source view of a Markdown file: its runs, no links. Plain if
-- there are none.
markdownSource :: MarkdownAnswer -> SourceBody
markdownSource = \case
  MarkdownDoc m -> lexed m
  MarkdownNotText -> PlainBody
  MarkdownTooSlow -> PlainBody

lexed :: Markdown -> SourceBody
lexed m =
  LexedBody
    LexedSource
      { runs = linkedRunsFromList [(r, 0) | r <- m.runs],
        regions = [],
        targets = [],
        outline = []
      }
