-- | A Markdown file, rendered (SPEC §4.2): GET api\/markdown\/\<side>\/\<path>.
-- See Note [Markdown page].
module HaskellReviewer.Server.Wire.Markdown
  ( encodeMarkdownPage,
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

Its file view says why.
-}

-- | The rendered file, or @not-text@ ('Nothing') when the file is no
-- text. See Note [Markdown page].
encodeMarkdownPage :: Generation -> Maybe Markdown -> LBS.ByteString
encodeMarkdownPage (Generation g) md =
  Cbor.cborBody . Cbor.obj $
    ("generation", Cbor.word64 g) : case md of
      Just m -> [("kind", Cbor.text "doc"), ("doc", encodeDoc noNames m.doc)]
      Nothing -> [("kind", Cbor.text "not-text")]

-- | The source view of a Markdown file: its runs, no links.
markdownSource :: Markdown -> SourceBody
markdownSource m =
  LexedBody
    LexedSource
      { runs = linkedRunsFromList [(r, 0) | r <- m.runs],
        regions = [],
        targets = [],
        outline = []
      }
