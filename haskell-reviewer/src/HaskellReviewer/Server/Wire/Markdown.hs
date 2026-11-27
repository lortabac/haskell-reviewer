-- | A Markdown file, rendered (SPEC §4.2): GET api\/markdown\/\<side>\/\<path>.
-- See Note [Markdown page].
module HaskellReviewer.Server.Wire.Markdown
  ( MarkdownAnswer (..),
    encodeMarkdownPage,
    warmUpMarkdown,
    warmUpDoc,
    markdownSource,
  )
where

import Control.Exception (evaluate)
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import HaskellReviewer.Core.Eol (Filtering (..))
import HaskellReviewer.Core.FileContent (FileNotes (..), unfiltered)
import HaskellReviewer.Core.Highlight (linkedRunsFromList)
import HaskellReviewer.Core.Markdown (Markdown (..))
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RelPath (relPathFromSegments)
import HaskellReviewer.Core.RichText
  ( AnchorRef (..),
    Block (..),
    Cell (..),
    CodeListing (..),
    DefItem (..),
    Doc (..),
    Example (..),
    FileRef (..),
    Header (..),
    Hyperlink (..),
    Inline (..),
    ListItem (..),
    MathStyle (..),
    ModLink (..),
    NameRef (..),
    Piece (..),
    RefNamespace (..),
    Since (..),
    TableBody (..),
    TaskItem (..),
    safeUrl,
  )
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
-- doc has every node, so the Markdown-only encoders are warm too: see
-- 'warmUpDoc'. See Note [Page warm-up] in
-- "HaskellReviewer.Server.Wire.Page".
warmUpMarkdown :: IO ()
warmUpMarkdown =
  sequence_
    [ evaluate (LBS.length (encodeMarkdownPage (Generation 1) n a))
    | n <- [unfiltered, FileNotes {filtered = Filtered}],
      a <-
        [ MarkdownDoc Markdown {doc = warmUpDoc, runs = []},
          MarkdownNotText,
          MarkdownNotLocal,
          MarkdownTooSlow
        ]
    ]

-- | A doc with every block and inline. A test checks that none is
-- missing (Server.MarkdownTest).
warmUpDoc :: Doc
warmUpDoc =
  Doc
    { blocks =
        [ Paragraph inlines,
          Heading Header {level = 1, title = [Str "h"], anchor = Just "h"},
          Heading Header {level = 2, title = [Str "h"], anchor = Nothing},
          CodeBlock [Str "c"],
          UnorderedList [[Paragraph [Str "u"]]],
          OrderedList
            [ ListItem {number = Just 1, content = [ThematicBreak]},
              ListItem {number = Nothing, content = []}
            ],
          DefinitionList [DefItem {term = [Str "t"], definition = [ThematicBreak]}],
          Examples [Example {expression = "e", results = ["r"]}],
          Property "p",
          Table
            TableBody
              { header = [[Cell {colspan = 1, rowspan = 1, content = [Str "c"]}]],
                body = [[Cell {colspan = 2, rowspan = 1, content = []}]]
              },
          BlockQuote [Paragraph [Str "q"]],
          ThematicBreak,
          Listing
            CodeListing
              { lang = Just "hs",
                pieces =
                  [Piece {display = Just d, text = "x"} | d <- [minBound .. maxBound]]
                    <> [Piece {display = Nothing, text = "y"}]
              },
          Listing CodeListing {lang = Nothing, pieces = []},
          TaskList
            [ TaskItem {done = True, content = [Paragraph [Str "a"]]},
              TaskItem {done = False, content = []}
            ]
        ],
      since = Just Since {package = Just "base", version = 4 :| [5]}
    }
  where
    path = fromMaybe (error "warm-up: bad path") (relPathFromSegments ["a", "b.md"])
    url = fromMaybe (error "warm-up: bad url") (safeUrl "https://example.com")
    inlines =
      [ Str "s",
        Emphasis [Str "e"],
        Bold [Str "b"],
        Code [Str "c"],
        Math InlineMath "m",
        Math DisplayMath "m",
        Link Hyperlink {url, label = Nothing},
        Link Hyperlink {url, label = Just [Str "l"]},
        Name nameRef {namespace = ValueNs},
        Name nameRef {namespace = TypeNs},
        Name nameRef {namespace = AnyNs},
        ModuleLink ModLink {name = modName, anchor = Nothing, label = Nothing},
        ModuleLink ModLink {name = modName, anchor = Just "a", label = Just lbl},
        Strike [Str "s"],
        FileLink FileRef {path, fragment = Nothing, label = Nothing},
        FileLink FileRef {path, fragment = Just "L1", label = Just [Str "l"]},
        AnchorLink AnchorRef {anchor = "h", label = Nothing},
        AnchorLink AnchorRef {anchor = "h", label = Just [Str "l"]}
      ]
    modName = ModuleName "M"
    lbl = [Str "l"]
    nameRef = NameRef {namespace = AnyNs, qualifier = Nothing, occ = "x", written = "x"}

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
