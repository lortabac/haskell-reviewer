-- | Rich text on the wire, for module pages (Haddock) and Markdown
-- files. Shape: Note [Module page] in "HaskellReviewer.Server.Wire.Page"
-- and Note [Markdown page] in "HaskellReviewer.Server.Wire.Markdown".
module HaskellReviewer.Server.Wire.RichText
  ( RichEnv (..),
    noNames,
    encodeDoc,
    encodeInlines,
  )
where

import Codec.CBOR.Encoding (Encoding)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RelPath (relPathText)
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
    urlText,
  )
import HaskellReviewer.Server.Cbor qualified as Cbor

-- | What only the page knows: where names and modules link.
data RichEnv = RichEnv
  { -- | A name's @target@ and @link@ fields.
    name :: NameRef -> [(Text, Encoding)],
    -- | A module's ref, if it's in the project.
    moduleRef :: ModuleName -> Maybe Text
  }

-- | No names, no modules: Markdown.
noNames :: RichEnv
noNames =
  RichEnv
    { name = const [("target", Cbor.null_), ("link", Cbor.null_)],
      moduleRef = const Nothing
    }

-- | @{blocks, since}@.
encodeDoc :: RichEnv -> Doc -> Encoding
encodeDoc env d =
  Cbor.obj
    [ ("blocks", blocks env d.blocks),
      ("since", Cbor.nullable since d.since)
    ]
  where
    since s =
      Cbor.obj
        [ ("package", Cbor.nullable Cbor.text s.package),
          ("version", Cbor.text (version s.version))
        ]

blocks :: RichEnv -> [Block] -> Encoding
blocks env = Cbor.list block
  where
    block = \case
      Paragraph is -> Cbor.obj [kind "paragraph", ("inlines", encodeInlines env is)]
      Heading h ->
        Cbor.obj
          [ kind "heading",
            ("level", Cbor.int h.level),
            ("inlines", encodeInlines env h.title),
            ("anchor", Cbor.nullable Cbor.text h.anchor)
          ]
      CodeBlock is -> Cbor.obj [kind "code", ("inlines", encodeInlines env is)]
      UnorderedList items ->
        Cbor.obj [kind "bullets", ("items", Cbor.list (blocks env) items)]
      OrderedList items ->
        Cbor.obj
          [ kind "numbered",
            ( "items",
              Cbor.list
                ( \i ->
                    Cbor.obj
                      [ ("number", Cbor.nullable (Cbor.text . decimal) i.number),
                        ("blocks", blocks env i.content)
                      ]
                )
                items
            )
          ]
      DefinitionList items ->
        Cbor.obj
          [ kind "definitions",
            ( "items",
              Cbor.list
                ( \i ->
                    Cbor.obj
                      [ ("term", encodeInlines env i.term),
                        ("blocks", blocks env i.definition)
                      ]
                )
                items
            )
          ]
      Examples es ->
        Cbor.obj
          [ kind "examples",
            ( "items",
              Cbor.list
                ( \e ->
                    Cbor.obj
                      [ ("expression", Cbor.text e.expression),
                        ("results", Cbor.list Cbor.text e.results)
                      ]
                )
                es
            )
          ]
      Property t -> Cbor.obj [kind "property", ("text", Cbor.text t)]
      Table t ->
        Cbor.obj
          [ kind "table",
            ("header", Cbor.list (Cbor.list cell) t.header),
            ("body", Cbor.list (Cbor.list cell) t.body)
          ]
      BlockQuote bs -> Cbor.obj [kind "quote", ("blocks", blocks env bs)]
      ThematicBreak -> Cbor.obj [kind "rule"]
      Listing l ->
        Cbor.obj
          [ kind "listing",
            ("lang", Cbor.nullable Cbor.text l.lang),
            ("pieces", Cbor.list piece l.pieces)
          ]
      TaskList items ->
        Cbor.obj
          [ kind "tasks",
            ( "items",
              Cbor.list
                ( \i ->
                    Cbor.obj [("done", Cbor.bool i.done), ("blocks", blocks env i.content)]
                )
                items
            )
          ]
    cell c =
      Cbor.obj
        [ ("colspan", Cbor.int c.colspan),
          ("rowspan", Cbor.int c.rowspan),
          ("inlines", encodeInlines env c.content)
        ]
    -- [class, text], class a display class number or null.
    piece p =
      Cbor.list
        id
        [ Cbor.nullable (Cbor.int . fromEnum) p.display,
          Cbor.text p.text
        ]

-- | A list of inlines.
encodeInlines :: RichEnv -> [Inline] -> Encoding
encodeInlines env = Cbor.list inline
  where
    inline = \case
      Str t -> Cbor.obj [kind "text", ("text", Cbor.text t)]
      Emphasis is -> Cbor.obj [kind "emphasis", ("inlines", encodeInlines env is)]
      Bold is -> Cbor.obj [kind "bold", ("inlines", encodeInlines env is)]
      Code is -> Cbor.obj [kind "code", ("inlines", encodeInlines env is)]
      Math style t ->
        Cbor.obj
          [ kind "math",
            ( "style",
              Cbor.text $ case style of
                InlineMath -> "inline"
                DisplayMath -> "display"
            ),
            ("tex", Cbor.text t)
          ]
      Link l ->
        Cbor.obj
          [ kind "link",
            ("url", Cbor.text (urlText l.url)),
            ("label", Cbor.nullable (encodeInlines env) l.label)
          ]
      Name r ->
        Cbor.obj $
          [ kind "name",
            ("written", Cbor.text r.written),
            ( "ns",
              Cbor.text $ case r.namespace of
                ValueNs -> "value"
                TypeNs -> "type"
                AnyNs -> "any"
            )
          ]
            <> env.name r
      ModuleLink l ->
        Cbor.obj
          [ kind "module",
            ("module", Cbor.text (let ModuleName n = l.name in n)),
            ("anchor", Cbor.nullable Cbor.text l.anchor),
            ("ref", Cbor.nullable Cbor.text (env.moduleRef l.name)),
            ("label", Cbor.nullable (encodeInlines env) l.label)
          ]
      Strike is -> Cbor.obj [kind "strike", ("inlines", encodeInlines env is)]
      FileLink l ->
        Cbor.obj
          [ kind "file",
            ("path", Cbor.text (relPathText l.path)),
            ("fragment", Cbor.nullable Cbor.text l.fragment),
            ("label", Cbor.nullable (encodeInlines env) l.label)
          ]
      AnchorLink l ->
        Cbor.obj
          [ kind "anchor",
            ("anchor", Cbor.text l.anchor),
            ("label", Cbor.nullable (encodeInlines env) l.label)
          ]

kind :: Text -> (Text, Encoding)
kind k = ("kind", Cbor.text k)

-- | Decimal text: list numbers and versions can be over 2^53.
decimal :: Int -> Text
decimal = Text.pack . show

-- | @1.2.3@.
version :: NonEmpty.NonEmpty Int -> Text
version = Text.intercalate "." . map decimal . NonEmpty.toList
