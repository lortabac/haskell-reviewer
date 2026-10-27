-- | A plain text dump of a module's doc comments as rich text, for golden
-- tests. One line per node, two spaces per level. Each name says what
-- it resolves to in the module, if anything.
module RichDump (dumpRich) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Decls
  ( ClassDecl (..),
    ConArg (..),
    ConArgs (..),
    ConDecl (..),
    Decl (..),
    DeclInfo (..),
    DocComment (..),
    FieldDecl (..),
    MethodDecl (..),
    PatSynDecl (..),
    TypeDecl (..),
    declInfo,
    formConstructors,
  )
import HaskellReviewer.Core.DocLinks (LocalNames, localNames, resolveLocal)
import HaskellReviewer.Core.ModuleModel
  ( DocItem (..),
    Export (..),
    Item (..),
    ItemName (..),
    Located (..),
    ModuleHeader (..),
    ModuleParse (..),
    ParsedModule (..),
    Section (..),
  )
import HaskellReviewer.Core.Names (renderDeclId)
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RichText
  ( Block (..),
    Cell (..),
    DefItem (..),
    Doc (..),
    Example (..),
    Header (..),
    Hyperlink (..),
    Inline (..),
    ListItem (..),
    MathStyle (..),
    ModLink (..),
    NameRef (..),
    RefNamespace (..),
    Since (..),
    TableBody (..),
    urlText,
  )
import HaskellReviewer.Core.Span (Span, renderSpan)
import ParseDump (dumpParse)

-- | Every doc comment of a parse: the module's, the export list's, the
-- doc items, then the declarations'.
dumpRich :: ModuleParse -> Text
dumpRich = \case
  r@(TextOnly _) -> dumpParse r
  Parsed m ->
    let names = localNames ((.value) <$> m.header.name) m.declarations
     in Text.unlines $
          maybe [] (comment names "module") m.header.haddock
            <> concatMap (export names) (concat m.exports)
            <> concatMap (docItem names) m.docItems
            <> concatMap (decl names) m.declarations

export :: LocalNames -> Export -> [Text]
export names = \case
  ExportItem i -> maybe [] (comment names ("export " <> i.name.occ)) i.doc
  ExportSection s -> heading names s
  ExportDoc d -> comment names "export doc" d
  _ -> []

docItem :: LocalNames -> DocItem -> [Text]
docItem names = \case
  DocHeading s -> heading names s
  DocChunk n d -> comment names ("chunk " <> n.value) d

heading :: LocalNames -> Section -> [Text]
heading names s =
  ("heading " <> showT s.level <> " " <> at s.heading.span)
    : nested (concatMap (inline names) s.heading.parsed)

-- | The doc comments of a declaration and of what's inside it.
decl :: LocalNames -> Decl -> [Text]
decl names d = case d of
  TypeD t -> typeDecl t
  ClassD c ->
    info c.info <> concatMap (info . (.info)) c.methods <> concatMap typeDecl c.associated
  PatSynD p -> info p.info <> concatMap info p.fields
  _ -> maybe [] info (declInfo d)
  where
    typeDecl t = info t.info <> concatMap con (formConstructors t.form)
    con c =
      info c.info <> case c.args of
        PrefixArgs as -> concatMap (arg c.info) as
        InfixArgs a b -> arg c.info a <> arg c.info b
        GadtArgs as -> concatMap (arg c.info) as
        RecordArgs fs -> concatMap (info . (.info)) fs
        GadtRecordArgs fs -> concatMap (info . (.info)) fs
    arg i (a :: ConArg) = maybe [] (comment names ("arg of " <> renderDeclId i.id)) a.doc
    info i = concatMap (comment names (renderDeclId i.id)) i.haddock

comment :: LocalNames -> Text -> DocComment Doc -> [Text]
comment names title c =
  (title <> " " <> at c.span)
    : nested
      ( maybe [] (pure . since) c.parsed.since
          <> concatMap (block names) c.parsed.blocks
      )
  where
    since s =
      "since "
        <> maybe "" (<> "-") s.package
        <> Text.intercalate "." (map showT (NonEmpty.toList s.version))

block :: LocalNames -> Block -> [Text]
block names = \case
  Paragraph is -> "para" : inlines is
  Heading h -> ("heading " <> showT h.level) : inlines h.title
  CodeBlock is -> "code block" : inlines is
  UnorderedList items -> "list" : nested (concatMap (\i -> "item" : blocks i) items)
  OrderedList items -> "ordered list" : nested (concatMap numbered items)
  DefinitionList items -> "definitions" : nested (concatMap definition items)
  Examples es -> "examples" : nested (concatMap example es)
  Property p -> ["property " <> quote p]
  Table t -> "table" : nested (rows "header row" t.header <> rows "row" t.body)
  where
    inlines = nested . concatMap (inline names)
    blocks = nested . concatMap (block names)
    numbered i = ("item " <> showT i.number) : blocks i.content
    definition i = ("term" : inlines i.term) <> ("definition" : blocks i.definition)
    example e = (">>> " <> quote e.expression) : nested (map quote e.results)
    rows title = concatMap (\r -> title : nested (concatMap cell r))
    cell c = ("cell " <> showT c.colspan <> "x" <> showT c.rowspan) : inlines c.content

inline :: LocalNames -> Inline -> [Text]
inline names = \case
  Str t -> [quote t]
  Emphasis is -> "emphasis" : sub is
  Bold is -> "bold" : sub is
  Code is -> "code" : sub is
  Math InlineMath t -> ["math " <> quote t]
  Math DisplayMath t -> ["display math " <> quote t]
  Link l -> ("link " <> urlText l.url) : maybe [] sub l.label
  Name r ->
    [ "name "
        <> ns r.namespace
        <> maybe "" ((<> ".") . modText) r.qualifier
        <> r.occ
        <> " as "
        <> quote r.written
        <> maybe "" ((" -> " <>) . renderDeclId) (resolveLocal names r)
    ]
  ModuleLink l ->
    ("module " <> modText l.name <> maybe "" ("#" <>) l.anchor) : maybe [] sub l.label
  where
    sub = nested . concatMap (inline names)
    ns = \case
      ValueNs -> "v:"
      TypeNs -> "t:"
      AnyNs -> ""

nested :: [Text] -> [Text]
nested = map ("  " <>)

at :: Span -> Text
at s = "@" <> renderSpan s

modText :: ModuleName -> Text
modText (ModuleName n) = n

showT :: Int -> Text
showT = Text.pack . show

-- | In double quotes, with newlines shown.
quote :: Text -> Text
quote t = "\"" <> Text.concatMap esc t <> "\""
  where
    esc = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      c -> Text.singleton c
