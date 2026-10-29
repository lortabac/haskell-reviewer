-- | A plain text dump of a 'ModuleParse', for golden tests. One line per
-- fact, two spaces per level. Spans are @line:col-line:col@.
module ParseDump (dumpParse, docLines) where

import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.ModuleModel
  ( DocComment (..),
    Export (..),
    Extensions (..),
    Import (..),
    ImportLevel (..),
    ImportList (..),
    ImportSource (..),
    Item (..),
    ItemName (..),
    ListKind (..),
    Located (..),
    ModuleExport (..),
    ModuleHeader (..),
    ModuleParse (..),
    NameKeyword (..),
    ParseError (..),
    ParsedModule (..),
    Qualified (..),
    Section (..),
    SubList (..),
    Subs (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
    WarningKind (..),
    WarningText (..),
    renderParseWarning,
  )
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.SourceText (SourceLines, sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Span, renderSpan)

-- | A parse of this text.
dumpParse :: Text -> ModuleParse -> Text
dumpParse text =
  Text.unlines . \case
    Parsed m -> "parsed" : parsed (sourceLines text) m
    TextOnly t ->
      ("text only: " <> reason t.reason) : map (indent . renderParseWarning) t.warnings

parsed :: SourceLines -> ParsedModule -> [Text]
parsed src m =
  header src m.header
    <> case m.exports of
      Nothing -> ["exports: everything"]
      Just es -> "exports:" : concatMap (map indent . export src) es
    <> ("imports:" : concatMap (map indent . importDecl src) m.imports)
    <> extensions m.extensions
    <> ("warnings:" : map (indent . renderParseWarning) m.warnings)

header :: SourceLines -> ModuleHeader -> [Text]
header src h =
  [ "module: " <> maybe "none" (\n -> name n.value <> " " <> at n.span) h.name
  ]
    <> maybe [] (docLines src "haddock") h.haddock
    <> maybe [] warningText h.warning

warningText :: WarningText -> [Text]
warningText w =
  ("warning " <> kind w.kind <> " " <> at w.span) : map (indent . quote) w.messages
  where
    kind = \case
      DeprecatedPragma -> "deprecated"
      WarningPragma Nothing -> "warning"
      WarningPragma (Just c) -> "warning in " <> quote c

export :: SourceLines -> Export -> [Text]
export src = \case
  ExportItem i -> item src i
  ExportModule m ->
    ("module " <> name m.name.value <> " " <> at m.name.span)
      : maybe [] (map indent . warningText) m.warning
  ExportSection s -> docLines src ("section " <> Text.pack (show s.level)) s.heading
  ExportDoc d -> docLines src "doc" d
  ExportDocNamed n -> ["named doc " <> quote n.value <> " " <> at n.span]

item :: SourceLines -> Item -> [Text]
item src i =
  (itemName i.name <> subs i.subs <> " " <> at i.span)
    : maybe [] (map indent . docLines src "doc") i.doc
      <> maybe [] (map indent . warningText) i.warning
  where
    subs = \case
      NoSubs -> ""
      AllSubs -> " (..)"
      SomeSubs l ->
        let shown = map itemName l.names
            withDots = case l.wildcardAt of
              Nothing -> shown
              Just k -> take k shown <> [".."] <> drop k shown
         in " (" <> Text.intercalate ", " withDots <> ")"

itemName :: ItemName -> Text
itemName n =
  keyword n.keyword <> maybe "" (\q -> name q <> ".") n.qualifier <> n.occ <> at n.span
  where
    keyword = \case
      NoKeyword -> ""
      TypeKeyword -> "type "
      PatternKeyword -> "pattern "
      DataKeyword -> "data "
      DefaultKeyword -> "default "

importDecl :: SourceLines -> Import -> [Text]
importDecl src i =
  Text.unwords
    ( ["import"]
        <> ["{-# SOURCE #-}" | i.source == SourceImport]
        <> ["safe" | i.safe]
        <> level
        <> ["qualified" | i.qualified == QualifiedPre]
        <> maybe [] (\p -> [quote p]) i.package
        <> [name i.name.value <> at i.name.span]
        <> ["qualified" | i.qualified == QualifiedPost]
        <> maybe [] (\a -> ["as", name a.value <> at a.span]) i.as
        <> [at i.span]
    )
    : maybe [] list i.list
  where
    level = case i.level of
      NormalLevel -> []
      SpliceLevel -> ["splice"]
      QuoteLevel -> ["quote"]
    list l =
      map indent $
        ( case l.kind of
            Hiding -> "hiding:"
            Only -> "only:"
        )
          : concatMap (map indent . item src) l.items

extensions :: Extensions -> [Text]
extensions e =
  [ "language: " <> e.language,
    "from pragmas: " <> Text.intercalate ", " e.fromPragmas,
    "enabled: " <> Text.intercalate ", " e.enabled
  ]

reason :: TextOnlyReason -> Text
reason = \case
  ParseFailed e -> "parse error " <> parseError e
  PragmaFailed e -> "pragma error " <> parseError e
  Preprocessed tool -> "preprocessed by " <> tool
  ParserCrashed t -> "parser crashed: " <> t
  TooSlow -> "parse took too long"
  where
    parseError e =
      Text.unwords (maybe [] pure e.code <> maybe [] (pure . at) e.span)
        <> ": "
        <> quote e.message

-- | A doc comment: a title line, then its text in the file.
docLines :: SourceLines -> Text -> DocComment a -> [Text]
docLines src title d =
  (title <> " " <> at d.span)
    : map (indent . ("| " <>)) (Text.lines (sliceSpan src d.span))

at :: Span -> Text
at s = "@" <> renderSpan s

name :: ModuleName -> Text
name (ModuleName n) = n

indent :: Text -> Text
indent = ("  " <>)

-- | In double quotes, with only quotes, backslashes and control
-- characters escaped (so non-ASCII stays readable).
quote :: Text -> Text
quote t = "\"" <> Text.concatMap esc t <> "\""
  where
    esc = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      c -> Text.singleton c
