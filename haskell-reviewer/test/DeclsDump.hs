-- | A plain text dump of a module's declarations, for golden tests. One
-- line per fact, two spaces per level. Spans are @line:col-line:col@.
module DeclsDump (dumpDecls, dumpTokens) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Decls
  ( ArgType (..),
    ClassDecl (..),
    ConArg (..),
    ConArgs (..),
    ConDecl (..),
    DataBody (..),
    Decl (..),
    DeclInfo (..),
    Definition (..),
    DerivStrategy (..),
    DerivingClause (..),
    Extent (..),
    FieldDecl (..),
    Fixity (..),
    FixityDir (..),
    MethodDecl (..),
    NewtypeBody (..),
    Part (..),
    PartRole (..),
    PatSynDecl (..),
    PatSynDir (..),
    RawDoc (..),
    Signature (..),
    Strictness (..),
    TypeDecl (..),
    TypeForm (..),
    Unpack (..),
    ValueDecl (..),
    headPieces,
  )
import HaskellReviewer.Core.ModuleModel
  ( DocItem (..),
    Located (..),
    ModuleParse (..),
    ParsedModule (..),
    Section (..),
    renderParseWarning,
  )
import HaskellReviewer.Core.Names (renderDeclId)
import HaskellReviewer.Core.SourceText (SourceLines, sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Span, renderSpan)
import HaskellReviewer.Core.Tokens (Token (..), Tokens, tokenList)
import ParseDump (dumpParse)

-- | The declarations and doc items of a parse of this text.
dumpDecls :: Text -> ModuleParse -> Text
dumpDecls src = \case
  r@(TextOnly _) -> dumpParse r
  Parsed m ->
    Text.unlines $
      [ "warnings: " <> Text.intercalate "; " (map renderParseWarning m.warnings)
      | not (null m.warnings)
      ]
        <> concatMap docItem m.docItems
        <> concatMap (decl (sourceLines src) m.tokens) m.declarations

-- | Every token with its class and text.
dumpTokens :: Text -> ModuleParse -> Text
dumpTokens src = \case
  TextOnly _ -> "text only\n"
  Parsed m ->
    Text.unlines
      [ at t.span <> " " <> Text.pack (show t.kind) <> " " <> quote (sliceSpan ls t.span)
      | t <- tokenList m.tokens
      ]
  where
    ls = sourceLines src

docItem :: DocItem -> [Text]
docItem = \case
  DocHeading s -> doc ("heading " <> Text.pack (show s.level)) s.heading
  DocChunk n d -> doc ("chunk " <> n.value) d

decl :: SourceLines -> Tokens -> Decl -> [Text]
decl src ts = \case
  TypeD t -> typeDecl src ts t
  ClassD c ->
    infoLines src ts "class" c.info
      <> nested
        ( opt "context" c.context
            <> map (("fundep " <>) . at) c.fundeps
            <> opt "kind sig" c.kindSig
            <> fixity c.fixity
            <> opt "minimal" c.minimal
            <> concatMap method c.methods
            <> concatMap (typeDecl src ts) c.associated
        )
    where
      method m =
        infoLines src ts "method" m.info
          <> nested
            ( signature m.signature
                <> opt "default sig" m.defaultSig
                <> ["has default" | m.hasDefault]
                <> fixity m.fixity
            )
  ValueD v ->
    infoLines src ts (definition v.definition) v.info
      <> nested (maybe [] signature v.signature <> fixity v.fixity)
  PatSynD p ->
    infoLines src ts ("pattern " <> direction p.direction) p.info
      <> nested
        ( maybe [] signature p.signature
            <> fixity p.fixity
            <> concatMap (infoLines src ts "field") p.fields
        )
  where
    definition = \case
      Equations -> "equations"
      PatternBound -> "pattern bound"
      NoDefinition -> "no definition"
    direction = \case
      Unidirectional -> "unidirectional"
      ImplicitBidirectional -> "bidirectional"
      ExplicitBidirectional -> "explicitly bidirectional"

typeDecl :: SourceLines -> Tokens -> TypeDecl -> [Text]
typeDecl src ts t =
  infoLines src ts (form t.form) t.info
    <> nested (opt "kind sig" t.kindSig <> fixity t.fixity <> formLines t.form)
  where
    form = \case
      DataType _ -> "data"
      Newtype _ -> "newtype"
      TypeData _ -> "type data"
      Synonym -> "synonym"
      OpenFamily _ -> "open family"
      DataFamily -> "data family"
      ClosedFamily _ -> "closed family"
    formLines = \case
      OpenFamily ds -> map (("default " <>) . slice) ds
      ClosedFamily es -> map (("equation " <>) . slice) es
      DataType b -> dataBody b.constructors b.derivings
      TypeData b -> dataBody b.constructors b.derivings
      Newtype b -> dataBody [b.constructor] b.derivings
      Synonym -> []
      DataFamily -> []
    dataBody :: [ConDecl] -> [DerivingClause] -> [Text]
    dataBody cs ds = concatMap con cs <> concatMap deriving_ ds
    con (c :: ConDecl) =
      infoLines src ts "constructor" c.info
        <> nested (fixity c.fixity <> conArgs c.args)
    conArgs = \case
      PrefixArgs as -> "prefix" : concatMap arg as
      InfixArgs a b -> "infix" : arg a <> arg b
      RecordArgs fs -> "record" : concatMap field fs
      GadtArgs as -> "gadt" : concatMap arg as
      GadtRecordArgs fs -> "gadt record" : concatMap field fs
    arg (a :: ConArg) = argType "arg" a.type_ : maybe [] (map indent . doc "doc") a.doc
    argType title (a :: ArgType) =
      Text.unwords
        ( [title, at a.type_, quote (slice a.type_)]
            <> strictness a.strictness
            <> unpack a.unpack
        )
    field f = infoLines src ts "field" f.info <> nested [argType "type" f.type_]
    strictness = \case
      Unmarked -> []
      Strict -> ["strict"]
      Lazy -> ["lazy"]
    unpack = \case
      NoUnpackPragma -> []
      Unpack -> ["unpack"]
      NoUnpack -> ["nounpack"]
    deriving_ d =
      Text.unwords (["deriving", at d.span] <> strategy d.strategy)
        : map (indent . ("class " <>) . slice) d.classes
    strategy = \case
      NoStrategy -> []
      StockStrategy -> ["stock"]
      NewtypeStrategy -> ["newtype"]
      AnyclassStrategy -> ["anyclass"]
      ViaStrategy s -> ["via", quote (slice s)]
    slice = sliceSpan src

-- | The lines every declaration has: id, kind, flags, extent, head,
-- Haddock.
infoLines :: SourceLines -> Tokens -> Text -> DeclInfo -> [Text]
infoLines src ts kind i =
  Text.unwords
    ([renderDeclId i.id, kind, "name" <> at i.nameSpan] <> ["exported" | i.exported])
    : nested
      ( ("extent:" : map (indent . part) (NonEmpty.toList i.extent.parts))
          <> ["head: " <> Text.unwords (headPieces src ts i.extent)]
          <> concatMap (doc "haddock") i.haddock
      )
  where
    part p = role p.role <> " " <> at p.span
    role = \case
      HeadPart -> "head"
      BodyPart -> "body"
      PragmaPart -> "pragma"
      DocPart -> "doc"

signature :: Signature -> [Text]
signature s =
  ["signature " <> at s.span <> " " <> Text.intercalate ", " (NonEmpty.toList s.names)]

fixity :: Maybe Fixity -> [Text]
fixity = \case
  Nothing -> []
  Just f ->
    [ Text.unwords
        ["fixity", dir f.direction, Text.pack (show f.precedence), at f.span]
    ]
  where
    dir = \case
      InfixLeft -> "infixl"
      InfixRight -> "infixr"
      InfixNone -> "infix"

opt :: Text -> Maybe Span -> [Text]
opt title = maybe [] (\s -> [title <> " " <> at s])

-- | A doc comment: a title line, then the text.
doc :: Text -> RawDoc -> [Text]
doc title d = (title <> " " <> at d.span) : map (indent . ("| " <>)) (Text.lines d.text)

nested :: [Text] -> [Text]
nested = map indent

at :: Span -> Text
at s = "@" <> renderSpan s

indent :: Text -> Text
indent = ("  " <>)

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
