-- | A plain text dump of a module's declarations, for golden tests. One
-- line per fact, two spaces per level. Spans are @line:col-line:col@.
module DeclsDump (dumpDecls, dumpTokens) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Decls
  ( AnnTarget (..),
    ArgType (..),
    ClassDecl (..),
    ConArg (..),
    ConArgs (..),
    ConDecl (..),
    DataBody (..),
    Decl (..),
    DeclInfo (..),
    Definition (..),
    DerivStrategy (..),
    Derived (..),
    DerivedClass (..),
    DerivingClause (..),
    Extent (..),
    FamInstDecl (..),
    FamInstForm (..),
    FieldDecl (..),
    Fixity (..),
    FixityDir (..),
    InstanceBody (..),
    InstanceDecl (..),
    InstanceForm (..),
    InstanceMethod (..),
    MethodDecl (..),
    ModulePragma (..),
    NewtypeBody (..),
    OpenFamilyBody (..),
    Part (..),
    PartRole (..),
    PatSynDecl (..),
    PatSynDir (..),
    PragmaKind (..),
    RawDoc (..),
    Signature (..),
    Strictness (..),
    TypeDecl (..),
    TypeForm (..),
    Unpack (..),
    ValueDecl (..),
    WarningKind (..),
    WarningText (..),
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
        <> map pragma m.pragmas

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

-- | One line per pragma of the Pragmas block.
pragma :: ModulePragma -> Text
pragma p =
  Text.unwords $
    ["pragma", at p.span] <> case p.kind of
      Rules ns -> "rules" : ns
      Complete ns t -> "complete" : ns <> maybe [] (\x -> [":: " <> x]) t
      Annotation t ->
        ["ann"] <> case t of
          AnnValue n -> [n]
          AnnType n -> ["type", n]
          AnnModule -> ["module"]
      Default c -> "default" : maybe [] pure c

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
            <> opt "roles" c.roles
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
      <> nested
        ( maybe [] signature v.signature
            <> map (("foreign export " <>) . at) v.foreignExports
            <> fixity v.fixity
        )
  PatSynD p ->
    infoLines src ts ("pattern " <> direction p.direction) p.info
      <> nested
        ( maybe [] signature p.signature
            <> fixity p.fixity
            <> concatMap (infoLines src ts "field") p.fields
        )
  InstanceD i -> instanceDecl src ts i
  FamInstD f -> famInstDecl src ts f
  SpliceD e -> "splice" : nested (extentLines src ts e)
  where
    definition = \case
      Equations -> "equations"
      PatternBound -> "pattern bound"
      ForeignImport -> "foreign import"
      NoDefinition -> "no definition"
    direction = \case
      Unidirectional -> "unidirectional"
      ImplicitBidirectional -> "bidirectional"
      ExplicitBidirectional -> "explicitly bidirectional"

-- | An instance, its methods and associated instances.
instanceDecl :: SourceLines -> Tokens -> InstanceDecl -> [Text]
instanceDecl src ts i =
  infoLines src ts (Text.unwords (kind <> ["orphan" | i.orphan])) i.info
    <> nested body
  where
    (kind, body) = case i.form of
      ExplicitInstance b ->
        ( ["instance"],
          concatMap method b.methods <> concatMap (famInstDecl src ts) b.associated
        )
      StandaloneDeriving s -> ("standalone deriving" : strategy src s, [])
      DerivedInstance d ->
        ("derived" : strategy src d.strategy <> ["on", renderDeclId d.onType], [])
    method m =
      ("method " <> m.name.value <> " name" <> at m.name.span)
        : nested (extentLines src ts m.extent)

-- | A type or data instance.
famInstDecl :: SourceLines -> Tokens -> FamInstDecl -> [Text]
famInstDecl src ts f =
  infoLines src ts (Text.unwords ([kind, "of", f.family] <> ["orphan" | f.orphan])) f.info
    <> nested body
  where
    (kind, body) = case f.form of
      TypeInstance -> ("type instance", [])
      DataInstance b -> ("data instance", dataBody src ts b.constructors b.derivings)
      NewtypeInstance b ->
        ("newtype instance", dataBody src ts [b.constructor] b.derivings)

typeDecl :: SourceLines -> Tokens -> TypeDecl -> [Text]
typeDecl src ts t =
  infoLines src ts (form t.form) t.info
    <> nested
      ( opt "kind sig" t.kindSig
          <> opt "roles" t.roles
          <> fixity t.fixity
          <> formLines t.form
      )
  where
    form = \case
      DataType _ -> "data"
      Newtype _ -> "newtype"
      TypeData _ -> "type data"
      Synonym -> "synonym"
      OpenFamily _ -> "open family"
      DataFamily _ -> "data family"
      ClosedFamily _ -> "closed family"
    formLines = \case
      OpenFamily b ->
        map (("default " <>) . slice) b.defaults <> map instanceLine b.instances
      ClosedFamily es -> map (("equation " <>) . slice) es
      DataType b -> dataBody src ts b.constructors b.derivings
      TypeData b -> dataBody src ts b.constructors b.derivings
      Newtype b -> dataBody src ts [b.constructor] b.derivings
      Synonym -> []
      DataFamily is -> map instanceLine is
    instanceLine = ("instance " <>) . renderDeclId
    slice = sliceSpan src

-- | Constructors and deriving clauses.
dataBody :: SourceLines -> Tokens -> [ConDecl] -> [DerivingClause] -> [Text]
dataBody src ts cs ds = concatMap con cs <> concatMap deriving_ ds
  where
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
      Text.unwords (["deriving", at d.span] <> strategy src d.strategy)
        : map (indent . derived) d.classes
    derived c = Text.unwords ["class", slice c.span, renderDeclId c.id]
    slice = sliceSpan src

strategy :: SourceLines -> DerivStrategy -> [Text]
strategy src = \case
  NoStrategy -> []
  StockStrategy -> ["stock"]
  NewtypeStrategy -> ["newtype"]
  AnyclassStrategy -> ["anyclass"]
  ViaStrategy s -> ["via", quote (sliceSpan src s)]

-- | The lines every declaration has: id, kind, flags, extent, head,
-- Haddock.
infoLines :: SourceLines -> Tokens -> Text -> DeclInfo -> [Text]
infoLines src ts kind i =
  Text.unwords
    ([renderDeclId i.id, kind, "name" <> at i.nameSpan] <> ["exported" | i.exported])
    : nested
      ( extentLines src ts i.extent
          <> concatMap (doc "haddock") i.haddock
          <> maybe [] warning i.warning
      )
  where
    warning w =
      Text.unwords ["warning", warningKind w.kind, at w.span]
        : map (indent . ("| " <>)) w.messages
    warningKind = \case
      DeprecatedPragma -> "deprecated"
      WarningPragma Nothing -> "warning"
      WarningPragma (Just c) -> "warning in " <> c

-- | The parts of an extent and its head tokens.
extentLines :: SourceLines -> Tokens -> Extent -> [Text]
extentLines src ts e =
  ("extent:" : map (indent . part) (NonEmpty.toList e.parts))
    <> ["head: " <> Text.unwords (headPieces src ts e)]
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
