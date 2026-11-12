{-# LANGUAGE StrictData #-}

-- | Identifiers in doc comments (@'foo'@) to declarations of the same
-- module (SPEC §11.4.9), and the names of a module's doc comments, for
-- resolution to go through the imports (SPEC §11.4.5).
--
-- Names are resolved when the page is encoded, not stored in the model.
module HaskellReviewer.Core.DocLinks
  ( -- * Local names
    LocalNames,
    localNames,
    noLocalNames,
    resolveLocal,

    -- * Names in docs
    DocRef (..),
    docRef,
    docNames,
  )
where

import Control.Applicative ((<|>))
import Data.Containers.ListUtils (nubOrdOn)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import HaskellReviewer.Core.Decls
  ( ClassDecl (..),
    ConArg (..),
    ConArgs (..),
    ConDecl (..),
    Decl (..),
    DeclInfo (..),
    DocComment (..),
    FamInstDecl (..),
    FieldDecl (..),
    InstanceBody (..),
    InstanceDecl (..),
    InstanceForm (..),
    MethodDecl (..),
    NameLevel (..),
    NamedDecl (..),
    PatSynDecl (..),
    TypeDecl (..),
    ValueDecl (..),
    famInstConstructors,
    formConstructors,
    namedDecls,
  )
import HaskellReviewer.Core.ModuleModel
  ( DocItem (..),
    Export (..),
    Item (..),
    ModuleHeader (..),
    ParsedModule (..),
    Section (..),
  )
import HaskellReviewer.Core.Names (DeclId, declName)
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RichText
  ( Block (..),
    Cell (..),
    DefItem (..),
    Doc (..),
    Header (..),
    Hyperlink (..),
    Inline (..),
    ListItem (..),
    ModLink (..),
    NameRef (..),
    RefNamespace (..),
    TableBody (..),
  )
import HaskellReviewer.Core.Span (Span)

{- Note [Local doc links]

Two tables, by occ name, as GHC's two namespaces:

- types: types, classes, synonyms, families, associated types, and the
  constructors of a @type data@ (they live at the type level);
- values: values, methods, foreign imports, data constructors (also of
  data instances), pattern synonyms, record fields.

Instances have no name, so nothing links to them. If a name is declared
twice, the first one wins.

@v'x'@ looks only at values, @t'T'@ only at types, plain @'T'@ at types
first, as Haddock prefers the type when both exist. A qualified name
counts only with this module's name (@Main@ when there is no @module@
line), as for the orphan check. A name that isn't local goes through
the imports: Note [Names in docs].
-}

-- | The names declared in a module. See Note [Local doc links].
data LocalNames = LocalNames
  { self :: ModuleName,
    types :: Map Text DeclId,
    values :: Map Text DeclId
  }

-- | The tables for these declarations. The module name is the
-- header's, if there is one.
localNames :: Maybe ModuleName -> [Decl] -> LocalNames
localNames name decls =
  LocalNames
    { self = fromMaybe (ModuleName "Main") name,
      types = table [n.info.id | n <- entries, n.level == TypeLevel],
      values = table [n.info.id | n <- entries, n.level == ValueLevel]
    }
  where
    entries = concatMap namedDecls decls
    -- Keep the first.
    table is = Map.fromListWith (\_ old -> old) [(k, i) | i <- is, Just k <- [declName i]]

-- | No names: for code that needs no links.
noLocalNames :: LocalNames
noLocalNames =
  LocalNames {self = ModuleName "Main", types = Map.empty, values = Map.empty}

-- | The local declaration a name in a comment means, if any.
resolveLocal :: LocalNames -> NameRef -> Maybe DeclId
resolveLocal names ref
  | maybe False (/= names.self) ref.qualifier = Nothing
  | otherwise = case ref.namespace of
      ValueNs -> value
      TypeNs -> type_
      AnyNs -> type_ <|> value
  where
    value = Map.lookup ref.occ names.values
    type_ = Map.lookup ref.occ names.types

-- * Names in docs

{- Note [Names in docs]

A name in a doc comment that isn't local ('resolveLocal') goes through
the imports, like a free token (Note [Resolution] in
"HaskellReviewer.Core.Resolve", where the rules for each namespace
are). Resolution looks up each distinct 'DocRef' once.

'docNames' lists the names of every doc comment the page sends: the
header's, the export list's (item docs, section headings, doc
comments), the doc items', and every declaration's, nested ones and
constructor arguments too. A comment several declarations share is
there once (by span), as the page sends it once (Note [Sent once] in
"HaskellReviewer.Server.Wire.Page"). They come in page order, as the
targets do.
-}

-- | The key a doc name resolves by: its namespace, qualifier and name.
-- Parens and backticks don't count.
data DocRef = DocRef
  { namespace :: RefNamespace,
    qualifier :: Maybe ModuleName,
    occ :: Text
  }
  deriving (Eq, Ord, Show)

-- | A name's key.
docRef :: NameRef -> DocRef
docRef r = DocRef {namespace = r.namespace, qualifier = r.qualifier, occ = r.occ}

-- | Every name in the module's doc comments, in page order. See Note
-- [Names in docs].
docNames :: ParsedModule -> [NameRef]
docNames m =
  concatMap snd . nubOrdOn fst $
    [doc d | d <- toList m.header.haddock]
      <> concatMap export (concat (toList m.exports))
      <> concatMap declDocs m.declarations
      <> map docItem m.docItems
  where
    doc (d :: DocComment Doc) = (d.span, docRefs d.parsed)
    heading (s :: Section) = (s.heading.span, inlineRefs s.heading.parsed)
    export = \case
      ExportItem i -> map doc (toList i.doc)
      ExportSection s -> [heading s]
      ExportDoc d -> [doc d]
      _ -> []
    docItem = \case
      DocHeading s -> heading s
      DocChunk _ d -> doc d

-- | The doc comments of a declaration and of those inside it, in page
-- order, by span.
declDocs :: Decl -> [(Span, [NameRef])]
declDocs = \case
  TypeD t -> typeDecl t
  ClassD c ->
    info c.info
      <> concatMap (info . (.info)) c.methods
      <> concatMap typeDecl c.associated
  ValueD v -> info v.info
  PatSynD p -> info p.info <> concatMap info p.fields
  InstanceD i ->
    info i.info <> case i.form of
      ExplicitInstance b -> concatMap famInst b.associated
      _ -> []
  FamInstD f -> famInst f
  SpliceD _ -> []
  where
    info (i :: DeclInfo) = [(d.span, docRefs d.parsed) | d <- i.haddock]
    typeDecl t = info t.info <> concatMap con (formConstructors t.form)
    famInst f = info f.info <> concatMap con (famInstConstructors f.form)
    con c = info c.info <> args c.args
    args = \case
      PrefixArgs as -> concatMap arg as
      InfixArgs a b -> arg a <> arg b
      RecordArgs fs -> concatMap field fs
      GadtArgs as -> concatMap arg as
      GadtRecordArgs fs -> concatMap field fs
    arg (a :: ConArg) = [(d.span, docRefs d.parsed) | d <- toList a.doc]
    field (f :: FieldDecl) = info f.info

-- | The names of a doc, in order.
docRefs :: Doc -> [NameRef]
docRefs d = concatMap blockRefs d.blocks

blockRefs :: Block -> [NameRef]
blockRefs = \case
  Paragraph is -> inlineRefs is
  Heading h -> inlineRefs h.title
  CodeBlock is -> inlineRefs is
  UnorderedList items -> concatMap (concatMap blockRefs) items
  OrderedList items -> concatMap (concatMap blockRefs . (.content)) items
  DefinitionList items ->
    concat [inlineRefs i.term <> concatMap blockRefs i.definition | i <- items]
  Examples _ -> []
  Property _ -> []
  Table t -> concat [inlineRefs c.content | row <- t.header <> t.body, c <- row]

inlineRefs :: [Inline] -> [NameRef]
inlineRefs = concatMap $ \case
  Str _ -> []
  Emphasis is -> inlineRefs is
  Bold is -> inlineRefs is
  Code is -> inlineRefs is
  Math _ _ -> []
  Link l -> foldMap inlineRefs l.label
  Name r -> [r]
  ModuleLink l -> foldMap inlineRefs l.label
