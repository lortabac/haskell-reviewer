{-# LANGUAGE StrictData #-}

-- StrictData, and 'forceModuleParse' for the lists: once a result is
-- built, nothing in it may point back at the GHC AST (D-61).

-- | Our own model of a module, built from the GHC AST, which is then
-- dropped (SPEC §11.4.4, D-61). Has the header, exports, imports,
-- extensions, declarations and tokens.
module HaskellReviewer.Core.ModuleModel
  ( -- * Result of a parse
    ModuleParse (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
    ParseError (..),
    ParseWarning (..),
    renderParseWarning,

    -- * Parsed module
    ParsedModule (..),
    ModuleHeader (..),
    WarningText (..),
    WarningKind (..),
    Located (..),
    RawDoc (..),
    DocItem (..),
    Export (..),
    ModuleExport (..),
    Section (..),
    Item (..),
    ItemName (..),
    NameKeyword (..),
    Subs (..),
    SubList (..),
    Import (..),
    ImportSource (..),
    Qualified (..),
    ImportLevel (..),
    ImportList (..),
    ListKind (..),
    Extensions (..),

    -- * Tier
    Tier (..),
    WhyNotPrecise (..),

    -- * Forcing
    forceModuleParse,
  )
where

import Data.Text (Text)
import HaskellReviewer.Core.Cpp.Warning (CppWarning, renderCppWarning)
import HaskellReviewer.Core.Decls
  ( Decl,
    Located (..),
    ModulePragma,
    RawDoc (..),
    WarningKind (..),
    WarningText (..),
    forceDecls,
    forcePragmas,
    forceWarningText,
  )
import HaskellReviewer.Core.Project (ModuleName)
import HaskellReviewer.Core.Span (Span)
import HaskellReviewer.Core.Tokens (Tokens)

-- * Result of a parse

-- | What we got from a module's text.
data ModuleParse
  = Parsed ParsedModule
  | -- | Shown as text, with the reason (SPEC §3.1, §8).
    TextOnly TextOnlyModule
  deriving (Eq, Show)

-- | A module we show as text.
data TextOnlyModule = TextOnlyModule
  { reason :: TextOnlyReason,
    -- | Found before we gave up. They may explain the reason (an
    -- unknown extension, say).
    warnings :: [ParseWarning]
  }
  deriving (Eq, Show)

-- | Why a module is text only.
data TextOnlyReason
  = ParseFailed ParseError
  | -- | A bad @LANGUAGE@ or @OPTIONS_GHC@ pragma, as GHC says it.
    PragmaFailed ParseError
  | -- | @-F -pgmF <tool>@, from ghc-options and @OPTIONS_GHC@ together.
    Preprocessed Text
  | -- | The parser threw. A bug, in GHC or ours.
    ParserCrashed Text
  | -- | CPP and GHC together took longer than the limit (Note [Parse
    -- time limit] in "HaskellReviewer.Effects.Parser").
    TooSlow
  deriving (Eq, Show)

-- | GHC's first error.
data ParseError = ParseError
  { message :: Text,
    -- | Like @GHC-58481@.
    code :: Maybe Text,
    span :: Maybe Span
  }
  deriving (Eq, Show)

-- | Something odd we worked around.
data ParseWarning
  = -- | Not an extension GHC knows: skipped.
    UnknownExtension Text
  | -- | Not a language GHC knows: Haskell98 instead (D-84).
    UnknownLanguage Text
  | -- | An @OPTIONS_GHC@ flag we don't apply.
    IgnoredPragmaFlag Text
  | -- | A cpp-options word that isn't @-D@, @-U@ or @-I@.
    IgnoredCppOption Text
  | -- | A CPP warning (@#error@, a bad @#if@, ...).
    FromCpp CppWarning
  | -- | markdown-unlit found no Haskell block (D-84).
    NoMarkdownCode
  | -- | Off under Safe Haskell, like GHC does (with a warning too).
    SafeTurnsOff Text
  | -- | Two Safe Haskell modes (first, then second). GHC fails; we keep
    -- the first, like GHC does before failing.
    SafeModesClash Text Text
  | -- | The parse worked but the second lex didn't: no tokens.
    CouldNotLex
  deriving (Eq, Show)

-- | For the user, like @unknown extension: Foo@.
renderParseWarning :: ParseWarning -> Text
renderParseWarning = \case
  UnknownExtension t -> "unknown extension: " <> t
  UnknownLanguage t -> "unknown language: " <> t
  IgnoredPragmaFlag t -> "ignored OPTIONS_GHC flag: " <> t
  IgnoredCppOption t -> "ignored cpp-options flag: " <> t
  FromCpp w -> renderCppWarning w
  NoMarkdownCode -> "no Haskell code block"
  SafeTurnsOff t -> "turned off by Safe: " <> t
  SafeModesClash a b -> "Safe Haskell modes clash: " <> a <> ", " <> b <> " ignored"
  CouldNotLex -> "could not lex"

-- * Parsed module

-- | The parts of a module we keep.
data ParsedModule = ParsedModule
  { header :: ModuleHeader,
    -- | 'Nothing': no export list, so everything is exported.
    exports :: Maybe [Export],
    imports :: [Import],
    extensions :: Extensions,
    warnings :: [ParseWarning],
    -- | Sorted by where they start.
    declarations :: [Decl],
    docItems :: [DocItem],
    -- | The Pragmas block (SPEC §4.2), in source order.
    pragmas :: [ModulePragma],
    -- | The whole module's, after CPP (Note [Packed tokens] in
    -- "HaskellReviewer.Core.Tokens").
    tokens :: Tokens
  }
  deriving (Eq, Show)

-- | A doc comment among the declarations that isn't on one.
data DocItem
  = -- | @-- * Heading@.
    DocHeading Section
  | -- | @-- $name@, with its text.
    DocChunk (Located Text) RawDoc
  deriving (Eq, Show)

-- | The @module@ line and what comes with it.
data ModuleHeader = ModuleHeader
  { -- | 'Nothing': no @module@ line (so it's @Main@).
    name :: Maybe (Located ModuleName),
    -- | The Haddock comment before @module@.
    haddock :: Maybe RawDoc,
    -- | A @DEPRECATED@ or @WARNING@ pragma on the module.
    warning :: Maybe WarningText
  }
  deriving (Eq, Show)

-- | One entry of the export list.
data Export
  = ExportItem Item
  | ExportModule ModuleExport
  | -- | @-- * Heading@.
    ExportSection Section
  | -- | A doc comment in the list.
    ExportDoc RawDoc
  | -- | @-- $name@: a chunk defined further down.
    ExportDocNamed (Located Text)
  deriving (Eq, Show)

-- | @module M@ in the export list.
data ModuleExport = ModuleExport
  { name :: Located ModuleName,
    warning :: Maybe WarningText
  }
  deriving (Eq, Show)

-- | A section heading in the export list.
data Section = Section
  { -- | How many stars.
    level :: Int,
    heading :: RawDoc
  }
  deriving (Eq, Show)

-- | A name in an export or import list, maybe with its subordinates.
data Item = Item
  { span :: Span,
    name :: ItemName,
    subs :: Subs,
    -- | Export lists only: @f -- ^ doc@.
    doc :: Maybe RawDoc,
    -- | Export lists only: @{-# DEPRECATED "..." #-} f@.
    warning :: Maybe WarningText
  }
  deriving (Eq, Show)

-- | A name as written.
data ItemName = ItemName
  { span :: Span,
    qualifier :: Maybe ModuleName,
    -- | Operators without parens.
    occ :: Text,
    keyword :: NameKeyword
  }
  deriving (Eq, Show)

-- | The keyword before a name.
data NameKeyword = NoKeyword | TypeKeyword | PatternKeyword | DataKeyword | DefaultKeyword
  deriving (Eq, Show)

-- | What comes in parens after a type or class.
data Subs
  = -- | @T@.
    NoSubs
  | -- | @T(..)@.
    AllSubs
  | -- | @T(A, b)@, @T(.., A)@.
    SomeSubs SubList
  deriving (Eq, Show)

-- | The names in @T(A, b)@.
data SubList = SubList
  { names :: [ItemName],
    -- | Where @..@ is among them, if it is: 0 for @T(.., A)@.
    wildcardAt :: Maybe Int
  }
  deriving (Eq, Show)

-- | One import.
data Import = Import
  { span :: Span,
    name :: Located ModuleName,
    -- | @import "pkg" M@.
    package :: Maybe Text,
    source :: ImportSource,
    safe :: Bool,
    qualified :: Qualified,
    level :: ImportLevel,
    as :: Maybe (Located ModuleName),
    list :: Maybe ImportList
  }
  deriving (Eq, Show)

-- | @import {-# SOURCE #-}@ or not.
data ImportSource = NormalImport | SourceImport
  deriving (Eq, Show)

-- | Where @qualified@ is.
data Qualified = NotQualified | QualifiedPre | QualifiedPost
  deriving (Eq, Show)

-- | @import splice@ and @import quote@ (ExplicitLevelImports).
data ImportLevel = NormalLevel | SpliceLevel | QuoteLevel
  deriving (Eq, Show)

-- | The names in parens.
data ImportList = ImportList
  { kind :: ListKind,
    items :: [Item]
  }
  deriving (Eq, Show)

-- | @import M (x)@ or @import M hiding (x)@.
data ListKind = Only | Hiding
  deriving (Eq, Show)

-- | The language and extensions the module was parsed with.
data Extensions = Extensions
  { -- | Like @GHC2021@.
    language :: Text,
    -- | Every extension that is on, by GHC's main name, sorted.
    enabled :: [Text],
    -- | Applied from the module's own pragmas, as written, in order,
    -- each once.
    -- @-cpp@ is @CPP@. Unknown names are warnings, not here.
    fromPragmas :: [Text]
  }
  deriving (Eq, Show)

-- * Tier

-- | Where a module's semantic data comes from (SPEC §3, D-87).
data Tier = Syntactic WhyNotPrecise
  deriving (Eq, Show)

-- | Why a module isn't at the precise tier. Shown in the header hint.
data WhyNotPrecise = NoHieFile
  deriving (Eq, Show)

-- * Forcing

-- | Walk every list and 'Maybe', so the result holds no thunk. StrictData
-- forces the rest, but not what is inside a list or a 'Maybe'.
forceModuleParse :: ModuleParse -> ()
forceModuleParse = \case
  Parsed m -> parsed m
  TextOnly t -> reason t.reason `seq` list t.warnings
  where
    reason = \case
      ParseFailed e -> parseError e
      PragmaFailed e -> parseError e
      _ -> ()
    parseError e = whnf e.code `seq` whnf e.span
    parsed m =
      header m.header `seq`
        maybe () (each export) m.exports `seq`
          each importDecl m.imports `seq`
            list m.extensions.enabled `seq`
              list m.extensions.fromPragmas `seq`
                list m.warnings `seq`
                  forceDecls m.declarations `seq`
                    list m.docItems `seq`
                      forcePragmas m.pragmas `seq`
                        m.tokens `seq`
                          ()
    header h = whnf h.name `seq` whnf h.haddock `seq` maybe () forceWarningText h.warning
    export = \case
      ExportItem i -> item i
      ExportModule m -> maybe () forceWarningText m.warning
      _ -> ()
    item i =
      itemName i.name `seq`
        subs i.subs `seq`
          whnf i.doc `seq`
            maybe () forceWarningText i.warning
    itemName n = whnf n.qualifier
    subs = \case
      SomeSubs l -> each itemName l.names `seq` whnf l.wildcardAt
      _ -> ()
    importDecl i =
      whnf i.package `seq`
        whnf i.as `seq`
          maybe () (each item . (.items)) i.list
    each f = foldr (\x r -> f x `seq` r) ()
    list = each (`seq` ())
    -- The value inside, to WHNF. With StrictData that is all of it.
    whnf = maybe () (`seq` ())
