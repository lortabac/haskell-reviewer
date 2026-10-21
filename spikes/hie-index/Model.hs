{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Sketch of the M1 module model, with the place for precise data
-- (M0 spike T08, D-87). Types as documentation, not in any build.
--
-- 'Span' and 'Namespace' are shared with the index. In M1 they live in
-- the main library; in M8 they move to @haskell-reviewer-index@, which the
-- main library then re-exports.
--
-- Rule: identity is always syntactic. Declarations and instances are
-- matched across sides by their syntactic 'DeclId', on any tier, so a
-- side with HIE and one without still pair up. HIE only picks CPP
-- branches (part of the parse key); otherwise the syntax is ours. Precise
-- data adds facts (exact links, types, instances used, TH declarations)
-- on top.
module Model where

import Data.ByteString (ByteString)
import Data.IntMap.Strict (IntMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Index (LineRange, Namespace (..), Span)

-- * Identity (D-44)

data ModuleId = ModuleId
  { package :: Text,
    component :: Component,
    moduleName :: Text
  }

data Component
  = Library
  | SubLibrary Text
  | Executable Text
  | TestSuite Text
  | Benchmark Text
  | -- | "Other files" and the implicit package (§2.3, §8).
    NoComponent

-- * Name keys

-- | What a link points at. Same parts as the index key (Note [Name keys]
-- in Index), with the unit already mapped.
data NameKey = NameKey
  { home :: ModuleRef,
    namespace :: Namespace,
    occ :: Text,
    -- | GHC's field parent: the type's first constructor, or the pattern
    -- synonym. For URLs and 'Field' the main binary maps a constructor to
    -- its type (the constructor's 'Index.Decl' has it as parent), except
    -- for data instance fields.
    fieldParent :: Maybe Text
  }

data ModuleRef = Home ModuleId | External PackageRef Text

-- | A dependency package, with how it was found (D-42).
data PackageRef = PackageRef
  { name :: Text,
    -- | Only from plan.json or HIE.
    version :: Maybe Text,
    foundBy :: PackageSource
  }

data PackageSource = FromPlan | FromHie | FromBuildDepends | NotFound

-- * Declarations

-- | §4.1 namespaces: what URLs, the UI and the semantic diff use. Fields
-- and instances have their own 'DeclId' case.
data DeclNs = DType | DClass | DCon | DVal | DPat

-- | Matching key across sides (§4.6, §11.4.8).
data DeclId
  = Named DeclNs Text
  | -- | Parent and field name. The parent is the field's type; for a
    -- pattern synonym record field, the pattern synonym; for a data
    -- instance field, the instance's first constructor (as in GHC). URL:
    -- @field:Config.port@ (§4.1). Unique even with DuplicateRecordFields.
    Field Text Text
  | Inst InstanceKey

-- | Class plus α-renamed head, printed (D-20). Computed from the syntactic
-- model; for TH-generated instances, printed from 'PType'. Both printers
-- must give the same text, so keys match across tiers.
newtype InstanceKey = InstanceKey Text

-- | The index key namespace of a declaration. Instances have no name.
keyNamespace :: DeclId -> Maybe Namespace
keyNamespace d = case d of
  Named DType _ -> Just NsTypeOrClass
  Named DClass _ -> Just NsTypeOrClass
  Named DCon _ -> Just NsCon
  Named DPat _ -> Just NsCon
  Named DVal _ -> Just NsVal
  Field _ _ -> Just NsField
  Inst _ -> Nothing

-- * Syntactic model (D-61)

-- | One per (blob, parse settings), shared by both sides. Never changed by
-- HIE data. Header, exports, imports, extensions, head tokens, Haddock
-- and the token stream are as in §11.4.4 and not sketched here.
data ModuleModel = ModuleModel
  { decls :: [Declaration],
    -- | Indexed by occurrence number; an unboxed table in the real code.
    occurrences :: [SynOccurrence],
    locals :: [LocalBinder],
    -- | The #if groups and excluded regions of our CPP pass.
    cpp :: Maybe CppInfo
  }

data Declaration = Declaration
  { declId :: DeclId,
    -- | Span of the name at its binding site. Same convention as the
    -- index (Note [Span convention]), so it joins with HIE by equality.
    nameSpan :: Maybe Span,
    extent :: NonEmpty Span,
    parent :: Maybe DeclId,
    signature :: Signature,
    exported :: Bool
  }

-- | Values only: whether a type signature covers this name. A value with
-- 'NoSignature' gets an inferred type at the precise tier (§4.2, §4.6).
data Signature = HasSignature Span | NoSignature | NotAValue

data SynOccurrence = SynOccurrence
  { span :: Span,
    occ :: Text,
    namespace :: Namespace,
    qualifier :: Maybe Text,
    scope :: SynScope
  }

-- | What the parser knows on its own. Imports are resolved per side.
data SynScope
  = BindsLocal LocalId
  | UsesLocal LocalId
  | BindsTop DeclId
  | -- | Top-level or imported: resolved per side (§11.4.5).
    Free

newtype LocalId = LocalId Int

data LocalBinder = LocalBinder {occ :: Text, binder :: Span}

-- | Groups, branches, excluded regions (D-55). Not sketched here.
data CppInfo = CppInfo

-- * Per side

-- | One module on one side: shared syntax, plus this side's tier and links.
data SideModule = SideModule
  { moduleId :: ModuleId,
    syntax :: ModuleModel,
    tier :: Tier,
    -- | One per occurrence, same order as syntax.occurrences.
    links :: [Link]
  }

-- | The module's tier label (header: "precise (41/43 modules)", §3).
-- Replaced as a unit, with 'links', whenever verified-current changes
-- (reload, an import going stale).
data Tier = Syntactic WhyNotPrecise | Precise PreciseOverlay

-- | Shown in the header hint (§8).
data WhyNotPrecise
  = NoHieFile
  | HieStale
  | ImportNotCurrent ModuleId
  | StanzaChanged
  | HieInSnapshot
  | NoCompanion Text
  | CompanionFailed Text
  | CannotMatch

-- | A link and how sure we are. Precise links are never heuristic.
data Link
  = Exact ExactBy Ref
  | -- | Best guess, then the other candidates. Dotted underline (§3).
    Heuristic Ref [Ref]
  | Unresolved

-- | Where an exact link comes from: the HIE index, or the import and
-- export lists (one candidate left, §3).
data ExactBy = ByHie | ByImports

data Ref = ToName NameKey | ToLocal LocalId

-- * Precise overlay

-- | What HIE adds to one module. Built from one decoded 'Index.ModuleIndex'
-- joined to the syntactic model: declarations by key, occurrences by span,
-- locals by (binder span, name) where that pair is unique. Occurrences
-- HIE doesn't cover keep their syntactic link.
data PreciseOverlay = PreciseOverlay
  { -- | Hash of the .hie bytes (the cache key). Both sides share the
    -- overlay when both are current with the same .hie.
    hieHash :: ByteString,
    -- | Type of each value declaration, signature or not.
    declTypes :: Map DeclId PType,
    -- | TH-generated declarations, with the splice span (§3.1).
    generated :: [GeneratedDecl],
    -- | Instance types, including derived and TH ones. Joined by span.
    instanceTypes :: Map DeclId PType,
    -- | Hover type per occurrence number: the type at the use if it
    -- differs, else the binder's or the name's type.
    hoverTypes :: IntMap PType,
    -- | Instance picked per occurrence number, for hover and links (§3).
    instanceUses :: IntMap InstanceRef,
    -- | For the CPP branch choice (Note [CPP branches from HIE]).
    coveredLines :: [LineRange]
  }

-- | An instance in a home module, or in a dependency.
data InstanceRef = InstanceRef ModuleRef InstanceKey

data GeneratedDecl = GeneratedDecl
  { declId :: DeclId,
    splice :: Maybe Span,
    parent :: Maybe DeclId,
    ty :: Maybe PType
  }

-- | A neutral type as a tree, with links ('Index.TypeNode' with name
-- keys). Printed by one printer for every GHC: links, Haddock-style
-- wrapping (§5.5), and a canonical α-renamed text for comparisons.
data PType
  = PVar Text
  | PCon NameKey [PType]
  | PPromoted NameKey [PType]
  | PTuple [PType]
  | PList PType
  | PApp PType [PType]
  | PFun (Maybe PType) PType PType
  | PForall [Text] PType
  | PQual [PType] PType
  | PLit Text
  | POther

-- * What the UI gets

-- | The type line of a value in the docs view (§4.2).
data TypeShown
  = -- | From the signature (tokens of the syntactic model).
    Declared
  | -- | No signature, precise tier: marked "inferred".
    Inferred PType
  | NoType

{- Note [Tiers in the UI]

- Docs view (§4.2): each value shows Declared, or Inferred (precise tier,
  no signature). TH-generated declarations appear with a "generated"
  marker. The module header shows the module's tier.
- Source view (§4.4): each token's link is a Link. Exact links are
  plain, Heuristic ones dotted with a tooltip. Hover: hoverTypes at the
  precise tier, else the declared signature of the target. At the
  precise tier, an overloaded name also shows the instance it picks
  (instanceUses), linked.
- References: built per side from links (§11.4.5). Each site keeps its
  Link's provenance, so a list says "exact" or "heuristic" per entry.
- Expanded diff (§4.6): "inferred type changed" holds when the
  declaration is a value with NoSignature on both sides, both sides are
  Precise, and the canonical text of the two declTypes differs. Then the
  entry is promoted to "signature changed".
- TH-generated declarations are compared across sides only when both
  sides are Precise. Otherwise they are "not compared": never reported
  as removed or added.
- Header: per side, the count of Precise modules (§3).
-}
