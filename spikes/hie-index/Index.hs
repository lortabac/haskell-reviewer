{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Sketch of the neutral HIE index (M0 spike T08, D-87).
--
-- Types as documentation, not in any build. The real types and the CBOR
-- codecs go in @haskell-reviewer-index@ in M8 (D-58). Check by hand:
-- @ghc -fno-code spikes/hie-index/Index.hs@.
--
-- A companion (@haskell-reviewer-hie-X.Y@) turns one @.hie@ file into one
-- 'ModuleIndex'. It does no project-level work: matching a record to a
-- module of the project, the verified-current check, resolution and
-- printing all happen in the main binary, so they are written once for
-- every GHC.
module Index
  ( -- * Stream
    Header (..),
    FileResult (..),
    currentMajor,
    currentMinor,

    -- * One module
    ModuleIndex (..),
    SourceId (..),
    StrId (..),
    NameIdx (..),
    LocalIdx (..),
    TypeId (..),
    NameEntry (..),
    Namespace (..),
    LocalEntry (..),
    Decl (..),
    DeclKind (..),
    Instance (..),
    InstanceUse (..),
    Occurrence (..),
    Target (..),
    Role (..),
    UseType (..),
    LineRange (..),

    -- * Types
    TypeNode (..),
    TupleSort (..),

    -- * Spans
    Span (..),
    Line (..),
    Col (..),
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32)

-- * Stream

-- | Format version written by this code. See Note [Versioning].
currentMajor, currentMinor :: Int
currentMajor = 1
currentMinor = 0

-- | First item on the companion's stdout, and first item of every cache
-- file. See Note [Index stream].
data Header = Header
  { -- | Always @"haskell-reviewer-index"@.
    magic :: Text,
    formatMajor :: Int,
    formatMinor :: Int,
    -- | Version of the repo the companion was built from. Part of the
    -- cache key, so a companion fix refreshes the cache.
    companionVersion :: Text,
    -- | Full version of the GHC the companion links, e.g. @"9.12.2"@.
    ghcVersion :: Text
  }

-- | One per input path, in input order. A crash or a timeout gives no
-- record: the main binary knows which path was next (§11.4.6). Paths are
-- CBOR bytes on the wire (they may not be UTF-8).
data FileResult
  = FileOk FilePath ModuleIndex
  | -- | Read but not converted, e.g. a GHC version the companion can't read.
    FileFailed FilePath Text

-- * One module

-- | Everything the main binary needs from one @.hie@ file. Indexes
-- ('StrId', 'NameIdx', …) point into the tables of the same record.
data ModuleIndex = ModuleIndex
  { -- | GHC version from the @.hie@ header, e.g. @"9.12.4"@.
    hieGhcVersion :: Text,
    -- | Interned strings. Unit ids, module names and names repeat a lot.
    strings :: [Text],
    -- | Unit id as GHC wrote it, e.g. @"pandoc-3.1-inplace"@. Mapped to
    -- package and component by the main binary (Note [Matching a record]).
    unit :: StrId,
    moduleName :: StrId,
    -- | @hie_hs_file@ as GHC got it, often relative to the package dir.
    sourcePath :: StrId,
    -- | Identity of the source bytes inside the @.hie@ (pre-CPP).
    source :: SourceId,
    names :: [NameEntry],
    locals :: [LocalEntry],
    -- | Shared type nodes. A node only points to nodes before it, so the
    -- table is a DAG and decoding can check that in one pass.
    types :: [TypeNode],
    -- | Top-level declarations of this module, including TH-generated ones.
    decls :: [Decl],
    instances :: [Instance],
    -- | Which instance each use of an overloaded name picks.
    instanceUses :: [InstanceUse],
    -- | Everything the module exports, re-exports included.
    exports :: [NameIdx],
    -- | Imported module names, as GHC saw them after CPP. Used for the
    -- transitive verified-current check.
    imports :: [StrId],
    -- | Sorted by start position. Several may share a span (record puns).
    occurrences :: [Occurrence],
    -- | Types at uses, only where they differ from the binder's type.
    useTypes :: [UseType],
    -- | Lines covered by source (not generated) AST nodes. See
    -- Note [CPP branches from HIE].
    coveredLines :: [LineRange]
  }

-- | Size plus git blob ids of the source bytes. The main binary already
-- has the blob id of every file on a side (D-75), so the check costs no
-- hashing on our side. Both formats, so the cache works for any repo.
data SourceId = SourceId
  { size :: Int,
    gitSha1 :: ByteString,
    gitSha256 :: ByteString
  }

newtype StrId = StrId Word32

newtype NameIdx = NameIdx Word32

newtype LocalIdx = LocalIdx Word32

newtype TypeId = TypeId Word32

-- | A top-level name, from this module or any other. The key part is
-- unit, module, namespace, name and field parent. See Note [Name keys].
data NameEntry = NameEntry
  { unit :: StrId,
    moduleName :: StrId,
    namespace :: Namespace,
    occ :: StrId,
    -- | For fields only: GHC's field parent (the first constructor of the
    -- type, or the pattern synonym). Tells apart duplicate record fields.
    fieldParent :: Maybe StrId,
    -- | The name's type as seen here (for hover). Nothing for type
    -- constructors and classes (we don't keep kinds).
    ty :: Maybe TypeId
  }

-- | GHC's own namespaces, which every GHC version gives us. Types and
-- classes share one, as do constructors and pattern synonyms, so the key
-- stays unique in a module. The §4.1 namespace is 'DeclKind'.
data Namespace = NsTypeOrClass | NsCon | NsField | NsVal

-- | A non-top-level binder: local variable, argument, type variable. Its
-- 'LocalIdx' is its id (the companion dedupes by GHC's Name unique).
data LocalEntry = LocalEntry
  { occ :: StrId,
    -- | The binder's span. Not unique: RecordWildCards binders share the
    -- @..@ span, TH locals share the splice span. Can be multi-line.
    binder :: Span,
    ty :: Maybe TypeId
  }

-- | A top-level declaration, found from binding sites in the AST and
-- from the exports.
data Decl = Decl
  { name :: NameIdx,
    kind :: DeclKind,
    -- | Span of the name at its binding site. Nothing when there is no
    -- real span in this file. See Note [TH-generated declarations].
    site :: Maybe Span,
    -- | The type of a constructor, the type of a field, the class of a
    -- method or associated type. Needed to show TH-generated declarations
    -- in the right place.
    parent :: Maybe NameIdx
  }

-- | Maps onto the §4.1 namespaces: type (data, newtype, synonym, family),
-- class, con, field, val (values and class methods), pat. GHC 9.12+ gives
-- it in hie_entity_infos; on 9.10 the companion works it out from
-- ContextInfo and DeclType.
data DeclKind
  = KData
  | KTypeSynonym
  | KTypeFamily
  | KClass
  | KCon
  | KPatSyn
  | KField
  | KClassMethod
  | KValue

-- | One class instance (explicit, derived, standalone, or from TH). Read
-- from the EvInstBind dfuns, before GeneratedInfo nodes are dropped
-- (derived instances live there).
data Instance = Instance
  { cls :: NameIdx,
    -- | The visible class arguments (every parameter of a
    -- multi-parameter class). The main binary prints the D-20 key.
    args :: [TypeId],
    -- | The full instance type, context included, for display.
    ty :: TypeId,
    -- | The dfun's span: the instance declaration, or the deriving item.
    site :: Maybe Span
  }

-- | An overloaded name at one use and the instance it picks (from an
-- EvidenceVarUse that leads to a dfun). The instance may be in another
-- module: the main binary finds it by module and D-20 key.
data InstanceUse = InstanceUse
  { -- | Position in 'occurrences'.
    occurrence :: Word32,
    -- | Where the dfun is defined.
    unit :: StrId,
    moduleName :: StrId,
    cls :: NameIdx,
    args :: [TypeId]
  }

-- | One identifier in the source. Only single-line spans are kept.
data Occurrence = Occurrence
  { span :: Span,
    target :: Target,
    role :: Role
  }

data Target = TGlobal NameIdx | TLocal LocalIdx

-- | Coarse, enough for references and the UI. Other HIE contexts (record
-- update, evidence) become 'Use' or are dropped.
data Role
  = Use
  | -- | Binding site of the name. Includes MatchBind: the function name in
    -- each of its equations.
    Def
  | -- | The name in its own type signature.
    Sig
  | Import
  | Export
  | -- | A method defined inside an instance (points at the class method).
    InstanceDef

-- | The type of an identifier at one use, e.g. @mempty :: Text@.
data UseType = UseType
  { -- | Position in 'occurrences'.
    occurrence :: Word32,
    ty :: TypeId
  }

-- | Inclusive.
data LineRange = LineRange {start :: Line, end :: Line}

-- * Types

-- | A small, GHC-neutral type AST. Kinds and invisible arguments are
-- dropped; casts keep their inner type. Printing (Haddock-style wrapping,
-- links, α-renaming for D-20 keys) is done once, in the main binary.
-- α-renaming goes by position, not by name. Unboxed sums are a 'TCon' of
-- the sum type constructor.
data TypeNode
  = -- | Type variable, by name.
    TVar StrId
  | -- | Type constructor applied to its visible arguments.
    TCon NameIdx [TypeId]
  | -- | Promoted data constructor (@'Just a@). A promoted tuple
    -- @'(a, b)@ is the tuple data constructor here, not a 'TTuple'.
    TPromoted NameIdx [TypeId]
  | TTuple TupleSort [TypeId]
  | TList TypeId
  | -- | Variable (or other non-constructor) head applied to arguments.
    TApp TypeId [TypeId]
  | -- | Multiplicity (Nothing for an ordinary arrow), argument, result.
    TFun (Maybe TypeId) TypeId TypeId
  | -- | Specified binders only (inferred ones are dropped). A required
    -- @forall a ->@ is printed as a plain forall: lossy.
    TForall [StrId] TypeId
  | -- | Constraints @=>@ body.
    TQual [TypeId] TypeId
  | -- | Type-level literal, as written: @42@, @"foo"@, @'c'@.
    TLit Text
  | -- | Anything else (coercions): printed as @_@.
    TOther

data TupleSort = Boxed | Unboxed | ConstraintTuple

-- * Spans

newtype Line = Line Word32

newtype Col = Col Word32

-- | See Note [Span convention] in HaskellReviewer.Core.Span.
data Span = Span
  { startLine :: Line,
    startCol :: Col,
    endLine :: Line,
    endCol :: Col
  }

{- Note [Index stream]

The main process starts a companion in batch mode and writes the .hie
paths (real paths, after the trust check of §3) to its stdin, separated
by NUL bytes, then closes stdin. The companion writes CBOR items to
stdout: one Header, then one FileResult per path, in input order, and
flushes stdout after each record. The main process times each file from the end of the previous record
(per-file timeout, §11.4.6). If the companion dies, the next path in
order is the one at fault: syntactic tier for it, a new companion for
the rest.

The main process checks each record before using it: the header (magic,
major), every index in range, type nodes only point backwards, spans
inside the source's line count, strings valid UTF-8. A bad record is
treated like a crash. The .hie files are the user's build output, but
the code they come from is untrusted (TH can make odd names), so names
are always shown as text.

Cache (§11.4.6): the main process writes Header + the one record,
atomically, to the file
  <cache>/hie/<formatMajor>/<sha256 of the .hie>-<companionVersion>.cbor
where <cache> is $XDG_CACHE_HOME/haskell-reviewer.
-}

{- Note [Versioning]

- formatMajor changes when an old reader would misread a new record:
  a field removed or with a new meaning, a new constructor or enum value,
  a change to a packed layout. The main binary reads exactly its own
  major. Other majors: syntactic tier, hint "rebuild the companion with
  make install-hie GHC=X.Y". The cache dir is per major, so old entries
  are never read (and get pruned after 30 days).
- formatMinor changes when fields are added: a new key in a CBOR map.
  Readers ignore keys they don't know. A missing key a reader needs makes
  the record bad.
- companionVersion is the repo version. It is in the cache key, not
  checked for compatibility: main binary and companion come from the same
  repo, and the major says whether they can talk.
- The main binary reads the GHC version from the .hie header itself, to
  pick the companion (§11.4.6). The versions in Header and ModuleIndex
  are for display: the companion decides what it can read (Q-HIE-PATCH).
-}

{- Note [CBOR layout]

Records are CBOR maps with small integer keys (compact, and allow
optional and new fields). Big tables are flat arrays of unsigned ints,
which cborg reads fast and which take 1-3 bytes per number.

Header: map {0 magic, 1 major, 2 minor, 3 companionVersion, 4 ghcVersion}.
FileResult: array [0, path, ModuleIndex] or [1, path, reason]; path is
bytes.

ModuleIndex: map
  0 hieGhcVersion   1 strings (array of text)   2 unit   3 moduleName
  4 sourcePath      5 source {0 size, 1 sha1 bytes, 2 sha256 bytes}
  6 names    array of [unit, module, ns, occ, fieldParent|null, ty|null]
  7 locals   flat: occ, line, col, endLine, endCol, ty+1 (0 = none), per
    local (endLine kept: TH locals can have multi-line spans)
  8 types    array of [tag, fields...] (one per TypeNode constructor)
  9 decls    array of [name, kind, site|null, parent|null]
 10 instances array of [cls, [args], ty, site|null]
 11 exports  flat array of NameIdx
 12 imports  flat array of StrId
 13 occurrences flat: lineDelta, startCol, length, target, role
    (lineDelta from the previous occurrence; target = 2*NameIdx for a
    global, 2*LocalIdx+1 for a local)
 14 useTypes flat: occurrence, ty
 15 coveredLines flat: start, end
 16 instanceUses array of [occurrence, unit, module, cls, [args]]
A span in a record: [line, col, endLine, endCol].
-}

{- Note [Name keys]

A reference names its target by (unit, module, namespace, occurrence
name, field parent). All five come from GHC's Name and OccName, which
every supported GHC has, so the key doesn't depend on the GHC version.

- unit is the raw unit id. The main binary turns it into a D-44 module
  (home units, Note [Matching a record]) or a package (plan.json, else
  the unit id without its hash; D-42 order).
- namespace is GHC's: type and class share one, constructor and pattern
  synonym share one. A key is unique in a module. The §4.1 namespace
  (type/class, con/pat) is the declaration's kind, found from the
  declaration, never part of the key.
- fieldParent tells apart DuplicateRecordFields fields. It is GHC's
  parent: the type's first constructor, or the pattern synonym. URLs use
  the type instead (field:Config.port, §4.1): the main binary maps the
  constructor to its type through the constructor's Decl. Data instance
  fields keep the constructor (the type would clash across instances).
- Locals are not global: an occurrence points at a LocalEntry by its
  LocalIdx. Binder spans are not unique (RecordWildCards, TH), so the
  join to the syntactic tier is on (binder span, name), and only where
  that pair is unique in the module. The rest keep their syntactic link.
- Instances have no name: the D-20 key is computed by the main binary.

Matching with our syntactic declarations: (module, namespace, name) for
named ones, plus the parent type for fields; instances by span (the
dfun's span vs our instance declaration or deriving item).
-}

{- Note [What the companion drops]

Kept: names, binders, uses, the types of names and binders, the types at
uses that differ, instances, the instance each overloaded use picks,
exports, imports, covered lines.

Dropped: the source text (we have it), the AST shape and node
annotations, expression types (in v1 only identifiers, generic and at
their use), kinds and invisible type arguments, evidence other than uses that
lead to a dfun, GeneratedInfo nodes (deriving code, record wildcard
expansions; their EvInstBind dfuns are read first), import/export module-name identifiers, the definition spans
of external names (we never browse dependencies), $-prefixed system names
(the dfun is read for the instance, then dropped), and every AST file
other than the module's own (LINE pragmas).
-}

{- Note [TH-generated declarations]

A declaration made by a top-level splice is in the renamed group that HIE
is built from, with the splice's span, or no span at all. The companion
only reports the binding site (Decl.site), or Nothing when there is no
real span in this file. Exported names with no binding site in the AST
are added with site Nothing, so nothing exported is lost.

The main binary marks a declaration as generated when it has no
syntactic declaration at that span (it knows its own splice spans). It
shows it under the splice that contains the span (or at the end of the
module), with a "generated" marker (§3.1) and its type from the name
table.
-}

{- Note [CPP branches from HIE]

.hie files don't record which #if branches were taken. coveredLines are
the lines of source AST nodes. The main binary, for each #if group of
its own CPP pass, takes the branch with covered lines as the active one
when exactly one branch has them. Groups with no covered lines (only
comments or blanks) keep our assumption. If any group changes, the module
is pre-processed again with those choices, then parsed again (the choice
is part of the parse settings key, §11.4.4), and the UI says "CPP
branches from the build" (§3.1).
-}

{- Note [Size estimate]

Not measured: no .hie files at hand (Q-HIE-SIZE, measure in M8 on pandoc
and wire-server built with -fwrite-ide-info).

Average module from the M0 counts: pandoc 125,395 lines / 358 modules =
350 lines; wire-server 383,275 / 1,964 = 195 lines. Guesses: 2.5
identifier occurrences per line, distinct names = 0.7 per line (cap
~250), locals = 0.3 per line, 1 declaration per 8 lines.

Bytes in CBOR: occurrence ~7 (5 small ints) + ~1.3 for useTypes;
name ~11 + ~16 of strings; local ~10; type nodes ~2 KB per module;
declarations ~15 each.

            occurrences  names   locals  types  decls  total
pandoc avg   875 x 8.5   250x27  105x10  2 KB   45x15  ~18 KB
wire avg     490 x 8.5   140x27   60x10  1.5 KB 25x15  ~10 KB

2,000 modules: ~20-40 MB of cache. GHC's .hie files hold the source,
the whole AST and every expression type, so we expect the index to be
several times smaller than the .hie files it comes from. In memory
(unboxed occurrence tables, names interned across modules): about
50-100 MB for 2,000 modules, inside the 2 GiB budget (§10.2). If it's
too big, cut in this order: useTypes, local types, Sig/Import/Export
roles.
-}

{- Note [Matching a record]

(Done by the main binary, here so the companion's output is complete.)

1. Candidates: modules on the side with the record's module name and the
   same source blob id. None: the .hie is stale for this side.
   hie_hs_src holds the raw bytes on disk, but D-75 hashes working-tree
   files after git's clean/EOL filter (CRLF). So on the working-tree side
   we also compare with the blob id of the raw file (we hash the raw
   bytes). Committed sides compare with the git blob only.
2. One candidate: done. Several (the same file in a library and a test
   suite): pick by unit id, through plan.json (id -> package and
   component), else the "<pkg>-<ver>-inplace-<component>" pattern, else
   the dist-newstyle path of the .hie (…/<pkg>-<ver>/{l,x,t,b}/<name>/…).
   Still several: not used, hint says why.
3. Every matched record maps its unit id to that component. Names whose
   unit is a home unit become home keys; others are external.
4. External names: the precise key names the defining module (often
   hidden, or in ghc-internal on 9.10+). Links and Hackage URLs keep the
   module the name was imported from, as the syntactic tier does; the
   precise package and version are used (D-42 source 2).
-}
