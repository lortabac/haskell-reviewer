-- | Link targets on the wire: one shape for the source view and the
-- module page. See Note [Source view] in
-- "HaskellReviewer.Server.Wire.Source".
module HaskellReviewer.Server.Wire.Targets
  ( -- * Targets
    encodeTarget,

    -- * Names
    reasonName,
    howName,
  )
where

import Data.Text (Text)
import HaskellReviewer.Core.Names (ExternalModule (..), renderDeclId)
import HaskellReviewer.Core.Project (ModuleName (..), PackageName (..))
import HaskellReviewer.Core.RelPath (relPathText)
import HaskellReviewer.Core.SourceLinks
  ( DeclLink (..),
    DeclRole (..),
    ExternalLink (..),
    GuessHow (..),
    LinkReason (..),
    LinkTarget (..),
    LocalLink (..),
    ModuleLink (..),
  )
import HaskellReviewer.Core.Span (Col (..), Line (..))
import HaskellReviewer.Server.Cbor (Encoding)
import HaskellReviewer.Server.Cbor qualified as Cbor

-- | A target, as an array, kind first. See Note [Source view] in
-- "HaskellReviewer.Server.Wire.Source".
encodeTarget :: LinkTarget -> Encoding
encodeTarget =
  Cbor.list id . \case
    ToLocal l ->
      let Col c = l.col
       in [Cbor.text "local", line l.line, Cbor.word32 c]
    ToDecl d ->
      [ Cbor.text "decl",
        Cbor.text d.ref,
        Cbor.text (relPathText d.path),
        Cbor.nullable line d.line,
        Cbor.text (renderDeclId d.id),
        reasons d.reasons,
        Cbor.int d.candidates,
        Cbor.nullable Cbor.text d.sig,
        Cbor.bool (d.role == AtDef)
      ]
    ToModule m ->
      [ Cbor.text "module",
        Cbor.text m.ref,
        Cbor.nullable (Cbor.text . relPathText) m.path,
        reasons m.reasons,
        Cbor.int m.candidates
      ]
    ToExternal x ->
      let ModuleName name = x.module_.name
       in [ Cbor.text "external",
            Cbor.text name,
            Cbor.nullable (\(PackageName p) -> Cbor.text p) x.module_.package,
            Cbor.text (howName x.how),
            Cbor.int x.count,
            Cbor.text x.query
          ]
  where
    reasons = Cbor.list (Cbor.text . reasonName)
    line (Line l) = Cbor.word32 l

-- * Names

-- | The wire code of a reason.
reasonName :: LinkReason -> Text
reasonName = \case
  ReasonAmbiguous -> "ambiguous"
  ReasonNotReady -> "not-ready"
  ReasonNotParsed -> "not-parsed"
  ReasonMadeBySplice -> "made-by-splice"
  ReasonMadeBySpliceHere -> "made-by-splice-here"
  ReasonViaSplice -> "via-splice"
  ReasonWildcard -> "wildcard"
  ReasonRecordDot -> "record-dot"
  ReasonDataKinds -> "data-kinds"
  ReasonCppImport -> "cpp-import"
  ReasonImportCycle -> "import-cycle"
  ReasonAmbiguousModule -> "ambiguous-module"
  ReasonUnlistedModule -> "unlisted-module"
  ReasonLooseImport -> "loose-import"
  ReasonListedNotFound -> "listed-not-found"
  ReasonThisModule -> "this-module"

-- | The wire code of an external link's how.
howName :: GuessHow -> Text
howName = \case
  HowListed -> "listed"
  HowOnlyOpen -> "only-open"
  HowOneOf -> "one-of"
