-- | A plain text dump of a module's occurrence table, for golden tests.
-- One line per token that names something:
-- @line:col-line:col "text" namespace role -> what@, then the binders:
-- @#3 x 4:7-4:8 scope 4:1-6:20@.
module OccurrencesDump (dumpOccurrences) where

import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    ParsedModule (..),
  )
import HaskellReviewer.Core.Names (renderDeclId)
import HaskellReviewer.Core.Occurrences
  ( Binder (..),
    BinderId (..),
    BinderName (..),
    Certainty (..),
    FreeContext (..),
    Namespace (..),
    NotNameKind (..),
    Occurrence (..),
    Resolution (..),
    Role (..),
    TokenIx (..),
    binderAt,
    binderCount,
    conflicts,
    hasTopSplices,
    occurrenceList,
  )
import HaskellReviewer.Core.SourceText (SourceLines, sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Span, renderSpan)
import HaskellReviewer.Core.Tokens (tokenSpanAt)

-- | The table of a parse of this text.
dumpOccurrences :: Text -> ModuleParse -> Text
dumpOccurrences text = \case
  TextOnly _ -> "text only\n"
  Parsed m ->
    let occs = m.occurrences
        n = binderCount occs
        spanOf (TokenIx t) = tokenSpanAt m.tokens t
        resolution = renderResolution src spanOf
     in Text.unlines $
          [ "conflicts: "
              <> tshow (conflicts occs)
              <> if hasTopSplices occs then ", top-level splices" else ""
          ]
            <> [ renderSpan (spanOf i)
                   <> " "
                   <> quote (sliceSpan src (spanOf i))
                   <> " "
                   <> nsText o.namespace
                   <> " "
                   <> roleText o.role
                   <> " -> "
                   <> resolution o.resolution
                   <> (if o.inSplice then " [splice]" else "")
                   <> maybe "" (\f -> " [pun: " <> resolution f <> "]") o.punField
               | (i, o) <- occurrenceList occs
               ]
            <> [ "#"
                   <> tshow b
                   <> " "
                   <> name bd
                   <> " "
                   <> renderSpan bd.span
                   <> " scope "
                   <> Text.intercalate "," (map renderSpan bd.scopes)
               | b <- [0 .. n - 1],
                 Just bd <- [binderAt occs (BinderId b)]
               ]
  where
    src = sourceLines text
    name bd = case bd.name of
      AtSpan -> sliceSpan src bd.span
      Stored t -> "(" <> t <> ")"

-- | One resolution, with anchors as their token's text and place.
renderResolution :: SourceLines -> (TokenIx -> Span) -> Resolution -> Text
renderResolution src spanOf = \case
  LocalRef (BinderId b) -> "local #" <> tshow b
  TopLevelRef d c -> "top " <> renderDeclId d <> certainty c
  FreeRef c -> "free" <> context c
  BuiltInRef -> "builtin"
  NotAName k -> "not-a-name " <> kindText k
  where
    certainty = \case
      Exact -> ""
      Heuristic -> " [heuristic]"
    context = \case
      Plain -> ""
      MethodOfClass t -> " [method of " <> anchor t <> "]"
      FieldOfCon t -> " [field of " <> anchor t <> "]"
      MaybeWildcardOf t -> " [maybe wildcard of " <> anchor t <> "]"
      QuasiQuoter -> " [quasi-quoter]"
    anchor t = let s = spanOf t in sliceSpan src s <> "@" <> renderSpan s
    kindText = \case
      ModuleWord -> "module"
      LabelWord -> "label"
      OtherWord -> "other"

nsText :: Namespace -> Text
nsText = \case
  VarNs -> "var"
  ConNs -> "con"
  TypeNs -> "type"
  TyVarNs -> "tyvar"
  FieldNs -> "field"
  IPNs -> "ip"
  ModuleNs -> "module"
  OtherNs -> "other"

roleText :: Role -> Text
roleText = \case
  Use -> "use"
  Def -> "def"
  Sig -> "sig"
  Import -> "import"
  Export -> "export"
  InstanceDef -> "instance-def"

quote :: Text -> Text
quote = Text.pack . show . Text.unpack

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
