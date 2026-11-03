-- | Parse a text with our pipeline and check where the names land.
module Check
  ( Result (..),
    parseResult,
    badNames,
    nameCount,
    renderSpan,
  )
where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Decls (DeclInfo (..), declInfo)
import HaskellReviewer.Core.ModuleModel
  ( Extensions (..),
    ModuleParse (..),
    ParsedModule (..),
    TextOnlyModule (..),
    renderParseWarning,
  )
import HaskellReviewer.Core.Names (DeclId (..))
import HaskellReviewer.Core.Parser (ParseSettings, parseSource)
import HaskellReviewer.Core.SourceText (sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (Token (..), tokenList)

-- | What a parse gave, spans on the file.
data Result = Result
  { outcome :: Text,
    -- | Top-level names and where they are.
    names :: [(Text, Span)],
    tokens :: [Token],
    warnings :: [Text],
    fromPragmas :: [Text]
  }

-- | Parse, then move every span with the function (identity, or a
-- layout's map).
parseResult :: ParseSettings -> (Span -> Span) -> Text -> Result
parseResult settings move t = case parseSource settings t of
  Parsed m ->
    Result
      { outcome = "parsed",
        names = mapMaybe (fmap named . declInfo) m.declarations,
        tokens = [tok {span = move tok.span} | tok <- tokenList m.tokens],
        warnings = map renderParseWarning m.warnings,
        fromPragmas = m.extensions.fromPragmas
      }
  TextOnly o ->
    Result
      { outcome = "text-only: " <> Text.take 120 (Text.pack (show o.reason)),
        names = [],
        tokens = [],
        warnings = map renderParseWarning o.warnings,
        fromPragmas = []
      }
  where
    named i = (nameOf i.id, move i.nameSpan)
    nameOf = \case
      Named _ n -> n
      Field _ n -> n
      Inst _ -> ""

-- | Names whose span doesn't cut the name out of the file. Instances
-- have no name: skipped.
badNames :: Text -> Result -> [(Text, Span, Text)]
badNames file r =
  [ (n, s, got)
  | (n, s) <- r.names,
    not (Text.null n),
    let got = sliceSpan sl s,
    got /= n,
    got /= "(" <> n <> ")"
  ]
  where
    sl = sourceLines file

-- | How many named decls (instances have no name).
nameCount :: Result -> Int
nameCount r = length [() | (n, _) <- r.names, not (Text.null n)]

-- | A span as line:col-line:col.
renderSpan :: Span -> Text
renderSpan (Span (Line a) (Col b) (Line c) (Col d)) =
  Text.pack (show a <> ":" <> show b <> "-" <> show c <> ":" <> show d)
