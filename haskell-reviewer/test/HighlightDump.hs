-- | A plain text dump of a module's highlight runs, for golden tests.
-- A header line, the warnings, then one line per run:
-- @line:col-line:col class "text"@, with only the first line of a run's
-- text.
module HighlightDump (dumpHighlight, dumpMarkdown) where

import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Highlight
  ( DisplayClass (..),
    Run (..),
    highlightModule,
    runList,
  )
import HaskellReviewer.Core.Markdown (Markdown (..))
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    ParsedModule (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
    renderParseWarning,
  )
import HaskellReviewer.Core.SourceText (sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (renderSpan)
import HaskellReviewer.Core.Tokens (tokenCount)

-- | The runs of a parse of this text (BOM included, as on disk).
dumpHighlight :: Text -> ModuleParse -> Text
dumpHighlight text p =
  Text.unlines (header : warnings <> dumpRuns text (runList (highlightModule p)))
  where
    header = case p of
      Parsed _ -> "parsed"
      TextOnly t ->
        "text only: " <> reason t.reason <> "; tokens: " <> tokens t.reason
    -- Only when there are some.
    warnings = case p of
      Parsed m -> map warning m.warnings
      TextOnly t -> map warning t.warnings
    warning w = "warning: " <> renderParseWarning w
    reason = \case
      ParseFailed _ _ -> "parse failed"
      PragmaFailed _ -> "pragma failed"
      Preprocessed tool -> "preprocessed by " <> tool
      ParserCrashed _ -> "crashed"
      TooSlow -> "too slow"
    tokens = \case
      ParseFailed _ (Just ts) -> Text.pack (show (tokenCount ts))
      _ -> "none"

-- | The runs of a Markdown file (BOM included, as on disk).
dumpMarkdown :: Text -> Markdown -> Text
dumpMarkdown text m = Text.unlines ("markdown" : dumpRuns text m.runs)

-- | One line per run.
dumpRuns :: Text -> [Run] -> [Text]
dumpRuns text = map run
  where
    src = sourceLines text
    run r = renderSpan r.span <> " " <> className r.display <> " " <> slice r
    slice r = case Text.lines (sliceSpan src r.span) of
      [] -> "\"\""
      [l] -> quote l
      l : _ -> quote l <> "…"
    quote = Text.pack . show . Text.unpack

-- | A class's name in the dumps.
className :: DisplayClass -> Text
className = \case
  KeywordClass -> "keyword"
  TypeConClass -> "typeCon"
  VariableClass -> "variable"
  OperatorClass -> "operator"
  LiteralClass -> "literal"
  CommentClass -> "comment"
  PragmaClass -> "pragma"
  ExcludedClass -> "cppExcluded"
  DirectiveClass -> "cppDirective"
