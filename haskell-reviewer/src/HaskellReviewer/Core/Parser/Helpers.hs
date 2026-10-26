{-# LANGUAGE StrictData #-}

-- | Small helpers over GHC's types, for the conversion modules and the
-- lexer: spans, names, doc text.
module HaskellReviewer.Core.Parser.Helpers
  ( -- * Spans
    SpanMap (..),
    toSpan,
    spanOf,
    locatedSpan,

    -- * Names and docs
    occText,
    moduleName,
    rawDoc,
    warningText,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Data.FastString (unpackFS)
import GHC.Hs (GhcPs, LHsDoc, WithHsDocIdentifiers (..))
import GHC.Hs.DocString (renderHsDocString)
import GHC.Parser.Annotation (HasLoc, getHasLoc)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SourceText (StringLiteral (..))
import GHC.Types.SrcLoc
  ( GenLocated (..),
    SrcSpan (..),
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanStartLine,
    unLoc,
  )
import GHC.Unit.Module.Warnings
  ( InWarningCategory (..),
    WarningCategory (..),
    WarningTxt (..),
  )
import HaskellReviewer.Core.Cpp (PositionMap, mapSpan)
import HaskellReviewer.Core.Decls (RawDoc (..), WarningKind (..), WarningText (..))
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.SourceText (ColumnMap, GhcRange (..), ghcToSpan)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import Language.Haskell.Syntax.Module.Name qualified as Ghc (ModuleName, moduleNameString)

-- * Spans

-- | From a GHC span to ours: tabs first, on the text GHC saw (the CPP
-- output), then back through CPP to the file.
data SpanMap = SpanMap
  { -- | Tabs, on the CPP output.
    columns :: ColumnMap,
    -- | CPP output to file.
    cpp :: PositionMap
  }

-- | GHC's span, in our columns, on the file. 'Nothing' for GHC's spans
-- with no place.
toSpan :: SpanMap -> SrcSpan -> Maybe Span
toSpan spans = \case
  RealSrcSpan r _ ->
    Just . mapSpan spans.cpp $
      ghcToSpan spans.columns $
        GhcRange
          { startLine = srcSpanStartLine r,
            startCol = srcSpanStartCol r,
            endLine = srcSpanEndLine r,
            endCol = srcSpanEndCol r
          }
  UnhelpfulSpan _ -> Nothing

-- | Like 'toSpan', for what the parser made: it gives real spans to
-- everything it parsed, so the fallback never shows.
spanOf :: SpanMap -> SrcSpan -> Span
spanOf spans l = fromMaybe (Span (Line 0) (Col 0) (Line 0) (Col 0)) (toSpan spans l)

-- | The span of anything GHC located.
locatedSpan :: (HasLoc l) => SpanMap -> GenLocated l a -> Span
locatedSpan spans (L l _) = spanOf spans (getHasLoc l)

-- * Names and docs

-- | The name without its qualifier or parens.
occText :: RdrName -> Text
occText = Text.pack . occNameString . rdrNameOcc

-- | GHC's module name as ours.
moduleName :: Ghc.ModuleName -> ModuleName
moduleName = ModuleName . Text.pack . Ghc.moduleNameString

-- | A doc comment, without the decorator.
rawDoc :: SpanMap -> LHsDoc GhcPs -> RawDoc
rawDoc spans d@(L _ w) =
  RawDoc {span = locatedSpan spans d, text = Text.pack (renderHsDocString w.hsDocString)}

-- | A @DEPRECATED@ or @WARNING@ pragma's text, at this span.
warningText :: Span -> WarningTxt GhcPs -> WarningText
warningText s w =
  WarningText
    { span = s,
      kind = case w of
        WarningTxt category _ _ -> WarningPragma (categoryText <$> category)
        DeprecatedTxt _ _ -> DeprecatedPragma,
      messages = case w of
        WarningTxt _ _ ms -> map message ms
        DeprecatedTxt _ ms -> map message ms
    }
  where
    categoryText (L _ c) = case unLoc c.iwc_wc of
      WarningCategory fs -> Text.pack (unpackFS fs)
    message (L _ m) = Text.pack (unpackFS m.hsDocString.sl_fs)
