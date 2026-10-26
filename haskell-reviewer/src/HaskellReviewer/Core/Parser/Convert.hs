{-# LANGUAGE StrictData #-}

-- | From GHC's AST to our module model (D-61). Only what the model needs
-- is copied out; the AST is dropped after this.
module HaskellReviewer.Core.Parser.Convert
  ( SpanMap (..),
    convertModule,
    toSpan,
  )
where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Data.FastString (unpackFS)
import GHC.Hs
  ( GhcPs,
    HsModule (..),
    IE (..),
    IEWildcard (..),
    IEWrappedName (..),
    ImportDecl (..),
    ImportDeclLevel (..),
    ImportDeclLevelStyle (..),
    ImportListInterpretation (..),
    LHsDoc,
    LIE,
    LIEWrappedName,
    LImportDecl,
    WithHsDocIdentifiers (..),
    XModulePs (..),
  )
import GHC.Hs qualified as Ghc (ImportDeclQualifiedStyle (..))
import GHC.Hs.Doc (HsDocString)
import GHC.Hs.DocString (renderHsDocString)
import GHC.Parser.Annotation (getLocA)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, isQual_maybe, rdrNameOcc)
import GHC.Types.PkgQual (RawPkgQual (..))
import GHC.Types.SourceText (StringLiteral (..))
import GHC.Types.SrcLoc
  ( GenLocated (..),
    Located,
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
import HaskellReviewer.Core.ModuleModel
  ( Export (..),
    Extensions,
    Import (..),
    ImportLevel (..),
    ImportList (..),
    ImportSource (..),
    Item (..),
    ItemName (..),
    ListKind (..),
    ModuleExport (..),
    ModuleHeader (..),
    NameKeyword (..),
    ParseWarning,
    ParsedModule (..),
    Qualified (..),
    RawDoc (..),
    Section (..),
    SubList (..),
    Subs (..),
    WarningKind (..),
    WarningText (..),
  )
import HaskellReviewer.Core.ModuleModel qualified as Model (Located (..))
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.SourceText (ColumnMap, GhcRange (..), ghcToSpan)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import Language.Haskell.Syntax.ImpExp.IsBoot (IsBootInterface (..))
import Language.Haskell.Syntax.Module.Name qualified as Ghc (ModuleName, moduleNameString)

-- | From a GHC span to ours: tabs first, on the text GHC saw (the CPP
-- output), then back through CPP to the file.
data SpanMap = SpanMap
  { -- | Tabs, on the CPP output.
    columns :: ColumnMap,
    -- | CPP output to file.
    cpp :: PositionMap
  }

-- | The model of a parsed module. The extensions and warnings come from
-- the flags.
convertModule ::
  SpanMap -> Extensions -> [ParseWarning] -> Located (HsModule GhcPs) -> ParsedModule
convertModule spans exts warnings (L _ m) =
  ParsedModule
    { header =
        ModuleHeader
          { name = located moduleName <$> hsmodName m,
            haddock = doc <$> hsmodHaddockModHeader ext,
            warning = warningText <$> hsmodDeprecMessage ext
          },
      exports = map export . unLoc <$> hsmodExports m,
      imports = map importDecl (hsmodImports m),
      extensions = exts,
      warnings = warnings
    }
  where
    ext = hsmodExt m

    -- The parser gives real spans to everything it parsed.
    spanOf l = fromMaybe (Span (Line 0) (Col 0) (Line 0) (Col 0)) (toSpan spans l)

    located f lx@(L _ x) = Model.Located {span = spanOf (getLocA lx), value = f x}

    doc :: LHsDoc GhcPs -> RawDoc
    doc d@(L _ w) = RawDoc {span = spanOf (getLocA d), text = docText w.hsDocString}

    warningText w =
      WarningText
        { span = spanOf (getLocA w),
          kind = case unLoc w of
            WarningTxt category _ _ -> WarningPragma (categoryText <$> category)
            DeprecatedTxt _ _ -> DeprecatedPragma,
          messages = case unLoc w of
            WarningTxt _ _ ms -> map message ms
            DeprecatedTxt _ ms -> map message ms
        }
    categoryText (L _ c) = case unLoc c.iwc_wc of
      WarningCategory fs -> Text.pack (unpackFS fs)
    message (L _ s) = Text.pack (unpackFS s.hsDocString.sl_fs)

    export :: LIE GhcPs -> Export
    export lie@(L _ ie) = case ie of
      IEVar w n d -> item w n NoSubs d
      IEThingAbs w n d -> item w n NoSubs d
      IEThingAll (w, _) n d -> item w n AllSubs d
      IEThingWith (w, _) n wildcard subs d -> item w n (someSubs wildcard subs) d
      IEModuleContents (w, _) mn ->
        ExportModule
          ModuleExport {name = located moduleName mn, warning = warningText <$> w}
      IEGroup _ level heading ->
        ExportSection Section {level = level, heading = doc heading}
      IEDoc _ d -> ExportDoc (doc d)
      IEDocNamed _ chunk ->
        ExportDocNamed
          Model.Located {span = spanOf (getLocA lie), value = Text.pack chunk}
      where
        item w n subs d =
          ExportItem
            Item
              { span = spanOf (getLocA lie),
                name = itemName n,
                subs = subs,
                doc = doc <$> d,
                warning = warningText <$> w
              }

    -- The parser gives nothing else in an import list.
    importItem :: LIE GhcPs -> Maybe Item
    importItem lie@(L _ ie) = case ie of
      IEVar _ n _ -> item n NoSubs
      IEThingAbs _ n _ -> item n NoSubs
      IEThingAll _ n _ -> item n AllSubs
      IEThingWith _ n wildcard subs _ -> item n (someSubs wildcard subs)
      _ -> Nothing
      where
        item n subs =
          Just
            Item
              { span = spanOf (getLocA lie),
                name = itemName n,
                subs = subs,
                doc = Nothing,
                warning = Nothing
              }

    someSubs wildcard subs =
      SomeSubs
        SubList
          { names = map itemName subs,
            wildcardAt = case wildcard of
              NoIEWildcard -> Nothing
              IEWildcard i -> Just i
          }

    itemName :: LIEWrappedName GhcPs -> ItemName
    itemName (L _ w) = case w of
      IEName _ n -> rdr NoKeyword n
      IEType _ n -> rdr TypeKeyword n
      IEPattern _ n -> rdr PatternKeyword n
      IEData _ n -> rdr DataKeyword n
      IEDefault _ n -> rdr DefaultKeyword n
      where
        rdr keyword n =
          ItemName
            { span = spanOf (getLocA n),
              qualifier = moduleName . fst <$> isQual_maybe (unLoc n),
              occ = occText (unLoc n),
              keyword = keyword
            }

    importDecl :: LImportDecl GhcPs -> Import
    importDecl d = case unLoc d of
      ImportDecl
        { ideclName = name,
          ideclPkgQual = pkg,
          ideclSource = source,
          ideclLevelSpec = level,
          ideclSafe = safe,
          ideclQualified = qualified,
          ideclAs = as,
          ideclImportList = list
        } ->
          Import
            { span = spanOf (getLocA d),
              name = located moduleName name,
              package = case pkg of
                RawPkgQual s -> Just (Text.pack (unpackFS s.sl_fs))
                NoRawPkgQual -> Nothing,
              source = case source of
                IsBoot -> SourceImport
                NotBoot -> NormalImport,
              safe = safe,
              qualified = case qualified of
                Ghc.NotQualified -> NotQualified
                Ghc.QualifiedPre -> QualifiedPre
                Ghc.QualifiedPost -> QualifiedPost,
              level = case level of
                NotLevelled -> NormalLevel
                LevelStylePre l -> importLevel l
                LevelStylePost l -> importLevel l,
              as = located moduleName <$> as,
              list = case list of
                Nothing -> Nothing
                Just (how, L _ items) ->
                  Just
                    ImportList
                      { kind = case how of
                          EverythingBut -> Hiding
                          Exactly -> Only,
                        items = mapMaybe importItem items
                      }
            }
    importLevel = \case
      ImportDeclSplice -> SpliceLevel
      ImportDeclQuote -> QuoteLevel

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

-- | A Haddock comment's text, without the decorator.
docText :: HsDocString -> Text
docText = Text.pack . renderHsDocString

moduleName :: Ghc.ModuleName -> ModuleName
moduleName = ModuleName . Text.pack . Ghc.moduleNameString

-- | The name without its qualifier or parens.
occText :: RdrName -> Text
occText = Text.pack . occNameString . rdrNameOcc
