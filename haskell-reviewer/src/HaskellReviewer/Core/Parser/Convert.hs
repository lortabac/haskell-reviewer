{-# LANGUAGE StrictData #-}

-- | From GHC's AST to our module model (D-61). Only what the model needs
-- is copied out; the AST is dropped after this.
module HaskellReviewer.Core.Parser.Convert
  ( convertModule,
  )
where

import Data.Maybe (mapMaybe)
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
    XModulePs (..),
  )
import GHC.Hs qualified as Ghc (ImportDeclQualifiedStyle (..))
import GHC.Parser.Annotation (getLocA)
import GHC.Types.Name.Reader (isQual_maybe)
import GHC.Types.PkgQual (RawPkgQual (..))
import GHC.Types.SourceText (StringLiteral (..))
import GHC.Types.SrcLoc (GenLocated (..), Located, unLoc)
import HaskellReviewer.Core.Exported (markExported)
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
  )
import HaskellReviewer.Core.ModuleModel qualified as Model (Located (..))
import HaskellReviewer.Core.Parser.ConvertDecls (Converted (..), convertDecls)
import HaskellReviewer.Core.Parser.Helpers
  ( SpanMap,
    moduleName,
    occText,
    rawDoc,
  )
import HaskellReviewer.Core.Parser.Helpers qualified as Helpers (spanOf, warningText)
import HaskellReviewer.Core.Tokens (Tokens)
import Language.Haskell.Syntax.ImpExp.IsBoot (IsBootInterface (..))

-- | The model of a parsed module. The extensions and warnings come from
-- the flags, the tokens from the second lex.
convertModule ::
  SpanMap ->
  Extensions ->
  [ParseWarning] ->
  Tokens ->
  Located (HsModule GhcPs) ->
  ParsedModule
convertModule spans exts warnings tokens (L _ m) =
  ParsedModule
    { header = header,
      exports = exports,
      imports = map importDecl (hsmodImports m),
      extensions = exts,
      warnings = warnings,
      declarations = markExported self exports converted.decls,
      docItems = converted.docItems,
      pragmas = converted.pragmas,
      tokens = tokens
    }
  where
    ext = hsmodExt m
    header =
      ModuleHeader
        { name = located moduleName <$> hsmodName m,
          haddock = doc <$> hsmodHaddockModHeader ext,
          warning = warningText <$> hsmodDeprecMessage ext
        }
    exports = map export . unLoc <$> hsmodExports m
    self = (.value) <$> header.name
    converted = convertDecls spans self (hsmodDecls m)

    spanOf = Helpers.spanOf spans

    located f lx@(L _ x) = Model.Located {span = spanOf (getLocA lx), value = f x}

    doc :: LHsDoc GhcPs -> RawDoc
    doc = rawDoc spans

    warningText w = Helpers.warningText (spanOf (getLocA w)) (unLoc w)

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
