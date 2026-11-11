-- | The source view of one file (SPEC §5.6): highlight runs with links,
-- line regions and an outline, as CBOR. See Note [Source view].
module HaskellReviewer.Server.Wire.Source
  ( -- * Types
    SourceView (..),
    SourceBody (..),
    LexedSource (..),

    -- * Building
    parsedSource,
    textOnlySource,

    -- * Bodies
    encodeSource,

    -- * Names
    reasonName,
    howName,
    outlineKindName,

    -- * Warm-up
    warmUpSource,
    sourceWarmUp,
    coveringView,
  )
where

import Control.Exception (evaluate)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import HaskellReviewer.Core.Highlight
  ( LinkedRuns,
    Run (..),
    highlightLinked,
    linkedRunList,
    linkedRunsFromList,
  )
import HaskellReviewer.Core.LineRegions
  ( LineKind (..),
    LineRegion (..),
    lineRegionList,
  )
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    ParsedModule (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
    textOnlyModule,
  )
import HaskellReviewer.Core.Modules
  ( ModuleId (..),
    ModuleIndex,
    moduleAtPath,
    modulePath,
    moduleRef,
  )
import HaskellReviewer.Core.Names (ExternalModule (..), renderDeclId)
import HaskellReviewer.Core.Parser (defaultParseSettings, parseSource)
import HaskellReviewer.Core.Project (ModuleName (..), PackageName (..))
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments, relPathText)
import HaskellReviewer.Core.Resolve
  ( ModuleState (..),
    ParsedInput (..),
    ResolveInput (..),
    isPartial,
  )
import HaskellReviewer.Core.Resolve qualified as Core (resolveModule)
import HaskellReviewer.Core.ResolveWarmUp
  ( resolveWarmUpIndex,
    resolveWarmUpInput,
    resolveWarmUpModules,
  )
import HaskellReviewer.Core.SourceLinks
  ( DeclLink (..),
    ExternalLink (..),
    GuessHow (..),
    LinkReason (..),
    LinkTarget (..),
    LocalLink (..),
    ModuleLink (..),
    OutlineEntry (..),
    OutlineKind (..),
    SourceLinks (..),
    linkAt,
    linksOf,
  )
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (tokenCount)
import HaskellReviewer.Effects.Parser (declsWarmUp, highlightWarmUp)
import HaskellReviewer.Server.Cbor (Encoding)
import HaskellReviewer.Server.Cbor qualified as Cbor
import HaskellReviewer.Server.Wire (encodeFileNotFound, encodeModuleNotFound)
import HaskellReviewer.Server.Wire.Page (syntacticTier)
import HaskellReviewer.Session.Snapshot (Generation (..))

{- Note [Source view]

GET api/source/<side>/<path> is CBOR, built per request (no cache). The
file's text is not in it: the client gets it from api/file. In JSON
terms:

  {generation, path, ref, tier, partial, highlight}

ref is the module's ref (null if the file is no module), tier as on
the module page, partial true when resolution read a module not ready
yet (the client asks again later). highlight is "plain" (nothing more:
not a module, generated, preprocessed, not text, too large, or text
only with nothing to show) or "lexed", which adds:

- runs: a flat int array, 6 per run: start line minus the previous
  run's start line (for the first run, minus 0: its start line), start
  column, end line minus
  start line, end column, display class (the DisplayClass enum), link
  (0 none, else 1 + the target's index). Code point columns, end
  exclusive. See Note [Highlight runs] in "HaskellReviewer.Core.Highlight".
- regions: a flat int array, 3 per region: kind (0 CPP directives,
  1 excluded by CPP, 2 literate prose), first line, last line.
- targets, each an array, kind first:
    ["local", line, col]
    ["decl", ref, path, line | null, id, reasons, candidates, sig | null]
    ["module", ref, path | null, reasons, candidates]
    ["external", module, package | null, how, count, query]
  reasons are text codes ('reasonName'), empty when exact; how is
  listed, only-open or one-of ('howName'); sig is the declaration's
  code for the hover, plain text. See Note [Source links] and Note
  [Hover signatures] in "HaskellReviewer.Core.SourceLinks".
- outline: [kind, id, label, line, firstLine, lastLine] per
  top-level declaration with an id, in source order. The label is the
  name, or the instance head: @instance Show T@, @type instance F [a]@.

Q-RTS-BLACKHOLE: request threads encode in parallel. Before the fill,
the listing thread encodes 'sourceWarmUp': the resolution warm-up
project's modules with their links, the parser's declaration and
highlighting modules, a text-only and a plain view, and one view made
of all of them stacked ('coveringView'), and both not-found bodies. A
test checks the covering view has every target kind, reason, how,
display class, region kind and outline kind, every shape of sig (Note
[Hover signatures] in "HaskellReviewer.Core.SourceLinks"), a cut one
and a null one. The sig shapes come from @w/src/Sigs.hs@ of the
resolution warm-up project. The parser's modules (@warm-up/M1.hs@...)
are encoded with that project's index ('resolveWarmUpIndex'), where
they have no file: their names link to module targets, never to
declarations, so they can't carry sigs.
-}

-- * Types

-- | One file, for GET api\/source\/\<side>\/\<path>.
data SourceView = SourceView
  { path :: RelPath,
    -- | 'Nothing' if the file is no module.
    moduleId :: Maybe ModuleId,
    partial :: Bool,
    body :: SourceBody
  }

-- | What we show.
data SourceBody
  = PlainBody
  | LexedBody LexedSource

-- | A lexed file. See Note [Source view].
data LexedSource = LexedSource
  { runs :: LinkedRuns,
    regions :: [LineRegion],
    targets :: [LinkTarget],
    outline :: [OutlineEntry]
  }

-- * Building

-- | A parsed module with its links.
parsedSource :: ParsedModule -> SourceLinks -> LexedSource
parsedSource m links =
  LexedSource
    { runs = highlightLinked (linkAt links) (Just m.tokens) m.regions,
      regions = lineRegionList m.regions,
      targets = links.targets,
      outline = links.outline
    }

-- | A text-only module: its tokens, if it lexed, and its regions. No
-- links. Plain when there is neither.
textOnlySource :: TextOnlyModule -> SourceBody
textOnlySource t
  | maybe 0 tokenCount tokens == 0 && null regions = PlainBody
  | otherwise =
      LexedBody
        LexedSource
          { runs = highlightLinked (const 0) tokens t.regions,
            regions = regions,
            targets = [],
            outline = []
          }
  where
    regions = lineRegionList t.regions
    tokens = case t.reason of
      ParseFailed _ ts -> ts
      _ -> Nothing

-- * Bodies

-- | GET api\/source\/\<side>\/\<path>. See Note [Source view].
encodeSource :: Generation -> ModuleIndex -> SourceView -> LBS.ByteString
encodeSource (Generation g) idx v =
  Cbor.cborBody . Cbor.obj $
    [ ("generation", Cbor.word64 g),
      ("path", Cbor.text (relPathText v.path)),
      ("ref", Cbor.nullable (Cbor.text . moduleRef idx) v.moduleId),
      ("tier", syntacticTier),
      ("partial", Cbor.bool v.partial)
    ]
      <> case v.body of
        PlainBody -> [("highlight", Cbor.text "plain")]
        LexedBody s ->
          [ ("highlight", Cbor.text "lexed"),
            ("runs", Cbor.list Cbor.word32 (runWords s.runs)),
            ("regions", Cbor.list Cbor.word32 (concatMap regionWords s.regions)),
            ("targets", Cbor.list target s.targets),
            ("outline", Cbor.list outlineEntry s.outline)
          ]

-- | The runs as flat words, lines as deltas. See Note [Source view].
runWords :: LinkedRuns -> [Word32]
runWords = go 0 . linkedRunList
  where
    go _ [] = []
    go prev ((r, link) : rest) =
      let Span (Line l1) (Col c1) (Line l2) (Col c2) = r.span
       in [l1 - prev, c1, l2 - l1, c2, fromIntegral (fromEnum r.display), link]
            <> go l1 rest

regionWords :: LineRegion -> [Word32]
regionWords r =
  let (Line a, Line b) = (r.firstLine, r.lastLine)
   in [kindCode r.kind, a, b]
  where
    kindCode = \case
      CppDirectives -> 0
      CppExcluded -> 1
      Prose -> 2

target :: LinkTarget -> Encoding
target =
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
        Cbor.nullable Cbor.text d.sig
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

outlineEntry :: OutlineEntry -> Encoding
outlineEntry e =
  Cbor.list
    id
    [ Cbor.text (outlineKindName e.kind),
      Cbor.text (renderDeclId e.id),
      Cbor.text e.label,
      line e.line,
      line e.firstLine,
      line e.lastLine
    ]

line :: Line -> Encoding
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

-- | The wire code of an outline kind: the page's declaration kinds.
outlineKindName :: OutlineKind -> Text
outlineKindName = \case
  OutlineType -> "type"
  OutlineClass -> "class"
  OutlineValue -> "value"
  OutlinePatSyn -> "patsyn"
  OutlineInstance -> "instance"
  OutlineFamInst -> "faminst"

-- * Warm-up

-- | Encode 'sourceWarmUp', 'coveringView' and the not-found bodies. See
-- Note [Source view].
warmUpSource :: IO ()
warmUpSource = do
  mapM_
    (evaluate . LBS.length . encodeSource (Generation 1) resolveWarmUpIndex)
    (coveringView : sourceWarmUp)
  _ <- evaluate (LBS.length (encodeFileNotFound (Generation 1)))
  _ <- evaluate (LBS.length (encodeModuleNotFound (Generation 1)))
  pure ()

-- | The warm-up views. See Note [Source view].
sourceWarmUp :: [SourceView]
sourceWarmUp =
  mapMaybe resolved resolveWarmUpModules
    <> zipWith
      parsed
      [1 :: Int ..]
      (map (defaultParseSettings,) [declsWarmUp, badModule, fieldsModule])
    <> zipWith parsed [10 ..] highlightWarmUp
    <> [ view "Slow.hs" Nothing (textOnlySource (textOnlyModule TooSlow [])),
         view "notes.txt" Nothing PlainBody
       ]
  where
    idx = resolveWarmUpIndex
    resolved i = case resolveWarmUpInput.state i of
      ModuleParsed p -> do
        file <- modulePath idx i
        let res = Core.resolveModule resolveWarmUpInput i
            links = linksOf idx resolveWarmUpInput.state i p res
        Just
          SourceView
            { path = file,
              moduleId = moduleAtPath idx file,
              partial = maybe False isPartial res,
              body = LexedBody (parsedSource p.parsed links)
            }
      _ -> Nothing
    parsed k (settings, t) =
      let file = warmUpPath ("M" <> showText k <> ".hs")
       in case parseSource settings t of
            Parsed m ->
              let i = LooseModule file
                  p = ParsedInput {text = t, parsed = m}
                  links = linksOf idx (const ModuleNotParsed) i p Nothing
               in SourceView
                    { path = file,
                      moduleId = Just i,
                      partial = False,
                      body = LexedBody (parsedSource m links)
                    }
            TextOnly o ->
              SourceView
                { path = file,
                  moduleId = Nothing,
                  partial = False,
                  body = textOnlySource o
                }
    view p i b = SourceView {path = warmUpPath p, moduleId = i, partial = False, body = b}
    -- A field of two types: a guess of the occurrence table.
    fieldsModule =
      "{-# LANGUAGE DuplicateRecordFields #-}\nmodule M where\n\
      \data S = S {f :: Int}\ndata T = T {f :: Int}\ng r = f r\n"
    -- Lexes, doesn't parse: text only with tokens.
    badModule = "module M where\nx = = 1\n"
    showText = Text.pack . show

-- | Every lexed warm-up view stacked into one, as if the files were one
-- after the other: lines shifted, links and targets renumbered. Not a
-- real file: it covers every value at once, for the coverage test and
-- the source-warm-up fixture. See Note [Source view].
coveringView :: SourceView
coveringView =
  SourceView
    { path = warmUpPath "Covering.hs",
      moduleId = Nothing,
      partial = any (.partial) sourceWarmUp,
      body = LexedBody (stack [s | SourceView {body = LexedBody s} <- sourceWarmUp])
    }

-- | See 'coveringView'.
stack :: [LexedSource] -> LexedSource
stack sources =
  LexedSource
    { runs =
        linkedRunsFromList
          (concat [map (shiftRun o) (linkedRunList s.runs) | (o, s) <- placed]),
      regions = concat [map (shiftRegion o.lines) s.regions | (o, s) <- placed],
      targets = concat [map (shiftTarget o.lines) s.targets | (o, s) <- placed],
      outline = concat [map (shiftEntry o.lines) s.outline | (o, s) <- placed]
    }
  where
    -- Each source with the lines and targets before it.
    placed = zip (scanl next (Offset 0 0) sources) sources
    next o s =
      Offset
        { lines = o.lines + lastLine s + 1,
          targets = o.targets + fromIntegral (length s.targets)
        }
    shiftRun o (r, link) =
      let Span l1 c1 l2 c2 = r.span
          moved = Span (plus o.lines l1) c1 (plus o.lines l2) c2
       in ( Run {span = moved, display = r.display},
            if link == 0 then 0 else link + o.targets
          )
    shiftRegion off r =
      LineRegion
        { kind = r.kind,
          firstLine = plus off r.firstLine,
          lastLine = plus off r.lastLine
        }
    shiftTarget off = \case
      ToLocal l -> ToLocal LocalLink {line = plus off l.line, col = l.col}
      t -> t
    shiftEntry off e =
      OutlineEntry
        { kind = e.kind,
          id = e.id,
          label = e.label,
          line = plus off e.line,
          firstLine = plus off e.firstLine,
          lastLine = plus off e.lastLine
        }
    plus off (Line l) = Line (l + off)
    lastLine s =
      maximum $
        0
          : [l | (r, _) <- linkedRunList s.runs, let Line l = r.span.endLine]
            <> [l | r <- s.regions, let Line l = r.lastLine]
            <> [l | e <- s.outline, let Line l = e.lastLine]

-- | Lines and targets before a stacked source.
data Offset = Offset
  { lines :: Word32,
    targets :: Word32
  }

warmUpPath :: Text -> RelPath
warmUpPath name =
  fromMaybe (error "warm-up: bad path") (relPathFromSegments ["warm-up", name])
