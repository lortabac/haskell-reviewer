-- | The module page: instance placement, links, signatures, exported
-- flags. The fixtures (module-page*) show the whole shape.
module HaskellReviewer.Server.PageTest (tests) where

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileList (EntryKind (..), ListSource (..), mkFileList)
import HaskellReviewer.Core.ModuleModel (ModuleParse (..), ParsedModule)
import HaskellReviewer.Core.Modules (ModuleId (..), ModuleIndex, buildModuleIndex)
import HaskellReviewer.Core.Parser (defaultParseSettings, parseSource)
import HaskellReviewer.Core.Project
  ( DetectedGhc (..),
    GhcSource (..),
    Project (..),
    ProjectSource (..),
    parserGhcVersion,
  )
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments)
import HaskellReviewer.Server.CborJson (cborToJson)
import HaskellReviewer.Server.Wire.Page
  ( ModuleAnswer (..),
    ModuleView (..),
    encodeModulePage,
    pageWarmUp,
  )
import HaskellReviewer.Session.Index (ParsedSource (..))
import HaskellReviewer.Session.Snapshot (Generation (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Page"
    [ testCase "instances: under their class and local types, the rest unattached"
        $ withPage
          "{-# LANGUAGE TypeFamilies #-}\n\
          \module M where\n\
          \class C a\n\
          \data T = T\n\
          \data U = U deriving Eq\n\
          \type family F a\n\
          \instance C T\n\
          \instance Show (T, U)\n\
          \instance Show Int\n\
          \type instance F T = Int\n"
        $ \p -> do
          listed p "class:C" @?= ["inst:C:T"]
          listed p "type:T" @?= ["inst:C:T", "inst:Show:(T, U)"]
          listed p "type:U" @?= ["inst:Eq:U", "inst:Show:(T, U)"]
          listed p "type:F" @?= ["inst:F:T"]
          [(i, field "attached" d) | d <- decls p, Just i <- [declId d], isInstance d]
            @?= [ ("inst:Eq:U", Bool True),
                  ("inst:C:T", Bool True),
                  ("inst:Show:(T, U)", Bool True),
                  ("inst:Show:Int", Bool False),
                  ("inst:F:T", Bool True)
                ],
      testCase "an instance naming an associated family is under its class"
        $ withPage
          "{-# LANGUAGE TypeFamilies #-}\n\
          \module M where\n\
          \class K a where { data KD a }\n\
          \instance K Int where { data KD Int = KDI deriving Eq }\n"
        $ \p -> listed p "class:K" @?= ["inst:K:Int", "inst:Eq:KD Int"],
      testCase "a shared signature is whole under its first name" $
        withPage "module M where\ndata T\nf, g :: T\nf = undefined\ng = f\n" $ \p ->
          codeText (field "code" (field "signature" (decl p "val:f"))) @?= "f, g :: T",
      testCase "links in code and docs; Main without a module line" $
        withPage "-- | 'Main.T', 'Other.T'.\nx :: Main.T\nx = T\ndata T = T\n" $ \p -> do
          linksIn (field "code" (field "signature" (decl p "val:x"))) @?= ["type:T"]
          targets (decl p "val:x") @?= [String "type:T", Null],
      testCase "exported flags follow the export list" $
        withPage "module M (T, f) where\ndata T = A\ndata U\nf = 1\ng = 2\n" $ \p ->
          [(i, field "exported" d) | d <- decls p, Just i <- [declId d]]
            @?= [ ("type:T", Bool True),
                  ("type:U", Bool False),
                  ("val:f", Bool True),
                  ("val:g", Bool False)
                ],
      testCase "shared pieces are sent once: the page grows linearly" $ do
        small <- pageSize (sharing 200)
        big <- pageSize (sharing 400)
        linear small big,
      testCase "a shared signature: the other names point to the first" $
        withPage "module M where\n-- | Doc.\nf, g :: Int\nf = 1\ng = 2\n" $ \p -> do
          field "names" (field "signature" (decl p "val:f"))
            @?= Aeson.toJSON ["f", "g" :: Text]
          field "signature" (decl p "val:g") @?= shared "val:f"
          [field "sharedWith" d | d <- list (field "docs" (decl p "val:g"))]
            @?= [String "val:f"]
          [field "sharedWith" x | x <- list (field "source" (decl p "val:g"))]
            @?= [String "val:f", String "val:f", Null],
      testCase "no self links in kind signatures, roles, equations"
        $ withPage
          "{-# LANGUAGE TypeFamilies, RoleAnnotations, StandaloneKindSignatures #-}\n\
          \module M where\n\
          \import Data.Kind (Type)\n\
          \type K :: Type -> Type\n\
          \type role K nominal\n\
          \data K a = K a\n\
          \type family Q a where\n\
          \  Q Int = Bool\n"
        $ \p -> do
          linksIn (field "kindSig" (decl p "type:K")) @?= []
          linksIn (field "roles" (decl p "type:K")) @?= []
          concatMap linksIn (list (field "equations" (decl p "type:Q"))) @?= [],
      testCase "strictness marks don't link to a local (!); pragmas don't link"
        $ withPage
          "module M where\n\
          \data T = T !Int ~Int\n\
          \(!), (~) :: Int -> Int -> Int\n\
          \(!) = (+)\n\
          \(~) = (+)\n\
          \{-# RULES \"r\" forall x. x ! x = T `seq` x #-}\n"
        $ \p -> do
          [linksIn (field "code" c) | c <- list (field "constructors" (decl p "type:T"))]
            @?= [[]]
          [linksIn (field "code" x) | x <- list (field "pragmas" (field "parse" p))]
            @?= [[]],
      testCase "list numbers and @since past 2^63: null, the page still decodes"
        $ withPage
          "-- | 9223372036854775808. item\n--\n-- @since 18446744073709551615\n\
          \module M where\n"
        $ \p -> do
          let d = field "doc" (field "haddock" (field "header" (field "parse" p)))
          field "since" d @?= Null
          [field "number" i | b <- list (field "blocks" d), i <- list (field "items" b)]
            @?= [Null],
      testCase "instance handles: a page grows linearly with derived classes" $ do
        -- A type with k parameters and k derived classes: each instance
        -- id has the whole type in it (Note [Handles]).
        let derived k =
              "module M where\ndata T "
                <> Text.unwords ["a" <> num i | i <- [1 .. k]]
                <> " = T deriving ("
                <> Text.intercalate ", " ["C" <> num i | i <- [1 .. k]]
                <> ")\n"
        small <- pageSize (derived 200)
        big <- pageSize (derived 400)
        linear small big,
      testCase "instance handles: a page grows linearly with mentioned types" $ do
        let mentions k =
              "module M where\n"
                <> Text.unlines ["data T" <> num i | i <- [1 .. k]]
                <> "instance C ("
                <> Text.intercalate " -> " ["T" <> num i | i <- [1 .. k]]
                <> ")\n"
        small <- pageSize (mentions 300)
        big <- pageSize (mentions 600)
        linear small big,
      testCase "instances with equal keys: a page grows linearly" $
        mapM_
          ( \(what, make) -> do
              small <- pageSize (make 300)
              big <- pageSize (make 600)
              assertBool
                (what <> " " <> show (small, big))
                (fromIntegral big < (2.2 :: Double) * fromIntegral small)
          )
          [ ( "type instances",
              \k ->
                "{-# LANGUAGE TypeFamilies #-}\nmodule M where\ntype family F a\n"
                  <> Text.replicate k "type instance F Int = Int\n"
            ),
            ( "data instances",
              \k ->
                "{-# LANGUAGE TypeFamilies #-}\nmodule M where\ndata family D a\n"
                  <> Text.replicate k "data instance D Int = Di deriving Show\n"
            ),
            ( "associated instances",
              \k ->
                "{-# LANGUAGE TypeFamilies #-}\nmodule M where\n\
                \class C a where { type A a }\n"
                  <> Text.replicate k "instance C Int where { type A Int = Int }\n"
            )
          ],
      testCase "sharedWith names one declaration: a field in two constructors"
        $ withRawPage
          "module M where\n\
          \data T\n\
          \  = A { f :: Int }\n\
          \  | B\n\
          \      { f, h :: Int\n\
          \        -- ^ Shared doc.\n\
          \      }\n"
        $ \p -> do
          let objs = [o | o@(Object _) <- everything p, field "n" o /= Null]
              byN n = [o | o <- objs, field "n" o == n]
              named t = [o | o <- objs, field "name" o == String t]
          -- h's doc points to B's f, which sends it.
          [hDoc] <- pure [d | h <- named "h", d <- list (field "docs" h)]
          [sender] <- pure (byN (field "sharedWith" hDoc))
          [field "doc" d /= Null | d <- list (field "docs" sender)] @?= [True]
          -- Two f, two handles; the second (B's) sends the doc.
          [_, fB] <- pure (named "f")
          length (nub (map (field "n") (named "f"))) @?= 2
          field "n" fB @?= field "n" sender,
      warmUpTest
    ]

-- | The warm-up pages take every path that matters. See Note [Page
-- warm-up] in "HaskellReviewer.Server.Wire.Page".
warmUpTest :: TestTree
warmUpTest =
  testCase "the page warm-up has every token class, kind and enum value" $ do
    pages <- mapM jsonOf pageWarmUp
    let values = concatMap everything pages
        kinds = nub [k | Object o <- values, Just (String k) <- [KeyMap.lookup "kind" o]]
        -- Code tokens: [class, text] or [class, text, id].
        classes = nub [c | Array a <- values, String c : String _ : _ <- [toList a]]
    -- Structured ids and linked tokens (Note [Handles]).
    let ids = [i | Object o <- values, Just i@(Object _) <- [KeyMap.lookup "id" o]]
    assertBool "a field id" (any ((/= Null) . field "field") ids)
    assertBool "an instance id" (any ((/= Null) . field "inst") ids)
    assertBool "a linked token" $
      or [True | Array a <- values, [String _, String _, Number _] <- [toList a]]
    -- A line past 256 columns (Note [Slicing]).
    assertBool "a long line" $
      or
        [ Text.length t > 256
        | Object o <- values,
          Just (String t) <- [KeyMap.lookup "text" o]
        ]
    filter
      (`notElem` kinds)
      [ -- Declarations, forms, docs, pragmas, exports, reasons.
        "type",
        "class",
        "value",
        "patsyn",
        "instance",
        "faminst",
        "splice",
        "explicit",
        "standalone",
        "derived",
        "paragraph",
        "heading",
        "code",
        "bullets",
        "numbered",
        "definitions",
        "examples",
        "property",
        "table",
        "text",
        "emphasis",
        "bold",
        "math",
        "link",
        "name",
        "module",
        "rules",
        "complete",
        "ann",
        "default",
        "item",
        "section",
        "doc",
        "named-doc",
        "chunk",
        "parsed",
        "text-only",
        "parse-error",
        "pragma-error",
        "preprocessed",
        "parser-crashed",
        "too-slow",
        "generated",
        "no-source",
        "not-text",
        "too-large",
        -- Warnings, sub lists, ANN targets.
        "deprecated",
        "warning",
        "none",
        "all",
        "some"
      ]
      @?= []
    filter
      (`notElem` classes)
      [ "sp",
        "kw",
        "var",
        "con",
        "op",
        "conop",
        "rop",
        "pun",
        "num",
        "str",
        "chr",
        "prag",
        "mac"
      ]
      @?= []
    -- The other enum-like fields: every value, by field name.
    let valuesOf k = nub [v | Object o <- values, Just (String v) <- [KeyMap.lookup k o]]
    filter
      (not . null . snd)
      [(k, filter (`notElem` valuesOf k) vs) | (k, vs) <- enums]
      @?= []
  where
    enums =
      [ ("strategy", ["none", "stock", "newtype", "anyclass", "via"]),
        ("strictness", ["none", "strict", "lazy"]),
        ("unpack", ["none", "unpack", "nounpack"]),
        ( "direction",
          ["infixl", "infixr", "infix", "unidirectional", "bidirectional", "explicit"]
        ),
        ("ns", ["value", "type", "any"]),
        ("style", ["inline", "display"]),
        ( "form",
          [ "data",
            "newtype",
            "type-data",
            "synonym",
            "open-family",
            "data-family",
            "closed-family",
            "type"
          ]
        ),
        ("definition", ["equations", "pattern", "foreign-import", "none"]),
        ("shape", ["prefix", "infix", "record", "gadt", "gadt-record"]),
        ("role", ["head", "body", "pragma", "doc"]),
        ("qualified", ["no", "pre", "post"]),
        ("level", ["normal", "splice", "quote"]),
        ("keyword", ["type", "pattern", "data", "default"]),
        ("reason", ["autogen", "missing"])
      ]
    jsonOf bytes = case cborToJson bytes of
      Left e -> assertFailure (show e)
      Right j -> maybe (assertFailure "not JSON") pure (Aeson.decode j)

-- * Helpers

num :: Int -> Text
num = Text.pack . show

-- | A module where many names share each kind of piece. See Note
-- [Sent once] in "HaskellReviewer.Server.Wire.Page".
sharing :: Int -> Text
sharing n =
  Text.unlines
    [ "{-# LANGUAGE GADTs, DerivingVia #-}",
      "module M where",
      "-- | A doc for all.",
      names "x" <> " :: Int -> Int -> Int -> Int",
      "infixl 6 " <> Text.intercalate ", " ["`x" <> num i <> "`" | i <- [0 .. n]],
      "{-# DEPRECATED " <> names "x" <> " \"Old.\" #-}",
      "class C a where",
      "  -- | A doc for all.",
      "  " <> names "m" <> " :: a -> a -> a -> a",
      "data G where",
      "  " <> names "G" <> " :: Int -> Int -> G",
      "data R = R {" <> names "f" <> " :: Maybe (Maybe Int)}",
      "newtype N = N Int deriving (" <> names "D" <> ") via (Maybe (Maybe Int))"
    ]
    <> Text.unlines ["x" <> num i <> " = undefined" | i <- [0 .. n]]
  where
    names p = Text.intercalate ", " [p <> num i | i <- [0 .. n]]

-- | The second page is at most 2.2 times the first, for twice the input.
linear :: Int64 -> Int64 -> Assertion
linear small big =
  assertBool (show (small, big)) (fromIntegral big < (2.2 :: Double) * fromIntegral small)

-- | The size of a module's page, in bytes.
pageSize :: Text -> IO Int64
pageSize src = case parseSource defaultParseSettings src of
  Parsed m -> pure (LBS.length (encodeModulePage (Generation 1) index (viewOf src m)))
  other -> assertFailure (take 300 (show other))

-- | @{sharedWith: id}@.
shared :: Text -> Value
shared i = Aeson.object ["sharedWith" Aeson..= i]

list :: Value -> [Value]
list = \case
  Array a -> toList a
  _ -> []

-- | The page of a module, as JSON, with handles and ids as id text
-- ('resolved').
withPage :: Text -> (Value -> Assertion) -> Assertion
withPage src k = withRawPage src (k . resolved)

-- | The page of a module, as JSON, as sent.
withRawPage :: Text -> (Value -> Assertion) -> Assertion
withRawPage src k = case parseSource defaultParseSettings src of
  Parsed m ->
    case cborToJson (encodeModulePage (Generation 1) index (viewOf src m)) of
      Left e -> assertFailure (show e)
      Right j -> maybe (assertFailure "not JSON") k (Aeson.decode j)
  other -> assertFailure (take 300 (show other))

-- | Ids as text, and every handle (sharedWith, instances, instance,
-- onType, target, code links) as the id text of its declaration. See
-- Note [Handles] in "HaskellReviewer.Server.Wire.Page".
resolved :: Value -> Value
resolved p = go p
  where
    parts = [t | String t <- list (field "idParts" (field "parse" p))]
    idText = \case
      String t -> t
      Object o
        | Just (String f) <- KeyMap.lookup "field" o,
          Just (Number n) <- KeyMap.lookup "parent" o ->
            "field:" <> part n <> "." <> f
        | Just (String c) <- KeyMap.lookup "inst" o,
          Just (Array hs) <- KeyMap.lookup "head" o ->
            "inst:" <> c <> ":" <> mconcat [part n | Number n <- toList hs]
      _ -> "?"
    part n = case drop (truncate n) parts of
      t : _ -> t
      [] -> "?"
    byHandle =
      KeyMap.fromList
        [ (handleKey n, String (idText i))
        | Object o <- everything p,
          Just (Number n) <- [KeyMap.lookup "n" o],
          Just i <- [KeyMap.lookup "id" o]
        ]
    ref = \case
      Number n -> fromMaybe Null (KeyMap.lookup (handleKey n) byHandle)
      v -> v
    go = \case
      Object o -> Object (KeyMap.mapWithKey field1 o)
      Array a -> Array (fmap token a)
      v -> v
    field1 k v
      | k == "id" = String (idText v)
      | k `elem` ["sharedWith", "instance", "onType", "target"] = ref v
      | k == "instances" = Array (fmap ref (arr v))
      | otherwise = go v
    arr = \case
      Array a -> a
      _ -> mempty
    -- A code token's link.
    token = \case
      Array a
        | [c@(String _), t@(String _), n@(Number _)] <- toList a ->
            Array (fromListV [c, t, ref n])
      v -> go v
    handleKey n = Key.fromText (Text.pack (show (truncate n :: Int)))
    fromListV xs = case Aeson.toJSON xs of
      Array a -> a
      _ -> mempty

viewOf :: Text -> ParsedModule -> ModuleView
viewOf src m =
  ModuleView
    { moduleId = LooseModule file,
      path = Just file,
      answer = ParsedAnswer ParsedSource {text = src, parsed = m}
    }

file :: RelPath
file = fromMaybe (error "bad path") (relPathFromSegments ["M.hs"])

-- | One loose module, M.hs.
index :: ModuleIndex
index =
  buildModuleIndex
    Project
      { source = ImplicitRootCabalFiles,
        withCompiler = Nothing,
        ghc = DetectedGhc {version = parserGhcVersion, source = GhcFromParser},
        packages = [],
        brokenPackages = [],
        warnings = []
      }
    (mkFileList FromDisk 0 [(file, EntryFile)])

field :: Text -> Value -> Value
field k = \case
  Object o -> fromMaybe Null (KeyMap.lookup (Key.fromText k) o)
  _ -> Null

decls :: Value -> [Value]
decls p = case field "declarations" (field "parse" p) of
  Array a -> toList a
  _ -> []

declId :: Value -> Maybe Text
declId d = case field "id" d of
  String i -> Just i
  _ -> Nothing

decl :: Value -> Text -> Value
decl p i = case [d | d <- decls p, declId d == Just i] of
  d : _ -> d
  [] -> Null

isInstance :: Value -> Bool
isInstance d = field "kind" d `elem` [String "instance", String "faminst"]

-- | The instance ids a declaration lists.
listed :: Value -> Text -> [Text]
listed p i = case field "instances" (decl p i) of
  Array a -> [t | String t <- toList a]
  _ -> []

-- | The text of some code.
codeText :: Value -> Text
codeText = \case
  Array a -> mconcat [t | Array tok <- toList a, String t <- take 1 (drop 1 (toList tok))]
  _ -> ""

-- | The targets of the links in some code.
linksIn :: Value -> [Text]
linksIn = \case
  Array a -> [t | Array tok <- toList a, String t <- drop 2 (toList tok)]
  _ -> []

-- | The targets of the names in a declaration's docs, null if not
-- resolved.
targets :: Value -> [Value]
targets d =
  [ t
  | Object o <- everything (field "docs" d),
    KeyMap.lookup "kind" o == Just (String "name"),
    Just t <- [KeyMap.lookup "target" o]
  ]

-- | A value and everything inside it.
everything :: Value -> [Value]
everything v =
  v : case v of
    Object o -> concatMap everything (KeyMap.elems o)
    Array a -> concatMap everything (toList a)
    _ -> []
