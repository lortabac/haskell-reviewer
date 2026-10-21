{-# LANGUAGE StrictData #-}
-- | Types shared by the spike modules.
module Types where



-- | Parse settings of a component, in Cabal's order.
data Settings = Settings
  { setLanguage :: String       -- ^ e.g. "Haskell2010"; absent = "Haskell98"
  , setExts     :: [String]     -- ^ -X flags from default-extensions
  , setGhcOpts  :: [String]     -- ^ kept ghc-options: -X…, -cpp, -pgmP…, -F, -pgmF…, -pgmL…
  , setCppDefs  :: [(String, String)] -- ^ -D from cpp-options (and -optP-D)
  } deriving (Eq, Ord, Show)

-- | Where a listed module comes from.
data ModSource
  = Source FilePath           -- ^ found: .hs, .lhs or .hsig
  | Generated FilePath        -- ^ preprocessor input (.hsc, .x, .y, …)
  | Autogen                   -- ^ autogen-modules, Paths_*, PackageInfo_*
  | Missing                   -- ^ nothing found
  deriving (Eq, Show)

data Component = Component
  { compName     :: String       -- ^ e.g. "lib", "exe:foo"
  , compSettings :: Settings
  , compModules  :: [(String, ModSource)]
  } deriving (Show)

data Package = Package
  { pkgName      :: String
  , pkgCabal     :: FilePath     -- ^ relative to the project root
  , pkgComps     :: [Component]
  , pkgParseSecs :: Double       -- ^ time to parse the .cabal file
  } deriving (Show)

-- | What the project reader found.
data ProjectInfo = ProjectInfo
  { piCabalFiles :: [FilePath]   -- ^ relative to root
  , piMissing    :: [String]     -- ^ package entries with no .cabal file
  , piNotes      :: [String]     -- ^ known limits hit (conditionals, URLs, …)
  } deriving (Show)

-- | Where CPP got turned on.
data CppSource = CppPragma | CppDefaultExt | CppGhcOptions
  deriving (Eq, Ord, Show)

-- | One parse failure, short.
data Failure = Failure
  { fPhase :: String            -- ^ lexer, parser or config
  , fCode  :: String            -- ^ e.g. GHC-58481, or "-"
  , fMsg   :: String            -- ^ first message line
  , fLine  :: Int
  } deriving (Eq, Show)

-- | CPP feature counts for one module.
data CppFeatures = CppFeatures
  { cfIncludes     :: Int
  , cfFnDefines    :: Int
  , cfMacroInCode  :: Int   -- ^ code lines using a defined macro
  , cfMinVersion   :: Int
  , cfGhcMacros    :: Int
  , cfOsArch       :: Int
  , cfDirectives   :: Int
  , cfUnsupported  :: [String]
  } deriving (Show)

-- | Results of the three CPP outcomes.
data CppOutcome = CppOutcome
  { coRaw   :: Maybe Failure     -- ^ (a) CPP off, text as is
  , coStrip :: Maybe Failure     -- ^ (b) directives blanked, all branches kept
  , coBasic :: Maybe Failure     -- ^ (c) mini evaluator
  , coFeatures :: CppFeatures
  } deriving (Show)

-- | Result for one (file, settings) pair.
data ModResult = ModResult
  { mrFile      :: FilePath
  , mrLines     :: Int
  , mrBytes     :: Int
  , mrFailure   :: Maybe Failure   -- ^ main result (for CPP: outcome c)
  , mrLexes     :: Bool            -- ^ lexTokenStream OK (only tried on failure)
  , mrCpp       :: Maybe (CppSource, CppOutcome)
  , mrTags      :: [String]        -- ^ e.g. "pgmF", "pgmP", "pgmL"
  , mrReadNs    :: Integer
  , mrHeaderNs  :: Integer
  , mrParseNs   :: Integer
  } deriving (Show)
