// The fixture projects: small git repos in a temp dir, built fresh by
// each worker. Fixed author and dates, no user git config.
import { execFileSync } from "node:child_process";
import { mkdir, mkdtemp, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

// Exactly 5 lines.
export const mainHs = [
  "module Main where",
  "",
  "main :: IO ()",
  "main =",
  '  putStrLn "hello"',
  "",
].join("\n");

export const readme = "# Fixture\n\nA test project.\n";
export const deepHs = "module Data.Deep.Nested.Mod where\n\nx :: Int\nx = 1\n";

// Markup that must show up as text, never run.
export const evilHs =
  '<script>window.__pwned=1</script>\n<img src=x onerror="window.__pwned=2">\n';
// A name with markup. Not "<b>bold</b>.txt": "/" can't be in a name.
export const markupName = "<b>bold.txt";

// Odd names, each with one known line.
export const oddFiles: Record<string, string> = {
  "odd/with space.txt": "a space\n",
  "odd/hash#and%percent.txt": "hash and percent\n",
  "odd/q?.txt": "question mark\n",
  "odd/ünï.txt": "not ascii\n",
};

export const bigSize = 1024 * 1024 + 1;

// What the file view says, exactly.
export const notices = {
  binary: "This file is not UTF-8 text, so it isn't shown (4 bytes).",
  tooLarge:
    "This file is too large to show (1,048,577 bytes; the limit is 1 MiB).",
  nested: "Nested git repository. Its files aren't shown.",
};

// The top-level folders, in tree order.
export const topFolders = ["data", "odd", "src", "vendor", "xss"];

const gitEnv: NodeJS.ProcessEnv = {
  ...process.env,
  GIT_CONFIG_GLOBAL: "/dev/null",
  GIT_CONFIG_NOSYSTEM: "1",
  GIT_AUTHOR_NAME: "Fixture",
  GIT_AUTHOR_EMAIL: "fixture@example.com",
  GIT_AUTHOR_DATE: "2000-01-01T00:00:00Z",
  GIT_COMMITTER_NAME: "Fixture",
  GIT_COMMITTER_EMAIL: "fixture@example.com",
  GIT_COMMITTER_DATE: "2000-01-01T00:00:00Z",
};

function git(cwd: string, ...args: string[]): void {
  execFileSync("git", ["-c", "init.defaultBranch=main", ...args], {
    cwd,
    env: gitEnv,
    stdio: ["ignore", "ignore", "pipe"],
  });
}

async function put(root: string, path: string, data: string | Buffer) {
  const full = join(root, path);
  await mkdir(dirname(full), { recursive: true });
  await writeFile(full, data);
}

export interface Project {
  root: string;
  remove: () => Promise<void>;
}

export async function makeProject(): Promise<Project> {
  const root = await mkdtemp(join(tmpdir(), "hr-e2e-"));
  git(root, "init", "-q");
  await put(root, "README.md", readme);
  await put(root, "src/Main.hs", mainHs);
  await put(root, "src/Data/Deep/Nested/Mod.hs", deepHs);
  await put(root, "xss/Evil.hs", evilHs);
  await put(root, `xss/${markupName}`, "markup in the name\n");
  for (const [path, text] of Object.entries(oddFiles)) {
    await put(root, path, text);
  }
  await put(root, "data/blob.bin", Buffer.from([0x00, 0x01, 0x02, 0xff]));
  await put(root, "data/big.txt", "a".repeat(bigSize));
  await symlink("src/Main.hs", join(root, "link-to-main"));
  await symlink("nowhere", join(root, "dangling"));
  await put(root, ".gitignore", "*.log\n");
  await put(root, "secret.log", "secret\n");
  git(root, "add", "-A");
  git(root, "commit", "-q", "-m", "fixture");
  await put(root, "untracked.txt", "not in git yet\n");
  await mkdir(join(root, "vendor/nested"), { recursive: true });
  git(join(root, "vendor/nested"), "init", "-q");
  return {
    root,
    remove: () => rm(root, { recursive: true, force: true }),
  };
}

// A cabal project: one package with every kind of module, a broken
// package, an Hpack-only dir and a loose file.
export const cabalFiles: Record<string, string> = {
  "cabal.project": "packages: pkg/ bad/ hp/ nope/\n",
  "pkg/pkg.cabal": [
    "cabal-version: 2.0",
    "name: pkg",
    "version: 0.1.0",
    "build-type: Simple",
    "",
    "library",
    "  hs-source-dirs: src",
    "  exposed-modules: Foo Lexer",
    "  signatures: Sig",
    "  other-modules: Paths_pkg",
    "  autogen-modules: Paths_pkg",
    "  build-depends: base",
    "",
    "executable app",
    "  main-is: Main.hs",
    "  hs-source-dirs: app",
    "  build-depends: base",
    "",
    "test-suite spec",
    "  type: exitcode-stdio-1.0",
    "  main-is: Spec.hs",
    "  hs-source-dirs: test",
    "  ghc-options: -F -pgmF hspec-discover",
    "  build-depends: base",
    "",
    "benchmark slow",
    "  type: exitcode-stdio-1.0",
    "  main-is: Bench.hs",
    "  hs-source-dirs: bench",
    "  buildable: False",
    "  build-depends: base",
    "",
  ].join("\n"),
  "pkg/src/Foo.hs": "module Foo where\n",
  "pkg/src/Lexer.x": "{\nmodule Lexer where\n}\n",
  "pkg/src/Sig.hsig": "signature Sig where\n",
  "pkg/app/Main.hs": "main = pure ()\n",
  "pkg/test/Spec.hs": "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}\n",
  "pkg/bench/Bench.hs": "main = pure ()\n",
  // Unparsable.
  "bad/bad.cabal": "name: bad\nlibrary\n  exposed-modules: {\n",
  "hp/package.yaml": "name: hp\n",
  "scripts/Tool.hs": "module Tool where\n",
};

export async function makeCabalProject(): Promise<Project> {
  const root = await mkdtemp(join(tmpdir(), "hr-e2e-"));
  git(root, "init", "-q");
  for (const [path, text] of Object.entries(cabalFiles)) {
    await put(root, path, text);
  }
  git(root, "add", "-A");
  git(root, "commit", "-q", "-m", "fixture");
  return {
    root,
    remove: () => rm(root, { recursive: true, force: true }),
  };
}

// A project for the docs view: an implicit package with one module of
// many kinds, one that links to it, and one that doesn't parse.
export const shapesHs = [
  "{-# LANGUAGE PatternSynonyms #-}",
  "{-# LANGUAGE TypeFamilies #-}",
  "-- | Shapes for the docs view tests. See <https://example.com/shapes the site>.",
  "--",
  "-- Markup is text: \\<b\\>bold\\</b\\>.",
  "--",
  "-- > <script>window.__pwned=1</script>",
  "module Shapes",
  "  ( -- * Types",
  "    Shape (..),",
  "    Box (..),",
  "    Container (..),",
  "    -- * Functions",
  "    area,",
  "    perimeter,",
  "    (<+>),",
  "    pattern Unit,",
  "    Family,",
  "    longSig,",
  "    old,",
  "  )",
  "where",
  "",
  "import Data.Kind (Type)",
  "",
  "-- | A shape, see 'area'.",
  "data Shape = Circle Double | Rect {width :: !Double, height :: Double}",
  "  deriving (Eq, Show)",
  "",
  "newtype Box a = Box a deriving newtype (Eq)",
  "",
  "-- | Things with elements.",
  "class Container f where",
  "  type Elem f :: Type",
  "  empty :: f",
  "  insert :: Elem f -> f -> f",
  "  {-# MINIMAL empty, insert #-}",
  "",
  "instance Container [a] where",
  "  type Elem [a] = a",
  "  empty = []",
  "  insert = (:)",
  "",
  "instance Num Bool where",
  "  fromInteger = (/= 0)",
  "",
  "-- | Both share one signature.",
  "area, perimeter :: Shape -> Double",
  "area _ = 0",
  "perimeter _ = 0",
  "",
  "infixl 6 <+>",
  "",
  "(<+>) :: Shape -> Shape -> Shape",
  "a <+> _ = a",
  "",
  "pattern Unit :: Shape",
  "pattern Unit = Circle 1",
  "",
  "type family Family a",
  "",
  "type instance Family Shape = Double",
  "",
  "-- | A very long signature, to test wrapping.",
  "longSig :: (Eq aaaaaaaaaaaaaaaaaaaa, Show bbbbbbbbbbbbbbbbbbbb) => aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbb -> (aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbb) -> Either aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbb -> Maybe [(aaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbb)]",
  "longSig = undefined",
  "",
  "old :: Int",
  "old = 1",
  '{-# DEPRECATED old "Use area" #-}',
  "",
  '{-# RULES "area/id" forall s. area s = area s #-}',
  "",
  "{-# COMPLETE Unit #-}",
  "",
].join("\n");

// The line of a declaration in shapesHs.
export function shapesLine(start: string): number {
  return shapesHs.split("\n").findIndex((l) => l.startsWith(start)) + 1;
}

export const otherHs = [
  '-- | See "Shapes#v:area" and "Shapes#t:Shape".',
  "module Other where",
  "",
  "x :: Int",
  "x = 1",
  "",
].join("\n");

export const brokenHs = "module Broken where\n\nx = = 1\n";

export async function makeDocsProject(): Promise<Project> {
  const root = await mkdtemp(join(tmpdir(), "hr-e2e-"));
  git(root, "init", "-q");
  await put(root, "Shapes.hs", shapesHs);
  await put(root, "Other.hs", otherHs);
  await put(root, "Broken.hs", brokenHs);
  git(root, "add", "-A");
  git(root, "commit", "-q", "-m", "fixture");
  return {
    root,
    remove: () => rm(root, { recursive: true, force: true }),
  };
}
