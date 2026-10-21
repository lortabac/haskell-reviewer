# T06 results: parser counts (M0 spike)

ghc-lib-parser 9.14.1.20251220 over the reference projects (D-82), each
module with its component's language and extensions. Pinned commits are
in `bench/projects.txt`. D-83 points here. Raw data: `<project>.json`.

Machine: AMD Ryzen AI 9 HX 370 (24 threads), 62 GB RAM, Linux, GHC 9.14.1.

## Counts

Files = unique module files in some component. Other = tracked
`.hs`/`.lhs`/`.hsig` files in no component, parsed with GHC2021.

| Project | Role | Packages | Listed modules | Files | Other | CPP files | Rejected | Other rejected |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| optparse-applicative | small | 1 | 33 | 33 | 1 | 13 (39.4%) | 0 | 0 |
| pandoc | typical | 4 | 359 | 358 | 8 | 36 (10.1%) | 0 | 0 |
| wire-server | large | 57 (+1 missing dir) | 2,059 | 1,964 | 7 | 2 (0.1%) | 0 | 0 |
| clash-compiler | extra | 9 | 432 | 428 | 552 | 193 (45.1%) | 2 | 68 |
| servant | extra | 40 | 230 | 230 | 38 | 50 (21.7%) | 0 | 0 |
| cabal | extra | 15 | 616 | 606 | 1,604 | 40 (6.6%) | 0 | 6 |

pandoc: 19 of the 36 CPP files have the pragma but no directives. With
directives only: 17 / 358 = 4.7%.

Listed modules without a source file:

| Project | Generated (.hsc, .x, .y, …) | Autogen | Missing |
|---|---:|---:|---:|
| optparse-applicative | 0 | 0 | 0 |
| pandoc | 0 | 1 | 0 |
| wire-server | 1 | 85 | 3 |
| clash-compiler | 0 | 4 | 0 |
| servant | 0 | 0 | 0 |
| cabal | 1 | 3 | 0 |

- wire-server: `services/wire-server-enterprise` is a git submodule, so
  its dir is empty. Autogen = `Paths_*` plus proto-lens modules in
  `autogen-modules`. Missing = `RunAllTests` (made by a custom Setup)
  and 2 proto-lens modules not listed in `autogen-modules`.
- `.hs-boot` files: clash 16, cabal 2, others 0.
- Modules with `-F -pgmF` (hspec-discover): wire-server 11, servant 16,
  cabal 4. They parse, but the real module comes from the preprocessor.
- servant has 30 `.lhs` files with `-pgmL markdown-unlit`; the spike
  reads their ```` ```haskell ```` blocks.
- Only one module turns CPP on outside the file (cabal,
  `default-extensions: CPP`). All others use `{-# LANGUAGE CPP #-}`.

## Rejections

Reference projects: 0 rejected of 2,355 files, and 0 of 16 other files.

Extras, by hand (rules in `../triage.txt`):

| Project | Files | Bucket | Tag |
|---|---:|---|---|
| clash-compiler | 1 | `parse error on input 'else'` (needs `MachDeps.h` for `WORD_SIZE_IN_BITS`) | needs-cpp |
| clash-compiler | 1 | `parse error on input 'CONS_PREC'` (macro used in code) | needs-cpp |
| clash-compiler, other | 68 | TH splices, quasi-quotes, … in testsuite inputs and examples; clash adds its own flags | spike-config |
| cabal, other | 3 | `*.template.hs` (templates, not Haskell) | test-data |
| cabal, other | 3 | test packages outside `cabal.project` | spike-config |

No old syntax. All rejected files lex fine.

## CPP

(a) raw: CPP off, text as is. (b) strip: directive lines blanked, all
branches kept. (c) basic: the mini evaluator in `../src/Cpp.hs`.

| Project | CPP files | (a) raw | (b) strip | (c) basic |
|---|---:|---:|---:|---:|
| optparse-applicative | 13 | 1 | 13 | 13 (100%) |
| pandoc | 36 | 20 | 33 | 36 (100%) |
| wire-server | 2 | 1 | 2 | 2 (100%) |
| clash-compiler | 193 | 115 | 188 | 191 (99.0%) |
| servant | 50 | 27 | 48 | 50 (100%) |
| cabal | 40 | 1 | 34 | 40 (100%) |

CPP files using each feature:

| Project | `#include` | fn-like `#define` | macro in code | `MIN_VERSION_*` | GHC macros | os/arch | no directives |
|---|---:|---:|---:|---:|---:|---:|---:|
| optparse-applicative | 0 | 0 | 0 | 4 | 9 | 0 | 1 |
| pandoc | 0 | 0 | 1 | 5 | 0 | 0 | 19 |
| wire-server | 0 | 0 | 0 | 0 | 0 | 0 | 1 |
| clash-compiler | 10 | 0 | 6 | 46 | 16 | 2 | 112 |
| servant | 0 | 0 | 0 | 20 | 5 | 0 | 26 |
| cabal | 0 | 0 | 1 | 17 | 3 | 17 | 0 |

"Macro in code" is an upper bound: a code line has a name that is a
known macro.

### Check against `cpp -traditional`

`../cpp-check.sh`: same kept lines in 326 of 334 CPP files. The other 8:
7 use an object-like macro in code (cpp expands it, the evaluator
doesn't; 3 need `WORD_SIZE_IN_BITS`), 1 is cpp eating `/**/` inside a
Haskell comment.

Limits: the check uses the evaluator's own assumptions (MIN_VERSION = 1,
`#include` empty, no cpp-options), so it checks mechanics only. Cabal's
`VERSION_pkg` macros were not made, so e.g. pandoc-cli takes non-default
branches. Parse results are not affected.

## The CPP rule (fixed in advance)

Basic CPP goes into M1 iff CPP files are ≥ 3% of files in the typical
OR large project, AND (c) parses ≥ 90% of those CPP files.

- typical (pandoc): 36 / 358 = **10.1% ≥ 3%**; (c) 36 / 36 = **100% ≥ 90%**.
  With directives only: 17 / 358 = 4.7%, (c) 17 / 17. Same outcome.
- large (wire-server): 2 / 1,964 = 0.1%.

**Result: basic CPP goes into M1.**

## Speed and memory

Spike only: read, decode, header, parse, keep a small result. No module
model. Lines = all parsed files.

| Project | Lines | -N1 wall | -N1 lines/s | -N24 wall | -N8 -A64m wall | -N8 -A64m lines/s |
|---|---:|---:|---:|---:|---:|---:|
| optparse-applicative | 6,344 | 0.08 s | 81k | 0.04 s | 0.03 s | 215k |
| pandoc | 125,395 | 1.37 s | 92k | 0.46 s | 0.29 s | 426k |
| wire-server | 383,275 | 2.86 s | 134k | 1.05 s | 0.62 s | 619k |

- Per module (read + header + parse) at -N1: pandoc p50 1.3 ms, p95
  12 ms, max 28 ms; wire-server p50 0.4 ms, p95 5.5 ms, max 48 ms.
  Parsing is ~90% of it.
- GC: with the default 4 MB nursery, productivity is ~60% and -N24 is
  only ~3× faster than -N1. `-A64m` gives ~90%. 8 cores with `-A64m`
  are as fast as 24.
- Peak heap at -N1: ~110 MB (wire-server, results only); with
  `-N8 -A64m` ~650 MB, mostly nursery.
- `.cabal` parsing: wire-server's 57 files in 0.1 s, 10 ms max.

ghc-lib-parser build (alex, happy, ghc-lib-parser; cold store, default
`-j`): 5 min 9 s wall, 320 s CPU, peak RSS 2.0 GB. Mostly one core.

## Q-OLD-SYNTAX checklist

Probe files in `../old-syntax-probes/` (Haskell2010, `-fno-code`). Code
of the first error, `ok` = accepted. 9.8 not tested.

| Case | 9.6.6 | 9.10.3 | 9.12.4 | 9.14.1 |
|---|---|---|---|---|
| `forall` as a term name | ok | 58481 | 58481 | 58481 |
| `arr !i` (proposal 229, bang) | 95644 | 95644 | 95644 | 95644 |
| `xs@ (x:_)` (229, @) | 33856 | 33856 | 33856 | 33856 |
| `a ~b` in a context | 18932 | 18932 | 18932 | 18932 |
| `x -1`, `negate -x` | 80003 | 80003 | 80003 | 80003 |
| `id $id` | ok | ok | ok | ok |
| `show @Int` without TypeApplications | ok | 23482 | 23482 | 23482 |
| `type a * b` with StarIsType | 47568 | 47568 | 47568 | 47568 |
| `x %1 = …` without LinearTypes | ok | ok | ok | ok |
| `import M (type T)` without ExplicitNamespaces | 47007 | 47007 | 47007 | 47007 |
| `(1; 2)` pattern without OrPatterns | 58481 | 58481 | 29847 | 29847 |
| NondecreasingIndentation layout | 07626 | 07626 | 07626 | 07626 |

`x -1` and `show @Int` fail after parsing (type checker, renamer); the
spike's parser accepts them.

No extension was removed between 9.10 and 9.14. Within 9.10–9.14 the
parser accepts and rejects the same cases. The ghc-lib-parser 9.10
cross-check was not run: no old-syntax group showed up.
