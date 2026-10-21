# parser-count (M0 spike, T06)

Throwaway. Runs ghc-lib-parser 9.14 over every module of the reference
projects and counts rejections and CPP use. Not in the main build.
Results: `results/summary.md`. Decision: D-83.

## Fetch the projects

From the repo root:

```
bench/fetch-projects.sh                # small, typical, large
bench/fetch-projects.sh --role extra   # clash-compiler, servant, cabal
bench/fetch-projects.sh pandoc         # by name
```

Pins are in `bench/projects.txt` (`name role url sha`). Projects go to
`bench/projects/<name>` (git-ignored), or to `$REFERENCE_PROJECTS_DIR`.
A dir already at the pin and clean is kept.

## Build

This dir has its own `cabal.project`, so build from here:

```
cd spikes/parser-count
export CABAL_DIR="$PWD/../../.cabal-home"   # the repo's usual store
cabal build
```

First build of ghc-lib-parser (with alex and happy): ~5 min, ~2 GB RAM.

## Run

```
B=$(cabal list-bin parser-count)
$B [--jobs N] [--json DIR] [--triage FILE] PROJECT_DIR...
```

- prints a table per project;
- `--json DIR` writes `DIR/<name>.json`;
- `--triage triage.txt` adds a tag to each rejected file;
- `--jobs N` sets the cores (default: all). Timings: `+RTS -N1 -RTS`
  or `+RTS -N8 -A64m -RTS`.

To redo the results:

```
for p in optparse-applicative pandoc wire-server clash-compiler servant cabal; do
  $B --triage triage.txt --json results ../../bench/projects/$p
done
```

`$B --cpp-dump FILE [NAME=VALUE...]` prints the mini CPP output of one
file.

## Other tools

- `./cpp-check.sh PROJECT_DIR results/<name>.json`: compares the mini
  CPP with `cpp -traditional` on every CPP module (needs `jq`, `cpp`).
- `old-syntax-probes/run.sh [GHC...]`: the Q-OLD-SYNTAX checklist.
  Compiles each probe with each GHC (default ghc-9.10, ghc-9.12,
  ghc-9.14) and prints the first error code.

## triage.txt

One rule per line: `tag | project | rule`. Project `*` matches any.
Rule: `file:PREFIX`, `code:GHC-NNNNN`, `msg:PREFIX` or `phase:NAME`.
The first match wins. `#` starts a comment. Tags used: old-syntax,
needs-cpp, spike-config, spike-limit, test-data.
