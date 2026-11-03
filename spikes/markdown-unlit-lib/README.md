# markdown-unlit-lib (M2 spike, T02)

Throwaway. Calls markdown-unlit 0.6.0 as a library behind the shapes of
our `unlitMarkdown` / `unlitLoose`, and tries a layout with a position
map (T02b prototype). Not in the main build, not in CI.
Results: `results/summary.md`.

## Build

This dir has its own `cabal.project` (it imports the root freeze), so
build from here. One GHC is enough:

    cd spikes/markdown-unlit-lib
    export CABAL_DIR="$PWD/../../.cabal-home"   # the repo's usual store
    cabal build -w ghc-9.14.1
    B=$(cabal list-bin -w ghc-9.14.1 markdown-unlit-lib)

Clean build (base-compat and markdown-unlit already in the store): ~40 s.

## Run

    $B linemap cases/*.lhs                 # P0 ours, P1 lib text, P2 lib layout
    SEED=path/to/servant/doc/tutorial/Server.lhs \
      $B alloc IMPL SHAPE BYTES +RTS -T    # one cost sample, one line of TSV
    ./alloc.sh $B 3                        # full grid -> results/alloc.tsv
    IMPLS=lib-has-block APPEND=1 ./alloc.sh $B 3   # add one impl's rows
    ./ladder.sh $B                         # S8/S9 ladders -> results/ladder.tsv
    $B census results/census.tsv DIR...    # every .lhs/.lhsig, ours vs lib
    $B diff FILE                           # our code lines vs the lib's

- IMPL: none, ours-md, lib-raw, lib-layout, ours-loose, lib-loose,
  lib-has-block, parse-ours, parse-lib.
- SHAPE: S1..S13 (see src/Shapes.hs). S1 repeats servant's
  tutorial Server.lhs (servant at c12cc37); `alloc.sh` looks for it
  under the main checkout's `.worktrees/.build/t13a-projects` (found
  with `git rev-parse --git-common-dir`), or takes `SEED`.
- census: a DIR with a cabal.project or a .cabal is a project, else
  its subdirs (not symlinks) are. Census used
  `.worktrees/.build/t13a-projects`, `t13b-projects/cabal`,
  `t13b-projects/clash-compiler`.
