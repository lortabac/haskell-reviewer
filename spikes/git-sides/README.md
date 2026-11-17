# git-sides (M3 spike, T01)

Throwaway. Settles which git calls are safe for diff-mode sides, how to
tell clean from dirty working-tree files without git running filters,
and whether one `cat-file --batch-command` is enough. Not in the main
build, not in CI. Results: `results/summary.md`. Decision: D-311.

## Build

Own `cabal.project` (imports the root freeze). One GHC is enough:

    cd spikes/git-sides
    export CABAL_DIR="$PWD/../../.cabal-home"
    cabal build -w ghc-9.14.1
    B=$(cabal list-bin -w ghc-9.14.1 git-sides)

## Run

    ./hgit.sh DIR ARGS...          # git with the product's env and flags
    ./hgit.sh --print-flags        # must equal `$B flags`
    /bin/sh ./calls.sh             # exact output of every call -> results/calls.txt
    /bin/sh ./planted.sh $B        # trap repo -> results/planted.tsv,
                                   #   worktree-check.tsv, list-side.txt;
                                   #   exit 1 on FAIL
    /bin/sh ./prepare-wire.sh      # copies wire-server to .worktrees/.build/t01-git-sides
    /bin/sh ./measure.sh $B        # listing, blob pool, stat check -> results/*.tsv
    $B read-blobs --pool N --workers W [--requester MS] [--runs R] [--warmup K] REPO OIDS
    $B worktree [--list] REPO      # own stat check: candidates, dirty, timings
    $B list-side REPO REV          # ls-tree --long, or the fallback when it
                                   #   dies (plain ls-tree + cat-file info,
                                   #   restart cat-file on death)

`GIT=/path/to/git` picks another git. The `-2.44.0` / `-2.44.1` result
files come from git built from the release tarballs
(`make CFLAGS=-std=gnu17 NO_CURL=1 NO_PERL=1 NO_TCLTK=1 NO_GETTEXT=1
NO_PYTHON=1 NO_EXPAT=1`; gcc 15 needs the -std).

## Planted repo

Traps write a file in `$T/markers`: filter drivers (repo config, with
and without `process`; user config for LFS with a fake `git-lfs` on
PATH), attributes from
`.gitattributes`, `.git/info/attributes` and `core.attributesFile`,
textconv / diff command / external diff, fsmonitor (hook and daemon),
every hook, lazy fetch through an `ext::` remote, a submodule with its
own filter, FIFOs. Index states: r1 zero stat (`read-tree`), r2 racy
(mtime in 2100), r3 touched, r4 untracked cache + split index + v4
(racy too). Every safe call must leave no marker, start no child
(trace2, strace if present) and leave `.git` the same. PASS-mtime: only
the mtime of `sharedindex.*` moved (git does that on every read of a
split index). Positive controls (status, hash-object per attribute
source, cat-file --filters/--textconv, diff, commit hooks, the
fsmonitor daemon, lazy fetch without GIT_NO_LAZY_FETCH) must fire, or
the trap is broken. The diff-files guard rows are recorded as fires /
quiet. The whole safe set also runs in the partial clone.

`worktree-check.tsv`: ours against `diff-files` with the traps removed
(in place and after `cp -a`), and `edge-*` rows against `git status`: a
symlink in a leading directory, a value-less `core.filemode`,
`core.autocrlf=yes`, `text=input`. An edge row with anything in
only_ours or only_diff_files is a FAIL.

## wire-server

`prepare-wire.sh` copies `bench/projects/wire-server` (never touched,
never run git in it) and fetches one more level (`--depth 2`) in the
copy: head 7568f59, base 7568f59^1. A second copy gets
`update-index --refresh` once (setup only), so stat data is fresh.

Pool rule, fixed before measuring: more than one `cat-file` only if,
with 8 workers, pool 2 or 4 cuts total time by >= 25% vs pool 1, or the
requester's p95 is > 10 ms with pool 1 and <= 10 ms with pool N.
Warm cache only.
