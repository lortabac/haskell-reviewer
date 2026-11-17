# git-sides results (T01)

Linux, git 2.56.0 (Fedora), plus 2.44.0 and 2.44.1 built from source.
Decision: D-311.

## Planted repo

Every safe call, in r1-r4, the fsmonitor-daemon repo and the partial
clone: no marker, no child (trace2 child_start = 0, strace: only git's
own execve), `.git` the same. Safe calls: --version, rev-parse
(--is-inside-work-tree; --show-prefix --show-object-format
--is-shallow-repository --git-path index; --verify --quiet
--end-of-options HEAD^{commit}), merge-base --all, ls-tree -r -z --long
[--full-tree] (top and lib/), cat-file --batch-command (contents, info,
missing), ls-files -z --stage [--debug], ls-files -z --others
--exclude-standard, check-attr -z --stdin, config -z --get-regexp,
config --get, config -z --name-only --get-regexp ^filter. The whole
safe set also runs in the partial clone. Filter traps: a driver with
`process` (x), a clean-only driver (cl), LFS from the user config.

| git    | planted.sh | notes |
|--------|-----------|-------|
| 2.56   | PASS | |
| 2.44.1 | PASS | daemon control skipped (no fsmonitor--daemon in the build) |
| 2.44.0 | FAIL | no GIT_NO_LAZY_FETCH: hardened cat-file, ls-tree --long and list-side lazy-fetched |

r4 (split index): ls-files, check-attr, diff-files and git-sides
worktree bump the mtime of `sharedindex.*` (PASS-mtime). Git does this
on every read of a split index; content unchanged.

diff-files --name-only guards:

| variant | r1 read-tree | r2-r4 racy |
|---|---|---|
| plain | fires (status in submodule) | fires (LFS clean) |
| --attr-source=<empty tree> | fires (child in submodule) | fires (info/attributes, core.attributesFile) |
| + -c core.attributesFile=/dev/null | fires (submodule) | fires (info/attributes) |
| + --ignore-submodules=all | quiet | fires |
| + every filter.* blanked (GIT_CONFIG_COUNT) | quiet | quiet |
| git-sides worktree (ours) | quiet | quiet |

Zero-stat entries make diff-files list the file dirty, with no content
check. Racy ones run the clean filter.

FIFO as .gitattributes / core.excludesFile: ls-files --others,
check-attr, diff-files and git-sides worktree block (killed at 5 s);
ls-files --stage and ls-tree don't.

Ours vs diff-files, traps removed: in place both say b.txt; after
cp -a, diff-files lists every file, ours still only b.txt.

Edge rows, ours vs `git status` (all equal): a symlink in a leading
directory (a/b deleted), a value-less `core.filemode` plus chmod +x
(x.sh mode changed), `core.autocrlf=yes` and `text=input` with a
touched CRLF file (clean).

Known differences from git:
- a touched or racy file under filter (LFS), ident or
  working-tree-encoding shows dirty (unless its raw bytes equal the
  blob); a racy file is hashed where git with nanoseconds trusts the
  stat. Both are extra dirty files.
- gitlinks: the spike treats every gitlink as clean and shows the
  index's commit id. Git compares the submodule's HEAD, so a moved HEAD
  is a false clean here. T04 lstats gitlinks as git does (missing or
  not a directory: deleted); HEAD is still not compared.
- binary test: the spike's `looksBinary` checks only a NUL or a lone
  CR. Git's `convert_is_binary` also counts `(printable >> 7) <
  nonprintable` (a trailing ^Z not counted). T04 does the full rule.

Partial-clone fallback (`list-side.txt`, proposal): on 2.44.1
`ls-tree --long` dies, so plain `ls-tree` + `cat-file info`, with one
`cat-file` restart for the one missing blob, no fetch. On 2.56
`ls-tree --long` works (size BAD).

## Call details (calls.txt)

- `rev-parse --verify a..b`: prints ids, exit 1. Check the exit code.
- criss-cross: merge-base --all prints 2; plain merge-base prints the
  first of them.
- shallow: merge-base exits 1 with no output.
- partial clone, missing blob: 2.56 `ls-tree --long` size BAD, cat-file
  `missing`; 2.44.1 both die (could not fetch from promisor remote).
- gitlink: ls-tree --long size `-`.
- cat-file: `flush` without --buffer is fatal; `remote-object-info`
  goes to the remote (2.56).
- --debug flags: skip-worktree 0x40004000, intent-to-add 0x20004000,
  assume-unchanged 0x8000.
- config -z: a key with no value prints `key NUL` (no newline); git
  reads it as true. `--type=bool` turns it and `yes` into `true`.

## wire-server (7568f59 vs ^1)

6876 entries; 6092 blobs, 42 MB per side (union 6204, 44 MB). Warm
cache. The TSVs are from a run at load 3-4; a quieter run (load ~1.3,
not kept) is in parentheses. Same ratios, about 2x faster.

Listing, median ms (`listing.tsv`, hyperfine):

| call | ms |
|---|---|
| ls-tree -r -z | 8.4 (4.8) |
| ls-tree -r -z --long | 14-14.5 (10.2-10.9) |
| ls-tree --long from libs/ | 12 (7.5) |
| ls-tree --long --full-tree from libs/ | 16 (9.4) |
| ls-files -z --stage | 5.9 (3.3) |
| ls-files -z --stage --debug | 9.6 (6.2) |
| ls-files --others --exclude-standard | 14 (11) |
| diff-files (refreshed) | 5.7 (3.1) |

Blobs, head, wall ms (`blobs.tsv`). The requester column is a whole
request (wait plus read), p95:

| pool | 1 worker | 8 workers | start-up | requester p95 |
|---|---|---|---|---|
| 1 | 379 (146) | 501 (270) | 48 (25-33) | 2.7 (1.3) |
| 2 | 442 (165) | 213 (108) | 76 (57) | 0.88 (0.43) |
| 4 | 497 (179) | 112 (60) | 147 (113) | 0.33 (0.15) |

VmHWM about 20-24 MB per cat-file.

Pool verdict: 2 (pool 2 cuts 8-worker wall time by 57% on head, 52% on
both sides; 60% and 50% in the quieter run; the requester rule doesn't
trigger).

Stat check (`worktree.tsv`, ms per run; `stale-stat` is the `cp -a`
copy, `refreshed` the one after `update-index --refresh`):

| step | refreshed | stale-stat |
|---|---|---|
| ls-files --debug, spawned and read in our exe | 41-47 | 36-66 |
| parse | 20-32 | 18-36 |
| lstat of every entry | 20-35 | 25-41 |
| candidates | 0 | 6874 (ctime) |
| ls-files --others + check-attr | 57-61 | 99-142 |
| read candidates | 0 | 83-182 |
| hash and compare | 0 | 42-91 (SHA-1 27-46, SHA-256 29-54) |

These timings predate the leading-directory check and the
O_NOFOLLOW open added in review round 1. A spot check with them (3
runs, load ~2.5, not in a TSV): lstat of every entry 27-50 ms; on the
stale-stat copy read 138-151 ms, hash and compare 75-87 ms.

Three edits: ours = content edit + CRLF file (no `text` attr);
diff-files also lists the touched file.
