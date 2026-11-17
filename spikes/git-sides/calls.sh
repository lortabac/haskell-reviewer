#!/bin/sh
# Exact output of every call and edge case, through hgit.sh.
# Usage: /bin/sh calls.sh. Writes results/calls.txt. GIT picks the git.
set -u
here=$(cd "$(dirname "$0")" && pwd)
HGIT=$here/hgit.sh
REALGIT=$(command -v "${GIT:-git}") || exit 2
GIT=$REALGIT
export GIT
T=$(mktemp -d)
HOME=$T/home
mkdir -p "$HOME"
export HOME
unset XDG_CONFIG_HOME GIT_DIR GIT_WORK_TREE 2>/dev/null
out=$here/results/calls.txt

sg() {
  env GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null HOME="$T/setup-home" \
    GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t GIT_COMMITTER_EMAIL=t@t \
    GIT_AUTHOR_DATE=2026-01-01T00:00:00Z GIT_COMMITTER_DATE=2026-01-01T00:00:00Z \
    "$REALGIT" -c core.hooksPath=/dev/null -c init.defaultBranch=main \
    -c protocol.file.allow=always "$@"
}

# call TITLE DIR [STDIN] -- ARGS...: record stdout (NUL as \0), stderr, exit.
call() {
  title=$1 dir=$2
  shift 2
  input=/dev/null
  if [ "$1" != -- ]; then input=$1; shift; fi
  shift
  HGIT_STDIN=1 timeout -k 2 20 "$HGIT" "$dir" "$@" < "$input" > "$T/o" 2> "$T/e"
  ex=$?
  {
    printf '### %s\n$ git %s   (in %s)\n' "$title" "$*" "$(echo "$dir" | sed "s|$T|\$T|")"
    printf -- '--- stdout\n'
    sed "s|$T|\$T|g" "$T/o" | od -An -c -w64 | sed 's/^ *//' | head -40 > "$T/oc"
    if LC_ALL=C grep -q '[^[:print:][:space:]]' "$T/o" || [ "$(tr -d '\0' < "$T/o" | wc -c)" != "$(wc -c < "$T/o")" ]; then
      cat "$T/oc"
    else
      sed "s|$T|\$T|g" "$T/o" | head -40
    fi
    printf -- '--- stderr\n'
    sed "s|$T|\$T|g" "$T/e" | head -10
    printf -- '--- exit %s\n\n' "$ex"
  } >> "$out"
}

: > "$out"
printf 'git: %s\n\n' "$("$REALGIT" --version)" >> "$out"

# Base repo: history, an annotated tag, a branch named like an option.
R=$T/repo
sg init -q "$R"
mkdir -p "$R/libs/a"
echo one > "$R/top.txt"
echo 'module A where' > "$R/libs/a/A.hs"
printf 'crlf\r\n' > "$R/libs/a/crlf.txt"
sg -C "$R" add -A
sg -C "$R" commit -qm c0
C0=$(sg -C "$R" rev-parse HEAD)
echo two > "$R/top.txt"
sg -C "$R" commit -qam c1
sg -C "$R" tag -a -m tag v1
sg -C "$R" branch -- -dash 2>/dev/null || sg -C "$R" update-ref refs/heads/-dash HEAD
TREE=$(sg -C "$R" rev-parse 'HEAD^{tree}')

call "version" "$R" -- --version
call "inside work tree" "$R" -- rev-parse --is-inside-work-tree
call "prefix, format, shallow, index path (top)" "$R" -- rev-parse --show-prefix --show-object-format --is-shallow-repository --git-path index
call "prefix etc. from ROOT=libs/a" "$R/libs/a" -- rev-parse --show-prefix --show-object-format --is-shallow-repository --git-path index
call "verify HEAD" "$R" -- rev-parse --verify --quiet --end-of-options 'HEAD^{commit}'
call "verify unknown rev" "$R" -- rev-parse --verify --quiet --end-of-options 'nope^{commit}'
call "verify a tree oid as a commit" "$R" -- rev-parse --verify --quiet --end-of-options "$TREE^{commit}"
call "verify an annotated tag (peeled)" "$R" -- rev-parse --verify --quiet --end-of-options 'v1^{commit}'
call "verify tag without ^{commit} (tag object id)" "$R" -- rev-parse --verify --quiet --end-of-options v1
call "verify a rev starting with -, behind --end-of-options" "$R" -- rev-parse --verify --quiet --end-of-options '-dash^{commit}'
call "same rev without --end-of-options (taken as an option)" "$R" -- rev-parse --verify --quiet '-dash^{commit}'
call "a..b to --verify must fail" "$R" -- rev-parse --verify --quiet --end-of-options "$C0..HEAD"
call "a...b to --verify must fail" "$R" -- rev-parse --verify --quiet --end-of-options "$C0...HEAD"

# Orphan branch: no merge base.
sg -C "$R" checkout -q --orphan orphan
sg -C "$R" commit -qm orphan
ORPH=$(sg -C "$R" rev-parse HEAD)
sg -C "$R" checkout -q main
call "merge-base, orphan (none)" "$R" -- merge-base --all "$C0" "$ORPH"
call "merge-base, linear" "$R" -- merge-base --all "$C0" HEAD
call "merge-base --end-of-options?" "$R" -- merge-base --all --end-of-options "$C0" HEAD

# Criss-cross: two merge bases.
X=$T/cross
sg init -q "$X"
echo 0 > "$X/f"; sg -C "$X" add f; sg -C "$X" commit -qm root
sg -C "$X" branch b
echo a > "$X/a"; sg -C "$X" add a; sg -C "$X" commit -qm a1
sg -C "$X" checkout -q b
echo b > "$X/b"; sg -C "$X" add b; sg -C "$X" commit -qm b1
sg -C "$X" checkout -q main
sg -C "$X" merge -q --no-edit b
M1=$(sg -C "$X" rev-parse HEAD)
sg -C "$X" checkout -q b
sg -C "$X" merge -q --no-edit 'main~1'
M2=$(sg -C "$X" rev-parse HEAD)
sg -C "$X" checkout -q main
echo a2 > "$X/a"; sg -C "$X" commit -qam a2
sg -C "$X" checkout -q b
echo b2 > "$X/b"; sg -C "$X" commit -qam b2
sg -C "$X" checkout -q main
call "merge-base --all, criss-cross (2 bases)" "$X" -- merge-base --all main b
call "merge-base (no --all), criss-cross" "$X" -- merge-base main b
printf 'note: the two merges are %s and %s\n\n' "$M1" "$M2" >> "$out"

# Shallow: merge base cut off.
S=$T/shallow
sg clone -q --depth 1 --no-local "file://$X" "$S" 2>/dev/null
sg -C "$S" fetch -q --depth 1 origin b 2>/dev/null
call "shallow: is-shallow" "$S" -- rev-parse --is-shallow-repository
call "shallow: merge-base none" "$S" -- merge-base --all HEAD FETCH_HEAD

# ROOT as a subdir.
call "ls-tree from libs/a (relative)" "$R/libs/a" -- ls-tree -r -z --long HEAD
call "ls-tree from libs/a --full-tree" "$R/libs/a" -- ls-tree -r -z --long --full-tree HEAD
call "ls-tree --full-tree --end-of-options" "$R/libs/a" -- ls-tree -r -z --long --full-tree --end-of-options HEAD
call "ls-files --stage from libs/a" "$R/libs/a" -- ls-files -z --stage
printf 'crlf.txt\0A.hs\0' > "$T/ca.in"
call "check-attr from libs/a (input: crlf.txt A.hs)" "$R/libs/a" "$T/ca.in" -- check-attr -z --stdin text eol crlf filter ident working-tree-encoding

# cat-file --batch-command, default format.
BLOB=$(sg -C "$R" rev-parse HEAD:top.txt)
printf 'info %s\ncontents %s\ninfo %s\ninfo 0000000000000000000000000000000000000001\ncontents HEAD:top.txt\nflush\n' \
  "$BLOB" "$BLOB" "$TREE" > "$T/b.in"
call "cat-file --batch-command (info, contents, tree, missing, a rev, flush without --buffer)" "$R" "$T/b.in" -- cat-file --batch-command
call "cat-file -Z --batch-command" "$R" "$T/b.in" -- cat-file -Z --batch-command

# SHA-256 repo.
H=$T/sha256
sg init -q --object-format=sha256 "$H"
echo hi > "$H/f"
sg -C "$H" add f
sg -C "$H" commit -qm s
call "sha256: object format" "$H" -- rev-parse --show-object-format
call "sha256: ls-files --stage" "$H" -- ls-files -z --stage
call "sha256: empty tree oid" "$H" -- hash-object -t tree /dev/null
call "sha1: empty tree oid" "$R" -- hash-object -t tree /dev/null
printf 'ours (git-sides is the check): blob of "hi\\n" sha256 = %s\n\n' \
  "$(printf 'blob 3\0hi\n' | sha256sum | cut -d' ' -f1)" >> "$out"

# Linked worktree.
sg -C "$R" worktree add -q "$T/wt" -b wt 2>/dev/null
call "linked worktree: --git-path index" "$T/wt" -- rev-parse --git-path index
call "linked worktree: --git-dir --git-common-dir" "$T/wt" -- rev-parse --git-dir --git-common-dir

# Partial clone with a missing blob.
P=$T/pcsrc
sg init -q "$P"
echo v1 > "$P/f"; sg -C "$P" add f; sg -C "$P" commit -qm v1
echo v2 > "$P/f"; sg -C "$P" commit -qam v2
sg -C "$P" config uploadpack.allowFilter true
sg clone -q --filter=blob:none "file://$P" "$T/pc"
OLD=$(sg -C "$P" rev-parse HEAD~1:f)
printf 'info %s\ncontents %s\n' "$OLD" "$OLD" > "$T/pc.in"
call "partial clone: ls-tree --long HEAD~1 (missing blob)" "$T/pc" -- ls-tree -r -z --long HEAD~1
call "partial clone: ls-tree HEAD~1 (no --long)" "$T/pc" -- ls-tree -r -z HEAD~1
call "partial clone: cat-file info/contents of the missing blob" "$T/pc" "$T/pc.in" -- cat-file --batch-command

# Gitlink.
sg -C "$R" submodule add -q "$X" sub
sg -C "$R" commit -qm sub
call "gitlink: ls-tree --long (size -)" "$R" -- ls-tree -z --long HEAD sub
call "gitlink: ls-files --stage --debug" "$R" -- ls-files -z --stage --debug sub

# Odd names.
N=$T/names
sg init -q "$N"
printf 'x\n' > "$N/$(printf 'caf\351')"
printf 'y\n' > "$N/$(printf 'new\nline')"
sg -C "$N" add -A
sg -C "$N" commit -qm n
call "non-UTF-8 and newline names: ls-files -z" "$N" -- ls-files -z --stage
call "non-UTF-8 and newline names: ls-tree -z" "$N" -- ls-tree -r -z HEAD
call "same without -z (quoted)" "$N" -- ls-tree -r HEAD

# config.
call "config: key unset" "$R" -- config --get core.autocrlf
sg -C "$R" config core.autocrlf input
sg -C "$R" config --add core.autocrlf true
call "config: key set twice (--get)" "$R" -- config --get core.autocrlf
call "config: key set twice (-z --get-regexp)" "$R" -- config -z --get-regexp '^core\.(autocrlf|filemode|symlinks|ignorecase|trustctime|checkstat)$'

# A key with no value (git reads it as true), and autocrlf=yes.
sg -C "$R" config --unset-all core.autocrlf
printf '[core]\n\tfilemode\n\tautocrlf = yes\n' >> "$R/.git/config"
call "config -z: value-less filemode (prints key NUL, no newline), autocrlf=yes" "$R" -- config -z --get-regexp '^core\.(autocrlf|filemode|symlinks|ignorecase|trustctime|checkstat)$'
call "config --type=bool for the same keys" "$R" -- config -z --type=bool --get-regexp '^core\.(autocrlf|filemode)$'
sg -C "$R" config --unset-all core.autocrlf
sg -C "$R" config --unset-all core.filemode
sg -C "$R" config core.filemode true

# ls-files flags: skip-worktree and intent-to-add.
echo ita > "$R/ita.txt"
sg -C "$R" add -N ita.txt
sg -C "$R" update-index --skip-worktree top.txt
sg -C "$R" update-index --assume-unchanged libs/a/A.hs
call "ls-files --debug flags: skip-worktree, intent-to-add, assume-unchanged" "$R" -- ls-files -z --stage --debug top.txt ita.txt libs/a/A.hs

# remote-object-info exists in 2.56: we never send it.
printf 'remote-object-info origin %s\n' "$OLD" > "$T/roi.in"
call "cat-file --batch-command: remote-object-info (never sent by us)" "$T/pc" "$T/roi.in" -- cat-file --batch-command

echo "wrote $out (tmp $T)"
