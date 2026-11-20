#!/bin/sh
# File pairing between two commits (D-324): every rename rule, for the
# oracle (git diff -M) and for T08b's api/diff-files. Diff HEAD~1..HEAD.
# All cases are under pkg/; other/ is outside ROOT when ROOT is pkg/.
# Lines are 10 bytes, so a kept line is 10%.
#
#   exact/a.txt -> exact/b.txt        R100
#   sim60/old.txt -> sim60/new.txt    6 of 10 lines kept: R060
#   sim40/old.txt, sim40/new.txt      4 of 10 kept: D and A
#   copy/dup.txt                      same as unchanged copy/orig.txt: A
#   d1/Same.hs -> d2/Same.hs          8 of 10 kept: R080 (basename step)
#   tie/one/T.txt, tie/two/U.txt      same blob, removed; tie/new/U.txt
#                                     added: R100 from tie/two/U.txt
#   mt/s1.txt, mt/s2.txt -> mt/dst.txt  both 60%: R060 from s1 (slot order)
#   ln/a -> ln/b                      symlink, same target: R100
#   ln/c, ln/d                        symlink, other target: D and A
#   sub/m1 -> sub/m2                  gitlink, same commit: R100
#   empty/a -> empty/b                empty file: R100
#   tc/f                              file -> symlink: T
#   mode/x.sh                         chmod +x only: M
#   lim/r1..r3 -> lim/q1..q3          80% each, distinct basenames: R080,
#                                     but none with a rename limit of 2
#   other/x (edited), other/new (added), other/moved.hs -> pkg/moved.hs
#
# With "worktree", also leaves the working tree dirty: pkg/wt/a.txt
# moved to pkg/wt/b.txt (not added), pkg/exact/b.txt edited,
# pkg/ita.txt added with -N, pkg/copy/orig.txt made executable.
#
# Usage: /bin/sh renames.sh DIR [sha1|sha256] [worktree]
. "$(dirname "$0")/lib.sh"
d=$1
repo_init "$d" "${2:-sha1}"
p=$d/pkg
# lines PREFIX FROM TO: 10-byte lines PREFIX-NNNN (PREFIX is 4 chars).
lines() {
  i=$2
  while [ "$i" -le "$3" ]; do
    printf '%s-%04d\n' "$1" "$i"
    i=$((i + 1))
  done
}
mkdir -p "$p/exact" "$p/sim60" "$p/sim40" "$p/copy" "$p/d1" "$p/tie/one" \
  "$p/tie/two" "$p/mt" "$p/ln" "$p/empty" "$p/tc" "$p/mode" "$p/lim" \
  "$p/wt" "$p/sub/m1" "$d/other"
lines exac 1 10 > "$p/exact/a.txt"
lines s60a 1 10 > "$p/sim60/old.txt"
lines s40a 1 10 > "$p/sim40/old.txt"
lines copy 1 10 > "$p/copy/orig.txt"
lines same 1 10 > "$p/d1/Same.hs"
printf 'tie\n' > "$p/tie/one/T.txt"
printf 'tie\n' > "$p/tie/two/U.txt"
{ lines mt00 1 6; lines mts1 7 10; } > "$p/mt/s1.txt"
{ lines mts2 1 4; lines mt00 5 10; } > "$p/mt/s2.txt"
ln -s target1 "$p/ln/a"
ln -s target-x "$p/ln/c"
: > "$p/empty/a"
lines tcff 1 3 > "$p/tc/f"
printf 'echo hi\n' > "$p/mode/x.sh"
for i in 1 2 3; do lines "lim$i" 1 10 > "$p/lim/r$i.txt"; done
lines wtaa 1 10 > "$p/wt/a.txt"
lines othx 1 10 > "$d/other/x"
lines othm 1 10 > "$d/other/moved.hs"
g -C "$d" add -A
sub=1234567890abcdef1234567890abcdef12345678
if [ "${2:-sha1}" = sha256 ]; then
  sub=1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
fi
g -C "$d" update-index --add --cacheinfo "160000,$sub,pkg/sub/m1"
commit_at "$d" 0 -m base

g -C "$d" mv pkg/exact/a.txt pkg/exact/b.txt
rm "$p/sim60/old.txt"
{ lines s60a 1 6; lines s60b 7 10; } > "$p/sim60/new.txt"
rm "$p/sim40/old.txt"
{ lines s40a 1 4; lines s40b 5 10; } > "$p/sim40/new.txt"
cp "$p/copy/orig.txt" "$p/copy/dup.txt"
mkdir -p "$p/d2"
rm "$p/d1/Same.hs"
{ lines same 1 8; lines samf 9 10; } > "$p/d2/Same.hs"
rm -r "$p/tie/one" "$p/tie/two"
mkdir -p "$p/tie/new"
printf 'tie\n' > "$p/tie/new/U.txt"
rm "$p/mt/s1.txt" "$p/mt/s2.txt"
lines mt00 1 10 > "$p/mt/dst.txt"
g -C "$d" mv pkg/ln/a pkg/ln/b
rm "$p/ln/c"
ln -s target-y "$p/ln/d"
g -C "$d" mv pkg/empty/a pkg/empty/b
rm "$p/tc/f"
ln -s elsewhere "$p/tc/f"
chmod +x "$p/mode/x.sh"
for i in 1 2 3; do
  rm "$p/lim/r$i.txt"
  { lines "lim$i" 1 8; lines "liq$i" 9 10; } > "$p/lim/q$i.txt"
done
{ lines othx 1 9; lines othy 10 10; } > "$d/other/x"
lines othn 1 3 > "$d/other/new"
g -C "$d" mv other/moved.hs pkg/moved.hs
g -C "$d" add -A
mv "$p/sub/m1" "$p/sub/m2"
g -C "$d" update-index --force-remove pkg/sub/m1
g -C "$d" update-index --add --cacheinfo "160000,$sub,pkg/sub/m2"
commit_at "$d" 1 -m head

if [ "${3:-}" = worktree ]; then
  mv "$p/wt/a.txt" "$p/wt/b.txt"
  printf 'edited\n' >> "$p/exact/b.txt"
  lines itaa 1 3 > "$p/ita.txt"
  g -C "$d" add -N pkg/ita.txt
  chmod +x "$p/copy/orig.txt"
fi
