#!/bin/sh
# Work trees with one file per case of the working-tree side's check
# (D-311, D-318). No traps here: planted.sh has those.
#
# Usage: /bin/sh worktree.sh DIR [sha1|sha256]
#
# Makes, in DIR:
#   w/    most cases; a merge in progress (conflict.txt)
#   ac/   core.autocrlf=true: f.txt touched, g.txt edited
#   lit/  only a content check can tell, and we can't make it: literal
#         attribute values (text=set; text=unset with crlf), ident with
#         an expanded keyword, a real filter (filter.up), all touched;
#         a size change with the same clean bytes; filter=unset with a
#         driver named unset
#   sl/   core.symlinks=false: link is a plain file holding its target
#   fm/   core.filemode=false: x.sh made executable
#   si/   a split index
#   sd/   ROOT is sd/pkg
#   bn/   binary and -crlf files with CRLF bytes, touched: clean
#   bnc/  a fresh clone of bn/ (racy entries): clean
# In lit/ to sd/ the files are old (2025) and the index is written
# after, so most entries are clean by stat alone.
. "$(dirname "$0")/lib.sh"
T=$1
fmt=${2:-sha1}
mkdir -p "$T"
w=$T/w
repo_init "$w" "$fmt"

printf '*.t text\n*.e eol=crlf\n*.bin filter=lfs -text\n*.id ident\n' \
  > "$w/.gitattributes"
printf '*.log\n' > "$w/.gitignore"
# git add makes LF of these in the index; the disk keeps CRLF.
printf 'a\r\nb\r\n' > "$w/crlf.t"
printf 'a\r\nb\r\n' > "$w/eol.e"
# No attribute yet: the index keeps CRLF.
printf 'x\r\ny\r\n' > "$w/crlf-auto.a"
pointer='version https://git-lfs.github.com/spec/v1
oid sha256:4d7a214614ab2935c943f9e0ff69d22eadbb8f32b1258daaa5e2ca24d17e2393
size 12'
printf '%s\n' "$pointer" > "$w/lfs.bin"
printf '%s\n' "$pointer" > "$w/lfs-dirty.bin"
printf '$Id$\nx\n' > "$w/ident.id"
for f in plain deleted staged skip fifo racy; do
  printf 'module M where\n-- %s\n' "$f" > "$w/$f.hs"
done
printf 'echo hi\n' > "$w/mode.sh"
mkdir "$w/dir"
printf 'module A where\n' > "$w/dir/a.hs"
printf 'base\n' > "$w/conflict.txt"
ln -s plain.hs "$w/link"
ln -s plain.hs "$w/link2"
g -C "$w" add -A
commit_at "$w" 0 -m c0

printf '*.a text=auto\n' >> "$w/.gitattributes"
# Looks binary to git (many control bytes), with CRLF: not converted.
printf '\001\002\003\004\005\006\007\001\002\003\r\n' > "$w/bin.a"
g -C "$w" add .gitattributes bin.a
commit_at "$w" 1 -m c1

# A conflict on conflict.txt.
g -C "$w" checkout -q -b other
printf 'theirs\n' > "$w/conflict.txt"
g -C "$w" add conflict.txt
commit_at "$w" 2 -m other
g -C "$w" checkout -q main
printf 'ours\n' > "$w/conflict.txt"
g -C "$w" add conflict.txt
commit_at "$w" 3 -m ours
git_at 4 -C "$w" merge -q other > /dev/null 2>&1 || true

# Changes. No add -A from here: it would resolve the conflict.
printf 'big data\n' > "$w/lfs-dirty.bin"
printf '$Id$\nx\ny\n' > "$w/ident.id"
printf 'x\r\ny\r\nz\r\n' > "$w/crlf-auto.a"
printf '\001\002\003\004\005\006\007\001\002\003\r\nmore\r\n' > "$w/bin.a"
printf 'module New where\n' > "$w/new.hs"
printf 'log\n' > "$w/ignored.log"
printf 'module M where\n-- staged, changed\n' > "$w/staged.hs"
g -C "$w" add staged.hs
rm "$w/deleted.hs"
mv "$w/dir" "$w/dir2"
ln -s dir2 "$w/dir"
chmod +x "$w/mode.sh"
printf 'module Ita where\n' > "$w/ita.hs"
g -C "$w" add -N ita.hs
g -C "$w" update-index --skip-worktree skip.hs
rm "$w/skip.hs"
commit=$(g -C "$w" rev-parse HEAD)
g -C "$w" update-index --add --cacheinfo "160000,$commit,sub"
mkdir "$w/sub"
g -C "$w" update-index --add --cacheinfo "160000,$commit,gone-sub"
rm "$w/fifo.hs"
mkfifo "$w/fifo.hs"
rm "$w/link2"
ln -s dir2/a.hs "$w/link2"

# plain.hs: old enough to be clean by stat alone.
touch -d 2025-01-01T00:00:00 "$w/plain.hs"
g -C "$w" update-index -q --refresh > /dev/null 2>&1 || true
# Only touched: the stat differs, the content doesn't.
touch -d 2025-06-01T00:00:00 "$w/crlf.t" "$w/eol.e" "$w/lfs.bin"
# Racy: the index says its mtime is in the future.
touch -d 2100-01-01T00:00:00 "$w/racy.hs"
g -C "$w" add racy.hs

# core.autocrlf=true. lib.sh's flags set it false: set it again last.
ac=$T/ac
repo_init "$ac" "$fmt"
g -C "$ac" config core.autocrlf true
printf 'a\r\nb\r\n' > "$ac/f.txt"
printf 'a\r\nb\r\n' > "$ac/g.txt"
g -c core.autocrlf=true -C "$ac" add -A
commit_at "$ac" 0 -m c0
touch -d 2025-06-01T00:00:00 "$ac/f.txt"
printf 'a\r\nb\r\nc\r\n' > "$ac/g.txt"

# old DIR: set every file's mtime to 2025, then let git write the index
# with that stat data.
old() {
  find "$1" -path "$1/.git" -prune -o -type f -exec touch -d 2025-01-01T00:00:00 {} +
  g -C "$1" update-index -q --refresh > /dev/null 2>&1 || true
}

l=$T/lit
repo_init "$l" "$fmt"
printf '*.txt text\n' > "$l/.gitattributes"
# Under text: LF in the index, CRLF on disk.
printf 'a\r\nb\r\n' > "$l/set.txt"
# No attribute yet: the index keeps CRLF.
printf 'x\r\ny\r\n' > "$l/u.dat"
# No ident yet: the index keeps the expanded keyword.
printf '$Id: deadbeef $\nx\n' > "$l/i.c"
printf 'hello\n' > "$l/f.u"
printf 'a\nb\n' > "$l/sz.e"
printf 'module P where\n' > "$l/plain.hs"
printf 'hello\n' > "$l/x.fu"
g -C "$l" add -A
commit_at "$l" 0 -m c0
old "$l"
printf '*.txt text=set\nu.dat text=unset crlf\ni.c ident\n*.u filter=up\nsz.e eol=crlf\n' \
  > "$l/.gitattributes"
printf '*.fu filter=unset\n' >> "$l/.gitattributes"
g -C "$l" config filter.up.clean 'tr a-z A-Z'
g -C "$l" config filter.unset.clean 'tr a-z A-Z'
touch -d 2025-06-01T00:00:00 "$l/set.txt" "$l/u.dat" "$l/i.c" "$l/f.u" "$l/x.fu"
# A new size; the clean bytes are the same.
printf 'a\r\nb\r\n' > "$l/sz.e"

s=$T/sl
repo_init "$s" "$fmt"
printf 't\n' > "$s/target.txt"
ln -s target.txt "$s/link"
ln -s target.txt "$s/link2"
g -C "$s" add -A
commit_at "$s" 0 -m c0
g -C "$s" config core.symlinks false
rm "$s/link" "$s/link2"
printf 'target.txt' > "$s/link"
printf 'elsewhere' > "$s/link2"
old "$s"

m=$T/fm
repo_init "$m" "$fmt"
printf 'echo x\n' > "$m/x.sh"
printf 'echo y\n' > "$m/y.sh"
g -C "$m" add -A
commit_at "$m" 0 -m c0
g -C "$m" config core.filemode false
old "$m"
chmod +x "$m/x.sh"
printf 'echo y2\n' > "$m/y.sh"

i=$T/si
repo_init "$i" "$fmt"
for f in a b c; do printf '%s\n' "$f" > "$i/$f.hs"; done
g -C "$i" add -A
commit_at "$i" 0 -m c0
g -C "$i" update-index --split-index
old "$i"
printf 'b2\n' > "$i/b.hs"

d=$T/sd
repo_init "$d" "$fmt"
mkdir "$d/pkg"
printf 'top\n' > "$d/top.hs"
printf 'a\n' > "$d/pkg/a.hs"
printf 'b\n' > "$d/pkg/b.hs"
g -C "$d" add -A
commit_at "$d" 0 -m c0
old "$d"
printf 'b2\n' > "$d/pkg/b.hs"
printf 'top2\n' > "$d/top.hs"

b=$T/bn
repo_init "$b" "$fmt"
printf '*.png binary\n*.bat -crlf\n' > "$b/.gitattributes"
# A PNG signature has CRLF in it.
printf '\211PNG\r\n\032\n\000\000\000\rIHDR' > "$b/a.png"
printf 'echo\r\n' > "$b/a.bat"
g -C "$b" add -A
commit_at "$b" 0 -m c0
g clone -q "$b" "$T/bnc"
old "$b"
touch -d 2025-06-01T00:00:00 "$b/a.png" "$b/a.bat"
