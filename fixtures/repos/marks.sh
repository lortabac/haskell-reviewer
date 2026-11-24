#!/bin/sh
# Two commits for the source view's change marks (D-348). Use --diff
# HEAD~1..HEAD.
#
# src/Marks.hs, head lines: 1 removed above (2 lines gone before it),
# 4-5 added, 7-8 modified (replace 1 line), 11 removed below (2 lines
# gone after it). Base lines: 1-2 removed, 5 added below, 7 modified,
# 11-12 removed.
#
# ren/Old.hs -> ren/New.hs: renamed, line 3 edited. added.txt,
# removed.txt, same.txt (unchanged), eol.txt (LF to CRLF only).
#
# Usage: /bin/sh marks.sh DIR [sha1|sha256]
. "$(dirname "$0")/lib.sh"
d=$1
format=${2:-sha1}
repo_init "$d" "$format"
mkdir -p "$d/src" "$d/ren"
cat > "$d/src/Marks.hs" <<'HS'
-- first removed
-- second removed
module Marks where

a = 1
b = 2
c = 3
d = 4
e = 5
f = 6
-- tail one
-- tail two
HS
cat > "$d/ren/Old.hs" <<'HS'
module Ren where

x = 1
y = 2
z = 3
w = 4
v = 5
u = 6
HS
printf 'gone one\ngone two\n' > "$d/removed.txt"
printf 'same\n' > "$d/same.txt"
printf 'one\ntwo\n' > "$d/eol.txt"
g -C "$d" add -A
commit_at "$d" 0 -m base

cat > "$d/src/Marks.hs" <<'HS'
module Marks where

a = 1
inserted1 = 0
inserted2 = 0
b = 2
c = 30
c2 = 31
d = 4
e = 5
f = 6
HS
g -C "$d" mv ren/Old.hs ren/New.hs
cat > "$d/ren/New.hs" <<'HS'
module Ren where

x = 100
y = 2
z = 3
w = 4
v = 5
u = 6
HS
g -C "$d" rm -q removed.txt
printf 'new one\nnew two\n' > "$d/added.txt"
printf 'one\r\ntwo\r\n' > "$d/eol.txt"
g -C "$d" add -A
commit_at "$d" 1 -m head
