#!/bin/sh
# A cabal project in two commits for the diff view (D-341). Use
# --diff HEAD~1..HEAD. ROOT is the repo's top.
#
# src/Calc.hs: lines 3, 5, 7 and 91 change, with a long equal run
# between (gaps). Removed lines have links: "xs" (a local) on 7,
# "double" (src/Util.hs) on 91. src/Util.hs is unchanged. big.txt:
# 20,000 lines, all changed (under 1 MiB a side).
#
# Kinds, for the notes: data.bin (binary), huge.txt (too large on
# head), sub (gitlink), link (symlink target), kind.txt (file to
# symlink), crlf.txt (CRLF on head), bom.txt (BOM added), ws.txt
# (whitespace only), added.txt, removed.txt.
#
# Usage: /bin/sh diffview.sh DIR [sha1|sha256]
. "$(dirname "$0")/lib.sh"
d=$1
format=${2:-sha1}
repo_init "$d" "$format"
mkdir -p "$d/src"
cat > "$d/diffview.cabal" <<'CABAL'
cabal-version: 3.0
name:          diffview
version:       0.1

library
  exposed-modules:  Calc Util
  hs-source-dirs:   src
  build-depends:    base
  default-language: Haskell2010
CABAL
cat > "$d/src/Util.hs" <<'HS'
module Util (double, triple) where

double :: Int -> Int
double n = n * 2

triple :: Int -> Int
triple n = n * 3
HS
# calc IMPORTS DOC TOTAL SCALE
calc() {
  {
    printf 'module Calc (total, scale) where\n\nimport Util (%s)\n\n' "$1"
    printf -- '-- | %s\ntotal :: [Int] -> Int\ntotal xs = %s\n\n' "$2" "$3"
    awk 'BEGIN { for (i = 1; i <= 40; i++) printf "f%02d :: Int\nf%02d = %d\n", i, i, i }'
    printf '\nscale :: Int -> Int\nscale n = %s n\n' "$4"
  } > "$d/src/Calc.hs"
}
calc double 'Sum of all.' 'sum xs' double
awk 'BEGIN { for (i = 1; i <= 20000; i++) printf "base line %05d\n", i }' > "$d/big.txt"
printf 'a\000b' > "$d/data.bin"
printf 'small\n' > "$d/huge.txt"
ln -s src/Util.hs "$d/link"
printf 'a file\n' > "$d/kind.txt"
printf 'one\ntwo\n' > "$d/crlf.txt"
printf 'text\n' > "$d/bom.txt"
printf 'x = 1\n' > "$d/ws.txt"
printf 'gone\n' > "$d/removed.txt"
g -C "$d" add -A
case $format in
  sha256)
    sub1=1111111111111111111111111111111111111111111111111111111111111111
    sub2=2222222222222222222222222222222222222222222222222222222222222222
    ;;
  *)
    sub1=1111111111111111111111111111111111111111
    sub2=2222222222222222222222222222222222222222
    ;;
esac
printf '160000 %s\tsub\000' "$sub1" | g -C "$d" update-index -z --index-info
commit_at "$d" 0 -m base

calc 'double, triple' 'Sum of all, doubled.' 'sum (map double xs)' triple
awk 'BEGIN { for (i = 1; i <= 20000; i++) printf "head line %05d\n", i }' > "$d/big.txt"
printf 'a\000bc' > "$d/data.bin"
# 40000 lines of 30 bytes: over 1 MiB.
awk 'BEGIN { for (i = 0; i < 40000; i++) printf "line %05d of a big text file\n", i }' > "$d/huge.txt"
rm "$d/link"
ln -s src/Calc.hs "$d/link"
rm "$d/kind.txt"
ln -s src/Util.hs "$d/kind.txt"
printf 'one\r\ntwo\r\n' > "$d/crlf.txt"
printf '\357\273\277text\n' > "$d/bom.txt"
printf 'x  =  1\n' > "$d/ws.txt"
g -C "$d" rm -q removed.txt
printf 'new\n' > "$d/added.txt"
g -C "$d" add -A
printf '160000 %s\tsub\000' "$sub2" | g -C "$d" update-index -z --index-info
commit_at "$d" 1 -m head
