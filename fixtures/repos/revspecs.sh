#!/bin/sh
# Branches, tags and merges for --diff (D-316). The project is in pkg/,
# so ROOT can be DIR/pkg; README is outside it.
#
#   main:     c0 - c1 - c2 ------- m (merges feature at f2)
#                    \             /
#   feature:          f1 - f2 ---- f3
#   lonely:   l0 (orphan: no merge base with main)
#   cross-a:  c0 - a1 - a2 (merges b1)
#   cross-b:  c0 - b1 - b2 (merges a1): two merge bases, a1 and b1
#
# Tags: v1 (annotated) on c1, light on c0, blobtag on a blob, a1, b1.
# c1 also changes README (outside ROOT).
# main...feature has merge base f2. Each commit adds one module to pkg/
# (c0: A, c1: B, c2: C, f1: F, f2: G, f3: H).
#
# Ends on main, clean.
#
# Usage: /bin/sh revspecs.sh DIR [sha1|sha256]
. "$(dirname "$0")/lib.sh"
d=$1
repo_init "$d" "${2:-sha1}"
mkdir -p "$d/pkg/src"
cat > "$d/pkg/pkg.cabal" <<'CABAL'
cabal-version: 3.0
name:          pkg
version:       0.1

library
  exposed-modules:  A
  other-modules:    B C F G H
  hs-source-dirs:   src
  build-depends:    base
  default-language: Haskell2010
CABAL
add() {
  printf 'module %s where\n\nx :: Int\nx = 1\n' "$1" > "$d/pkg/src/$1.hs"
  g -C "$d" add -A
  commit_at "$d" "$2" -m "$1"
}
printf 'pkg\n' > "$d/README"
add A 0
g -C "$d" tag light
printf 'pkg, more\n' > "$d/README"
add B 1
git_at 1 -C "$d" tag -a -m v1 v1
g -C "$d" checkout -q -b feature
add F 2
add G 3
g -C "$d" checkout -q main
add C 4
git_at 5 -C "$d" merge -q --no-ff -m merge feature
g -C "$d" checkout -q feature
add H 6

g -C "$d" checkout -q --orphan lonely
g -C "$d" rm -q -rf .
printf 'lonely\n' > "$d/README"
g -C "$d" add -A
commit_at "$d" 7 -m lonely

g -C "$d" checkout -q -f main
blob=$(printf 'a blob\n' | g -C "$d" hash-object -w --stdin)
g -C "$d" tag blobtag "$blob"

g -C "$d" checkout -q -b cross-a light
printf 'a\n' > "$d/a.txt"
g -C "$d" add -A
commit_at "$d" 8 -m a1
g -C "$d" checkout -q -b cross-b light
printf 'b\n' > "$d/b.txt"
g -C "$d" add -A
commit_at "$d" 9 -m b1
g -C "$d" tag b1
g -C "$d" checkout -q cross-a
g -C "$d" tag a1
git_at 10 -C "$d" merge -q --no-ff -m a2 b1
g -C "$d" checkout -q cross-b
git_at 11 -C "$d" merge -q --no-ff -m b2 a1
g -C "$d" checkout -q main
