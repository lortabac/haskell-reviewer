#!/bin/sh
# A tiny cabal project in two commits, for the two sides of a diff.
# ROOT is the repo's top.
#
# HEAD~1 (base): A and B. HEAD (head): A changed, B gone, C added.
#
# Usage: /bin/sh sides.sh DIR [sha1|sha256]
. "$(dirname "$0")/lib.sh"
d=$1
repo_init "$d" "${2:-sha1}"
mkdir -p "$d/src"
cabal_file() {
  cat > "$d/sides.cabal" <<CABAL
cabal-version: 3.0
name:          sides
version:       0.1

library
  exposed-modules:  $1
  hs-source-dirs:   src
  build-depends:    base
  default-language: Haskell2010
CABAL
}
cabal_file "A B"
printf 'module A (a) where\n\nimport B (b)\n\n-- | One.\na :: Int\na = b\n' > "$d/src/A.hs"
printf 'module B (b) where\n\nb :: Int\nb = 1\n' > "$d/src/B.hs"
printf 'sides\n' > "$d/README.md"
g -C "$d" add -A
commit_at "$d" 0 -m base
cabal_file "A C"
printf 'module A (a) where\n\nimport C (c)\n\n-- | Two.\na :: Int\na = c + 1\n' > "$d/src/A.hs"
g -C "$d" rm -q src/B.hs
printf 'module C (c) where\n\nc :: Int\nc = 2\n' > "$d/src/C.hs"
ln -s README.md "$d/readme-link"
g -C "$d" add -A
commit_at "$d" 1 -m head
