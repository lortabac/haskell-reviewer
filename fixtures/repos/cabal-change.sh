#!/bin/sh
# .cabal changes between two commits, for the module tree per side and
# "changed modules first" (D-317). ROOT is the repo's top; diff
# HEAD~1..HEAD.
#
#   added/  base: C.hs is in src/ but not in the .cabal (a loose
#           module). Head lists it.
#   ext/    head adds default-extensions: OverloadedStrings.
#   moved/  head moves src/ to lib/ (hs-source-dirs), same files.
#
# Changed: head added:C and every ext module; base the loose
# added/src/C.hs and every ext module. Nothing in moved/.
#
# Usage: /bin/sh cabal-change.sh DIR [sha1|sha256]
. "$(dirname "$0")/lib.sh"
d=$1
repo_init "$d" "${2:-sha1}"
mkdir -p "$d/added/src" "$d/ext/src" "$d/moved/src"
printf 'packages: added ext moved\n' > "$d/cabal.project"
# cabal_file DIR NAME MODULES SOURCE-DIRS [EXTRA-LINE]
cabal_file() {
  cat > "$d/$1/$1.cabal" <<CABAL
cabal-version: 3.0
name:          $1
version:       0.1

library
  exposed-modules:  $2
  hs-source-dirs:   $3
  build-depends:    base
  default-language: Haskell2010
${4:-}
CABAL
}
module() {
  printf 'module %s where\n\nx :: Int\nx = 1\n' "$2" > "$d/$1/$2.hs"
}
cabal_file added "A B" src
module added/src A
module added/src B
module added/src C
cabal_file ext "X Y" src
module ext/src X
module ext/src Y
cabal_file moved "M N" src
module moved/src M
module moved/src N
g -C "$d" add -A
commit_at "$d" 0 -m base
cabal_file added "A B C" src
cabal_file ext "X Y" src "  default-extensions: OverloadedStrings"
cabal_file moved "M N" lib
g -C "$d" mv moved/src moved/lib
g -C "$d" add -A
commit_at "$d" 1 -m head
