#!/bin/sh
# A tiny cabal project in one commit, then changed in the working tree,
# for --diff REV (head is the working tree). ROOT is the repo's top.
#
# HEAD: A, B, S, data.bin (an LFS pointer, filter=lfs).
# Working tree: A edited; S changed and staged (disk = index); New.hs
# untracked; data.bin edited (filtered); B as committed.
#
# Usage: /bin/sh edits.sh DIR [sha1|sha256]
. "$(dirname "$0")/lib.sh"
d=$1
repo_init "$d" "${2:-sha1}"
mkdir -p "$d/src"
cat > "$d/edits.cabal" <<'CABAL'
cabal-version: 3.0
name:          edits
version:       0.1

library
  exposed-modules:  A B S
  hs-source-dirs:   src
  build-depends:    base
  default-language: Haskell2010
CABAL
printf '*.bin filter=lfs -text\n' > "$d/.gitattributes"
printf 'module A (a) where\n\na :: Int\na = 1\n' > "$d/src/A.hs"
printf 'module B (b) where\n\nb :: Int\nb = 1\n' > "$d/src/B.hs"
printf 'module S (s) where\n\ns :: Int\ns = 1\n' > "$d/src/S.hs"
printf 'version https://git-lfs.github.com/spec/v1
oid sha256:4d7a214614ab2935c943f9e0ff69d22eadbb8f32b1258daaa5e2ca24d17e2393
size 12\n' > "$d/data.bin"
g -C "$d" add -A
commit_at "$d" 0 -m base
printf 'module A (a) where\n\na :: Int\na = 2\n' > "$d/src/A.hs"
printf 'module S (s) where\n\ns :: Int\ns = 2\n' > "$d/src/S.hs"
g -C "$d" add src/S.hs
printf 'module New where\n' > "$d/src/New.hs"
printf 'big data\n' > "$d/data.bin"
