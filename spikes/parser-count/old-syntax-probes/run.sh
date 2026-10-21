#!/bin/sh
# Q-OLD-SYNTAX checklist: compile each probe (Haskell2010, no code) with
# each GHC and print the first error code, or "ok".
# Usage: ./run.sh [GHC...]   (default: ghc-9.10 ghc-9.12 ghc-9.14)
cd "$(dirname "$0")" || exit 1
[ $# -gt 0 ] || set -- ghc-9.10 ghc-9.12 ghc-9.14
out=$(mktemp -d) || exit 1
printf '%-14s' probe; for g in "$@"; do printf '%-12s' "$g"; done; echo
for f in *.hs; do
  printf '%-14s' "${f%.hs}"
  for g in "$@"; do
    code=$("$g" -XHaskell2010 -fno-code -outputdir "$out/$g" "$f" 2>&1 \
           | grep -o 'error: \[GHC-[0-9]*\]' | head -1 | grep -o 'GHC-[0-9]*')
    printf '%-12s' "${code:-ok}"
  done
  echo
done
rm -rf "$out"
