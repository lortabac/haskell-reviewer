#!/bin/bash
# Compare the mini CPP (parser-count --cpp-dump) with `cpp -traditional`
# on every CPP module listed in a results JSON. Blank lines and #include
# lines are ignored on both sides. Prints the files that differ.
#
# Usage: ./cpp-check.sh PROJECT_DIR results/<name>.json
set -u
root=$1 json=$2
bin=$(cabal list-bin parser-count)
gv='MIN_VERSION_GLASGOW_HASKELL(a,b,c,d)=((a)<9||((a)==9&&((b)<14||((b)==14&&((c)<1||((c)==1&&(d)<=0)))))))'
same=0 differ=0
for f in $(jq -r '.cpp.modules[].file' "$json"); do
  p="$root/$f"
  mv=$(grep -o 'MIN_VERSION_[A-Za-z0-9_]*' "$p" | sort -u | grep -v GLASGOW \
       | sed 's/^/-D/; s/$/(a,b,c)=1/')
  mine=$("$bin" --cpp-dump "$p" | sed 's/[[:space:]]*$//' | grep -v '^$')
  # shellcheck disable=SC2086
  theirs=$(sed 's/^[[:space:]]*#[[:space:]]*include.*$//' "$p" \
    | cpp -traditional -P -undef -w -D__GLASGOW_HASKELL__=914 \
        -D__GLASGOW_HASKELL_PATCHLEVEL1__=1 -D__GLASGOW_HASKELL_TH__=1 \
        -Dlinux_HOST_OS=1 -Dx86_64_HOST_ARCH=1 -Dlinux_BUILD_OS=1 \
        -Dx86_64_BUILD_ARCH=1 "-D$gv" $mv - 2>/dev/null \
    | sed 's/[[:space:]]*$//' | grep -v '^$')
  if [ "$mine" = "$theirs" ]; then
    same=$((same + 1))
  else
    differ=$((differ + 1))
    echo "differs: $f"
  fi
done
echo "same: $same, differ: $differ"
