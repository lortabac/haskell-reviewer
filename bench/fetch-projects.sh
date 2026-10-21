#!/bin/sh
# Fetch the reference projects listed in bench/projects.txt.
#
# Usage: bench/fetch-projects.sh [--role ROLE | NAME...]
#   no args      small, typical and large
#   --role ROLE  every project with that role (e.g. extra)
#   NAME...      these projects
#
# Each project goes to ${REFERENCE_PROJECTS_DIR:-bench/projects}/<name>,
# as a shallow clone of the pinned commit. A dir already at the right
# commit and clean is left alone; anything else is deleted and fetched
# again. Never fetches submodules, never builds or runs anything there.
# Exits non-zero if any project fails.

set -u
set -f  # no globbing: names are split on spaces only

here=$(cd "$(dirname "$0")" && pwd)
list="$here/projects.txt"
dest=${REFERENCE_PROJECTS_DIR:-$here/projects}

GIT_TERMINAL_PROMPT=0
export GIT_TERMINAL_PROMPT

role=""
names=""
case "${1:-}" in
  --role)
    [ $# -eq 2 ] || { echo "usage: $0 [--role ROLE | NAME...]" >&2; exit 2; }
    role=$2 ;;
  -*)
    echo "usage: $0 [--role ROLE | NAME...]" >&2; exit 2 ;;
  *)
    names="$*" ;;
esac

# Is this line selected?
wanted() {
  if [ -n "$role" ]; then
    [ "$2" = "$role" ]
  elif [ -n "$names" ]; then
    for n in $names; do [ "$n" = "$1" ] && return 0; done
    return 1
  else
    case "$2" in small|typical|large) return 0 ;; *) return 1 ;; esac
  fi
}

fetch() {
  name=$1 url=$2 sha=$3
  dir="$dest/$name"
  if [ -d "$dir" ] \
     && [ "$(git -C "$dir" rev-parse HEAD 2>/dev/null)" = "$sha" ] \
     && [ -z "$(git -C "$dir" status --porcelain 2>/dev/null)" ]; then
    echo "$name: up to date"
    return 0
  fi
  echo "$name: fetching $sha"
  rm -rf "$dir" || return 1
  mkdir -p "$dir" || return 1
  git -C "$dir" init -q || return 1
  git -C "$dir" -c core.hooksPath=/dev/null fetch -q --depth 1 --no-tags \
      --no-recurse-submodules "$url" "$sha" || return 1
  git -C "$dir" -c core.hooksPath=/dev/null -c submodule.recurse=false \
      checkout -q --detach FETCH_HEAD || return 1
  [ "$(git -C "$dir" rev-parse HEAD)" = "$sha" ] || {
    echo "$name: HEAD is not $sha" >&2; return 1; }
}

[ -f "$list" ] || { echo "missing $list" >&2; exit 1; }

failed=0
found=""
# fd 3, so git can't eat the list from stdin
while read -r name prole url sha rest <&3; do
  case "$name" in ''|'#'*) continue ;; esac
  wanted "$name" "$prole" || continue
  found="$found $name"
  if ! printf '%s\n' "$name" | grep -Eq '^[a-z0-9-]+$'; then
    echo "$name: bad name" >&2; failed=1; continue
  fi
  if ! printf '%s\n' "$sha" | grep -Eq '^[0-9a-f]{40}$'; then
    echo "$name: bad sha '$sha'" >&2; failed=1; continue
  fi
  fetch "$name" "$url" "$sha" || { echo "$name: FAILED" >&2; failed=1; }
done 3< "$list"

# Names asked for but not in the list
for n in $names; do
  case " $found " in *" $n "*) ;; *) echo "$n: not in $list" >&2; failed=1 ;; esac
done
[ -n "$found" ] || { echo "nothing selected" >&2; failed=1; }

exit $failed
