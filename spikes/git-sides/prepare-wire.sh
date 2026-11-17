#!/bin/sh
# Copy bench/projects/wire-server (never run git there) to $BUILD and
# deepen the copy by one level, so base = 7568f59^1 is there.
# $BUILD/wire-server: plain cp -a (every entry stat-dirty).
# $BUILD/wire-server-refreshed: plus `update-index --refresh` once.
set -u
here=$(cd "$(dirname "$0")" && pwd)
common=$(git -C "$here" rev-parse --path-format=absolute --git-common-dir) || exit 1
main=$(dirname "$common")
SRC=${WIRE_SRC:-$main/bench/projects/wire-server}
BUILD=${BUILD:-$main/.worktrees/.build/t01-git-sides}
PIN=7568f595e42ece6f0791682336014853a9dfcac4
URL=https://github.com/wireapp/wire-server
sg() { git -c core.hooksPath=/dev/null -c core.fsmonitor=false "$@"; }

[ "$(cat "$SRC/.git/HEAD")" = "$PIN" ] || { echo "$SRC is not at $PIN" >&2; exit 1; }
mkdir -p "$BUILD"
stamp=$BUILD/wire.stamp
touch "$stamp"
sleep 1
rm -rf "$BUILD/wire-server" "$BUILD/wire-server-refreshed"
cp -a "$SRC" "$BUILD/wire-server" || exit 1
sg -C "$BUILD/wire-server" fetch -q --depth 2 --no-tags --no-recurse-submodules "$URL" "$PIN" || {
  echo "fetch failed" >&2; exit 1; }
sg -C "$BUILD/wire-server" rev-parse --verify --quiet "$PIN^1^{commit}" >/dev/null || {
  echo "no base commit after the fetch" >&2; exit 1; }
cp -a "$BUILD/wire-server" "$BUILD/wire-server-refreshed" || exit 1
sg -C "$BUILD/wire-server-refreshed" update-index -q --refresh
# The source must be untouched.
changed=$(find "$SRC" -newer "$stamp" | head -5)
if [ -n "$changed" ]; then
  echo "source changed: $changed" >&2
  exit 1
fi
echo "source untouched; HEAD $(cat "$SRC/.git/HEAD")"
echo "base $(sg -C "$BUILD/wire-server" rev-parse "$PIN^1")"
