#!/bin/sh
# Run git the way Effects/Git.hs does (Note [Git hardening]).
#
# Usage: hgit.sh DIR ARGS...    run git in DIR
#        hgit.sh --print-flags  print args and env, to diff with `git-sides flags`
#
# Knobs (spike only):
#   GIT        git binary (default: git, looked up in HGIT_PATH)
#   HGIT_PATH  PATH for git (default /usr/bin:/bin)
#   HGIT_STDIN 1: keep stdin (cat-file, check-attr --stdin); else /dev/null
#   HGIT_TRACE file for GIT_TRACE2_EVENT
#   HGIT_ENV   extra K=V words for env (split on spaces)
#   HGIT_WRAP  command words put in front of git (e.g. strace ...)
set -u
: "${HGIT_PATH:=/usr/bin:/bin}"
: "${GIT:=git}"

if [ "${1:-}" = --print-flags ]; then
  for a in --no-pager -c core.fsmonitor=false -c core.untrackedCache=false \
           -c core.hooksPath=/dev/null -c safe.bareRepository=explicit; do
    printf 'arg %s\n' "$a"
  done
  [ -n "${HOME+x}" ] && printf 'env HOME=%s\n' "$HOME"
  [ -n "${XDG_CONFIG_HOME+x}" ] && printf 'env XDG_CONFIG_HOME=%s\n' "$XDG_CONFIG_HOME"
  printf 'env PATH=%s\n' "$HGIT_PATH"
  printf 'env %s\n' LC_ALL=C GIT_OPTIONAL_LOCKS=0 GIT_NO_LAZY_FETCH=1 \
    GIT_NO_REPLACE_OBJECTS=1 GIT_TERMINAL_PROMPT=0
  exit 0
fi

[ $# -ge 1 ] || { echo "usage: $0 DIR ARGS... | --print-flags" >&2; exit 2; }
dir=$1
shift
cd "$dir" || exit 128

# Built back to front with set --, so "$@" keeps git's args intact.
set -- --no-pager -c core.fsmonitor=false -c core.untrackedCache=false \
  -c core.hooksPath=/dev/null -c safe.bareRepository=explicit "$@"
set -- "$GIT" "$@"
if [ -n "${HGIT_WRAP:-}" ]; then
  # shellcheck disable=SC2086
  set -- ${HGIT_WRAP} "$@"
fi
set -- LC_ALL=C GIT_OPTIONAL_LOCKS=0 GIT_NO_LAZY_FETCH=1 \
  GIT_NO_REPLACE_OBJECTS=1 GIT_TERMINAL_PROMPT=0 "$@"
[ -n "${HGIT_TRACE:-}" ] && set -- "GIT_TRACE2_EVENT=$HGIT_TRACE" "$@"
for kv in ${HGIT_ENV:-}; do
  set -- "$kv" "$@"
done
set -- "PATH=$HGIT_PATH" "$@"
[ -n "${XDG_CONFIG_HOME+x}" ] && set -- "XDG_CONFIG_HOME=$XDG_CONFIG_HOME" "$@"
[ -n "${HOME+x}" ] && set -- "HOME=$HOME" "$@"

if [ "${HGIT_STDIN:-0}" = 1 ]; then
  exec env -i "$@"
else
  exec env -i "$@" </dev/null
fi
