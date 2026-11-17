# Shared set-up for the fixture repos. Source it from a script in this
# dir: . "$(dirname "$0")/lib.sh"
#
# git here is the one on PATH. Authors and dates are fixed, and no
# user or system config is read, so a repo is the same everywhere.
set -eu
umask 022

# Tests may run from a git hook, with GIT_DIR and friends set.
for v in $(env | sed -n 's/^\(GIT_[A-Za-z0-9_]*\)=.*/\1/p'); do
  unset "$v"
done

fixture_home=$(mktemp -d)
trap 'rm -rf "$fixture_home"' EXIT
HOME=$fixture_home
GIT_CONFIG_GLOBAL=/dev/null
GIT_CONFIG_NOSYSTEM=1
GIT_AUTHOR_NAME=Test
GIT_AUTHOR_EMAIL=test@example.com
GIT_COMMITTER_NAME=Test
GIT_COMMITTER_EMAIL=test@example.com
export HOME GIT_CONFIG_GLOBAL GIT_CONFIG_NOSYSTEM GIT_AUTHOR_NAME \
  GIT_AUTHOR_EMAIL GIT_COMMITTER_NAME GIT_COMMITTER_EMAIL
unset XDG_CONFIG_HOME 2>/dev/null || true

# Flags for every set-up call: nothing from the repo runs a command.
# No spaces in them: they are split on purpose.
gflags="-c init.defaultBranch=main -c core.hooksPath=/dev/null
  -c core.fsmonitor=false -c commit.gpgSign=false -c core.autocrlf=false
  -c protocol.file.allow=always"

# git with the flags above.
g() {
  # shellcheck disable=SC2086
  git $gflags "$@"
}

# commit_at DIR N ARGS...: commit in DIR at 2026-01-01 plus N minutes.
# env, not VAR=... g: POSIX doesn't say if a shell function passes
# such variables on.
commit_at() {
  dir=$1
  date="$((1767225600 + $2 * 60)) +0000"
  shift 2
  # shellcheck disable=SC2086
  env GIT_AUTHOR_DATE="$date" GIT_COMMITTER_DATE="$date" \
    git $gflags -C "$dir" commit -q "$@"
}

# repo_init DIR [sha1|sha256]
repo_init() {
  g init -q --object-format="${2:-sha1}" "$1"
}
