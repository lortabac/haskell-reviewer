#!/bin/sh
# The planted repo: traps that write a marker file when git runs
# something. Every safe call must fire none, start no child and leave
# .git as it was. Positive controls must fire, or the trap is broken.
#
# Usage: /bin/sh planted.sh [GIT_SIDES_BIN]
# Writes results/planted.tsv and results/worktree-check.tsv. Exit 1 on
# any FAIL. GIT=/path/to/git picks another git.
set -u
here=$(cd "$(dirname "$0")" && pwd)
HGIT=$here/hgit.sh
SIDES=${1:-}
REALGIT=$(command -v "${GIT:-git}") || { echo "no git" >&2; exit 2; }
GIT=$REALGIT
export GIT
T=$(mktemp -d)
M=$T/markers
mkdir -p "$M" "$T/bin" "$T/hooks" "$T/home" "$T/setup-home"
out=$here/results/planted.tsv
wcheck=$here/results/worktree-check.tsv
HAVE_STRACE=0
command -v strace >/dev/null 2>&1 && HAVE_STRACE=1

# hgit.sh passes HOME on, so $T/home/.gitconfig is the "user config".
HOME=$T/home
HGIT_PATH=$T/bin:/usr/bin:/bin
export HOME HGIT_PATH
unset XDG_CONFIG_HOME GIT_DIR GIT_WORK_TREE GIT_CONFIG_PARAMETERS GIT_CONFIG_COUNT 2>/dev/null

# Setup git: fixed identity and dates, no system or global config.
sg() {
  env GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null HOME="$T/setup-home" \
    GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t GIT_COMMITTER_EMAIL=t@t \
    GIT_AUTHOR_DATE=2026-01-01T00:00:00Z GIT_COMMITTER_DATE=2026-01-01T00:00:00Z \
    "$REALGIT" -c core.hooksPath=/dev/null -c core.fsmonitor=false \
    -c init.defaultBranch=main -c protocol.file.allow=always "$@"
}

# --- trap programs ---------------------------------------------------------
cat > "$T/bin/mark" <<EOF
#!/bin/sh
: > "$M/\$1"
echo "\$1 \$\$" >> "$T/markers.log"
EOF
cat > "$T/bin/fsmon" <<EOF
#!/bin/sh
"$T/bin/mark" fsmonitor-hook
exit 1
EOF
# Fake git-lfs: marks, cats stdin for clean/smudge, fails filter-process.
cat > "$T/bin/git-lfs" <<EOF
#!/bin/sh
"$T/bin/mark" "lfs-\$1"
case "\$1" in
  clean|smudge) cat ;;
  *) exit 1 ;;
esac
EOF
chmod +x "$T/bin/mark" "$T/bin/fsmon" "$T/bin/git-lfs"

hooks="applypatch-msg pre-applypatch post-applypatch pre-commit pre-merge-commit
prepare-commit-msg commit-msg post-commit pre-rebase post-checkout post-merge
pre-push pre-receive update proc-receive post-receive post-update
reference-transaction push-to-checkout pre-auto-gc post-rewrite
sendemail-validate fsmonitor-watchman p4-changelist p4-prepare-changelist
p4-post-changelist p4-pre-submit post-index-change"
for h in $hooks; do
  printf '#!/bin/sh\n"%s" hook-%s\n' "$T/bin/mark" "$h" > "$T/hooks/$h"
  chmod +x "$T/hooks/$h"
done

# What `git lfs install` writes, in the user config.
cat > "$T/home/.gitconfig" <<'EOF'
[filter "lfs"]
	clean = git-lfs clean -- %f
	smudge = git-lfs smudge -- %f
	process = git-lfs filter-process
	required = true
EOF
printf '*.ca filter=x\n' > "$T/attrs"

# --- repos -----------------------------------------------------------------
# Submodule source.
sg init -q "$T/subsrc"
printf '*.x filter=x\n' > "$T/subsrc/.gitattributes"
echo subx > "$T/subsrc/a.x"
echo subplain > "$T/subsrc/p.txt"
sg -C "$T/subsrc" add -A
sg -C "$T/subsrc" commit -qm s0

lfs_pointer='version https://git-lfs.github.com/spec/v1
oid sha256:4d7a214614ab2935c943f9e0ff69d22eadbb8f32b1258daaa5e2ca24d17e2393
size 12'

future() { touch -d 2100-01-01T00:00:00 "$@"; }

# mkrepo DIR STATE: same files and history; the index state differs.
mkrepo() {
  d=$1 state=$2
  sg init -q "$d"
  printf '*.x filter=x\n*.cl filter=cl\n*.bin filter=lfs diff=lfs merge=lfs -text\n*.txt diff=x\n' > "$d/.gitattributes"
  echo xdata > "$d/a.x"
  echo cldata > "$d/a.cl"
  echo infodata > "$d/a.info"
  echo cadata > "$d/a.ca"
  printf '%s\n' "$lfs_pointer" > "$d/a.bin"
  echo t1 > "$d/a.txt"
  echo b1 > "$d/b.txt"
  echo 'main = pure ()' > "$d/plain.hs"
  mkdir "$d/lib"
  echo 'module L where' > "$d/lib/L.hs"
  sg -C "$d" add -A
  sg -C "$d" commit -qm c0
  echo t2 > "$d/a.txt"
  sg -C "$d" commit -qam c1
  sg -C "$d" submodule add -q "$T/subsrc" sub
  sg -C "$d" commit -qm sub
  # Racy entries inside the submodule.
  future "$d/sub/a.x" "$d/sub/p.txt"
  sg -C "$d/sub" add -A
  case $state in
    r1) sg -C "$d" read-tree HEAD ;;
    r2)
      future "$d/a.x" "$d/a.cl" "$d/a.info" "$d/a.ca" "$d/a.bin" "$d/a.txt" "$d/plain.hs" "$d/lib/L.hs"
      sg -C "$d" add -A ;;
    r3) touch -d 2025-06-01T00:00:00 "$d/plain.hs" ;;
    r4)
      sg -C "$d" config core.untrackedCache true
      sg -C "$d" config core.splitIndex true
      sg -C "$d" config index.version 4
      future "$d/a.x" "$d/a.cl" "$d/a.info" "$d/a.ca" "$d/a.bin" "$d/a.txt" "$d/plain.hs" "$d/lib/L.hs"
      sg -C "$d" update-index --index-version 4 --split-index --untracked-cache
      sg -C "$d" add -A
      sg -C "$d" status -s >/dev/null ;;
  esac
  echo b2 > "$d/b.txt"
  echo 'module U where' > "$d/untracked.hs"
}

# Traps go in last, after every add and commit.
traps() {
  d=$1
  gd=$(sg -C "$d" rev-parse --absolute-git-dir)
  c() { sg -C "$d" config "$@"; }
  c filter.x.clean "$T/bin/mark x-clean; cat"
  c filter.x.smudge "$T/bin/mark x-smudge; cat"
  c filter.x.process "$T/bin/mark x-process; exit 1"
  c filter.x.required true
  # A clean-only driver (no process), so filter.cl.clean itself is tried.
  c filter.cl.clean "$T/bin/mark cl-clean; cat"
  c filter.cl.smudge "$T/bin/mark cl-smudge; cat"
  c filter.cl.required true
  printf '*.info filter=x\n' > "$gd/info/attributes"
  c core.attributesFile "$T/attrs"
  c diff.x.textconv "$T/bin/mark textconv; cat"
  c diff.x.command "$T/bin/mark diff-command"
  c diff.external "$T/bin/mark diff-external"
  c core.fsmonitor "$T/bin/fsmon"
  c core.hooksPath "$T/hooks"
  for h in $hooks; do cp "$T/hooks/$h" "$gd/hooks/$h"; done
  if [ -d "$d/sub" ]; then
    s() { sg -C "$d/sub" config "$@"; }
    s filter.x.clean "$T/bin/mark sub-x-clean; cat"
    s filter.x.process "$T/bin/mark sub-x-process; exit 1"
    s filter.x.required true
    s core.fsmonitor "$T/bin/fsmon"
  fi
}

for st in r1 r2 r3 r4; do mkrepo "$T/$st" "$st"; done
mkrepo "$T/fsd" r2

# Partial clone with a missing blob, then an ext:: remote that marks.
sg init -q "$T/pcsrc"
echo v1 > "$T/pcsrc/f.txt"
sg -C "$T/pcsrc" add -A
sg -C "$T/pcsrc" commit -qm v1
echo v2 > "$T/pcsrc/f.txt"
sg -C "$T/pcsrc" commit -qam v2
sg -C "$T/pcsrc" config uploadpack.allowFilter true
sg -C "$T/pcsrc" config uploadpack.allowAnySHA1InWant true
sg clone -q --filter=blob:none "file://$T/pcsrc" "$T/pc"
OLDBLOB=$(sg -C "$T/pcsrc" rev-parse HEAD~1:f.txt)
sg -C "$T/pc" config remote.origin.url "ext::sh -c touch% $M/lazy-fetch"
sg -C "$T/pc" config protocol.ext.allow always
if GIT_NO_LAZY_FETCH=1 sg -C "$T/pc" cat-file -e "$OLDBLOB" 2>/dev/null; then
  echo "setup: old blob is not missing in the partial clone" >&2
  exit 2
fi
rm -f "$M/lazy-fetch"

# FIFOs: worktree .gitattributes and core.excludesFile.
sg init -q "$T/fifo"
printf '*.x filter=x\n' > "$T/fifo/.gitattributes"
echo a > "$T/fifo/a.x"
sg -C "$T/fifo" add -A
sg -C "$T/fifo" commit -qm f
rm "$T/fifo/.gitattributes"
mkfifo "$T/fifo/.gitattributes" "$T/excludes.fifo"
sg -C "$T/fifo" config core.excludesFile "$T/excludes.fifo"

for r in r1 r2 r3 r4 fsd; do traps "$T/$r"; done
traps "$T/pc"
sg -C "$T/fsd" config core.fsmonitor true
EMPTY_TREE=$(sg -C "$T/r1" hash-object -t tree /dev/null)
rm -f "$M"/* "$T/markers.log"

# --- witnesses ---------------------------------------------------------------
# snap GITDIR PREFIX: PREFIX.list has path size mtime ctime inode type,
# PREFIX.sums the sha256 of every file.
snap() {
  (cd "$1" && find . -printf '%p %s %T@ %C@ %i %y\n' | LC_ALL=C sort) > "$2.list"
  (cd "$1" && find . -type f -exec sha256sum {} + | LC_ALL=C sort) > "$2.sums"
}

# no / yes / sharedindex-mtime (only the times of sharedindex.* moved:
# git freshens them on every read of a split index).
gitdir_change() {
  if cmp -s "$T/before.list" "$T/after.list" && cmp -s "$T/before.sums" "$T/after.sums"; then
    echo no
  elif cmp -s "$T/before.sums" "$T/after.sums" &&
    [ "$(awk '$1 ~ /^\.\/sharedindex\./ { $3 = ""; $4 = "" } { print }' "$T/before.list")" = \
      "$(awk '$1 ~ /^\.\/sharedindex\./ { $3 = ""; $4 = "" } { print }' "$T/after.list")" ]; then
    echo sharedindex-mtime
  else
    echo yes
  fi
}

daemons() { pgrep -f 'fsmonitor--daemon run' | LC_ALL=C sort; }

fail=0
printf 'repo\tstate\tcall\tvariant\texpect\tmarkers\tchildren\tgitdir_changed\texit\tverdict\n' > "$out"

# row KIND REPO STATE CALL VARIANT EXPECT DIR INPUT LIMIT -- CMD...
#   KIND: safe (must be quiet), guard (record), sides (must be quiet),
#   control (EXPECT is a marker prefix that must appear), fifo.
row() {
  kind=$1 repo=$2 state=$3 call=$4 variant=$5 expect=$6 dir=$7 input=$8 limit=$9
  shift 10
  rm -f "$M"/*
  : > "$T/trace.json"
  rm -f "$T/strace"
  gd=$(sg -C "$dir" rev-parse --absolute-git-dir)
  snap "$gd" "$T/before"
  d0=$(daemons)
  ipc0=no
  [ -S "$gd/fsmonitor--daemon.ipc" ] && ipc0=yes
  wrap=""
  [ "$HAVE_STRACE" = 1 ] && wrap="strace -f -qq -e trace=execve -o $T/strace"
  case $kind in
    sides)
      ( cd "$dir" && HGIT_TRACE=$T/trace.json timeout -k 2 "$limit" $wrap "$@" ) \
        < "$input" > "$T/out" 2> "$T/err" ;;
    control)
      ( cd "$dir" && GIT_TRACE2_EVENT=$T/trace.json PATH=$HGIT_PATH timeout -k 2 "$limit" "$@" ) \
        < "$input" > "$T/out" 2> "$T/err" ;;
    *)
      HGIT_STDIN=1 HGIT_TRACE=$T/trace.json HGIT_WRAP=$wrap timeout -k 2 "$limit" "$@" \
        < "$input" > "$T/out" 2> "$T/err" ;;
  esac
  ex=$?
  sleep 0.2
  marks=$(ls "$M" | LC_ALL=C sort | tr '\n' ',' | sed 's/,$//')
  [ -n "$marks" ] || marks=-
  t2=$(grep -c '"event":"child_start"' "$T/trace.json")
  if [ -f "$T/strace" ]; then
    # Successful execs that are not git (or git-sides itself).
    sx=$(grep ' = 0$' "$T/strace" | grep 'execve(' |
      grep -v "execve(\"$REALGIT\"" | grep -cv "execve(\"${SIDES:-/nonexistent}\"")
  else
    sx=na
  fi
  d1=$(daemons)
  if [ "$d0" != "$d1" ]; then
    marks="$marks+daemon"
    for p in $d1; do
      case " $d0 " in *" $p "*) ;; *) kill "$p" 2>/dev/null ;; esac
    done
  fi
  [ $ipc0 = no ] && [ -S "$gd/fsmonitor--daemon.ipc" ] && marks="$marks+ipc"
  snap "$gd" "$T/after"
  changed=$(gitdir_change)
  fired=no
  [ "$marks" != - ] && fired=yes
  [ "$t2" != 0 ] && fired=yes
  [ "$sx" != na ] && [ "$sx" != 0 ] && fired=yes
  case $kind in
    safe|sides)
      if [ $fired = no ] && [ $changed = no ]; then v=PASS
      elif [ $fired = no ] && [ $changed = sharedindex-mtime ]; then v=PASS-mtime
      else v=FAIL; fi ;;
    guard)
      if [ $fired = yes ]; then v=fires; else v=quiet; fi
      [ "$v" = "$expect" ] || v="$v(differs)" ;;
    control)
      case "$marks" in *"$expect"*) v=PASS ;; *) v=FAIL ;; esac ;;
    fifo)
      if [ "$marks" != - ]; then v=FAIL
      elif [ $ex = 124 ] || [ $ex = 137 ]; then v=blocks
      else v=ok; fi ;;
  esac
  case $v in FAIL*) fail=1; echo "FAIL: $repo $state $call $variant" >&2; head -c 600 "$T/err" >&2 ;; esac
  printf '%s\t%s\t%s\t%s\t%s\t%s\tt2=%s;exec=%s\t%s\t%s\t%s\n' \
    "$repo" "$state" "$call" "$variant" "$expect" "$marks" "$t2" "$sx" "$changed" "$ex" "$v" >> "$out"
}

# Inputs for cat-file and check-attr.
mkinputs() {
  d=$1
  sg -C "$d" ls-tree -r HEAD | awk '$2 == "blob" { print "contents " $3; print "info " $3 }' > "$T/batch.in"
  echo "info 0000000000000000000000000000000000000001" >> "$T/batch.in"
  { sg -C "$d" ls-files -z; sg -C "$d" ls-files -z --others --exclude-standard; } > "$T/paths.in" 2>/dev/null
}

CFGRE='^core\.(autocrlf|filemode|symlinks|ignorecase|trustctime|checkstat)$'

safe_set() {
  repo=$1 st=$2 d=$3 lim=${4:-20} kind=${5:-safe}
  mkinputs "$d"
  N=/dev/null
  row "$kind" "$repo" "$st" "--version" plain quiet "$d" $N "$lim" -- "$HGIT" "$d" --version
  row "$kind" "$repo" "$st" "rev-parse --is-inside-work-tree" plain quiet "$d" $N "$lim" -- "$HGIT" "$d" rev-parse --is-inside-work-tree
  row "$kind" "$repo" "$st" "rev-parse --show-prefix --show-object-format --is-shallow-repository --git-path index" plain quiet "$d" $N "$lim" -- \
    "$HGIT" "$d" rev-parse --show-prefix --show-object-format --is-shallow-repository --git-path index
  row "$kind" "$repo" "$st" "rev-parse --verify --quiet --end-of-options HEAD^{commit}" plain quiet "$d" $N "$lim" -- \
    "$HGIT" "$d" rev-parse --verify --quiet --end-of-options 'HEAD^{commit}'
  row "$kind" "$repo" "$st" "merge-base --all HEAD HEAD~1" plain quiet "$d" $N "$lim" -- "$HGIT" "$d" merge-base --all HEAD HEAD~1
  row "$kind" "$repo" "$st" "ls-tree -r -z --long HEAD" top quiet "$d" $N "$lim" -- "$HGIT" "$d" ls-tree -r -z --long HEAD
  if [ -d "$d/lib" ]; then
    row "$kind" "$repo" "$st" "ls-tree -r -z --long HEAD" lib/ quiet "$d" $N "$lim" -- "$HGIT" "$d/lib" ls-tree -r -z --long HEAD
    row "$kind" "$repo" "$st" "ls-tree -r -z --long --full-tree HEAD" lib/ quiet "$d" $N "$lim" -- "$HGIT" "$d/lib" ls-tree -r -z --long --full-tree HEAD
  fi
  row "$kind" "$repo" "$st" "cat-file --batch-command" "contents+info+missing" quiet "$d" "$T/batch.in" "$lim" -- "$HGIT" "$d" cat-file --batch-command
  row "$kind" "$repo" "$st" "ls-files -z --stage" plain quiet "$d" $N "$lim" -- "$HGIT" "$d" ls-files -z --stage
  row "$kind" "$repo" "$st" "ls-files -z --stage --debug" plain quiet "$d" $N "$lim" -- "$HGIT" "$d" ls-files -z --stage --debug
  row "$kind" "$repo" "$st" "ls-files -z --others --exclude-standard" plain quiet "$d" $N "$lim" -- "$HGIT" "$d" ls-files -z --others --exclude-standard
  row "$kind" "$repo" "$st" "check-attr -z --stdin text eol crlf filter ident working-tree-encoding" all-paths quiet "$d" "$T/paths.in" "$lim" -- \
    "$HGIT" "$d" check-attr -z --stdin text eol crlf filter ident working-tree-encoding
  row "$kind" "$repo" "$st" "config -z --get-regexp core.(...)" plain quiet "$d" $N "$lim" -- "$HGIT" "$d" config -z --get-regexp "$CFGRE"
  row "$kind" "$repo" "$st" "config --get core.autocrlf" plain quiet "$d" $N "$lim" -- "$HGIT" "$d" config --get core.autocrlf
  row "$kind" "$repo" "$st" "config -z --name-only --get-regexp ^filter." plain quiet "$d" $N "$lim" -- "$HGIT" "$d" config -z --name-only --get-regexp '^filter\.'
}

# GIT_CONFIG_COUNT words that blank every filter driver git can see.
blank_filters() {
  d=$1
  n=0
  words=""
  for k in $("$HGIT" "$d" config -z --name-only --get-regexp '^filter\.' | tr '\0' '\n' | LC_ALL=C sort -u); do
    case $k in
      *.required) v=false ;;
      *.clean|*.smudge|*.process) v= ;;
      *) continue ;;
    esac
    words="$words GIT_CONFIG_KEY_$n=$k GIT_CONFIG_VALUE_$n=$v"
    n=$((n + 1))
  done
  printf 'GIT_CONFIG_COUNT=%s%s' "$n" "$words"
}

guards() {
  repo=$1 st=$2 d=$3
  N=/dev/null
  row guard "$repo" "$st" "diff-files --name-only" plain fires "$d" $N 20 -- "$HGIT" "$d" diff-files --name-only
  row guard "$repo" "$st" "diff-files --name-only" "--attr-source=<empty tree>" fires "$d" $N 20 -- \
    "$HGIT" "$d" "--attr-source=$EMPTY_TREE" diff-files --name-only
  row guard "$repo" "$st" "diff-files --name-only" "+ -c core.attributesFile=/dev/null" fires "$d" $N 20 -- \
    "$HGIT" "$d" "--attr-source=$EMPTY_TREE" -c core.attributesFile=/dev/null diff-files --name-only
  row guard "$repo" "$st" "diff-files --name-only" "+ --ignore-submodules=all" fires "$d" $N 20 -- \
    "$HGIT" "$d" "--attr-source=$EMPTY_TREE" -c core.attributesFile=/dev/null diff-files --ignore-submodules=all --name-only
  bl=$(blank_filters "$d")
  HGIT_ENV=$bl
  export HGIT_ENV
  row guard "$repo" "$st" "diff-files --name-only" "+ filters blanked (GIT_CONFIG_COUNT)" quiet "$d" $N 20 -- \
    "$HGIT" "$d" "--attr-source=$EMPTY_TREE" -c core.attributesFile=/dev/null diff-files --ignore-submodules=all --name-only
  # Only the blanking, nothing else.
  row guard "$repo" "$st" "diff-files --name-only" "filters blanked + --ignore-submodules=all only" quiet "$d" $N 20 -- \
    "$HGIT" "$d" diff-files --ignore-submodules=all --name-only
  unset HGIT_ENV
  if [ -n "$SIDES" ]; then
    row sides "$repo" "$st" "git-sides worktree" ours quiet "$d" $N 60 -- "$SIDES" worktree --list "$d"
  fi
}

UG=$REALGIT  # unhardened git for the controls
# A failing filter-process makes git die before it gets to the hook or
# diff driver (2.44 even when not required); these controls switch the
# filters off, so they test the hooks and diff drivers.
NOREQ="-c filter.x.process= -c filter.x.required=false -c filter.lfs.process= -c filter.lfs.required=false"

controls() {
  repo=$1 st=$2 d=$3
  N=/dev/null
  row control "$repo" "$st" "status (unhardened)" control fsmonitor-hook "$d" $N 20 -- "$UG" status --porcelain
  row control "$repo" "$st" "status (unhardened)" "index written, filters off" hook-post-index-change "$d" $N 20 -- "$UG" $NOREQ status --porcelain
  row control "$repo" "$st" "cat-file --filters HEAD:a.x" control x- "$d" $N 20 -- "$UG" cat-file --filters HEAD:a.x
  row control "$repo" "$st" "cat-file --textconv HEAD:a.txt" control textconv "$d" $N 20 -- "$UG" cat-file --textconv HEAD:a.txt
  row control "$repo" "$st" "hash-object a.x" "repo config + .gitattributes" x- "$d" $N 20 -- "$UG" hash-object a.x
  row control "$repo" "$st" "hash-object a.info" "info/attributes" x- "$d" $N 20 -- "$UG" hash-object a.info
  row control "$repo" "$st" "hash-object a.ca" "core.attributesFile" x- "$d" $N 20 -- "$UG" hash-object a.ca
  row control "$repo" "$st" "hash-object a.cl" "clean-only driver" cl-clean "$d" $N 20 -- "$UG" hash-object a.cl
  row control "$repo" "$st" "hash-object a.bin" "LFS from user config" lfs- "$d" $N 20 -- "$UG" hash-object a.bin
  row control "$repo" "$st" "hash-object --attr-source=<empty> a.info" "attr-source hole" x- "$d" $N 20 -- \
    "$UG" "--attr-source=$EMPTY_TREE" hash-object a.info
  row control "$repo" "$st" "diff HEAD" "filters off" diff- "$d" $N 20 -- "$UG" $NOREQ diff HEAD
  row control "$repo" "$st" "hash-object a.x in sub" "submodule config" sub-x- "$d/sub" $N 20 -- "$UG" hash-object a.x
  row control "$repo" "$st" "commit --allow-empty" "hooks, filters off" hook-pre-commit "$d" $N 20 -- "$UG" $NOREQ -c user.name=t -c user.email=t@t commit -q --allow-empty -m hooked
}

for st in r1 r2 r3 r4; do
  safe_set main "$st" "$T/$st"
  guards main "$st" "$T/$st"
  controls main "$st" "$T/$st"
done

# fsmonitor daemon (core.fsmonitor=true).
safe_set fsd r2 "$T/fsd"
guards fsd r2 "$T/fsd"
if "$REALGIT" version --build-options | grep -q 'fsmonitor--daemon'; then
  row control fsd r2 "status (unhardened)" "core.fsmonitor=true" "+daemon" "$T/fsd" /dev/null 20 -- "$UG" $NOREQ status --porcelain
else
  # This git has no daemon: core.fsmonitor=true does nothing here.
  printf 'fsd\tr2\tstatus (unhardened)\tcore.fsmonitor=true\t+daemon\t-\t-\t-\t-\tskipped (no fsmonitor--daemon in this build)\n' >> "$out"
fi
GIT_DIR=$T/fsd/.git "$REALGIT" fsmonitor--daemon stop >/dev/null 2>&1

# Partial clone.
printf 'info %s\ncontents %s\n' "$OLDBLOB" "$OLDBLOB" > "$T/pc.in"
row safe pc - "cat-file --batch-command" "missing blob" quiet "$T/pc" "$T/pc.in" 20 -- "$HGIT" "$T/pc" cat-file --batch-command
row safe pc - "ls-tree -r -z --long HEAD~1" "missing blob" quiet "$T/pc" /dev/null 20 -- "$HGIT" "$T/pc" ls-tree -r -z --long HEAD~1
row safe pc - "ls-tree -r -z HEAD~1" "missing blob" quiet "$T/pc" /dev/null 20 -- "$HGIT" "$T/pc" ls-tree -r -z HEAD~1
safe_set pc - "$T/pc"
lside=$here/results/list-side.txt
if [ -n "$SIDES" ]; then
  # Proposed fallback when ls-tree --long dies (2.44.1): plain ls-tree,
  # sizes from cat-file info, restart cat-file when it dies.
  row sides pc - "git-sides list-side HEAD~1" "missing blob" quiet "$T/pc" /dev/null 60 -- "$SIDES" list-side "$T/pc" HEAD~1
  { echo "git: $("$REALGIT" --version)"; cat "$T/out"; } > "$lside"
fi
row control pc - "cat-file -p <missing> (no GIT_NO_LAZY_FETCH)" control lazy-fetch "$T/pc" /dev/null 20 -- "$UG" cat-file -p "$OLDBLOB"

# FIFOs: record whether a call blocks. Never killed by us otherwise.
fifo_set() {
  d=$T/fifo
  N=/dev/null
  printf '.gitattributes\0a.x\0' > "$T/fifo.in"
  for c in "ls-files -z --stage" "ls-files -z --stage --debug" "ls-files -z --others --exclude-standard" "ls-tree -r -z --long HEAD"; do
    # shellcheck disable=SC2086
    row fifo fifo - "$c" "" blocks? "$d" $N 5 -- "$HGIT" "$d" $c
  done
  row fifo fifo - "check-attr -z --stdin text eol crlf filter ident working-tree-encoding" "" blocks? "$d" "$T/fifo.in" 5 -- \
    "$HGIT" "$d" check-attr -z --stdin text eol crlf filter ident working-tree-encoding
  row fifo fifo - "diff-files --name-only" "" blocks? "$d" $N 5 -- "$HGIT" "$d" diff-files --name-only
  if [ -n "$SIDES" ]; then
    row fifo fifo - "git-sides worktree" "" blocks? "$d" $N 5 -- "$SIDES" worktree --list "$d"
  fi
}
fifo_set

# --- our stat check vs diff-files, traps removed --------------------------------
untrap() {
  d=$1
  gd=$(sg -C "$d" rev-parse --absolute-git-dir)
  for k in filter.x filter.cl core.attributesFile diff.x diff.external core.fsmonitor core.hooksPath; do
    sg -C "$d" config --remove-section "$k" 2>/dev/null || sg -C "$d" config --unset-all "$k" 2>/dev/null
  done
  rm -f "$gd/info/attributes" "$gd"/hooks/*
  if [ -d "$d/sub" ]; then
    sg -C "$d/sub" config --remove-section filter.x 2>/dev/null
    sg -C "$d/sub" config --unset core.fsmonitor 2>/dev/null
  fi
}

printf 'state\tcopy\tours\tdiff_files\tonly_ours\tonly_diff_files\n' > "$wcheck"
if [ -n "$SIDES" ]; then
  for st in r1 r2 r3 r4; do
    untrap "$T/$st"
    rm -rf "$T/cp-$st"
    cp -a "$T/$st" "$T/cp-$st"
    for v in in-place cp-a; do
      case $v in in-place) d=$T/$st ;; *) d=$T/cp-$st ;; esac
      HOME=$T/setup-home "$SIDES" worktree --list "$d" | awk -F'\t' 'NF == 3 && $1 == "dirty" { print $3 }' | LC_ALL=C sort > "$T/ours"
      (cd "$d" && env -i HOME="$T/setup-home" PATH=/usr/bin:/bin GIT_OPTIONAL_LOCKS=0 "$REALGIT" diff-files --name-only 2> "$T/df.err" || echo "ERROR:$(head -c 200 "$T/df.err")") | LC_ALL=C sort > "$T/df"
      printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$st" "$v" \
        "$(tr '\n' ' ' < "$T/ours")" "$(tr '\n' ' ' < "$T/df")" \
        "$(LC_ALL=C comm -23 "$T/ours" "$T/df" | tr '\n' ' ')" \
        "$(LC_ALL=C comm -13 "$T/ours" "$T/df" | tr '\n' ' ')" >> "$wcheck"
    done
  done
fi

# Edge cases for our stat check, no traps: ours vs `git status` (which
# compares content). A row is bad if only_ours or only_status is not empty.
edge() {  # NAME: set up $T/edge-NAME, then compare
  name=$1
  d=$T/edge-$name
  ours=$(HOME=$T/setup-home "$SIDES" worktree --list "$d" | awk -F'\t' 'NF == 3 && $1 == "dirty" { print $3 }' | LC_ALL=C sort)
  git_says=$(sg -C "$d" status --porcelain --untracked-files=no --ignore-submodules=all | cut -c4- | LC_ALL=C sort)
  printf '%s\n' "$ours" | sed '/^$/d' > "$T/ours"
  printf '%s\n' "$git_says" | sed '/^$/d' > "$T/st"
  printf '%s\t%s\t%s\t%s\t%s\t%s\n' "edge-$name" "vs status" \
    "$(tr '\n' ' ' < "$T/ours")" "$(tr '\n' ' ' < "$T/st")" \
    "$(LC_ALL=C comm -23 "$T/ours" "$T/st" | tr '\n' ' ')" \
    "$(LC_ALL=C comm -13 "$T/ours" "$T/st" | tr '\n' ' ')" >> "$wcheck"
}
if [ -n "$SIDES" ]; then
  # A symlink in a leading directory: git says a/b is deleted.
  d=$T/edge-symlink-dir
  sg init -q "$d"; mkdir "$d/a"; echo b > "$d/a/b"
  sg -C "$d" add -A; sg -C "$d" commit -qm e
  mv "$d/a" "$d/a2"; ln -s a2 "$d/a"
  edge symlink-dir
  # core.filemode with no value (true), then chmod +x: a mode change.
  d=$T/edge-filemode-novalue
  sg init -q "$d"; echo 'echo hi' > "$d/x.sh"
  sg -C "$d" add -A; sg -C "$d" commit -qm e
  sg -C "$d" config --unset core.filemode
  printf '[core]\n\tfilemode\n' >> "$d/.git/config"
  chmod +x "$d/x.sh"
  edge filemode-novalue
  # core.autocrlf=yes, a CRLF file, touched: git says clean.
  d=$T/edge-autocrlf-yes
  sg init -q "$d"; sg -C "$d" config core.autocrlf yes
  printf 'a\r\nb\r\n' > "$d/f.txt"
  sg -C "$d" add -A; sg -C "$d" commit -qm e
  touch -d 2025-06-01T00:00:00 "$d/f.txt"
  edge autocrlf-yes
  # text=input attribute, a CRLF file, touched: git says clean.
  d=$T/edge-text-input
  sg init -q "$d"; printf '*.t text=input\n' > "$d/.gitattributes"
  printf 'a\r\nb\r\n' > "$d/f.t"
  sg -C "$d" add -A; sg -C "$d" commit -qm e
  touch -d 2025-06-01T00:00:00 "$d/f.t"
  edge text-input
  for r in "$T"/edge-*; do
    case $(awk -F'\t' -v n="${r##*/}" '$1 == n { print ($5 $6 == "") ? "ok" : "bad" }' "$wcheck") in
      ok) ;;
      *) fail=1; echo "FAIL: ${r##*/}: ours differs from git status" >&2 ;;
    esac
  done
fi

left=$(daemons)
[ -z "$left" ] || echo "note: fsmonitor daemons still running: $left" >&2
echo "tmp: $T"
if [ $fail = 0 ]; then echo PASS; else echo FAIL; exit 1; fi
