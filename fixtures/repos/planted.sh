#!/bin/sh
# Repos with traps: config, attributes and hooks that write a marker
# file when git runs something (from the T01 spike, D-311). Safe calls
# must fire none of them.
#
# Usage: /bin/sh planted.sh DIR
#
# Makes, in DIR:
#   markers/   a trap that fires writes a file here
#   bin/       mark, fsmon, a fake git-lfs, and git: a wrapper that sets
#              GIT_TRACE2_EVENT=DIR/trace.json and runs the real git.
#              Put bin/ on PATH: git would find git-lfs there.
#   home/      HOME, with LFS filters in .gitconfig (as `git lfs install`)
#   hooks/     every hook, each one marks
#   repo/      two commits (HEAD~1, HEAD) and every trap
#   pc/        a partial clone of pcsrc/ (blob:none) with the traps and an
#              ext:: remote that marks on a fetch. The blob of f.txt in
#              HEAD~1 is missing; its id is in missing-blob.
. "$(dirname "$0")/lib.sh"
T=$1
realgit=$(command -v git)
M=$T/markers
mkdir -p "$M" "$T/bin" "$T/hooks" "$T/home"

cat > "$T/bin/mark" <<MARK
#!/bin/sh
: > "$M/\$1"
MARK
cat > "$T/bin/fsmon" <<MARK
#!/bin/sh
"$T/bin/mark" fsmonitor-hook
exit 1
MARK
cat > "$T/bin/git-lfs" <<MARK
#!/bin/sh
"$T/bin/mark" "lfs-\$1"
case "\$1" in
  clean|smudge) cat ;;
  *) exit 1 ;;
esac
MARK
cat > "$T/bin/git" <<MARK
#!/bin/sh
GIT_TRACE2_EVENT=$T/trace.json
export GIT_TRACE2_EVENT
exec "$realgit" "\$@"
MARK
chmod +x "$T/bin/mark" "$T/bin/fsmon" "$T/bin/git-lfs" "$T/bin/git"

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

cat > "$T/home/.gitconfig" <<'CFG'
[filter "lfs"]
	clean = git-lfs clean -- %f
	smudge = git-lfs smudge -- %f
	process = git-lfs filter-process
	required = true
CFG
printf '*.ca filter=x\n' > "$T/attrs"

# The repo: c0, then c1 changes a.txt.
r=$T/repo
repo_init "$r"
printf '*.x filter=x\n*.cl filter=cl\n*.bin filter=lfs diff=lfs merge=lfs -text\n*.txt diff=x\n' \
  > "$r/.gitattributes"
echo xdata > "$r/a.x"
echo cldata > "$r/a.cl"
echo infodata > "$r/a.info"
echo cadata > "$r/a.ca"
cat > "$r/a.bin" <<'PTR'
version https://git-lfs.github.com/spec/v1
oid sha256:4d7a214614ab2935c943f9e0ff69d22eadbb8f32b1258daaa5e2ca24d17e2393
size 12
PTR
echo t1 > "$r/a.txt"
echo 'main = pure ()' > "$r/plain.hs"
mkdir "$r/lib"
echo 'module L where' > "$r/lib/L.hs"
g -C "$r" add -A
commit_at "$r" 0 -m c0
echo t2 > "$r/a.txt"
g -C "$r" add -A
commit_at "$r" 1 -m c1

# The partial clone.
s=$T/pcsrc
repo_init "$s"
echo v1 > "$s/f.txt"
g -C "$s" add -A
commit_at "$s" 0 -m v1
echo v2 > "$s/f.txt"
g -C "$s" add -A
commit_at "$s" 1 -m v2
g -C "$s" config uploadpack.allowFilter true
g -C "$s" config uploadpack.allowAnySHA1InWant true
g clone -q --filter=blob:none "file://$s" "$T/pc"
g -C "$s" rev-parse HEAD~1:f.txt > "$T/missing-blob"
g -C "$T/pc" config remote.origin.url "ext::sh -c touch% $M/lazy-fetch"
g -C "$T/pc" config protocol.ext.allow always
# shellcheck disable=SC2086
if env GIT_NO_LAZY_FETCH=1 git $gflags -C "$T/pc" cat-file -e "$(cat "$T/missing-blob")" \
  2>/dev/null; then
  echo "planted.sh: the old blob is not missing in the partial clone" >&2
  exit 2
fi

# Traps go in last, after every add and commit.
traps() {
  d=$1
  gd=$d/.git
  c() { g -C "$d" config "$@"; }
  c filter.x.clean "$T/bin/mark x-clean; cat"
  c filter.x.smudge "$T/bin/mark x-smudge; cat"
  c filter.x.process "$T/bin/mark x-process; exit 1"
  c filter.x.required true
  # A clean-only driver (no process), so the clean command itself is
  # what would run.
  c filter.cl.clean "$T/bin/mark cl-clean; cat"
  c filter.cl.smudge "$T/bin/mark cl-smudge; cat"
  c filter.cl.required true
  mkdir -p "$gd/info"
  printf '*.info filter=x\n' > "$gd/info/attributes"
  c core.attributesFile "$T/attrs"
  c diff.x.textconv "$T/bin/mark textconv; cat"
  c diff.x.command "$T/bin/mark diff-command"
  c diff.external "$T/bin/mark diff-external"
  c core.fsmonitor "$T/bin/fsmon"
  c core.hooksPath "$T/hooks"
  mkdir -p "$gd/hooks"
  for h in $hooks; do cp "$T/hooks/$h" "$gd/hooks/$h"; done
}
traps "$r"
traps "$T/pc"
rm -f "$M"/* "$T/trace.json"
