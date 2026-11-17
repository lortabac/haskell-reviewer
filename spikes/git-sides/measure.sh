#!/bin/sh
# Timings on the wire-server copies from prepare-wire.sh. Warm cache.
# Usage: /bin/sh measure.sh GIT_SIDES_BIN
# Writes results/listing.tsv, blobs.tsv, worktree.tsv, wire-edits.tsv.
#
# Pool rule (fixed before measuring): more than one cat-file only if,
# with 8 workers, pool 2 or 4 cuts total wall time by >= 25% vs pool 1,
# or the requester's p95 is > 10 ms with pool 1 and <= 10 ms with
# pool N. Then the smallest N that passes; else one handle.
set -u
here=$(cd "$(dirname "$0")" && pwd)
SIDES=$1
common=$(git -C "$here" rev-parse --path-format=absolute --git-common-dir) || exit 1
main=$(dirname "$common")
BUILD=${BUILD:-$main/.worktrees/.build/t01-git-sides}
FRESH=$BUILD/wire-server
REFR=$BUILD/wire-server-refreshed
PIN=7568f595e42ece6f0791682336014853a9dfcac4
R=$here/results
H=$(mktemp -d)
mkdir -p "$H/home"
GIT=$(command -v git)
HGIT_PATH=/usr/bin:/bin
HOME=$H/home
export HGIT_PATH HOME
unset XDG_CONFIG_HOME
BASE=$("$here/hgit.sh" "$REFR" rev-parse --verify --quiet --end-of-options "$PIN^1^{commit}") || exit 1
ENVS="env -i HOME=$H/home PATH=/usr/bin:/bin LC_ALL=C GIT_OPTIONAL_LOCKS=0 GIT_NO_LAZY_FETCH=1 GIT_NO_REPLACE_OBJECTS=1 GIT_TERMINAL_PROMPT=0"
FLAGS="--no-pager -c core.fsmonitor=false -c core.untrackedCache=false -c core.hooksPath=/dev/null -c safe.bareRepository=explicit"

# --- listing -------------------------------------------------------------------
hf() {  # NAME DIR ARGS...
  name=$1 dir=$2
  shift 2
  hyperfine -N --warmup 3 --runs 20 --style none --export-csv "$H/hf.csv" \
    "$ENVS $GIT -C $dir $FLAGS $*" >/dev/null 2>&1 || { printf '%s\tFAILED\n' "$name" >> "$R/listing.tsv"; return; }
  out_bytes=$($ENVS "$GIT" -C "$dir" $FLAGS "$@" | wc -c)
  # command,mean,stddev,median,user,system,min,max (seconds)
  tail -n 1 "$H/hf.csv" | awk -F, -v n="$name" -v b="$out_bytes" \
    '{ printf "%s\t%.1f\t%.1f\t%.1f\t%.1f\t%s\n", n, $4 * 1000, $7 * 1000, $8 * 1000, $3 * 1000, b }' >> "$R/listing.tsv"
}
printf 'call\tmedian_ms\tmin_ms\tmax_ms\tstddev_ms\tstdout_bytes\n' > "$R/listing.tsv"
for side in head base; do
  case $side in head) c=$PIN ;; *) c=$BASE ;; esac
  hf "ls-tree -r -z ($side)" "$REFR" ls-tree -r -z "$c"
  hf "ls-tree -r -z --long ($side)" "$REFR" ls-tree -r -z --long "$c"
  hf "ls-tree -r -z --long ($side), ROOT=libs/" "$REFR/libs" ls-tree -r -z --long "$c"
  hf "ls-tree -r -z --long --full-tree ($side), ROOT=libs/" "$REFR/libs" ls-tree -r -z --long --full-tree "$c"
done
hf "rev-parse --show-prefix --show-object-format --is-shallow-repository" "$REFR" rev-parse --show-prefix --show-object-format --is-shallow-repository
hf "merge-base --all base head" "$REFR" merge-base --all "$BASE" "$PIN"
hf "ls-files -z --stage" "$REFR" ls-files -z --stage
hf "ls-files -z --stage --debug" "$REFR" ls-files -z --stage --debug
hf "ls-files -z --others --exclude-standard" "$REFR" ls-files -z --others --exclude-standard
hf "diff-files --name-only (refreshed copy)" "$REFR" diff-files --name-only

# --- blobs -----------------------------------------------------------------------
"$here/hgit.sh" "$REFR" ls-tree -r "$PIN" | awk '$2 == "blob" { print $3 }' | LC_ALL=C sort -u > "$H/head.oids"
{ cat "$H/head.oids"; "$here/hgit.sh" "$REFR" ls-tree -r "$BASE" | awk '$2 == "blob" { print $3 }'; } | LC_ALL=C sort -u > "$H/union.oids"
printf 'input\tpool\tworkers\trequester_ms\toids\tbytes\twall_med_ms\twall_min_ms\twall_max_ms\tstartup_ms\twait_p50_ms\twait_p95_ms\twait_max_ms\tread_p50_ms\tread_p95_ms\treq_n\treq_p50_ms\treq_p95_ms\treq_max_ms\tcatfile_vmhwm_kb\n' > "$R/blobs.tsv"
for input in head union; do
  for pool in 1 2 4; do
    for spec in "1 -" "8 -" "8 20"; do
      set -- $spec
      req=""
      [ "$2" != - ] && req="--requester $2"
      # shellcheck disable=SC2086
      line=$(timeout -s KILL 600 "$SIDES" read-blobs --pool "$pool" --workers "$1" $req --runs 10 --warmup 1 \
        "$REFR" "$H/$input.oids" +RTS -N8 -RTS 2>/dev/null) || line="FAILED"
      printf '%s\t%s\n' "$input" "$line" >> "$R/blobs.tsv"
    done
  done
done

# --- working tree ---------------------------------------------------------------------
kvrow() {  # turn key\tvalue lines into one row
  awk -F'\t' -v pre="$1" 'NF == 2 { k[++n] = $1; v[n] = $2 }
    END { if (hdr) { } s = pre; for (i = 1; i <= n; i++) s = s "\t" v[i]; print s }'
}
first=1
for copy in stale-stat refreshed; do
  case $copy in stale-stat) d=$FRESH ;; *) d=$REFR ;; esac
  for i in 1 2 3 4 5 6; do
    timeout -s KILL 300 "$SIDES" worktree "$d" > "$H/wt.out" 2>&1 || echo "FAILED" >> "$H/wt.out"
    if [ $first = 1 ]; then
      { printf 'copy\trun'; awk -F'\t' 'NF == 2 { printf "\t%s", $1 }' "$H/wt.out"; echo; } > "$R/worktree.tsv"
      first=0
    fi
    # run 1 is the warm-up
    [ "$i" = 1 ] && continue
    kvrow "$copy	$((i - 1))" < "$H/wt.out" >> "$R/worktree.tsv"
  done
done

# --- three edits: ours vs diff-files ---------------------------------------------------
E=$BUILD/wire-server-edited
rm -rf "$E"
cp -a "$REFR" "$E"
git -C "$E" -c core.hooksPath=/dev/null -c core.fsmonitor=false update-index -q --refresh
files=$("$here/hgit.sh" "$E" ls-files -- 'libs/*.hs' | head -3)
f1=$(echo "$files" | sed -n 1p)
f2=$(echo "$files" | sed -n 2p)
f3=$(echo "$files" | sed -n 3p)
sleep 1
echo '-- edited' >> "$E/$f1"
touch "$E/$f2"
sed -i 's/$/\r/' "$E/$f3"
attr3=$("$here/hgit.sh" "$E" check-attr text eol -- "$f3" | tr '\n' ' ')
"$SIDES" worktree --list "$E" | awk -F'\t' 'NF == 3 && $1 == "dirty" { print $3 " (" $2 ")" }' | LC_ALL=C sort > "$H/ours"
$ENVS "$GIT" -C "$E" $FLAGS diff-files --name-only | LC_ALL=C sort > "$H/df"
{
  printf 'edit\tpath\n'
  printf 'content\t%s\ntouch\t%s\ncrlf\t%s (attrs: %s)\n' "$f1" "$f2" "$f3" "$attr3"
  printf 'ours\t%s\n' "$(tr '\n' ';' < "$H/ours")"
  printf 'diff-files\t%s\n' "$(tr '\n' ';' < "$H/df")"
} > "$R/wire-edits.tsv"
rm -rf "$E" "$H"
echo done
