#!/usr/bin/env bash
# Cost grid: one process per sample. Writes results/alloc.tsv.
# Usage: ./alloc.sh BIN [SAMPLES]
# IMPLS="a b" runs only those impls on the grid. APPEND=1 adds rows to
# results/alloc.tsv (no header, no end-to-end part).
set -u
# S1 repeats servant's tutorial Server.lhs (servant at c12cc37), from
# the main checkout's .worktrees/.build cache.
root=$(dirname "$(git rev-parse --path-format=absolute --git-common-dir)")
export SEED=${SEED:-$root/.worktrees/.build/t13a-projects/servant/doc/tutorial/Server.lhs}
B=$1
N=${2:-3}
IMPLS=${IMPLS:-none ours-md lib-raw lib-layout ours-loose lib-loose lib-has-block}
APPEND=${APPEND:-0}
out=results/alloc.tsv
if [ "$APPEND" != 1 ]; then
  printf 'impl\tshape\ttarget\tinput_chars\talloc_bytes\twall_ms\tmax_live\tmax_mem\tresult\n' > "$out"
fi
for size in 65536 262144 1048576; do
  for s in S1 S2 S3 S4 S5 S6 S7 S8 S9 S10 S11 S12 S13; do
    for impl in $IMPLS; do
      for _ in $(seq "$N"); do
        if ! line=$(timeout -s KILL 20 "$B" alloc "$impl" "$s" "$size" +RTS -T -RTS); then
          printf '%s\t%s\t%s\tTIMEOUT\n' "$impl" "$s" "$size" >> "$out"
          break
        fi
        echo "$line" >> "$out"
      done
    done
  done
done
[ "$APPEND" = 1 ] && exit 0
# End to end on S1: unlit + parse.
for size in 65536 262144 1048576; do
  for impl in ours-md lib-layout parse-ours parse-lib; do
    for _ in $(seq "$N"); do
      line=$(timeout -s KILL 60 "$B" alloc "$impl" S1 "$size" +RTS -T -RTS) \
        || line=$(printf '%s\tS1\t%s\tTIMEOUT' "$impl" "$size")
      echo "$line" >> "$out"
    done
  done
done
