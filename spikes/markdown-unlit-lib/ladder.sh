#!/usr/bin/env bash
# S8/S9 ladders: size doubles from 16 bytes to 1 MiB, stop at first
# timeout. Writes results/ladder.tsv. Usage: ./ladder.sh BIN
set -u
B=$1
out=results/ladder.tsv
printf 'impl\tshape\ttarget\tinput_chars\talloc_bytes\twall_ms\tmax_live\tmax_mem\tresult\n' > "$out"
for s in S8 S9; do
  for impl in lib-raw lib-layout lib-loose; do
    size=16
    while [ "$size" -le 1048576 ]; do
      if ! line=$(timeout -s KILL 20 "$B" alloc "$impl" "$s" "$size" +RTS -T -RTS); then
        printf '%s\t%s\t%s\tTIMEOUT\n' "$impl" "$s" "$size" >> "$out"
        break
      fi
      echo "$line" >> "$out"
      size=$((size * 2))
    done
  done
done
