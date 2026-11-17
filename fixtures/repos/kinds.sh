#!/bin/sh
# A repo with one of each kind of entry, in one commit. ROOT is proj/;
# outside.txt and other/ are outside it.
#
# Usage: /bin/sh kinds.sh DIR [sha1|sha256]
#
# proj/sub (a gitlink) and the non-UTF-8 name proj/caf\351.txt are in
# the index and the commit only, not on disk: macOS (APFS) refuses
# non-UTF-8 names.
. "$(dirname "$0")/lib.sh"
d=$1
format=${2:-sha1}
repo_init "$d" "$format"
mkdir -p "$d/proj" "$d/other"
echo top > "$d/outside.txt"
printf 'module X where\n' > "$d/other/x.hs"
p=$d/proj
printf 'module A where\n\nx :: Int\nx = 1\n' > "$p/a.hs"
printf '#!/bin/sh\necho hi\n' > "$p/run.sh"
chmod 755 "$p/run.sh"
: > "$p/empty.txt"
ln -s a.hs "$p/link"
printf 'bin\000ary\001\002\n' > "$p/data.dat"
# 40000 lines of 29 bytes: over 1 MiB.
awk 'BEGIN { for (i = 0; i < 40000; i++) printf "line %05d of a big text file\n", i }' > "$p/big.txt"
printf '*.bin filter=lfs diff=lfs merge=lfs -text\n' > "$p/.gitattributes"
cat > "$p/lfs.bin" <<'PTR'
version https://git-lfs.github.com/spec/v1
oid sha256:4d7a214614ab2935c943f9e0ff69d22eadbb8f32b1258daaa5e2ca24d17e2393
size 12
PTR
g -C "$d" add -A
case $format in
  sha256) sub=1111111111111111111111111111111111111111111111111111111111111111 ;;
  *) sub=1111111111111111111111111111111111111111 ;;
esac
blob=$(printf 'caf\n' | g -C "$d" hash-object -w --stdin)
printf '160000 %s\tproj/sub\000100644 %s\tproj/caf\351.txt\000' "$sub" "$blob" |
  g -C "$d" update-index -z --index-info
commit_at "$d" 0 -m kinds
