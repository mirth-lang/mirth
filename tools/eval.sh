#!/bin/bash
set -euo pipefail
make bin/mirth2

readonly TMP=bin/eval
readonly PKG=bin/eval
rm -rf $TMP
mkdir -p $PKG

echo "module(eval.main)" > $PKG/main.mth
ls lib/std | grep .mth | sed 's/.mth/\)/' | sed 's/^/import\(std./' >> $PKG/main.mth
ls lib/arg-parser | grep .mth | sed 's/.mth/\)/' | sed 's/^/import\(arg-parser./' >> $PKG/main.mth
echo "def(main,+World -- +World," >> $PKG/main.mth
echo $* >> $PKG/main.mth
echo "?? 0 prim-posix-exit)" >> $PKG/main.mth
bin/mirth2 $PKG/main.mth -o $TMP/main.c
gcc $TMP/main.c -o $TMP/main
$TMP/main
