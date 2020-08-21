#!/usr/bin/env bash

UPDATE=0
VERIFY=0

case "$1" in
  -u ) UPDATE=1 ;;
  -v ) VERIFY=1 ;;
  * )
    echo "USAGE: ./mirth-test [-v|-u]"
    echo "  -v  Run tests (i.e. verify program)."
    echo "  -u  Update tests."
    exit 1 ;;
esac

set -euo pipefail

make -q bin/mirth1

readonly TMP=$(mktemp -d)

for filename in $(ls src/tests/ | grep .mth)
do
    rm -f $TMP/*
    echo tests/$filename
    echo 0 > $TMP/exit
    bin/mirth1 tests/$filename > $TMP/mout 2> $TMP/merr || echo 1 > $TMP/exit
    cat $TMP/mout | sed 's/^/# mirth-test # mout # /' >> $TMP/actual
    cat $TMP/merr | sed 's/^/# mirth-test # merr # /' >> $TMP/actual
    cat $TMP/exit | sed 's/^/# mirth-test # exit # /' >> $TMP/actual

    if [ "$VERIFY" == "1" ] ; then
        cat src/tests/$filename | grep "mirth-test" > $TMP/expected || echo -n
        diff $TMP/expected $TMP/actual || (echo "tests/$filename FAILED" && exit 1)
    elif [ "$UPDATE" == "1" ] ; then
        cat src/tests/$filename | grep -v "mirth-test" > $TMP/source || echo -n
        cat $TMP/source $TMP/actual > src/tests/$filename
    fi
done
