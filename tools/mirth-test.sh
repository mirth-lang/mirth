#!/usr/bin/env bash

C99FLAGS="-std=c99 -Wall -Wextra -Wno-unused-variable -Wno-unused-but-set-variable -Wno-unused-function -Wno-unused-parameter -Wno-unused-value -Wno-overlength-strings -Wno-sometimes-uninitialized -Werror -pedantic"
CC="gcc $C99FLAGS"

UPDATE=0
VERIFY=0
PRINT=0

case "$1" in
  -u ) UPDATE=1 ;;
  -v ) VERIFY=1 ;;
  -p ) PRINT=1 ;;
  * )
    echo "USAGE: ./mirth-test [-v|-u|-p] [test]"
    echo "  -v  Run tests."
    echo "  -u  Update tests."
    echo "  -p  Print test comments."
    exit 1 ;;
esac

if [ -n "$2" ]; then
    FILES="$2"
else
    FILES="$(ls test/*.mth)"
fi

set -euo pipefail

readonly TMP=$(mktemp -d)
make bin/mirth2 > $TMP/mkout 2> $TMP/mkerr || (cat $TMP/mkout && cat $TMP/mkerr && echo "Make failed." && exit 1)
cp bin/mirth2 $TMP/mirth
mkdir $TMP/test

FAILED=0

set +e
mkdir -p bin/test

for filepath in $FILES
do
    filename="$(basename $filepath)"
    targetname="${filename%.*}.c"
    targetpath="bin/test/$targetname"

    rm -f $TMP/test/*
    echo "${filepath}"
    rm -f "${targetpath}"
    $TMP/mirth --debug "$filepath" -o "${targetpath}"> $TMP/test/mout 2> $TMP/test/merr
    MIRTH_BUILD_FAILED=$?
    cat $TMP/test/mout | sed 's/^/# mirth-test # mout # /' >> $TMP/test/actual
    cat $TMP/test/merr | egrep ': (error|warning):' | sed 's/^[^:]*:/# mirth-test # merr # /' >> $TMP/test/actual
    if [ "$MIRTH_BUILD_FAILED" != "0" ] ; then
        echo "# mirth-test # mret # $MIRTH_BUILD_FAILED" >> $TMP/test/actual
    else
        echo "=> ${targetpath}"
        $CC -o $TMP/test/ctarget ${targetpath} > $TMP/test/cout 2> $TMP/test/cerr
        TARGET_FAILED=$?
        cat $TMP/test/cout | sed "s/^/# mirth-test # cout # /" >> $TMP/test/actual
        cat $TMP/test/cerr | sed "s/^/# mirth-test # cerr # /" >> $TMP/test/actual
        if [ "$TARGET_FAILED" != "0" ] ; then
            echo "# mirth-test # cret # $TARGET_FAILED" >> $TMP/test/actual
        else
            $TMP/test/ctarget > $TMP/test/pout 2> $TMP/test/perr
            PROGRAM_FAILED=$?
            cat $TMP/test/pout | sed "s/^/# mirth-test # pout # /" >> $TMP/test/actual
            cat $TMP/test/perr | sed "s/^/# mirth-test # perr # /" >> $TMP/test/actual
            if [ "$PROGRAM_FAILED" != "0" ] ; then
                echo "# mirth-test # pret # $PROGRAM_FAILED" >> $TMP/test/actual
            fi
        fi
    fi

    if [ "$VERIFY" == "1" ] ; then
        cat $filepath | grep "# mirth-test #" > $TMP/test/expected || echo -n
        diff --text --strip-trailing-cr $TMP/test/expected $TMP/test/actual
        DIFF_FAILED=$?
        if [ "$DIFF_FAILED" != "0" ] ; then
            echo "-- $filepath FAILED"
            FAILED=1
        fi
    elif [ "$UPDATE" == "1" ] ; then
        cat $filepath | grep -v "# mirth-test #" > $TMP/test/source || echo -n
        cat $TMP/test/source $TMP/test/actual > $filepath
    elif [ "$PRINT" == "1" ] ; then
        cat $TMP/test/actual
    fi
done

rm -rf $TMP

if [ "$FAILED" == "1" ] ; then
    echo "Golden tests failed."
    echo "Update golden tests with:"
    echo "    make test-update"
    exit 1
fi
