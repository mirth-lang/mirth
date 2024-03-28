#!/usr/bin/env bash

C99FLAGS="-std=c99 -Wall -Wextra -Wno-unused-variable -Wno-unused-function -Wno-unused-parameter -Wno-unused-value -Wno-overlength-strings -Werror -pedantic"
CC="gcc $C99FLAGS"

UPDATE=0
VERIFY=0

case "$1" in
  -u ) UPDATE=1 ;;
  -v ) VERIFY=1 ;;
  * )
    echo "USAGE: ./mirth-test [-v|-u]"
    echo "  -v  Run tests."
    echo "  -u  Update tests."
    exit 1 ;;
esac

set -euo pipefail

readonly TMP=$(mktemp -d)

make bin/mirth2.c > $TMP/mkout 2> $TMP/mkerr || (cat $TMP/mkout && cat $TMP/mkerr && echo "Make failed." && exit 1)
$CC -o $TMP/mirth bin/mirth2.c || (echo "Mirth build failed." && exit 1)
mkdir $TMP/test

FAILED=0

set +e

for filename in $(find src/tests -name "*.mth" -type f -printf '%f\n')
do
    rm -f $TMP/test/*
    echo tests/$filename
    binary_name="${filename%.*}"
    $TMP/mirth tests/$filename  -o "${binary_name}.c"> $TMP/test/mout 2> $TMP/test/merr
    MIRTH_BUILD_FAILED=$?
    cat $TMP/test/mout | sed 's/^/# mirth-test # mout # /' >> $TMP/test/actual
    cat $TMP/test/merr | egrep ': (error|warning):' | sed 's/^[^:]*:/# mirth-test # merr # /' >> $TMP/test/actual
    if [ "$MIRTH_BUILD_FAILED" != "0" ] ; then
        echo "# mirth-test # mret # $MIRTH_BUILD_FAILED" >> $TMP/test/actual
	rm -f bin/${binary_name}
    else
            echo "=> bin/${binary_name}"
            $CC -o $TMP/test/ctarget bin/${binary_name}.c > $TMP/test/cout 2> $TMP/test/cerr
            TARGET_FAILED=$?
            rm -f bin/${binary_name}
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
        cat src/tests/$filename | grep "mirth-test" > $TMP/test/expected || echo -n
        diff --text --strip-trailing-cr $TMP/test/expected $TMP/test/actual
        DIFF_FAILED=$?
        if [ "$DIFF_FAILED" != "0" ] ; then
            echo "tests/$filename FAILED"
            FAILED=1
        fi
    elif [ "$UPDATE" == "1" ] ; then
        cat src/tests/$filename | grep -v "mirth-test" > $TMP/test/source || echo -n
        cat $TMP/test/source $TMP/test/actual > src/tests/$filename
    fi
done

rm -rf $TMP

if [ "$FAILED" == "1" ] ; then
    echo "Golden tests failed."
    echo "Update golden tests with:"
    echo "    make test-update"
    exit 1
fi
