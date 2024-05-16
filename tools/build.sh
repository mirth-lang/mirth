#!/bin/bash

set -euo pipefail

make
case "$1" in
    test/*.mth )
        bash tools/run.sh "$1"
        ;;
    examples/snake.mth )
        make play-snake
        ;;
    *.mth )
        bin/mirth2 -c "$1"
        ;;
esac
