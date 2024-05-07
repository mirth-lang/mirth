#!/bin/bash
set -euo pipefail
make bin/mirth2debug
bin/mirth2debug -p std:lib/std -p mirth:src -p arg-parser:lib/arg-parser \
           -p examples:examples -p mirth-tests:test --debug -o bin/test.c $1
echo "Compiling."
gcc -o bin/test bin/test.c
echo "Running."
bin/test
