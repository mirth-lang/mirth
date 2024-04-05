#!/bin/bash
set -uo pipefail
make bin/mirth2 && bin/mirth2 -p std:src/std -p posix:src/posix -p mirth:src/mirth -p args:src/args -p snake:src/snake -p mirth-tests:src/mirth-tests --debug -o bin/test.c $1 && gcc -o bin/test bin/test.c && bin/test
