#!/bin/bash
set -uo pipefail
make bin/mirth2debug && bin/mirth2debug --debug -p std:src/std -p posix:src/posix -p mirth:src/mirth -p args:src/args -p snake:src/snake -p mirth-tests:src/mirth-tests --debug -o bin/test.c $1 && gcc -g -o bin/test bin/test.c && bin/test
