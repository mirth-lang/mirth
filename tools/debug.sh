#!/bin/bash
set -euo pipefail
make bin/mirth2debug
bin/mirth2debug --debug -o bin/test.c $1
echo "Compiling."
gcc -o bin/test bin/test.c
echo "Running."
bin/test
