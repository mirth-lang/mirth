#!/bin/bash
set -euo pipefail
make bin/mirth2
bin/mirth2 -o bin/test.c $1
echo "Compiling."
gcc -o bin/test bin/test.c
echo "Running."
bin/test
