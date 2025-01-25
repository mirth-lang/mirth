#!/bin/bash

# Checks that all the examples compile under mirth.
set -euo pipefail
make bin/mirth2
for x in $(ls examples | grep .mth) ; do
    echo examples/$x
    bin/mirth2 -c examples/$x
done
