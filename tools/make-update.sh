#!/bin/bash

make bin/mirth3.c
diff -q bin/mirth0.c bin/mirth3.c || make update
make
