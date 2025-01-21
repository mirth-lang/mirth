#!/bin/bash

make bin/mirth2.c
diff -q bin/mirth0.c bin/mirth2.c || make update
make
