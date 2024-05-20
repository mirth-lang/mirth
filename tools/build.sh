#!/bin/bash

set -euo pipefail

DirName="$(dirname "$1")"
BaseName="$(basename "$1")"

make
case "$DirName" in
    test )
        case "$BaseName" in
            *.mth )
                bash tools/mirth-test.sh -u "$DirName/$BaseName"
                ;;
        esac
        ;;
    examples )
        case "$BaseName" in
            snake.mth )
                make play-snake
                ;;
            *.mth )
                bin/mirth2 -c "$DirName/$BaseName"
                ;;
        esac
        ;;
    tools )
        case "$BaseName" in
            make-update.sh )
                bash tools/make-update.sh
                ;;
            mirth-test.sh )
                bash tools/mirth-test.sh -v
                ;;
            build32.bat|build64.bat )
                powershell $1
                diff --strip-trailing-cr bin/mirth3.c bin/wmirth3.c
                ;;
        esac
        ;;
    * )
        case "$BaseName" in
            *.mth )
                bin/mirth2 -c "$1"
                ;;
        esac
        ;;
esac
