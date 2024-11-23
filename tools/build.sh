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
            fractal.mth )
                make play-fractal
                ;;
            snake.mth )
                make play-snake
                ;;
            *.mth )
                ctarget="bin/${BaseName%.*}.c"
                bintarget="bin/${BaseName%.*}"
                makerules="$(cat Makefile | grep "$bintarget" | grep "$ctarget")"
                if [ -n "$makerules" ] ; then
                    bin/mirth2 --debug "$DirName/$BaseName" -o "$ctarget"
                    make "$bintarget"
                    "$bintarget"
                else
                    bin/mirth2 -c "$DirName/$BaseName"
                fi
                ;;
        esac
        ;;
    tools )
        case "$BaseName" in
            build.sh )
                echo "Hi :-)"
                ;;
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
            *.sh )
                bash tools/$BaseName
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
