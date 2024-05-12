#!/bin/sh

set -euo pipefail

rm -rf $HOME/.mirth
mkdir -p $HOME/.mirth/bin
cp $1 $HOME/.mirth/bin/mirth
echo "Mirth installed at ~/.mirth"
echo 'Please add mirth to your PATH:'
echo
echo '    export PATH=$HOME/.mirth/bin:$PATH'
