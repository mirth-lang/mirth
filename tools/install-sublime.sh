#!/bin/bash

LIN_PATH=~/.config/sublime-text-3
OSX_PATH=~/Library/"Application Support"/"Sublime Text"
WIN_PATH="$APPDATA/Sublime Text"

set -euo pipefail

SCRIPT_PATH="${BASH_SOURCE[0]:-$0}"
SCRIPT_DIR="$(dirname "$SCRIPT_PATH")"

if [ -d "$OSX_PATH" ] ; then
	CONFIG_PATH="$OSX_PATH"
elif [ -d "$LIN_PATH" ] ; then
	CONFIG_PATH="$LIN_PATH"
elif [ -d "$WIN_PATH" ] ; then
	CONFIG_PATH="$WIN_PATH"
else
	echo "Failed to find sublime config dir. Please install $SCRIPT_DIR/mirth-sublime manually."
	exit 1
fi

echo "Found sublime config dir at '$CONFIG_PATH'"
PACKAGE_PATH="$CONFIG_PATH/Packages/Mirth"

echo "Copying '$SCRIPT_DIR/mirth-sublime' to '$PACKAGE_PATH'"

rm -f "$PACKAGE_PATH/*"
cp -r "$SCRIPT_DIR/mirth-sublime" "$PACKAGE_PATH"
