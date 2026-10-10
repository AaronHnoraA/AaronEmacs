#!/bin/bash

# @raycast.schemaVersion 1
# @raycast.title Emacs M-x
# @raycast.mode silent
# @raycast.packageName Emacs
# @raycast.icon ⌨️
# @raycast.description Run an Emacs command from a floating minibuffer frame.

CLIENT=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient

# The application to hand the keyboard back to when the read is cancelled.
front=$(lsappinfo info -only bundleid "$(lsappinfo front)" \
            | sed -nE 's/.*[bB]undle(ID|Identifier)"?="([^"]+)".*/\2/p' | head -n1)
[ "$front" = "org.gnu.Emacs" ] && front=""

if ! "$CLIENT" --eval "(my/global-mx \"$front\")" >/dev/null 2>&1; then
    echo "Emacs server is not running"
    exit 1
fi
