#!/bin/bash

# @raycast.schemaVersion 1
# @raycast.title Emacs Terminal
# @raycast.mode silent
# @raycast.packageName Emacs
# @raycast.icon 🖥️
# @raycast.description Open a new Ghostel terminal in its own Emacs frame.

exec "$(dirname "$0")/../emacs-app" terminal
