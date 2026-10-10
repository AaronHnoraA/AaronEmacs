#!/bin/bash

# @raycast.schemaVersion 1
# @raycast.title Emacs M-x
# @raycast.mode silent
# @raycast.packageName Emacs
# @raycast.icon ⌨️
# @raycast.description Run an Emacs command from a floating minibuffer frame.

exec "$(dirname "$0")/../emacs-app" mx
