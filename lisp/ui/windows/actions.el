;;; actions.el --- Buffer and window operations using existing commands -*- lexical-binding: t; -*-

;;; Code:

(require 'transient)

(transient-define-prefix buffer-actions-menu ()
  "Create, find, hide and end buffers through the existing lifecycle."
  [["Create / open"
    ("f" "file / new filename" file-open)
    ("s" "persistent scratch" persistent-scratch)
    ("p" "project scratch" project-scratch)
    ("c" "indirect clone" clone-indirect-buffer)]
   ["Find"
    ("b" "all buffers" consult-buffer)
    ("w" "workspace buffers" +workspace/switch-buffer)
    ("i" "Ibuffer" ibuffer)
    ("t" "terminals" terminal-switcher)
    ("a" "agents" agent-manager)]
   ["Manage"
    ("r" "rename" rename-buffer)
    ("g" "revert" revert-buffer-quick)
    ("h" "hide view" +windows/close-view)
    ("k" "close current (DWIM)" +windows/kill-buffer-dwim)]
   ["Clean up"
    ("K" "other workspace buffers" +windows/kill-workspace-other-buffers)
    ("u" "undisplayed buffers" kill-buried-buffers)]])

(transient-define-prefix window-actions-menu ()
  "Arrange work panes and tool views without duplicating their ownership."
  [["Split / focus"
    ("v" "split right" +windows/split-right)
    ("b" "split below" +windows/split-below)
    ("o" "choose window" ace-window)
    ("s" "swap windows" +windows/swap-window-dwim)]
   ["Layout"
    ("m" "maximize / restore" +windows/toggle-delete-other-windows)
    ("u" "undo layout" winner-undo)
    ("U" "redo layout" winner-redo)
    ("d" "close pane" +windows/close-window)]
   ["Tools / workspace"
    ("t" "terminals" terminal-switcher)
    ("a" "agents" agent-manager)
    ("w" "workspaces" workspace-menu)]])

(provide 'ui/windows/actions)
;;; actions.el ends here
