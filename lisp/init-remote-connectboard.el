;;; init-remote-connectboard.el --- Compatibility entry points for Remote -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The former SSH/RPC entry board is now a compatibility shell around the
;; target-oriented framework.  New code should call `remote-board',
;; `remote-open-target', or the routing APIs directly.

;;; Code:

(require 'remote-board)

(defun my/remote-connectboard (&optional show-board)
  "Open a logical target, or show `remote-board' with SHOW-BOARD."
  (interactive "P")
  (if show-board
      (remote-board)
    (call-interactively #'remote-open-target)))

(defun my/remote-connectboard-dispatch (&rest _ignored)
  "Open the unified Remote board."
  (interactive)
  (remote-board))

(defalias 'my/remote-connectboard-refresh #'remote-board-refresh)
(defalias 'my/remote-connectboard-edit-config #'remote-edit-config)
(defalias 'my/remote-connectboard-copy #'remote-copy-target-uri)
(defalias 'my/remote-connectboard-open #'remote-open-target)
(defalias 'my/remote-connectboard-entries #'remote-target-list)

(my/leader!
  "o r" '(:def remote-open-target :which-key "remote open")
  "o R" '(:def remote-board :which-key "remote board")
  "o C" '(:def remote-edit-config :which-key "remote config"))

(provide 'init-remote-connectboard)
;;; init-remote-connectboard.el ends here
