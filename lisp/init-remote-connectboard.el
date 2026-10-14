;;; init-remote-connectboard.el --- Compatibility entry points for Remote -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The former SSH/RPC entry board is now a compatibility shell around the
;; target-oriented framework.  New code should call `remote-board',
;; `remote-open-target', or the routing APIs directly.

;;; Code:

(require 'remote-board)

(defvar dired-mode-map)

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
  "o C" '(:def remote-edit-config :which-key "remote config")
  "o f" '(:def remote-files :which-key "remote files")
  "o M" '(:def remote-machines :which-key "remote machines"))

(defvar remote-files-terminal-function)
(declare-function my/ghostel--open-in-directory "init-ghostel"
                  (buffer-name directory))
(declare-function remote-files-default-terminal "remote-files"
                  (name directory))

(defun my/remote-files-terminal (name directory)
  "Open the file manager's terminal NAME in DIRECTORY with Ghostel."
  (if (fboundp 'my/ghostel--open-in-directory)
      (my/ghostel--open-in-directory name directory)
    (remote-files-default-terminal name directory)))

(with-eval-after-load 'remote-files
  (setq remote-files-terminal-function #'my/remote-files-terminal))

;; The transfer menu belongs to the folder shown.  In a folder popped up for
;; choosing the other side, the same key offers to confirm instead.
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-t") #'remote-files)
  (define-key dired-mode-map (kbd "C-c C-c") #'remote-files-pick-confirm)
  (define-key dired-mode-map (kbd "C-c C-k") #'remote-files-pick-cancel))

(autoload 'remote-files-pick-confirm "remote-files" nil t)
(autoload 'remote-files-pick-cancel "remote-files" nil t)

(provide 'init-remote-connectboard)
;;; init-remote-connectboard.el ends here
