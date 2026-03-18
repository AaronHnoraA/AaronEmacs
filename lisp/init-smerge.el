;;; init-smerge.el --- Merge conflict helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'transient)

(defun my/smerge-auto-enable ()
  "Enable `smerge-mode' when conflict markers are present."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook #'my/smerge-auto-enable)

(use-package smerge-mode
  :ensure nil
  :commands (smerge-mode
             smerge-next
             smerge-prev
             smerge-keep-upper
             smerge-keep-lower
             smerge-keep-base
             smerge-keep-all
             smerge-ediff))

(transient-define-prefix my/smerge-dispatch ()
  "Merge conflict dispatch."
  [["Move"
    ("n" "next" smerge-next)
    ("p" "previous" smerge-prev)]
   ["Keep"
    ("u" "upper/ours" smerge-keep-upper)
    ("l" "lower/theirs" smerge-keep-lower)
    ("b" "base" smerge-keep-base)
    ("a" "all" smerge-keep-all)]
   ["Resolve"
    ("e" "ediff" smerge-ediff)
    ("q" "quit mode" smerge-mode)]])

(my/leader-key-label "g" "git/merge")
(my/evil-global-leader-set "g m" #'my/smerge-dispatch "merge conflict")

(provide 'init-smerge)
;;; init-smerge.el ends here
