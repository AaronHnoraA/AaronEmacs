;;; init-smerge.el --- Merge conflict helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'transient)

(declare-function evil-local-set-key "evil-core" (state key def))

(defun my/smerge-auto-enable ()
  "Enable `smerge-mode' when conflict markers are present."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(defun my/smerge-keep-upper-and-advance ()
  "Keep upper section and move to the next conflict."
  (interactive)
  (smerge-keep-upper)
  (ignore-errors (smerge-next)))

(defun my/smerge-keep-lower-and-advance ()
  "Keep lower section and move to the next conflict."
  (interactive)
  (smerge-keep-lower)
  (ignore-errors (smerge-next)))

(defun my/smerge-keep-all-and-advance ()
  "Keep all sections and move to the next conflict."
  (interactive)
  (smerge-keep-all)
  (ignore-errors (smerge-next)))

(defun my/smerge-keep-base-and-advance ()
  "Keep base section and move to the next conflict."
  (interactive)
  (smerge-keep-base)
  (ignore-errors (smerge-next)))

(defun my/smerge-setup-buffer ()
  "Install local merge-conflict key bindings for the current buffer."
  (local-set-key (kbd "q") #'my/smerge-dispatch)
  (local-set-key (kbd "n") #'smerge-next)
  (local-set-key (kbd "p") #'smerge-prev)
  (local-set-key (kbd "o") #'my/smerge-keep-upper-and-advance)
  (local-set-key (kbd "t") #'my/smerge-keep-lower-and-advance)
  (local-set-key (kbd "b") #'my/smerge-keep-all-and-advance)
  (local-set-key (kbd "B") #'my/smerge-keep-base-and-advance)
  (local-set-key (kbd "e") #'smerge-ediff)
  (when (featurep 'evil)
    (dolist (state '(normal motion visual))
      (evil-local-set-key state (kbd "q") #'my/smerge-dispatch)
      (evil-local-set-key state (kbd "n") #'smerge-next)
      (evil-local-set-key state (kbd "p") #'smerge-prev)
      (evil-local-set-key state (kbd "]c") #'smerge-next)
      (evil-local-set-key state (kbd "[c") #'smerge-prev)
      (evil-local-set-key state (kbd "o") #'my/smerge-keep-upper-and-advance)
      (evil-local-set-key state (kbd "t") #'my/smerge-keep-lower-and-advance)
      (evil-local-set-key state (kbd "b") #'my/smerge-keep-all-and-advance)
      (evil-local-set-key state (kbd "B") #'my/smerge-keep-base-and-advance)
      (evil-local-set-key state (kbd "e") #'smerge-ediff))))

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
             smerge-ediff)
  :hook (smerge-mode . my/smerge-setup-buffer))

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
