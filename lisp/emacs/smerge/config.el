;;; emacs/smerge/config.el --- Merge conflict helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-funcs)
(require 'smerge-mode)
(require 'transient)

(declare-function evil-define-minor-mode-key "evil-core" (state mode key def &rest bindings))

(defun +smerge-auto-enable ()
  "Enable `smerge-mode' when conflict markers are present."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(defun +smerge/keep-upper-and-advance ()
  "Keep upper section and move to the next conflict."
  (interactive)
  (smerge-keep-upper)
  (ignore-errors (smerge-next)))

(defun +smerge/keep-lower-and-advance ()
  "Keep lower section and move to the next conflict."
  (interactive)
  (smerge-keep-lower)
  (ignore-errors (smerge-next)))

(defun +smerge/keep-all-and-advance ()
  "Keep all sections and move to the next conflict."
  (interactive)
  (smerge-keep-all)
  (ignore-errors (smerge-next)))

(defun +smerge/keep-base-and-advance ()
  "Keep base section and move to the next conflict."
  (interactive)
  (smerge-keep-base)
  (ignore-errors (smerge-next)))

(add-hook 'find-file-hook #'+smerge-auto-enable)

(with-eval-after-load 'evil
  ;; Evil's minor-mode map disappears with `smerge-mode'; buffer-local keys do
  ;; not, and would also intercept ordinary text in insert/Emacs state.
  (evil-define-minor-mode-key
   'normal 'smerge-mode
   (kbd "q") #'merge-conflict-menu
   (kbd "n") #'smerge-next
   (kbd "p") #'smerge-prev
   (kbd "]c") #'smerge-next
   (kbd "[c") #'smerge-prev
   (kbd "o") #'+smerge/keep-upper-and-advance
   (kbd "t") #'+smerge/keep-lower-and-advance
   (kbd "b") #'+smerge/keep-all-and-advance
   (kbd "B") #'+smerge/keep-base-and-advance
   (kbd "e") #'smerge-ediff))

(transient-define-prefix merge-conflict-menu ()
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

(define-leader-key!
  "g m" '(:def merge-conflict-menu :which-key "merge conflict"))

(provide 'emacs/smerge/config)
;;; emacs/smerge/config.el ends here
