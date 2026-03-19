;;; init-smartparens.el --- Pair editing -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function smartparens-mode "smartparens" (&optional arg))

(defun my/pairs-disable-smartparens ()
  "Disable stale `smartparens-mode' state when reloading the init."
  (remove-hook 'prog-mode-hook #'smartparens-mode)
  (remove-hook 'text-mode-hook #'smartparens-mode)
  (when (fboundp 'smartparens-mode)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p smartparens-mode)
          (smartparens-mode -1))))))

(defun my/pairs--inside-string-or-comment-p ()
  "Return non-nil when point is inside a string or comment."
  (nth 8 (syntax-ppss)))

(defun my/pairs--between-empty-delimiters-p ()
  "Return non-nil when point is inside an empty (), [] or {} pair."
  (let ((origin (point)))
    (save-excursion
      (skip-chars-backward " \t")
      (let ((open (char-before)))
        (and open
             (memq open '(?\( ?\[ ?\{))
             (progn
               (goto-char origin)
               (skip-chars-forward " \t")
               (eq (char-after) (matching-paren open))))))))

(defun my/pairs-newline ()
  "Insert a VSCode-like newline inside empty delimiter pairs."
  (interactive)
  (if (or (use-region-p)
          (my/pairs--inside-string-or-comment-p)
          (not (my/pairs--between-empty-delimiters-p)))
      (newline-and-indent)
    (let ((left (save-excursion
                  (skip-chars-backward " \t")
                  (point)))
          (right (save-excursion
                   (skip-chars-forward " \t")
                   (point))))
      (delete-region left right)
      (goto-char left)
      (newline)
      (save-excursion
        (newline))
      (indent-according-to-mode)
      (save-excursion
        (forward-line 1)
        (indent-according-to-mode)))))

(defvar my/pairs-newline-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'my/pairs-newline)
    map)
  "Keymap for `my/pairs-newline-mode'.")

(define-minor-mode my/pairs-newline-mode
  "Use `my/pairs-newline' for RET in programming buffers."
  :lighter nil
  :keymap my/pairs-newline-mode-map)

(defun my/pairs-enable-existing-prog-buffers ()
  "Enable `my/pairs-newline-mode' in existing programming buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'prog-mode)
        (my/pairs-newline-mode 1)))))

(use-package elec-pair
  :ensure nil
  :demand t
  :init
  ;; Prefer the built-in pairing engine: it only deletes adjacent empty
  ;; pairs and supports newline expansion between delimiters.
  (my/pairs-disable-smartparens)
  :custom
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-open-newline-between-pairs t)
  (electric-pair-skip-whitespace t)
  :config
  (electric-pair-mode 1)
  (add-hook 'prog-mode-hook #'my/pairs-newline-mode)
  (my/pairs-enable-existing-prog-buffers)
  (with-eval-after-load 'evil
    (evil-define-key* 'insert my/pairs-newline-mode-map
      (kbd "RET") #'my/pairs-newline)))

(provide 'init-smartparens)



;;; init-smartparens.el ends here
