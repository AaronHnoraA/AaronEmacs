;;; init-smartparens.el --- Pair editing -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)

(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function smartparens-mode "smartparens" (&optional arg))

(defconst my/pairs-delimiter-cycle
  '((?\( . ?\))
    (?\[ . ?\])
    (?{ . ?}))
  "Delimiter pairs cycled by `my/pairs-cycle-delimiters'.")

(defun my/pairs--cycle-syntax-table ()
  "Return a syntax table recognizing all supported delimiter pairs."
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    table))

(defun my/pairs--delimiter-bounds-at-point ()
  "Return the enclosing supported delimiter bounds around point.
The result is (OPEN-POS . CLOSE-POS), with both positions on delimiters."
  (with-syntax-table (my/pairs--cycle-syntax-table)
    (let* ((ppss (syntax-ppss))
           (open-chars (mapcar #'car my/pairs-delimiter-cycle))
           (close-chars (mapcar #'cdr my/pairs-delimiter-cycle))
           (open-pos
            (cond
             ((nth 8 ppss)
              (user-error "Point is inside a string or comment"))
             ((memq (char-after) open-chars)
              (point))
             ((memq (char-before) open-chars)
              (1- (point)))
             ((memq (char-before) close-chars)
              (condition-case nil
                  (scan-sexps (point) -1)
                (scan-error nil)))
             ((nth 1 ppss)
              (nth 1 ppss)))))
      (unless (and open-pos (memq (char-after open-pos) open-chars))
        (user-error "No enclosing (), [] or {} pair"))
      (let* ((open-char (char-after open-pos))
             (close-pos
              (condition-case nil
                  (1- (scan-sexps open-pos 1))
                (scan-error nil)))
             (expected-close (cdr (assq open-char my/pairs-delimiter-cycle))))
        (unless (and close-pos (eq (char-after close-pos) expected-close))
          (user-error "Delimiter at point is unbalanced"))
        (cons open-pos close-pos)))))

;;;###autoload
(defun my/pairs-cycle-delimiters (&optional arg)
  "Cycle the enclosing delimiters among (), [] and {}.
Point may be on either delimiter or anywhere inside the pair.  With a
negative prefix ARG, cycle backwards.  Other numeric values move by that
many steps."
  (interactive "p")
  (pcase-let* ((`(,open-pos . ,close-pos)
                (my/pairs--delimiter-bounds-at-point))
               (old-open (char-after open-pos))
               (old-close (char-after close-pos))
               (index (cl-position old-open my/pairs-delimiter-cycle
                                   :key #'car))
               (step (or arg 1))
               (new-pair (nth (mod (+ index step)
                                   (length my/pairs-delimiter-cycle))
                              my/pairs-delimiter-cycle)))
    (save-excursion
      (atomic-change-group
        (subst-char-in-region close-pos (1+ close-pos)
                              old-close (cdr new-pair))
        (subst-char-in-region open-pos (1+ open-pos)
                              old-open (car new-pair))))
    (message "%c%c → %c%c"
             old-open old-close (car new-pair) (cdr new-pair))
    new-pair))

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
