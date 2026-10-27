;;; init-first-use-modes-tests.el --- First-use mode activation -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'saveplace)
(require 'savehist)

(ert-deftest first-use-file-modes-start-before-the-first-visit ()
  (let* ((dir (file-truename (make-temp-file "first-use-file-" t)))
         (file (expand-file-name "note.txt" dir))
         (save-place-file (expand-file-name "places" dir))
         (recentf-save-file (expand-file-name "recentf" dir))
         (save-place-alist (list (cons (abbreviate-file-name file) 4)))
         (save-place-loaded t)
         visited)
    (unwind-protect
        (progn
          (with-temp-file file (insert "abcdef\n"))
          ;; Batch loads init.el through -l after Emacs has already run
          ;; after-init-hook.  Model the interactive startup activation.
          (should (memq 'save-place-mode after-init-hook))
          (save-place-mode 1)
          (should-not recentf-mode)
          (should-not global-so-long-mode)
          (should-not global-auto-revert-mode)
          ;; Batch startup leaves first-use triggers unwired; wire the same
          ;; before-after-find-file trigger used in an interactive session.
          (aaron-run-hook-on 'aaron-first-file-hook '(find-file-hook))
          (setq visited (find-file-noselect file))
          (with-current-buffer visited
            (should save-place-mode)
            (should recentf-mode)
            (should global-so-long-mode)
            (should global-auto-revert-mode)
            (should auto-revert--global-mode)
            (should (= (point) 4))))
      (when (buffer-live-p visited)
        (kill-buffer visited))
      (when global-auto-revert-mode (global-auto-revert-mode -1))
      (when global-so-long-mode (global-so-long-mode -1))
      (when recentf-mode (recentf-mode -1))
      (when save-place-mode (save-place-mode -1))
      (delete-directory dir t))))

(ert-deftest first-use-input-and-buffer-modes-start-on-their-events ()
  (let ((savehist-file (make-temp-file "first-use-history-"))
        (input-hooks (cl-remove-if-not
                      (lambda (fn) (memq fn '(savehist-mode which-key-mode)))
                      aaron-first-input-hook))
        (buffer-hooks (cl-remove-if-not
                       (lambda (fn) (eq fn 'gcmh-mode))
                       aaron-first-buffer-hook)))
    (unwind-protect
        (progn
          (should (equal (sort (copy-sequence input-hooks)
                               (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                         '(savehist-mode which-key-mode)))
          (should (equal buffer-hooks '(gcmh-mode)))
          (should-not savehist-mode)
          (should-not which-key-mode)
          (should-not gcmh-mode)
          (should show-paren-mode)
          (let ((aaron-first-input-hook input-hooks)
                (aaron-first-buffer-hook buffer-hooks))
            (aaron-run-hooks 'aaron-first-input-hook 'aaron-first-buffer-hook))
          (should savehist-mode)
          (should which-key-mode)
          (should gcmh-mode)
          (should show-paren-mode))
      (when savehist-mode (savehist-mode -1))
      (when which-key-mode (which-key-mode -1))
      (when gcmh-mode (gcmh-mode -1))
      (delete-file savehist-file))))

;;; init-first-use-modes-tests.el ends here
