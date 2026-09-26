;;; init-snippets-tests.el --- Snippet table regressions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-snippets)
(require 'yasnippet)

(ert-deftest init-snippets-stale-compiled-cache-is-ignored ()
  "A snippet newer than `.yas-compiled-snippets.el' must still load."
  (let* ((directory (file-name-as-directory (make-temp-file "yas-stale" t)))
         (compiled (expand-file-name ".yas-compiled-snippets.el" directory))
         (snippet (expand-file-name "fresh" directory)))
    (unwind-protect
        (progn
          (with-temp-file compiled (insert ";; empty compiled cache\n"))
          (set-file-times compiled (time-subtract (current-time) 3600))
          (with-temp-file snippet
            (insert "# -*- mode: snippet -*-\n# key: fresh\n# --\nfresh body"))
          (should (my/yas--compiled-snippets-stale-p directory))
          (let (sources)
            (cl-letf (((symbol-function 'yas--load-directory-2)
                       (lambda (dir _mode) (push dir sources))))
              (my/yas--skip-stale-compiled-a
               (lambda (&rest _) (ert-fail "Loaded the stale compiled cache"))
               directory 'probe-mode))
            (should (equal sources (list directory))))
          ;; A cache newer than every source is used as before.
          (set-file-times compiled (time-add (current-time) 3600))
          (should-not (my/yas--compiled-snippets-stale-p directory)))
      (delete-directory directory t))))

(ert-deftest init-snippets-jutext-uses-the-shared-research-catalog ()
  "JuText reads the Markdown/TeX catalog Noema shares with Emacs."
  (let ((parents (expand-file-name "snippets/noema-research-mode/.yas-parents"
                                   user-emacs-directory)))
    (should (file-readable-p parents))
    (should (member "markdown-mode"
                    (split-string (with-temp-buffer
                                    (insert-file-contents parents)
                                    (buffer-string)))))))

(ert-deftest init-snippets-global-table-holds-no-language-syntax ()
  "`fundamental-mode' snippets apply in every buffer; keep Lean out of them."
  (dolist (file (directory-files
                 (expand-file-name "snippets/fundamental-mode" user-emacs-directory)
                 t "\\`[^.]"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (re-search-forward
                   "^\\(theorem\\|lemma\\|structure\\|inductive\\|instance\\) " nil t)))))

(ert-deftest init-snippets-programmatic-expansion-enables-yasnippet ()
  "LSP completion expands snippets before the idle activation has run."
  (with-temp-buffer
    (prog-mode)
    (when (timerp my/yas--pending-enable)
      (cancel-timer my/yas--pending-enable))
    (yas-minor-mode -1)
    (yas-expand-snippet "printf(${1:format})$0")
    (should yas-minor-mode)
    (should (equal (buffer-string) "printf(format)"))
    (should (yas-active-snippets))))

(provide 'init-snippets-tests)
;;; init-snippets-tests.el ends here
