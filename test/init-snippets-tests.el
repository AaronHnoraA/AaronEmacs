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

(ert-deftest init-snippets-emacs-math-shortcuts-survive-reload ()
  "Punctuation math snippets expand in Emacs without shared catalog files."
  (yas-reload-all t)
  (dolist (case '((tex-mode ";" "$a$ ")
                  (tex-mode ":" "$$\na\n$$\n")
                  (markdown-mode ";" "\\(\\) ")
                  (markdown-mode ":" "\\[\n\n\\]\n")
                  (noema-research-mode ";" "\\(\\) ")
                  (noema-research-mode ":" "\\[\n\n\\]\n")))
    (with-temp-buffer
      (setq major-mode (nth 0 case))
      (yas-minor-mode 1)
      (insert (nth 1 case))
      (should (yas-expand))
      (should (equal (buffer-string) (nth 2 case))))))

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

(ert-deftest init-snippets-template-names-are-unique-per-table ()
  "Yasnippet keys templates by name; a duplicate silently drops a key."
  (dolist (directory (directory-files
                      (expand-file-name "snippets" user-emacs-directory)
                      t "\\`[^.]"))
    (when (file-directory-p directory)
      (let ((seen (make-hash-table :test #'equal)))
        (dolist (file (directory-files directory t "\\`[^.]"))
          (unless (file-directory-p file)
            (with-temp-buffer
              (insert-file-contents file)
              (when (re-search-forward "^# name: \\(.*\\)$" nil t)
                (let ((name (match-string 1)))
                  (should-not (gethash name seen))
                  (puthash name file seen))))))))))

(ert-deftest init-snippets-lsp-capf-does-not-shadow-template-group ()
  "lsp-mode's bare `company-capf' must not hide the group with Yasnippet."
  (with-temp-buffer
    (setq-local lsp-completion-mode t
                company-backends
                (cons 'company-capf (default-value 'company-backends)))
    (my/company-drop-shadowing-capf-h)
    (should (equal company-backends (default-value 'company-backends))))
  (with-temp-buffer
    (setq-local lsp-completion-mode t
                company-backends '(company-capf company-dabbrev))
    (my/company-drop-shadowing-capf-h)
    (should (equal company-backends '(company-capf company-dabbrev)))))

(ert-deftest init-snippets-empty-prefix-offers-only-typed-keys ()
  "At an empty prefix offer typed multi-character keys and math shortcuts."
  (with-temp-buffer
    (insert "p ..")
    (should
     (equal (my/company--typed-key-at-empty-prefix-a
             (lambda (&rest _) (list ".." "." "cpy" "for"))
             'candidates "")
            '("..")))
    (should
     (equal (my/company--typed-key-at-empty-prefix-a
             (lambda (&rest _) (list "for" "fori"))
             'candidates "fo")
            '("for" "fori"))))
  (with-temp-buffer
    (insert ":")
    (should (equal (my/company--typed-key-at-empty-prefix-a
                    (lambda (&rest _) '(":" ";" "." ".."))
                    'candidates "")
                   '(":")))))

(ert-deftest init-snippets-auctex-punctuation-appears-in-company ()
  "LaTeX offers the injected math shortcuts immediately after typing them."
  (with-temp-buffer
    (LaTeX-mode)
    (my/yas--enable-now)
    (require 'company-yasnippet)
    (dolist (key '(";" ":"))
      (erase-buffer)
      (insert key)
      (should (equal (company-yasnippet 'prefix) '("" . 1)))
      (should (seq-some (lambda (candidate) (string= candidate key))
                        (company-yasnippet 'candidates "")))
      (company-manual-begin)
      (should (seq-some (lambda (candidate) (string= candidate key))
                        company-candidates))
      (company-abort))))

(provide 'init-snippets-tests)
;;; init-snippets-tests.el ends here

(ert-deftest init-snippets-notebook-cells-follow-projection-language ()
  "A shared raw cell works in Python, JS, SQL and Lisp projections."
  (dolist (prefix '("#" "//" "--" ";"))
    (with-temp-buffer
      (setq-local my/noema-jupyter-cell-mode t
                  my/noema-jupyter-notebook--comment-prefix prefix)
      (yas-minor-mode 1)
      (should (memq 'jupyter-notebook-mode yas--extra-modes))
      (let ((template
             (with-temp-buffer
               (insert-file-contents
                (expand-file-name "snippets/jupyter-notebook-mode/jraw" user-emacs-directory))
               (goto-char (point-min))
               (search-forward "# --\n")
               (buffer-substring-no-properties (point) (point-max)))))
        (yas-expand-snippet template)
        (should (string-match-p
                 (concat "\\`" (regexp-quote prefix)
                         " %% \\[raw\\] id=[A-Za-z0-9_-]+\n"
                         (regexp-quote prefix) " ") (buffer-string))))
      (setq-local my/noema-jupyter-cell-mode nil)
      (my/yas-jupyter-setup)
      (should-not (memq 'jupyter-notebook-mode yas--extra-modes)))))
