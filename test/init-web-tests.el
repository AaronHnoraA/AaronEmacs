;;; init-web-tests.el --- Tests for HTML, CSS and Vue support -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Vue files open in web-mode with the configured indentation and get the
;; +vue language server; CSS indentation comes from the config store.  Run
;; with the full configuration loaded (scripts/test-all).

;;; Code:

(require 'ert)
(require 'cl-lib)

(ert-deftest web-vue-files-open-in-web-mode-with-store-indentation ()
  (let ((file (make-temp-file "web-" nil ".vue" "<template>\n<div/>\n</template>\n")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (unwind-protect
              (progn
                (should (derived-mode-p 'web-mode))
                (should (eql web-mode-markup-indent-offset +web-indent-offset))
                (should (eql web-mode-code-indent-offset +web-indent-offset))
                (should-not indent-tabs-mode)
                (should-not electric-indent-mode))
            (kill-buffer)))
      (delete-file file))))

(ert-deftest web-css-indentation-is-a-config-setting ()
  (should (eq (plist-get (gethash 'css-indent-offset config--registry) :kind) 'variable))
  (should (eql (config-get 'css-indent-offset) 2)))

(ert-deftest web-vue-language-server-is-registered-for-vue-files-only ()
  (require 'lsp-mode)
  (let ((client (gethash '+vue lsp-clients)))
    (should client)
    (should (memq 'web-mode (lsp--client-major-modes client)))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/a.vue")
      (web-mode)
      (should (funcall (lsp--client-activation-fn client) buffer-file-name 'web-mode))
      (setq buffer-file-name "/tmp/a.html")
      (should-not (funcall (lsp--client-activation-fn client) buffer-file-name 'web-mode))
      (set-buffer-modified-p nil)
      (setq buffer-file-name nil))))

(ert-deftest web-default-server-ids-carry-no-legacy-prefix ()
  (should-not (string-match-p
               "my-"
               (prin1-to-string
                (symbol-function 'set-language-server!)))))

(ert-deftest +javascript-module-typescript-files-open-like-ts ()
  "TypeScript's .mts and .cts files take the mode .ts has."
  (require 'lang/javascript/config)
  (let ((ts (assoc-default "x.ts" auto-mode-alist #'string-match))
        (dir (make-temp-file "mts-" t)))
    (should ts)
    (unwind-protect
        (dolist (name '("a.mts" "b.cts"))
          (let ((file (expand-file-name name dir)))
            (write-region "export const a = 1;\n" nil file nil 'silent)
            (with-current-buffer (find-file-noselect file)
              (unwind-protect (should (derived-mode-p ts))
                (kill-buffer)))))
      (delete-directory dir t))))

(ert-deftest +javascript-node-stack-frames-are-compilation-errors ()
  "Node stack frames in compilation output point at their file and line."
  (require 'compile)
  (require 'lang/javascript/config)
  (let ((regexp (cadr (assq 'node compilation-error-regexp-alist-alist))))
    (should (memq 'node compilation-error-regexp-alist))
    (should (string-match regexp "    at render (/srv/app/src/view.js:42:7)"))
    (should (equal (match-string 1 "    at render (/srv/app/src/view.js:42:7)")
                   "/srv/app/src/view.js"))
    (should (string-match regexp "    at /srv/app/index.mjs:3:15"))
    (should (equal (list (match-string 1 "    at /srv/app/index.mjs:3:15")
                         (match-string 2 "    at /srv/app/index.mjs:3:15")
                         (match-string 3 "    at /srv/app/index.mjs:3:15"))
                   '("/srv/app/index.mjs" "3" "15")))))

(ert-deftest +filetypes-json-lock-files-are-not-toml ()
  "flake.lock, composer.lock and Pipfile.lock are JSON; Cargo.lock stays TOML."
  (require 'lang/filetypes/config)
  (cl-flet ((mode (file) (assoc-default file auto-mode-alist #'string-match)))
    (let ((json (mode "/p/data.json"))
          (toml (mode "/p/Cargo.lock")))
      (dolist (file '("/p/flake.lock" "/p/composer.lock" "/p/Pipfile.lock"))
        (should (eq (mode file) json)))
      (should-not (eq toml json))
      (should (eq (mode "/p/uv.lock") toml))
      ;; From Doom's lang/json and lang/data.
      (should (eq (mode "/p/.jshintrc") json))
      (should (eq (mode "/p/style.xslt") (mode "/p/data.xml"))))))

(ert-deftest +tree-sitter-visiting-never-offers-to-install-a-grammar ()
  "Opening a file whose grammar is missing does not ask to build it."
  (require 'treesit)
  (should (eq treesit-auto-install-grammar 'never))
  (let ((dir (make-temp-file "grammar-" t))
        asked)
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (setq asked t) nil))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (setq asked t) nil)))
          (let ((file (expand-file-name "a.ts" dir)))
            (write-region "const a = 1;\n" nil file nil 'silent)
            (kill-buffer (find-file-noselect file))
            (should-not asked)))
      (delete-directory dir t))))

(ert-deftest +sh-bats-zunit-and-compdef-files-open-as-shell ()
  "Bats and ZUnit tests and zsh completion functions open in sh-mode."
  (require 'sh-script)
  (let ((dir (make-temp-file "sh-types-" t)))
    (unwind-protect
        (dolist (spec '(("t.bats" . "@test x {\n}\n")
                        ("t.zunit" . "@test x {\n}\n")
                        ("_mycmd" . "#compdef mycmd\n_arguments\n")
                        ("plain" . "hello\n")))
          (let ((file (expand-file-name (car spec) dir)))
            (write-region (cdr spec) nil file nil 'silent)
            (with-current-buffer (find-file-noselect file)
              (unwind-protect
                  (if (equal (car spec) "plain")
                      (should-not (derived-mode-p 'sh-base-mode))
                    (should (derived-mode-p 'sh-base-mode)))
                (kill-buffer)))))
      (delete-directory dir t))))

(ert-deftest +eshell-history-records-a-repeated-command-once ()
  "A command run twice in a row leaves one history entry."
  (require 'em-hist)
  (with-temp-buffer
    (let ((eshell-history-ring (make-ring 10))
          (eshell-hist--new-items 0))
      (should eshell-hist-ignoredups)
      (eshell-add-input-to-history "ls -l")
      (eshell-add-input-to-history "ls -l")
      (should (= (ring-length eshell-history-ring) 1)))))

(ert-deftest +eshell-history-skips-input-that-starts-with-a-space ()
  "A command typed with a leading space, or a blank one, stays out of history."
  (require 'em-hist)
  (should (funcall eshell-input-filter "ls -l"))
  (should-not (funcall eshell-input-filter " export TOKEN=x"))
  (should-not (funcall eshell-input-filter ""))
  (should-not (funcall eshell-input-filter "   ")))

(ert-deftest +eshell-emacs-command-visits-files-here ()
  "`emacs FILE' and `e FILE' in eshell visit FILE in this Emacs."
  (require 'eshell)
  (let* ((dir (make-temp-file "eshell-e" t))
         (default-directory (file-name-as-directory dir))
         (buffers nil))
    (unwind-protect
        (progn
          (eshell/emacs "one.txt" '("two.txt"))
          (eshell/e "three.txt")
          (dolist (name '("one.txt" "two.txt" "three.txt"))
            (let ((buffer (find-buffer-visiting (expand-file-name name dir))))
              (should buffer)
              (push buffer buffers))))
      (mapc #'kill-buffer buffers)
      (delete-directory dir t))))

(ert-deftest +yaml-ts-mode-indents-under-a-key ()
  "In YAML files (yaml-ts-mode), RET after \"key:\" indents the value."
  (let* ((file (make-temp-file "indent" nil ".yml" "a:\n  b: 1\n"))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'yaml-ts-mode))
          (goto-char (point-max))
          (insert "c:")
          (newline-and-indent)
          (should (= (current-column) 2)))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-file file))))

(provide 'init-web-tests)
;;; init-web-tests.el ends here
