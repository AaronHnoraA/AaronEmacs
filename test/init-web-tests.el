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

(provide 'init-web-tests)
;;; init-web-tests.el ends here
