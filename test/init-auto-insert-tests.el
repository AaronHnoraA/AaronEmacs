;;; init-auto-insert-tests.el --- Template expansion regressions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'init-auto-insert)
(require 'remote-framework)

(defun init-auto-insert-test--expand (file-name)
  "Expand the Python default template in a buffer visiting FILE-NAME."
  (with-temp-buffer
    (setq buffer-file-name file-name)
    (my/auto-insert--insert-template-file
     (expand-file-name "templates/python/default.py" user-emacs-directory))
    (prog1 (buffer-string)
      (setq buffer-file-name nil))))

(ert-deftest init-auto-insert-expands-placeholders-on-local-target ()
  (let ((text (init-auto-insert-test--expand "/tmp/project/a.py")))
    (should-not (string-match-p "{{" text))
    (should (string-match-p "^/tmp/project/a\\.py$" text))
    (should (string-match-p (format-time-string "Created: %Y-%m-%d") text))))

(ert-deftest init-auto-insert-expands-placeholders-on-remote-target ()
  ;; The file records its own path on the target, not Emacs's /fs: identity.
  (let ((text (init-auto-insert-test--expand
               (remote-make-file-name "server" "/home/test/a.py"))))
    (should-not (string-match-p "{{" text))
    (should-not (string-match-p "/fs:" text))
    (should (string-match-p "^/home/test/a\\.py$" text))))

(ert-deftest init-auto-insert-places-point-at-the-cursor-token ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/project/a.py")
    (my/auto-insert--insert-template-file
     (expand-file-name "templates/python/default.py" user-emacs-directory))
    (should-not (search-forward "{{cursor}}" nil t))
    ;; The Python template puts the cursor inside `main'.
    (should (save-excursion (re-search-backward "^def main" nil t)))
    (should (< (point) (point-max)))
    (setq buffer-file-name nil)))

(provide 'init-auto-insert-tests)
;;; init-auto-insert-tests.el ends here
