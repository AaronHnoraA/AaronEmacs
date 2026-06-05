;;; note-code-tests.el --- Tests for Typst note-code links -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'init-note-code)

(ert-deftest my/note-code-parses-lean-default-mirror ()
  (with-temp-buffer
    (insert "#note-code(lang: lean)[group-cancel]")
    (goto-char 20)
    (should (equal (my/note-code-at-point)
                   '(:lang "lean" :path nil :tag "group-cancel"
                     :begin 1 :end 37)))))

(ert-deftest my/note-code-parses-generic-explicit-path ()
  (with-temp-buffer
    (insert "#note-code(lang: \"python\", path: \"src/demo.py\")[example]")
    (goto-char 25)
    (let ((call (my/note-code-at-point)))
      (should (equal (plist-get call :lang) "python"))
      (should (equal (plist-get call :path) "src/demo.py"))
      (should (equal (plist-get call :tag) "example")))))

(ert-deftest my/note-code-resolves-lean-mirror-and-explicit-path ()
  (let* ((root (make-temp-file "note-code-test-" t))
         (my/note-code-root root)
         (my/typst-roam-root root)
         (buffer-file-name (expand-file-name "math/group.typ" root)))
    (should (equal (my/note-code-lean-mirror-path)
                   (expand-file-name ".lean/math/group.lean" root)))
    (should (equal (my/note-code-source-path
                    '(:lang "python" :path "/src/demo.py" :tag "x"))
                   (expand-file-name "src/demo.py" root)))))

(ert-deftest my/note-code-requires-path-for-non-lean ()
  (let ((my/note-code-root temporary-file-directory)
        (my/typst-roam-root temporary-file-directory))
    (should-error
     (my/note-code-source-path '(:lang "rust" :path nil :tag "x"))
     :type 'user-error)))

(ert-deftest my/note-code-jumps-to-aaronnote-and-generic-tags ()
  (with-temp-buffer
    (insert "-- @aaronnote first\n#check Nat\n// @note-code second\nvalue\n")
    (my/note-code--goto-tag "second")
    (should (looking-at-p "value"))))

(ert-deftest my/note-code-preamble-uses-only-roam-entrypoint ()
  (let* ((root (make-temp-file "note-code-test-" t))
         (my/note-code-root root)
         (my/typst-roam-root root)
         (buffer-file-name (expand-file-name "math/group.typ" root)))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "math/group.typ" root))
      (my/note-code--ensure-preamble)
      (should (string-match-p
               (regexp-quote "#import \"/_typst/roam.typ\": *")
               (buffer-string)))
      (should-not (string-match-p "note-code\\.typ" (buffer-string))))))

(provide 'note-code-tests)
;;; note-code-tests.el ends here
