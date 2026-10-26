;;; note-code-tests.el --- Tests for Typst note-code links -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'noema/bridge/note-code)

(ert-deftest +note-code-opens-local-source-from-remote-buffer ()
  (let ((default-directory "/fs:box:/work/")
        visited context)
    (cl-letf (((symbol-function '+note-code-at-point)
               (lambda () '(:lang "python" :path "source.py" :tag "sample")))
              ((symbol-function '+note-code-source-path)
               (lambda (_call) "/Users/test/project/source.py"))
              ((symbol-function 'file-exists-p) (lambda (_file) t))
              ((symbol-function 'find-file)
               (lambda (file &rest _)
                 (setq visited file context default-directory)))
              ((symbol-function '+note-code--goto-tag) #'ignore))
      (+note-code/open-at-point))
    (should (equal visited "/Users/test/project/source.py"))
    (should-not (file-remote-p context))))

(ert-deftest +note-code-parses-lean-default-root-path ()
  (with-temp-buffer
    (insert "#note-code(lang: lean)[group-cancel]")
    (goto-char 20)
    (should (equal (+note-code-at-point)
                   '(:lang "lean" :path nil :tag "group-cancel"
                     :begin 1 :end 37)))))

(ert-deftest +note-code-parses-generic-explicit-path ()
  (with-temp-buffer
    (insert "#note-code(lang: \"python\", path: \"src/demo.py\")[example]")
    (goto-char 25)
    (let ((call (+note-code-at-point)))
      (should (equal (plist-get call :lang) "python"))
      (should (equal (plist-get call :path) "src/demo.py"))
      (should (equal (plist-get call :tag) "example")))))

(ert-deftest +note-code-resolves-lean-root-path-and-explicit-path ()
  (let* ((root (make-temp-file "note-code-test-" t))
         (+note-code-root root)
         (+noema-roam-root root)
         (buffer-file-name (expand-file-name "math/group.typ" root)))
    (cl-letf (((symbol-function '+noema-roam-root) (lambda () root)))
      (should (equal (+note-code-lean-path-for-note)
                     (expand-file-name "math/group.lean" root)))
      (should (equal (+note-code-source-path
                      '(:lang "python" :path "/src/demo.py" :tag "x"))
                     (expand-file-name "src/demo.py" root))))))

(ert-deftest +note-code-requires-path-for-non-lean ()
  (let ((+note-code-root temporary-file-directory)
        (+noema-roam-root temporary-file-directory))
    (should-error
     (+note-code-source-path '(:lang "rust" :path nil :tag "x"))
     :type 'user-error)))

(ert-deftest +note-code-jumps-to-noema-and-generic-tags ()
  (with-temp-buffer
    (insert "-- @noema first\n#check Nat\n// @note-code second\nvalue\n")
    (+note-code--goto-tag "second")
    (should (looking-at-p "value"))))

(ert-deftest +note-code-jumps-to-tag-written-before-noema-rename ()
  (with-temp-buffer
    (insert "-- @aaronnote legacy\n#check Nat\n")
    (+note-code--goto-tag "legacy")
    (should (looking-at-p "#check"))))

(ert-deftest +note-code-preamble-uses-only-roam-entrypoint ()
  (let* ((root (make-temp-file "note-code-test-" t))
         (+note-code-root root)
         (+noema-roam-root root)
         (buffer-file-name (expand-file-name "math/group.typ" root)))
    (cl-letf (((symbol-function '+noema-roam-root) (lambda () root)))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "math/group.typ" root))
        (+note-code--ensure-preamble)
        (should (string-match-p
                 (regexp-quote "#import \"/_typst/roam.typ\": *")
                 (buffer-string)))
        (should-not (string-match-p "note-code\\.typ" (buffer-string)))))))

(ert-deftest +note-code-ref-path-uses-root-relative-roam-path ()
  (let* ((root (file-name-as-directory (make-temp-file "note-code-test-" t)))
         (buffer-file-name (expand-file-name "project/demo/source.py" root)))
    (cl-letf (((symbol-function '+note-code--root)
               (lambda () root)))
      (should (equal (+note-code--ref-path)
                     "/project/demo/source.py")))))

(ert-deftest +note-code-ref-path-uses-root-relative-project-path ()
  (let* ((roam (file-name-as-directory (make-temp-file "note-code-roam-" t)))
         (project (file-name-as-directory (make-temp-file "note-code-project-" t)))
         (buffer-file-name (expand-file-name "src/demo.py" project)))
    (cl-letf (((symbol-function '+note-code--root)
               (lambda () roam))
              ((symbol-function 'project-current)
               (lambda (&rest _args) 'mock-project))
              ((symbol-function 'project-root)
               (lambda (_project) project)))
      (should (equal (+note-code--ref-path)
                     "/src/demo.py")))))

(provide 'note-code-tests)
;;; note-code-tests.el ends here
