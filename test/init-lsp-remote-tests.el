;;; init-lsp-remote-tests.el --- Logical LSP URI tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'init-lsp)
(require 'lsp-mode)

(ert-deftest lsp-mode-local-logical-path-does-not-leak-fs-syntax ()
  (let ((default-directory "/fs:local:/tmp/"))
    (should
     (equal
      (lsp--path-to-uri "/fs:local:/tmp/A.java")
      "file:///tmp/A.java"))
    (should
     (equal
      (lsp--uri-to-path "file:///tmp/A.java")
      "/fs:local:/tmp/A.java"))))

(ert-deftest lsp-mode-remote-uri-returns-to-current-logical-target ()
  (let ((default-directory "/fs:box:/work/"))
    (should
     (equal
      (my/lsp-mode--uri-to-logical-a
       (lambda (_uri) "/work/src/Main.java")
       "file:///work/src/Main.java")
      "/fs:box:/work/src/Main.java"))))

(ert-deftest lsp-mode-uri-prefers-workspace-target-over-current-buffer ()
  "Async callbacks must not borrow an unrelated buffer's target."
  (let ((default-directory "/fs:local:/tmp/")
        (lsp--cur-workspace
         (make-lsp--workspace :root "/fs:box:/work/")))
    (should
     (equal
      (my/lsp-mode--uri-to-logical-a
       (lambda (_uri) "/work/src/Main.java")
       "file:///work/src/Main.java")
      "/fs:box:/work/src/Main.java"))))

(ert-deftest lsp-mode-local-workspace-uses-the-same-uri-projection ()
  (let ((default-directory "/fs:box:/work/")
        (lsp--cur-workspace
         (make-lsp--workspace :root "/fs:local:/tmp/project/")))
    (should
     (equal
      (my/lsp-mode--uri-to-logical-a
       (lambda (_uri) "/tmp/project/Main.java")
       "file:///tmp/project/Main.java")
      "/fs:local:/tmp/project/Main.java"))))

(ert-deftest eglot-uri-prefers-server-project-over-current-buffer ()
  (let ((default-directory "/fs:local:/tmp/"))
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'server))
              ((symbol-function 'eglot--project)
               (lambda (_server) 'project))
              ((symbol-function 'project-root)
               (lambda (_project) "/fs:box:/work/")))
      (should
       (equal
        (my/eglot--uri-to-logical-a
         (lambda (_uri) "/work/src/Main.java")
         "file:///work/src/Main.java")
        "/fs:box:/work/src/Main.java")))))

(provide 'init-lsp-remote-tests)
;;; init-lsp-remote-tests.el ends here
