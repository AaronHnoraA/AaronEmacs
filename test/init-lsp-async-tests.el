;;; init-lsp-async-tests.el --- Completion never waits on the server -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Contracts A1-A3 of docs/design/lsp-workflow.md ("Editing Never Waits On
;; The Server").  The server is mocked: a synchronous `lsp-request' records
;; that it was used, `lsp-request-async' that the call was sent without
;; waiting.  Run with the full configuration loaded.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lsp-mode)
(require 'lsp-completion)
(require 'tools/lsp/completion)

(defvar lsp-async-tests--sync nil)
(defvar lsp-async-tests--async nil)

(defmacro lsp-async-tests--with-server (&rest body)
  "Run BODY with the server requests recorded instead of sent."
  (declare (indent 0))
  `(let ((lsp-async-tests--sync nil)
         (lsp-async-tests--async nil))
     (cl-letf (((symbol-function 'lsp-request)
                (lambda (method params &rest _)
                  (push (list method params) lsp-async-tests--sync)
                  nil))
               ((symbol-function 'lsp-request-async)
                (lambda (method params _callback &rest _)
                  (push (list method params) lsp-async-tests--async)
                  nil)))
       ,@body)))

(ert-deftest lsp-async-a1-completion-command-is-sent-async ()
  (lsp-async-tests--with-server
    (+lsp-completion--exit-a
     (lambda () (lsp-send-execute-command "java.completion.onDidSelect" ["x"])))
    (should-not lsp-async-tests--sync)
    (should (equal (car lsp-async-tests--async)
                   '("workspace/executeCommand"
                     (:command "java.completion.onDidSelect" :arguments ["x"]))))))

(ert-deftest lsp-async-a1-command-after-async-resolve-is-async ()
  (lsp-async-tests--with-server
    (let (pending)
      ;; The exit function starts an async resolve and returns; the resolve's
      ;; callback runs the command later, outside the exit function.
      (+lsp-completion--exit-a
       (lambda ()
         (+lsp-completion--resolve-async-a
          (lambda (_item callback &optional _cleanup) (setq pending callback))
          "item"
          (lambda (_resolved) (lsp-send-execute-command "later")))))
      (funcall pending 'resolved)
      (should-not lsp-async-tests--sync)
      (should (equal (caar lsp-async-tests--async) "workspace/executeCommand")))))

(ert-deftest lsp-async-a2-docsig-uses-unresolved-detail ()
  (lsp-async-tests--with-server
    (let ((candidate (propertize "foo" 'lsp-completion-item
                                 (lsp-make-completion-item :label "foo" :detail "int foo()"))))
      (cl-letf (((symbol-function 'lsp-completion--resolve-async)
                 (lambda (&rest _) (error "Must not resolve"))))
        (should (equal (lsp-completion--company-docsig candidate) " int foo()")))
      (should-not lsp-async-tests--sync))))

(ert-deftest lsp-async-a2-docsig-resolves-later-and-refreshes ()
  (lsp-async-tests--with-server
    (with-temp-buffer
      (let* ((item (lsp-make-completion-item :label "bar"))
             (candidate (propertize "bar" 'lsp-completion-item item))
             (company-candidates (list candidate))
             (company-selection 0)
             callback requests refreshed)
        (cl-letf (((symbol-function 'lsp-completion--resolve-async)
                   (lambda (_item cb &optional _cleanup)
                     (setq requests (1+ (or requests 0)) callback cb)))
                  ((symbol-function 'company-call-frontends)
                   (lambda (command) (push command refreshed))))
          (should-not (lsp-completion--company-docsig candidate))
          (should-not (lsp-completion--company-docsig candidate))
          (should (eql requests 1))
          (funcall callback (lsp-make-completion-item :label "bar" :detail "void bar()"))
          (should (equal refreshed '(post-command)))
          (should (equal (lsp-completion--company-docsig candidate) " void bar()"))
          (should (eql requests 1))))
      (should-not lsp-async-tests--sync))))

(ert-deftest lsp-async-a3-explicit-commands-stay-sync ()
  (lsp-async-tests--with-server
    (lsp-send-execute-command "editor.organizeImports")
    (should (equal (caar lsp-async-tests--sync) "workspace/executeCommand"))
    (should-not lsp-async-tests--async)))

(provide 'init-lsp-async-tests)
;;; init-lsp-async-tests.el ends here
