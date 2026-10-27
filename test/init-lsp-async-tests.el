;;; init-lsp-async-tests.el --- Completion never waits on the server -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Contracts A1-A4 of docs/design/lsp-workflow.md ("Editing Never Waits On
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

(ert-deftest lsp-async-a4-typescript-snippet-resolves-after-acceptance ()
  "An accepted TypeScript function inserts its resolved snippet later."
  (with-temp-buffer
    (insert "greet")
    (let* ((item (lsp-make-completion-item
                  :label "greet" :insertTextFormat lsp/insert-text-format-snippet))
           (markers (list 1 (copy-marker (point) t)))
           (candidate (propertize "greet"
                                  'lsp-completion-item item
                                  'lsp-completion-start-point 1
                                  'lsp-completion-markers markers
                                  'lsp-completion-prefix ""))
           (lsp-enable-snippet t)
           (lsp-completion-enable-additional-text-edit nil)
           (lsp-signature-auto-activate nil)
           callback snippet)
      (setq-local lsp-inhibit-lsp-hooks t)
      (setq-local lsp-completion--cache
                  (list 1 (list candidate) :markers markers))
      (cl-letf (((symbol-function 'lsp-completion--find-workspace)
                 (lambda (id) (eq id '+typescript)))
                ((symbol-function 'lsp-feature?)
                 (lambda (method) (equal method "completionItem/resolve")))
                ((symbol-function 'lsp-completion--resolve-async)
                 (lambda (_item fn &optional _cleanup) (setq callback fn)))
                ((symbol-function 'lsp-request)
                 (lambda (&rest _) (error "Completion must not wait on LSP")))
                ((symbol-function 'lsp--indent-lines) #'ignore)
                ((symbol-function 'lsp--expand-snippet)
                 (lambda (text &rest _) (setq snippet text))))
        (should (+lsp-completion--typescript-snippet-needs-resolve-p candidate))
        (lsp-completion--exit-fn candidate 'finished)
        (should (equal (buffer-string) "greet"))
        (should callback)
        (should-not lsp-inhibit-lsp-hooks)
        (should-not lsp-completion--cache)
        (funcall callback
                 (lsp-make-completion-item
                  :label "greet" :insertTextFormat lsp/insert-text-format-snippet
                  :insertText "greet(${1:name})$0"))
        (should (equal (buffer-string) "greet(${1:name})$0"))
        (should (equal snippet "greet(${1:name})$0"))))))

(ert-deftest lsp-async-a4-typescript-late-response-keeps-user-edits ()
  "A late snippet resolve cannot overwrite typing after completion."
  (with-temp-buffer
    (insert "greet")
    (let* ((item (lsp-make-completion-item
                  :label "greet" :insertTextFormat lsp/insert-text-format-snippet))
           (candidate (propertize "greet"
                                  'lsp-completion-item item
                                  'lsp-completion-markers
                                  (list 1 (copy-marker (point) t))))
           (lsp-enable-snippet t)
           callback applied)
      (cl-letf (((symbol-function 'lsp-completion--find-workspace)
                 (lambda (id) (eq id '+typescript)))
                ((symbol-function 'lsp-feature?) (lambda (_) t))
                ((symbol-function 'lsp-completion--resolve-async)
                 (lambda (_item fn &optional _cleanup) (setq callback fn))))
        (should (+lsp-completion--typescript-snippet-needs-resolve-p candidate))
        (+lsp-completion--exit-a (lambda (&rest _) (setq applied t))
                                 candidate 'finished)
        (should callback)
        (insert "!")
        (funcall callback
                 (lsp-make-completion-item
                  :label "greet" :insertTextFormat lsp/insert-text-format-snippet
                  :insertText "greet(${1:name})$0"))
        (should (equal (buffer-string) "greet!"))
        (should-not applied)))))

(ert-deftest lsp-async-a4-typescript-provisional-text-edit-needs-resolve ()
  "A plain-label edit needs resolve; an already-filled snippet does not."
  (let* ((edit (lsp-make-text-edit :newText "greet"))
         (item (lsp-make-completion-item
                :label "greet" :insertTextFormat lsp/insert-text-format-snippet
                :textEdit edit))
         (candidate (propertize "greet" 'lsp-completion-item item))
         (lsp-enable-snippet t))
    (cl-letf (((symbol-function 'lsp-completion--find-workspace)
               (lambda (id) (eq id '+typescript)))
              ((symbol-function 'lsp-feature?) (lambda (_) t)))
      (should (+lsp-completion--typescript-snippet-needs-resolve-p candidate))
      (lsp-put edit :newText "greet(${1:name})$0")
      (should-not (+lsp-completion--typescript-snippet-needs-resolve-p candidate)))))

(ert-deftest lsp-async-a4-typescript-late-response-keeps-inactive-buffer ()
  "Do not expand a snippet after the user switches away from its window."
  (save-window-excursion
    (let ((source (generate-new-buffer " *lsp-a4-source*"))
          (other (generate-new-buffer " *lsp-a4-other*"))
          (lsp-enable-snippet t)
          callback applied)
      (unwind-protect
          (progn
            (switch-to-buffer source)
            (insert "greet")
            (let ((candidate (propertize
                              "greet" 'lsp-completion-item
                              (lsp-make-completion-item
                               :label "greet"
                               :insertTextFormat lsp/insert-text-format-snippet)
                              'lsp-completion-markers
                              (list 1 (copy-marker (point) t)))))
              (cl-letf (((symbol-function 'lsp-completion--find-workspace)
                         (lambda (id) (eq id '+typescript)))
                        ((symbol-function 'lsp-feature?) (lambda (_) t))
                        ((symbol-function 'lsp-completion--resolve-async)
                         (lambda (_item fn &optional _cleanup) (setq callback fn))))
                (+lsp-completion--exit-a (lambda (&rest _) (setq applied t))
                                         candidate 'finished)
                (should callback)
                (switch-to-buffer other)
                (funcall callback
                         (lsp-make-completion-item
                          :label "greet"
                          :insertTextFormat lsp/insert-text-format-snippet
                          :insertText "greet(${1:name})$0"))
                (with-current-buffer source
                  (should (equal (buffer-string) "greet")))
                (should-not applied))))
        (kill-buffer source)
        (kill-buffer other)))))

(provide 'init-lsp-async-tests)
;;; init-lsp-async-tests.el ends here
