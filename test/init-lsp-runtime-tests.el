;;; init-lsp-runtime-tests.el --- Runtime-aware Eglot tests -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -Q -L lisp -l test/init-lsp-runtime-tests.el \
;;     -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'init-lsp-runtime)

(define-derived-mode my/runtime-test-mode prog-mode "Runtime-Test")

(defmacro my/runtime-test-with-registry (&rest body)
  "Evaluate BODY with isolated runtime registries."
  (declare (indent 0))
  `(let ((my/language-server-runtime-providers nil)
         (my/language-server-runtime--project-contexts
          (make-hash-table :test #'equal))
         (my/language-server-runtime--eglot-configurations
          (make-hash-table :test #'equal))
         (my/language-server-runtime--eglot-idle-timers
          (make-hash-table :test #'eq)))
     ,@body))

(ert-deftest my/runtime-provider-prepares-asynchronously ()
  (my/runtime-test-with-registry
    (let (finish received)
      (my/register-language-server-runtime-provider
       'async
       (lambda (_buffer callback)
         (setq finish callback)
         'pending)
       :modes '(my/runtime-test-mode))
      (with-temp-buffer
        (my/runtime-test-mode)
        (should (eq (my/language-server-runtime-prepare
                     (lambda (runtime error)
                       (setq received (list runtime error))))
                    'pending))
        (let ((runtime
               (my/language-server-runtime-create
                :id "runtime-a" :root temporary-file-directory)))
          (funcall finish runtime nil)
          (should (eq my/language-server-runtime-state 'ready))
          (should (eq (car received) runtime)))))))

(ert-deftest my/runtime-project-identity-separates-equal-roots ()
  (my/runtime-test-with-registry
    (with-temp-buffer
      (my/runtime-test-mode)
      (let* ((root (file-name-as-directory temporary-file-directory))
             (first (my/language-server-runtime-create :id "one" :root root))
             (second (my/language-server-runtime-create :id "two" :root root))
             (one (my/language-server-runtime-project-object first))
             (two (my/language-server-runtime-project-object second)))
        (should (equal (project-root one) root))
        (should-not (equal one two))
        (should (eq (my/language-server-runtime-project-context one) first))
        (should (eq (my/language-server-runtime-project-context two) second))))))

(ert-deftest my/runtime-eglot-project-wins-over-later-project-finders ()
  (my/runtime-test-with-registry
    (with-temp-buffer
      (my/runtime-test-mode)
      (setq-local
       my/language-server-runtime-current
       (my/language-server-runtime-create
        :id "kernel" :root "/fs:local:/work/project/"))
      (should
       (equal
        (my/language-server-runtime--eglot-current-project-a
         (lambda () '(projectile . "/work/project/")))
        '(my/language-server-runtime-project
          "/fs:local:/work/project/" "kernel"))))))

(ert-deftest my/runtime-workspace-configuration-is-keyed-by-project ()
  (my/runtime-test-with-registry
    (let* ((project '(my/language-server-runtime-project "/tmp/project/" "one"))
           (server 'server))
      (puthash project '(:python (:pythonPath "/kernel/python"))
               my/language-server-runtime--eglot-configurations)
      (cl-letf (((symbol-function 'eglot--project) (lambda (_server) project))
                ((symbol-function 'my/language-server--merge-values)
                 (lambda (base override) (append base override))))
        (should
         (equal
          (my/language-server-runtime--eglot-configuration-a
           (lambda (_server _path) '(:base t)) server nil)
          '(:base t :python (:pythonPath "/kernel/python"))))))))

(ert-deftest my/runtime-idle-shutdown-is-live-and-idempotent ()
  (my/runtime-test-with-registry
    (let ((shutdowns 0))
      (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_server) t))
                ((symbol-function 'eglot--managed-buffers) (lambda (_server) nil))
                ((symbol-function 'eglot-shutdown)
                 (lambda (&rest _) (cl-incf shutdowns))))
        (puthash 'server 'timer my/language-server-runtime--eglot-idle-timers)
        (my/language-server-runtime--shutdown-if-idle 'server)
        (should (= shutdowns 1))
        (should-not (gethash 'server
                             my/language-server-runtime--eglot-idle-timers))))))

(ert-deftest my/runtime-drops-queued-eglot-message-after-process-exit ()
  (let ((called nil))
    (cl-letf (((symbol-function 'eglot--project)
               (lambda (_connection) '(projectile . "/work/")))
              ((symbol-function 'jsonrpc-running-p) (lambda (_connection) nil)))
      (should-not
       (my/language-server-runtime--jsonrpc-receive-live-a
        (lambda (&rest _) (setq called t)) 'server '(:method "request")))
      (should-not called))))

(ert-deftest my/runtime-drops-queued-eglot-reply-after-process-exit ()
  (let ((called nil))
    (cl-letf (((symbol-function 'eglot--project)
               (lambda (_connection) '(projectile . "/work/")))
              ((symbol-function 'jsonrpc-running-p) (lambda (_connection) nil)))
      (should-not
       (my/language-server-runtime--jsonrpc-send-live-a
        (lambda (&rest _) (setq called t)) 'server :id 1 :result nil))
      (should-not called))))

(provide 'init-lsp-runtime-tests)
;;; init-lsp-runtime-tests.el ends here
