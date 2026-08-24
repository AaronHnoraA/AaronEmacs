;;; init-lsp-runtime-tests.el --- Runtime-aware lsp-mode tests -*- lexical-binding: t; -*-

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
         (my/language-server-runtime--configurations
          (make-hash-table :test #'equal))
         (my/language-server-runtime--idle-timers
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

(ert-deftest my/runtime-expected-fallback-is-silent-but-diagnostic ()
  (let ((fallback
         (my/language-server-runtime-fallback-create
          :reason "kernel launcher is intentionally opaque" :expected t))
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (push args messages))))
      (my/language-server-runtime-report-fallback fallback))
    (should-not messages)
    (with-temp-buffer
      (setq-local my/language-server-runtime-state 'unsupported)
      (setq-local my/language-server-runtime-error fallback)
      (should
       (equal (my/language-server-runtime-description)
              "fallback — kernel launcher is intentionally opaque")))
    (my/runtime-test-with-registry
      (let (finish)
        (my/register-language-server-runtime-provider
         'expected
         (lambda (_buffer callback)
           (setq finish callback)
           'pending)
         :modes '(my/runtime-test-mode))
        (with-temp-buffer
          (my/runtime-test-mode)
          (should (eq (my/language-server-runtime-prepare) 'pending))
          (funcall finish nil fallback)
          (should (eq my/language-server-runtime-state 'unsupported))
          (should (eq my/language-server-runtime-error fallback)))))))

(ert-deftest my/runtime-unexpected-fallback-is-reported ()
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (push args messages))))
      (my/language-server-runtime-report-fallback "probe failed"))
    (should
     (equal (car messages)
            '("Language-server runtime fallback: %s" "probe failed")))))

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

(ert-deftest my/runtime-project-wins-over-later-project-finders ()
  (my/runtime-test-with-registry
    (with-temp-buffer
      (my/runtime-test-mode)
      (setq-local
       my/language-server-runtime-current
       (my/language-server-runtime-create
        :id "kernel" :root "/fs:local:/work/project/"))
      (should
       (equal
        (my/language-server-runtime--calculate-root-a
         (lambda (_session _file-name) "/work/project/")
         'session "/work/project/file.py")
        "/fs:local:/work/project/")))))

(ert-deftest my/runtime-workspace-configuration-is-keyed-by-project ()
  (my/runtime-test-with-registry
    (with-temp-buffer
      (my/runtime-test-mode)
      (let* ((runtime
              (my/language-server-runtime-create
               :id "one" :root "/tmp/project/"))
             (project
              (progn (setq-local my/language-server-runtime-current runtime)
                     (my/language-server-runtime-project-object))))
        (setq-local my/language-server--workspace-configuration
                    '(:python (:pythonPath "/kernel/python")))
        (my/language-server-runtime-register-lsp-configuration)
        (should
         (equal
          (my/language-server-runtime-configuration project)
          '(:python (:pythonPath "/kernel/python"))))))))

(ert-deftest my/runtime-idle-shutdown-is-live-and-idempotent ()
  (my/runtime-test-with-registry
    (let ((shutdowns 0))
      (cl-letf (((symbol-function 'lsp--workspace-buffers) (lambda (_workspace) nil))
                ((symbol-function 'my/lsp-mode-shutdown-workspace)
                 (lambda (&rest _) (cl-incf shutdowns))))
        (puthash 'workspace 'timer my/language-server-runtime--idle-timers)
        (my/language-server-runtime--shutdown-if-idle 'workspace)
        (should (= shutdowns 1))
        (should-not (gethash 'workspace
                             my/language-server-runtime--idle-timers))))))

(provide 'init-lsp-runtime-tests)
;;; init-lsp-runtime-tests.el ends here

(ert-deftest my/noema-jupyter-server-kernels-are-an-expected-lsp-fallback ()
  "A `server:' kernel has no target process to probe.

Before this it fell through to kernelspec lookup and reported
\"kernelspec `server:hub:python3' was not found on target `local'\", which
described neither the kernel nor the reason, and counted as an unexpected
fallback so it kept re-reporting itself."
  (require 'init-aaronnote-jupyter-lsp)
  (let ((fallback
         (my/noema-jupyter-cell--lsp-unprobeable-connector "server:hub:python3")))
    (should (my/language-server-runtime-fallback-p fallback))
    (should (my/language-server-runtime-fallback-expected fallback))
    (should (string-match-p
             "Jupyter server"
             (my/language-server-runtime-fallback-reason fallback)))))

(ert-deftest my/noema-jupyter-attached-kernels-are-an-expected-lsp-fallback ()
  (require 'init-aaronnote-jupyter-lsp)
  (let ((fallback
         (my/noema-jupyter-cell--lsp-unprobeable-connector "attach:/tmp/k.json")))
    (should (my/language-server-runtime-fallback-p fallback))
    (should (my/language-server-runtime-fallback-expected fallback))))

(ert-deftest my/noema-jupyter-launchable-kernels-are-not-fallbacks ()
  "An ordinary kernelspec must still be probed, not shortcut to a fallback."
  (require 'init-aaronnote-jupyter-lsp)
  (should-not (my/noema-jupyter-cell--lsp-unprobeable-connector "python3")))
