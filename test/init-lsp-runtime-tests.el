;;; init-lsp-runtime-tests.el --- Runtime-aware lsp-mode tests -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -Q -L lisp -l test/init-lsp-runtime-tests.el \
;;     -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'init-lsp-runtime)
(require 'lsp-mode)

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

(ert-deftest my/runtime-lsp-workspaces-are-separated-by-runtime-id ()
  "Equal roots and client IDs must not merge distinct kernel runtimes."
  (my/runtime-test-with-registry
    (let* ((root "/fs:local:/work/project/")
           (client (make-lsp--client :server-id 'my-python))
           (ordinary (make-lsp--workspace :root root :client client))
           (first (make-lsp--workspace :root root :client client))
           (second (make-lsp--workspace :root root :client client))
           (session (make-lsp-session))
           opened)
      (puthash my/language-server-runtime--workspace-metadata-key "one"
               (lsp--workspace-metadata first))
      (puthash my/language-server-runtime--workspace-metadata-key "two"
               (lsp--workspace-metadata second))
      (puthash root (list ordinary first second)
               (lsp-session-folder->servers session))
      (cl-letf (((symbol-function 'lsp--open-in-workspace)
                 (lambda (workspace) (setq opened workspace))))
        (with-temp-buffer
          (setq-local my/language-server-runtime-current
                      (my/language-server-runtime-create
                       :id "one" :root root))
          (should
           (eq first
               (my/language-server-runtime--find-workspace-a
                #'ignore session client root)))
          (should (eq opened first))
          (setq opened nil)
          (setq-local my/language-server-runtime-current
                      (my/language-server-runtime-create
                       :id "missing" :root root))
          (should-not
           (my/language-server-runtime--find-workspace-a
            #'ignore session client root))
          (should-not opened)
          (setq-local my/language-server-runtime-current nil)
          (should
           (eq ordinary
               (my/language-server-runtime--find-workspace-a
                #'ignore session client root))))))))

(ert-deftest my/runtime-tags-new-lsp-workspace-without-changing-root ()
  (let* ((root "/fs:box:/work/project/")
         (workspace (make-lsp--workspace :root root))
         (runtime (my/language-server-runtime-create
                   :id "kernel-a" :root root)))
    (with-temp-buffer
      (setq-local my/language-server-runtime-current runtime)
      (let ((lsp--cur-workspace workspace))
        (my/language-server-runtime--tag-current-workspace-h)))
    (should (equal (lsp--workspace-root workspace) root))
    (should
     (equal (my/language-server-runtime-workspace-id workspace)
            "kernel-a"))))

(ert-deftest my/runtime-workspace-configuration-survives-warm-buffer-gap ()
  (my/runtime-test-with-registry
    (let* ((root "/fs:local:/work/project/")
           (runtime (my/language-server-runtime-create
                     :id "kernel-a" :root root))
           (project (list 'my/language-server-runtime-project
                          root "kernel-a"))
           (configuration '(:python (:pythonPath "/kernel/bin/python")))
           (workspace (make-lsp--workspace :root root)))
      (puthash project runtime my/language-server-runtime--project-contexts)
      (puthash project configuration
               my/language-server-runtime--configurations)
      (puthash my/language-server-runtime--workspace-metadata-key "kernel-a"
               (lsp--workspace-metadata workspace))
      (should
       (equal
        (my/language-server-runtime-configuration-for-workspace workspace)
        configuration)))))

(ert-deftest my/runtime-uninitializing-one-kernel-keeps-its-siblings ()
  (my/runtime-test-with-registry
    (let* ((root "/fs:local:/work/project/")
           (one (list 'my/language-server-runtime-project root "one"))
           (two (list 'my/language-server-runtime-project root "two"))
           (workspace (make-lsp--workspace :root root)))
      (puthash one 'runtime-one my/language-server-runtime--project-contexts)
      (puthash two 'runtime-two my/language-server-runtime--project-contexts)
      (puthash one 'config-one my/language-server-runtime--configurations)
      (puthash two 'config-two my/language-server-runtime--configurations)
      (puthash my/language-server-runtime--workspace-metadata-key "one"
               (lsp--workspace-metadata workspace))
      (my/language-server-runtime-uninitialized-h workspace)
      (should-not (gethash one my/language-server-runtime--project-contexts))
      (should-not (gethash one my/language-server-runtime--configurations))
      (should (eq (gethash two my/language-server-runtime--project-contexts)
                  'runtime-two))
      (should (eq (gethash two my/language-server-runtime--configurations)
                  'config-two)))))

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

(ert-deftest my/noema-jupyter-kernel-change-fully-disconnects-lsp-mode ()
  "A kernel switch must also detach a still-starting lsp-mode workspace."
  (require 'init-aaronnote-jupyter-lsp)
  (with-temp-buffer
    (setq-local lsp-managed-mode nil)
    (setq-local lsp-mode t)
    (setq-local my/language-server-runtime-current
                (my/language-server-runtime-create
                 :id "old-kernel" :root "/fs:local:/work/"))
    (setq-local my/language-server-runtime--workspace 'old-workspace)
    (let (events)
      (cl-letf (((symbol-function
                  'my/language-server-runtime--buffer-leaving-h)
                 (lambda () (push 'warm events)))
                ((symbol-function 'lsp-disconnect)
                 (lambda () (push 'disconnect events)))
                ((symbol-function 'my/language-server-runtime-invalidate)
                 (lambda () (push 'invalidate events))))
        (my/noema-jupyter-cell-lsp-runtime-changing))
      (should (equal (nreverse events) '(warm disconnect invalidate)))
      (should-not my/language-server-runtime--workspace))))

(ert-deftest my/noema-jupyter-kernel-capf-precedes-lsp-capf ()
  "Live kernel names retain the completion priority they had under Eglot."
  (require 'init-aaronnote-jupyter-lsp)
  (with-temp-buffer
    (setq-local my/noema-jupyter-cell-mode t)
    (setq-local completion-at-point-functions
                '(lsp-completion-at-point
                  my/noema-jupyter-cell-capf t))
    (my/noema-jupyter-cell--lsp-capf-priority-h)
    (should
     (equal completion-at-point-functions
            '(my/noema-jupyter-cell-capf
              lsp-completion-at-point t)))))

(ert-deftest my/noema-jupyter-keeps-lsp-in-tab-line-above-its-header ()
  (require 'init-aaronnote-jupyter-lsp)
  (with-temp-buffer
    (setq-local my/noema-jupyter-cell-mode t)
    (setq-local lsp-managed-mode t)
    (setq-local header-line-format
                '((t (:eval (window-parameter nil 'lsp-headerline--string)))
                  (:eval (my/noema-jupyter-cell--header-line))))
    (my/lsp-tab-line-sync-h)
    (my/noema-jupyter-cell--lsp-ui-h)
    (should
     (equal header-line-format
            '(:eval (my/noema-jupyter-cell--header-line))))
    (should
     (memq #'my/lsp-tab-line-breadcrumb
           my/tab-line-leading-segment-functions))
    (setq-local lsp-managed-mode nil)
    (my/lsp-tab-line-sync-h)
    (should-not
     (memq #'my/lsp-tab-line-breadcrumb
           my/tab-line-leading-segment-functions))))

(ert-deftest my/noema-jupyter-lsp-breadcrumb-is-view-only-after-moving ()
  (require 'init-aaronnote-jupyter-lsp)
  (let* ((map (make-sparse-keymap))
         (breadcrumb
          (propertize "project > symbol"
                      'local-map map
                      'mouse-face 'highlight))
         (rendered (my/breadcrumb-view-only-string breadcrumb)))
    (should-not (get-text-property 0 'local-map rendered))
    (should-not (get-text-property 0 'mouse-face rendered))))
