;;; init-lsp-runtime-tests.el --- Runtime-aware lsp-mode tests -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -Q -L lisp -l test/init-lsp-runtime-tests.el \
;;     -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'tools/lsp/runtime)
(require 'lsp-mode)

(define-derived-mode +runtime-test-mode prog-mode "Runtime-Test")

(defmacro +runtime-test-with-registry (&rest body)
  "Evaluate BODY with isolated runtime registries."
  (declare (indent 0))
  `(let ((+language-server-runtime-providers nil)
         (+language-server-runtime--project-contexts
          (make-hash-table :test #'equal))
         (+language-server-runtime--configurations
          (make-hash-table :test #'equal))
         (+language-server-runtime--idle-timers
          (make-hash-table :test #'eq)))
     ,@body))

(ert-deftest +runtime-provider-prepares-asynchronously ()
  (+runtime-test-with-registry
    (let (finish received)
      (set-language-server-runtime-provider!
       'async
       (lambda (_buffer callback)
         (setq finish callback)
         'pending)
       :modes '(+runtime-test-mode))
      (with-temp-buffer
        (+runtime-test-mode)
        (should (eq (+language-server-runtime-prepare
                     (lambda (runtime error)
                       (setq received (list runtime error))))
                    'pending))
        (let ((runtime
               (+language-server-runtime-create
                :id "runtime-a" :root temporary-file-directory)))
          (funcall finish runtime nil)
          (should (eq +language-server-runtime-state 'ready))
          (should (eq (car received) runtime)))))))

(ert-deftest +runtime-provider-synchronous-callback-is-not-left-preparing ()
  "A cached Remote probe may finish before its provider returns `pending'."
  (+runtime-test-with-registry
    (let ((runtime (+language-server-runtime-create
                    :id "cached-kernel" :root temporary-file-directory))
          received)
      (set-language-server-runtime-provider!
       'cached
       (lambda (_buffer callback)
         (funcall callback runtime nil)
         'pending)
       :modes '(+runtime-test-mode))
      (with-temp-buffer
        (+runtime-test-mode)
        (should (eq (+language-server-runtime-prepare
                     (lambda (value error)
                       (setq received (list value error))))
                    runtime))
        (should (eq +language-server-runtime-state 'ready))
        (should (equal received (list runtime nil)))))))

(ert-deftest +runtime-expected-fallback-is-silent-but-diagnostic ()
  (let ((fallback
         (+language-server-runtime-fallback-create
          :reason "kernel launcher is intentionally opaque" :expected t))
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (push args messages))))
      (+language-server-runtime-report-fallback fallback))
    (should-not messages)
    (with-temp-buffer
      (setq-local +language-server-runtime-state 'unsupported)
      (setq-local +language-server-runtime-error fallback)
      (should
       (equal (+language-server-runtime-description)
              "fallback — kernel launcher is intentionally opaque")))
    (+runtime-test-with-registry
      (let (finish)
        (set-language-server-runtime-provider!
         'expected
         (lambda (_buffer callback)
           (setq finish callback)
           'pending)
         :modes '(+runtime-test-mode))
        (with-temp-buffer
          (+runtime-test-mode)
          (should (eq (+language-server-runtime-prepare) 'pending))
          (funcall finish nil fallback)
          (should (eq +language-server-runtime-state 'unsupported))
          (should (eq +language-server-runtime-error fallback)))))))

(ert-deftest +runtime-unexpected-fallback-is-reported ()
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (push args messages))))
      (+language-server-runtime-report-fallback "probe failed"))
    (should
     (equal (car messages)
            '("Language-server runtime fallback: %s" "probe failed")))))

(ert-deftest +runtime-project-identity-separates-equal-roots ()
  (+runtime-test-with-registry
    (with-temp-buffer
      (+runtime-test-mode)
      (let* ((root (file-name-as-directory temporary-file-directory))
             (first (+language-server-runtime-create :id "one" :root root))
             (second (+language-server-runtime-create :id "two" :root root))
             (one (+language-server-runtime-project-object first))
             (two (+language-server-runtime-project-object second)))
        (should (equal (project-root one) root))
        (should-not (equal one two))
        (should (eq (+language-server-runtime-project-context one) first))
        (should (eq (+language-server-runtime-project-context two) second))))))

(ert-deftest +runtime-project-wins-over-later-project-finders ()
  (+runtime-test-with-registry
    (with-temp-buffer
      (+runtime-test-mode)
      (setq-local
       +language-server-runtime-current
       (+language-server-runtime-create
        :id "kernel" :root "/fs:local:/work/project/"))
      (should
       (equal
        (+language-server-runtime--calculate-root-a
         (lambda (_session _file-name) "/work/project/")
         'session "/work/project/file.py")
        "/fs:local:/work/project/")))))

(ert-deftest +runtime-workspace-configuration-is-keyed-by-project ()
  (+runtime-test-with-registry
    (with-temp-buffer
      (+runtime-test-mode)
      (let* ((runtime
              (+language-server-runtime-create
               :id "one" :root "/tmp/project/"))
             (project
              (progn (setq-local +language-server-runtime-current runtime)
                     (+language-server-runtime-project-object))))
        (setq-local +language-server--workspace-configuration
                    '(:python (:pythonPath "/kernel/python")))
        (+language-server-runtime-register-lsp-configuration)
        (should
         (equal
          (+language-server-runtime-configuration project)
          '(:python (:pythonPath "/kernel/python"))))))))

(ert-deftest +runtime-idle-shutdown-is-live-and-idempotent ()
  (+runtime-test-with-registry
    (let ((shutdowns 0))
      (cl-letf (((symbol-function 'lsp--workspace-buffers) (lambda (_workspace) nil))
                ((symbol-function '+lsp-mode-shutdown-workspace)
                 (lambda (&rest _) (cl-incf shutdowns))))
        (puthash 'workspace 'timer +language-server-runtime--idle-timers)
        (+language-server-runtime--shutdown-if-idle 'workspace)
        (should (= shutdowns 1))
        (should-not (gethash 'workspace
                             +language-server-runtime--idle-timers))))))

(ert-deftest +runtime-lsp-workspaces-are-separated-by-runtime-id ()
  "Equal roots and client IDs must not merge distinct kernel runtimes."
  (+runtime-test-with-registry
    (let* ((root "/fs:local:/work/project/")
           (client (make-lsp--client :server-id '+python))
           (ordinary (make-lsp--workspace :root root :client client))
           (first (make-lsp--workspace :root root :client client))
           (second (make-lsp--workspace :root root :client client))
           (session (make-lsp-session))
           opened)
      (puthash +language-server-runtime--workspace-metadata-key "one"
               (lsp--workspace-metadata first))
      (puthash +language-server-runtime--workspace-metadata-key "two"
               (lsp--workspace-metadata second))
      (puthash root (list ordinary first second)
               (lsp-session-folder->servers session))
      (cl-letf (((symbol-function 'lsp--open-in-workspace)
                 (lambda (workspace) (setq opened workspace))))
        (with-temp-buffer
          (setq-local +language-server-runtime-current
                      (+language-server-runtime-create
                       :id "one" :root root))
          (should
           (eq first
               (+language-server-runtime--find-workspace-a
                #'ignore session client root)))
          (should (eq opened first))
          (setq opened nil)
          (setq-local +language-server-runtime-current
                      (+language-server-runtime-create
                       :id "missing" :root root))
          (should-not
           (+language-server-runtime--find-workspace-a
            #'ignore session client root))
          (should-not opened)
          (setq-local +language-server-runtime-current nil)
          (should
           (eq ordinary
               (+language-server-runtime--find-workspace-a
                #'ignore session client root))))))))

(ert-deftest +runtime-tags-new-lsp-workspace-without-changing-root ()
  (let* ((root "/fs:box:/work/project/")
         (workspace (make-lsp--workspace :root root))
         (runtime (+language-server-runtime-create
                   :id "kernel-a" :root root)))
    (with-temp-buffer
      (setq-local +language-server-runtime-current runtime)
      (let ((lsp--cur-workspace workspace))
        (+language-server-runtime--tag-current-workspace-h)))
    (should (equal (lsp--workspace-root workspace) root))
    (should
     (equal (+language-server-runtime-workspace-id workspace)
            "kernel-a"))))

(ert-deftest +runtime-workspace-configuration-survives-warm-buffer-gap ()
  (+runtime-test-with-registry
    (let* ((root "/fs:local:/work/project/")
           (runtime (+language-server-runtime-create
                     :id "kernel-a" :root root))
           (project (list '+language-server-runtime-project
                          root "kernel-a"))
           (configuration '(:python (:pythonPath "/kernel/bin/python")))
           (workspace (make-lsp--workspace :root root)))
      (puthash project runtime +language-server-runtime--project-contexts)
      (puthash project configuration
               +language-server-runtime--configurations)
      (puthash +language-server-runtime--workspace-metadata-key "kernel-a"
               (lsp--workspace-metadata workspace))
      (should
       (equal
        (+language-server-runtime-configuration-for-workspace workspace)
        configuration)))))

(ert-deftest +runtime-uninitializing-one-kernel-keeps-its-siblings ()
  (+runtime-test-with-registry
    (let* ((root "/fs:local:/work/project/")
           (one (list '+language-server-runtime-project root "one"))
           (two (list '+language-server-runtime-project root "two"))
           (workspace (make-lsp--workspace :root root)))
      (puthash one 'runtime-one +language-server-runtime--project-contexts)
      (puthash two 'runtime-two +language-server-runtime--project-contexts)
      (puthash one 'config-one +language-server-runtime--configurations)
      (puthash two 'config-two +language-server-runtime--configurations)
      (puthash +language-server-runtime--workspace-metadata-key "one"
               (lsp--workspace-metadata workspace))
      (+language-server-runtime-uninitialized-h workspace)
      (should-not (gethash one +language-server-runtime--project-contexts))
      (should-not (gethash one +language-server-runtime--configurations))
      (should (eq (gethash two +language-server-runtime--project-contexts)
                  'runtime-two))
      (should (eq (gethash two +language-server-runtime--configurations)
                  'config-two)))))

(provide 'init-lsp-runtime-tests)
;;; init-lsp-runtime-tests.el ends here

(ert-deftest +noema-jupyter-kernel-change-preserves-source-lsp ()
  "A new execution kernel does not detach a file-owned analyzer."
  (require 'noema/jupyter/cell)
  (with-temp-buffer
    (setq-local +noema-jupyter-cell-kernel "old"
                +noema-jupyter-cell-language "python"
                lsp-managed-mode t)
    (cl-letf (((symbol-function 'lsp-disconnect)
               (lambda () (ert-fail "Kernel switch disconnected LSP")))
              ((symbol-function '+noema-jupyter-cell--auto-switch-language-mode)
               #'ignore))
      (+noema-jupyter-cell--apply-session-snapshot
       '((document . ((kernel . "new") (language . "python"))))))
    (should (equal +noema-jupyter-cell-kernel "new"))
    (should lsp-managed-mode)))

(ert-deftest +lsp-nested-envrcs-own-separate-roots-and-toolchains ()
  "Buffers in one project may use independent environments at once."
  (let* ((project (make-temp-file "lsp-multi-env-" t))
         (one (expand-file-name "one/" project))
         (two (expand-file-name "two/" project))
         (first (generate-new-buffer " *lsp-env-one*"))
         (second (generate-new-buffer " *lsp-env-two*"))
         (+enable-direnv t))
    (unwind-protect
        (progn
          (make-directory one t)
          (make-directory two t)
          (with-temp-file (expand-file-name ".envrc" one))
          (with-temp-file (expand-file-name ".envrc" two))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _) 'test-project))
                    ((symbol-function 'project-root)
                     (lambda (_) project)))
            (with-current-buffer first
              (setq-local buffer-file-name (expand-file-name "a.ipynb" one)
                          default-directory one)
              (should (equal (+language-server--project-root-for-buffer)
                             (remote-canonicalize-file-name one)))
              (should (equal (+language-server-toolchain--canonical-root)
                             (remote-canonicalize-file-name one))))
            (with-current-buffer second
              (setq-local buffer-file-name (expand-file-name "b.ipynb" two)
                          default-directory two)
              (should (equal (+language-server--project-root-for-buffer)
                             (remote-canonicalize-file-name two)))
              (should (equal (+language-server-toolchain--canonical-root)
                             (remote-canonicalize-file-name two))))))
      (kill-buffer first)
      (kill-buffer second)
      (delete-directory project t))))

(ert-deftest +lsp-direnv-ancestor-keeps-specific-project-root ()
  (let* ((parent (make-temp-file "lsp-env-ancestor-" t))
         (project (expand-file-name "project/" parent))
         (+enable-direnv t))
    (unwind-protect
        (progn
          (make-directory project t)
          (with-temp-file (expand-file-name ".envrc" parent))
          (with-temp-buffer
            (setq-local buffer-file-name (expand-file-name "a.ipynb" project)
                        default-directory project)
            (cl-letf (((symbol-function 'project-current)
                       (lambda (&rest _) 'test-project))
                      ((symbol-function 'project-root)
                       (lambda (_) project)))
              (should (equal (+language-server--project-root-for-buffer)
                             (remote-canonicalize-file-name project))))))
      (delete-directory parent t))))

(ert-deftest +lsp-ignores-project-root-outside-current-buffer ()
  "A stale project selection cannot move another file's LSP workspace."
  (let* ((directory (make-temp-file "lsp-current-file-" t))
         (other (make-temp-file "lsp-stale-project-" t))
         (+enable-direnv nil))
    (unwind-protect
        (with-temp-buffer
          (setq-local buffer-file-name (expand-file-name "a.ipynb" directory)
                      default-directory directory)
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _) 'stale-project))
                    ((symbol-function 'project-root)
                     (lambda (_) other)))
            (should (equal (+language-server--project-root-for-buffer)
                           (file-name-as-directory
                            (remote-canonicalize-file-name directory))))))
      (delete-directory directory t)
      (delete-directory other t))))

(ert-deftest +noema-jupyter-kernel-capf-precedes-lsp-capf ()
  "Live kernel names retain the completion priority they had under Eglot."
  (require 'noema/jupyter/lsp)
  (with-temp-buffer
    (setq-local +noema-jupyter-cell-mode t)
    (setq-local completion-at-point-functions
                '(lsp-completion-at-point
                  +noema-jupyter-cell-capf t))
    (+noema-jupyter-cell--lsp-capf-priority-h)
    (should
     (equal completion-at-point-functions
            '(+noema-jupyter-cell-capf
              lsp-completion-at-point t)))))

(ert-deftest +noema-jupyter-keeps-lsp-in-tab-line-above-its-header ()
  (require 'noema/jupyter/lsp)
  (with-temp-buffer
    (setq-local +noema-jupyter-cell-mode t)
    (setq-local lsp-managed-mode t)
    (setq-local header-line-format
                '((t (:eval (window-parameter nil 'lsp-headerline--string)))
                  (:eval (+noema-jupyter-cell--header-line))))
    (+lsp-tab-line-sync-h)
    (+noema-jupyter-cell--lsp-ui-h)
    (should
     (equal header-line-format
            '(:eval (+noema-jupyter-cell--header-line))))
    (should
     (memq #'+lsp-tab-line-breadcrumb
           +tab-line-leading-segment-functions))
    (setq-local lsp-managed-mode nil)
    (+lsp-tab-line-sync-h)
    (should-not
     (memq #'+lsp-tab-line-breadcrumb
           +tab-line-leading-segment-functions))))

(ert-deftest +noema-jupyter-lsp-breadcrumb-is-view-only-after-moving ()
  (require 'noema/jupyter/lsp)
  (let* ((map (make-sparse-keymap))
         (breadcrumb
          (propertize "project > symbol"
                      'local-map map
                      'mouse-face 'highlight))
         (rendered (+breadcrumb-view-only-string breadcrumb)))
    (should-not (get-text-property 0 'local-map rendered))
    (should-not (get-text-property 0 'mouse-face rendered))))
