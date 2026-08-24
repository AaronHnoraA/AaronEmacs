;;; remote-compat-tests.el --- Upgrade and provider contract tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'remote-framework)

(ert-deftest remote-compat-report-describes-capabilities-not-versions ()
  (let ((report (remote-compat-report)))
    (should (stringp (plist-get report :emacs-version)))
    (should (plist-member report :foreign-handler-registration))
    (should (plist-member report :external-operations))
    (should (plist-member report :structured-errors))
    (should (plist-member report :public-file-notify))))

(ert-deftest remote-backend-bridge-is-the-only-probe-owner ()
  (let ((remote-backends (make-hash-table :test #'equal))
        (remote-backend-contracts (make-hash-table :test #'equal))
        (remote-link-plugins (make-hash-table :test #'equal))
        (calls 0))
    (remote-register-link-plugin "legacy" :capabilities '(metadata))
    (should-not
     (remote-link-plugin-backend-id
      (remote-get-link-plugin "legacy")))
    (remote-register-backend
     "negotiated" :capabilities '(metadata)
     :project (lambda (file _pipeline _route) file)
     :probe
     (lambda (_route _context _handle)
       (cl-incf calls)
       '(:status ok :protocol-version "test/1")))
    (let* ((plugin (remote-get-link-plugin "negotiated"))
           (route
            (remote-route-create
             :target-id "lab" :pipeline-id "lab/test"
             :backend-id "negotiated" :capability 'metadata
             :adapter-id "emacs-file"))
           (context
            (remote-context-create
             :target-id "lab" :localname "/"
             :workspace-root "/fs:lab:/")))
      (should (equal (remote-link-plugin-backend-id plugin) "negotiated"))
      (should
       (equal (plist-get (remote-backend-probe route context) :status) 'ok))
      (remote-backend-probe route context)
      (should (= calls 1))
      (remote-backend-probe route context nil t)
      (should (= calls 2)))))

(ert-deftest remote-backend-probe-rejects-typed-incompatibility ()
  (let ((remote-backends (make-hash-table :test #'equal))
        (remote-backend-contracts (make-hash-table :test #'equal))
        (remote-link-plugins (make-hash-table :test #'equal)))
    (remote-register-backend
     "future" :capabilities '(metadata)
     :project (lambda (file _pipeline _route) file)
     :probe
     (lambda (&rest _)
       '(:status incompatible :detail "protocol mismatch")))
    (let ((route
           (remote-route-create
            :target-id "lab" :pipeline-id "lab/test"
            :backend-id "future" :capability 'metadata
            :adapter-id "emacs-file"))
          (context
           (remote-context-create
            :target-id "lab" :localname "/"
            :workspace-root "/fs:lab:/")))
      (should-error (remote-backend-probe route context)
                    :type 'remote-backend-incompatible)
      ;; A cached negative result is still a gate, not merely a Doctor note.
      (should-error (remote-backend-probe route context)
                    :type 'remote-backend-incompatible))))

(ert-deftest remote-file-operation-supports-result-projectors ()
  (let ((remote-file-operations (make-hash-table :test #'eq))
        (remote-fs-file-name-handler-alist nil)
        seen)
    (let ((spec
           (remote-register-file-operation
            'remote-test-projection
            :capability 'metadata :path-arguments '(0)
            :result-projector
            (lambda (result target operation-spec)
              (setq seen
                    (list result target
                          (remote-file-operation-spec-operation
                           operation-spec)))
              (list :logical target :value result)))))
      (should
       (equal (remote-fs--transform-result spec 'physical "lab")
              '(:logical "lab" :value physical)))
      (should (equal seen '(physical "lab" remote-test-projection))))))

(ert-deftest remote-dir-locals-projector-preserves-its-public-result-union ()
  (should
   (equal
    (remote-accelerator--project-dir-locals-result
     "/ssh:lab:/srv/project/" "lab" nil)
    "/fs:lab:/srv/project/"))
  (should
   (equal
    (remote-accelerator--project-dir-locals-result
     '("/ssh:lab:/srv/project/" project-class (1 2 3 4)) "lab" nil)
    '("/fs:lab:/srv/project/" project-class (1 2 3 4))))
  (should-not
   (remote-accelerator--project-dir-locals-result nil "lab" nil)))

(ert-deftest remote-unknown-operation-detects-nested-physical-identities ()
  (should
   (remote-fs--physical-path-in-result-p
    '(:result [ok ((path . "/ssh:host:/srv/data"))])))
  (should-not
   (remote-fs--physical-path-in-result-p
    '(:result [ok ((path . "/fs:lab:/srv/data"))]))))

(ert-deftest remote-accelerator-selection-is-route-scoped-and-fallible ()
  (let ((remote-operation-providers nil)
        called)
    (remote-register-operation-provider
     "unavailable" :operations '(locate-dominating-file)
     :applicable (lambda (&rest _) nil)
     :invoke (lambda (&rest _) (error "must not run"))
     :append t)
    (remote-register-operation-provider
     "available" :operations '(locate-dominating-file)
     :applicable (lambda (&rest _) t)
     :invoke
     (lambda (_operation route _context args _default)
       (setq called (remote-route-link-plugin-id route))
       (car args))
     :append t)
    (let* ((route
            (remote-route-create
             :target-id "lab" :pipeline-id "lab/test"
             :backend-id "tramp" :capability 'metadata
             :adapter-id "emacs-file"))
           (context
            (remote-context-create
             :target-id "lab" :localname "/srv"
             :workspace-root "/fs:lab:/srv/"))
           (provider
            (remote-operation-provider-for
             'locate-dominating-file route context '("/ssh:lab:/srv")
             "/ssh:lab:/srv/")))
      (should (equal (remote-operation-provider-id provider) "available"))
      (should
       (equal
        (remote-operation-provider-call
         provider 'locate-dominating-file route context
         '("/ssh:lab:/srv") "/ssh:lab:/srv/")
        "/ssh:lab:/srv"))
      (should (equal called "tramp")))))

(ert-deftest remote-tramp-hlo-provider-does-not-enable-rpc-globally ()
  (let ((locate-dominating-stop-dir-regexp nil))
    (cl-letf (((symbol-function 'locate-library)
               (lambda (library &rest _)
                 (and (equal library "tramp-hlo") "/tmp/tramp-hlo.el"))))
      (let ((rpc
             (remote-route-create
              :target-id "lab" :pipeline-id "lab/test"
              :backend-id "tramp-rpc" :capability 'metadata
              :adapter-id "emacs-file"))
            (tramp
             (remote-route-create
              :target-id "lab" :pipeline-id "lab/test"
              :backend-id "tramp" :capability 'metadata
              :adapter-id "emacs-file")))
        (should-not
         (remote-accelerator--tramp-hlo-probe
          'locate-dominating-file rpc nil nil "/ssh:lab:/srv/"))
        (should
         (remote-accelerator--tramp-hlo-probe
          'locate-dominating-file tramp nil nil "/ssh:lab:/srv/"))))))

(ert-deftest remote-tramp-hlo-dir-locals-requires-path-preservation ()
  (let ((remote-accelerator-probe-cache (make-hash-table :test #'equal))
        (route
         (remote-route-create
          :target-id "lab" :pipeline-id "lab/test"
          :backend-id "tramp" :capability 'metadata
          :adapter-id "emacs-file")))
    (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) t))
              ((symbol-function 'process-file)
               (lambda (&rest _) 1)))
      (should-not
       (remote-accelerator--tramp-hlo-probe
        'dir-locals--all-files route nil '("/ssh:lab:/var/project/")
        "/ssh:lab:/var/project/")))))

(ert-deftest remote-tramp-rpc-release-contract-requires-exact-clean-tag ()
  (let ((was-bound (boundp 'tramp-rpc-deploy-version))
        (old-value (and (boundp 'tramp-rpc-deploy-version)
                        (symbol-value 'tramp-rpc-deploy-version))))
    (unwind-protect
        (progn
          (set 'tramp-rpc-deploy-version "1.2.3")
          (cl-letf
              (((symbol-function 'remote-backend-tramp-rpc--source-root)
                (lambda () "/checkout/"))
               ((symbol-function 'locate-dominating-file)
                (lambda (&rest _) "/checkout/"))
               ((symbol-function 'remote-backend-tramp-rpc--git-output)
                (lambda (_root &rest arguments)
                  (pcase arguments
                    ('("describe" "--exact-match" "--tags" "HEAD")
                     "v1.2.3")
                    ('("rev-parse" "HEAD") "abc123")
                    ('("status" "--porcelain" "--untracked-files=no")
                     "")))))
            (should
             (plist-get (remote-backend-tramp-rpc-release-contract)
                        :release-checkout))
            (cl-letf
                (((symbol-function 'remote-backend-tramp-rpc--git-output)
                  (lambda (_root &rest arguments)
                    (pcase arguments
                      ('("describe" "--exact-match" "--tags" "HEAD") nil)
                      ('("rev-parse" "HEAD") "def456")
                      ('("status" "--porcelain" "--untracked-files=no")
                       "")))))
              (should-not
               (plist-get (remote-backend-tramp-rpc-release-contract)
                          :release-checkout)))))
      (if was-bound
          (set 'tramp-rpc-deploy-version old-value)
        (makunbound 'tramp-rpc-deploy-version)))))

(ert-deftest remote-watch-terminal-close-enters-public-removal-api ()
  (let* ((remote-file-watches (make-hash-table :test #'equal))
         (watch
          (remote-file-watch-create
           :id "watch-test" :descriptor '(remote-file-watch . "watch-test")
           :physical-descriptor 'physical :state 'open))
         public-call physical-call)
    (puthash "watch-test" watch remote-file-watches)
    (cl-letf (((symbol-function 'file-notify-rm-watch)
               (lambda (descriptor)
                 (setq public-call descriptor)
                 (remote-fs-handle-file-notify-rm-watch descriptor)))
              ((symbol-function 'remote-fs--watch-remove-physical)
               (lambda (value)
                 (setq physical-call value)
                 (setf (remote-file-watch-physical-descriptor value) nil))))
      (remote-fs--watch-close watch 'explicit-close)
      (should (equal public-call '(remote-file-watch . "watch-test")))
      (should (eq physical-call watch))
      (should (eq (remote-file-watch-state watch) 'closed))
      (should-not (gethash "watch-test" remote-file-watches)))))

(ert-deftest remote-logical-watch-public-api-selects-fs-handler ()
  "Opaque logical descriptors must not fall back to TRAMP process handling."
  (let ((descriptor '(remote-file-watch . "watch-test"))
        seen-handler)
    (should
     (eq
      (remote-fs--logical-watch-public-api-a
       (lambda (_descriptor)
         (setq seen-handler
               (find-file-name-handler
                "/fs:box:/work/" 'file-notify-valid-p))
         'valid)
       descriptor)
      'valid))
    (should (eq seen-handler #'remote-fs-file-name-handler))))

(ert-deftest remote-inotify-events-map-to-public-file-notify-actions ()
  (should (eq (remote-fs--inotify-action "CREATE,ISDIR") 'created))
  (should (eq (remote-fs--inotify-action "MODIFY") 'changed))
  (should (eq (remote-fs--inotify-action "ATTRIB") 'attribute-changed))
  (should (eq (remote-fs--inotify-action "MOVED_FROM") 'deleted))
  (should (eq (remote-fs--inotify-action "MOVED_TO") 'created))
  (should (eq (remote-fs--inotify-action "IGNORED") 'stopped)))

(ert-deftest remote-foreign-handler-normalizes-file-name-and-vector-inputs ()
  (let* ((file "/fs:lab:/srv/project")
         (vector (tramp-dissect-file-name file nil)))
    (should (remote-fs-foreign-p file))
    (should (remote-fs-foreign-p vector))
    (should-not (remote-fs-foreign-p "/ssh:lab:/srv/project"))
    (should-not (remote-fs-foreign-p '(not a file name)))))

(ert-deftest remote-file-operation-surface-covers-active-tramp ()
  (let ((report (remote-file-operation-coverage-report)))
    (should (plist-get report :upstream))
    (should-not (plist-get report :missing))
    (dolist (operation '(tramp-get-home-directory tramp-get-remote-gid
                         tramp-get-remote-groups tramp-get-remote-uid
                         tramp-set-file-uid-gid file-local-name))
      (should (remote-get-file-operation operation)))))

(ert-deftest remote-external-operation-handlers-retain-public-signatures ()
  (dolist (pair
           '((locate-dominating-file .
              remote-accelerator-handle-locate-dominating-file)
             (dir-locals--all-files .
              remote-accelerator-handle-dir-locals--all-files)
             (dir-locals-find-file .
              remote-accelerator-handle-dir-locals-find-file)))
    (should
     (remote-compat-function-signatures-compatible-p
      (car pair) (cdr pair))))
  (when (remote-compat-tramp-external-operations-p)
    (should-error
     (remote-compat-tramp-add-external-operation
      'locate-dominating-file (lambda (&rest _) nil) 'remote-test)
     :type 'remote-operation-contract-error)))

(ert-deftest remote-tramp-hlo-new-optional-argument-falls-back ()
  (let ((route
         (remote-route-create
          :target-id "lab" :pipeline-id "lab/test"
          :backend-id "tramp" :capability 'metadata
          :adapter-id "emacs-file")))
    (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) t)))
      (should-not
       (remote-accelerator--tramp-hlo-probe
        'dir-locals--all-files route nil
        '("/ssh:lab:/srv/project/" t) "/ssh:lab:/srv/project/")))))

(ert-deftest remote-vector-operation-projects-through-selected-backend ()
  (let* ((logical (tramp-dissect-file-name "/fs:lab:/srv/project" nil))
         (route
          (remote-route-create
           :target-id "lab" :pipeline-id "lab/test"
           :backend-id "tramp" :capability 'metadata
           :adapter-id "emacs-file")))
    (cl-letf (((symbol-function 'remote-project-file-name)
               (lambda (_file _route) "/ssh:lab:/srv/project")))
      (let ((projected
             (car (remote-fs--translate-args
                   'tramp-get-remote-uid (list logical 'integer) route))))
        (should (tramp-file-name-p projected))
        (should (equal (tramp-file-name-method projected) "ssh"))
        (should (equal (tramp-file-name-localname projected)
                       "/srv/project"))))))

(ert-deftest remote-capability-registration-rejects-uncovered-surfaces ()
  (let ((remote-adapters (make-hash-table :test #'equal)))
    (should-error
     (remote-register-adapter "bad" :capabilities '(future-teleport)))
    (should-error
     (remote-register-adapter "incomplete-lsp" :capabilities '(lsp))))
  (let ((remote-backends (make-hash-table :test #'equal))
        (remote-link-plugins (make-hash-table :test #'equal))
        (remote-backend-contracts (make-hash-table :test #'equal)))
    (should-error
     (remote-register-backend "bad" :capabilities '(metadata))
     :type 'remote-operation-contract-error)))

(ert-deftest remote-backend-negotiation-cannot-invent-capabilities ()
  (let ((remote-backends (make-hash-table :test #'equal))
        (remote-link-plugins (make-hash-table :test #'equal))
        (remote-backend-contracts (make-hash-table :test #'equal)))
    (remote-register-backend
     "bad-probe" :capabilities '(metadata)
     :project (lambda (file _pipeline _route) file)
     :probe (lambda (&rest _)
              '(:status ok :capabilities (metadata file-read))))
    (let ((route
           (remote-route-create
            :target-id "lab" :pipeline-id "lab/test"
            :backend-id "bad-probe" :capability 'metadata
            :adapter-id "emacs-file"))
          (context
           (remote-context-create
            :target-id "lab" :localname "/"
            :workspace-root "/fs:lab:/")))
      (should-error
       (remote-backend-probe route context)
       :type 'remote-operation-contract-error))))

(ert-deftest remote-backend-contract-cache-is-session-generational ()
  (let ((remote-backends (make-hash-table :test #'equal))
        (remote-link-plugins (make-hash-table :test #'equal))
        (remote-backend-contracts (make-hash-table :test #'equal))
        (remote-connection-pool (make-hash-table :test #'equal))
        (calls 0))
    (remote-register-backend
     "generational" :capabilities '(metadata)
     :project (lambda (file _pipeline _route) file)
     :probe (lambda (&rest _) (cl-incf calls) '(:status ok)))
    (let* ((route
            (remote-route-create
             :target-id "lab" :pipeline-id "lab/test"
             :backend-id "generational" :capability 'metadata
             :adapter-id "emacs-file"))
           (key (remote-connection-route-key route))
           (context
            (remote-context-create
             :target-id "lab" :localname "/"
             :workspace-root "/fs:lab:/")))
      (puthash key (remote-connection-create :key key :generation 1)
               remote-connection-pool)
      (remote-backend-probe route context)
      (remote-backend-probe route context)
      (should (= calls 1))
      (puthash key (remote-connection-create :key key :generation 2)
               remote-connection-pool)
      (remote-backend-probe route context)
      (should (= calls 2)))))

(ert-deftest remote-session-close-invalidates-observations-as-one-unit ()
  (let* ((remote-connection-pool (make-hash-table :test #'equal))
         (remote-backend-contracts (make-hash-table :test #'equal))
         (remote-accelerator-probe-cache (make-hash-table :test #'equal))
         (remote-fs-path-expansion-cache (make-hash-table :test #'equal))
         (remote-path-facts-cache (make-hash-table :test #'equal))
         (remote-environment-cache (make-hash-table :test #'equal))
         (remote-environments-by-id (make-hash-table :test #'equal))
         (route
          (remote-route-create
           :target-id "local" :pipeline-id "local/native"
           :backend-id "native" :capability 'metadata
           :adapter-id "emacs-file"))
         (key (remote-connection-route-key route))
         (connection
          (remote-connection-create
           :key key :target-id "local" :link-id "local/native"
           :plugin-id "native" :state 'open :generation 41)))
    (puthash key connection remote-connection-pool)
    (puthash '("local" "local/native" "native" 41) '(:status ok)
             remote-backend-contracts)
    (puthash '("local" "local/native" locate-dominating-file 41) t
             remote-accelerator-probe-cache)
    (puthash '("local" "~/src") "/tmp/src"
             remote-fs-path-expansion-cache)
    (puthash "local" 'facts remote-path-facts-cache)
    (puthash '("local" providers) 'environment remote-environment-cache)
    (remote-connection-invalidate route nil 'test-close)
    (should-not (gethash key remote-connection-pool))
    (should (= (hash-table-count remote-backend-contracts) 0))
    (should (= (hash-table-count remote-accelerator-probe-cache) 0))
    (should (= (hash-table-count remote-fs-path-expansion-cache) 0))
    (should (= (hash-table-count remote-path-facts-cache) 0))
    (should (= (hash-table-count remote-environment-cache) 0))))

(ert-deftest remote-tramp-rpc-private-shims-degrade-on-shape-change ()
  (cl-letf (((symbol-function 'tramp-rpc--cached-system-info)
             (lambda (_one _two) nil)))
    (should-not
     (remote-backend-tramp-rpc--private-compatible-p
      'tramp-rpc--cached-system-info))))

(ert-deftest remote-background-retries-reentrancy-and-coalesces-jobs ()
  (let ((remote-background-jobs (make-hash-table :test #'equal))
        (remote-background-target-epochs (make-hash-table :test #'equal))
        (remote-background-retry-jitter 0)
        (attempts 0)
        delivered
        coalesced-delivered)
    (let* ((job
            (remote-background-submit
             '(test background)
             (lambda ()
               (cl-incf attempts)
               (if (= attempts 1)
                   (signal 'remote-file-error '("busy"))
                 'ready))
             :target-id "local"
             :owner-buffer nil
             :delays '(0)
             :callback (lambda (value) (setq delivered value))))
           (same
            (remote-background-submit
             '(test background) (lambda () 'wrong)
             :target-id "local" :owner-buffer nil
             :callback
             (lambda (value) (setq coalesced-delivered value)))))
      (should (eq job same))
      (cancel-timer (remote-background-job-timer job))
      (remote-background--run job)
      (should (= attempts 1))
      (cancel-timer (remote-background-job-timer job))
      (remote-background--run job)
      (should (= attempts 2))
      (should (eq delivered 'ready))
      (should (eq coalesced-delivered 'ready))
      (should-not (gethash '(test background) remote-background-jobs)))))

(ert-deftest remote-background-discards-observations-from-old-epochs ()
  (let ((remote-background-jobs (make-hash-table :test #'equal))
        (remote-background-target-epochs (make-hash-table :test #'equal))
        (remote-background-retry-jitter 0)
        (attempts 0)
        delivered)
    (let ((job
           (remote-background-submit
            '(test epoch)
            (lambda ()
              (cl-incf attempts)
              (when (= attempts 1)
                (remote-background-invalidate-target "local"))
              attempts)
            :target-id "local" :owner-buffer nil :delays '(0)
            :callback (lambda (value) (setq delivered value)))))
      (cancel-timer (remote-background-job-timer job))
      (remote-background--run job)
      (should-not delivered)
      (cancel-timer (remote-background-job-timer job))
      (remote-background--run job)
      (should (= delivered 2)))))

(ert-deftest remote-exec-none-effects-suppresses-only-its-cache-flush ()
  (let (seen)
    (cl-letf (((symbol-function 'remote--context-value)
               (lambda (&optional _) 'context))
              ((symbol-function 'remote--call-with-process-route)
               (lambda (_adapter _capability _context _constraints function)
                 (funcall function 'route temporary-file-directory nil)))
              ((symbol-function 'remote--prepare-backend-execution)
               (lambda (&rest _)
                 (remote-backend-execution-create
                  :physical-directory temporary-file-directory
                  :command '("true"))))
              ((symbol-function 'process-file)
               (lambda (&rest _)
                 (setq seen process-file-side-effects)
                 0)))
      (let ((process-file-side-effects t))
        (remote-exec "true" :filesystem-effects 'none)
        (should-not seen)
        (remote-exec "true" :filesystem-effects 'unknown)
        (should seen)))))

(ert-deftest remote-watch-deduplicates-and-resyncs-stopped-streams ()
  (let* ((remote-file-watches (make-hash-table :test #'equal))
         (remote-background-jobs (make-hash-table :test #'equal))
         (remote-background-target-epochs (make-hash-table :test #'equal))
         (remote-background-retry-jitter 0)
         (events 0)
         (rescans 0)
         (recoveries 0)
         (watch
          (remote-file-watch-create
           :id "watch-resync"
           :descriptor '(remote-file-watch . "watch-resync")
           :file "/fs:local:/tmp/" :target-id "local"
           :state 'open :sequence 0
           :callback (lambda (_event) (cl-incf events))
           :metadata
           (list :resync
                 (lambda (_watch _reason) (cl-incf rescans))))))
    (puthash "watch-resync" watch remote-file-watches)
    (remote-fs--watch-deliver
     watch '(physical changed "/tmp/value"))
    (remote-fs--watch-deliver
     watch '(physical changed "/tmp/value"))
    (should (= events 1))
    (should (= (remote-file-watch-sequence watch) 1))
    (cl-letf (((symbol-function 'remote-file-watch-recover)
               (lambda (value)
                 (cl-incf recoveries)
                 (setf (remote-file-watch-state value) 'open)
                 value)))
      (remote-fs--watch-deliver
       watch '(physical stopped "/tmp/value"))
      (let ((job
             (gethash '(file-watch-resync "watch-resync")
                      remote-background-jobs)))
        (should job)
        (cancel-timer (remote-background-job-timer job))
        (remote-background--run job)))
    (should (= events 2))
    (should (= rescans 1))
    (should (= recoveries 1))
    (should (eq (remote-file-watch-state watch) 'open))))

(provide 'remote-compat-tests)
;;; remote-compat-tests.el ends here
