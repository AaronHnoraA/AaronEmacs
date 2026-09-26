;;; remote-compat-tests.el --- Upgrade and provider contract tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'remote-framework)

(ert-deftest remote-canonical-expand-preserves-plain-and-normalizes-dots ()
  (should
   (equal (remote-fs-handle-expand-file-name
           "/fs:local:/tmp/plain\nname")
          "/fs:local:/tmp/plain\nname"))
  (should
   (equal (remote-fs-handle-expand-file-name
           "/fs:local:/tmp/.git/config")
          "/fs:local:/tmp/.git/config"))
  (should
   (equal (remote-fs-handle-expand-file-name
           "/fs:local:/tmp/branch/../value")
          "/fs:local:/tmp/value"))
  (should
   (equal (remote-fs-handle-expand-file-name
           "/fs:local:/tmp//value")
          "/fs:local:/tmp/value")))

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

(ert-deftest remote-accelerator-index-observes-provider-replacement ()
  (let ((remote-operation-providers nil)
        (remote-operation-provider-index (make-hash-table :test #'eq))
        (remote-operation-provider-index-source nil))
    (remote-register-operation-provider
     "registered" :operations '(file-attributes)
     :invoke (lambda (&rest _) nil))
    (should
     (equal (remote-operation-provider-id
             (remote-operation-provider-for
              'file-attributes nil nil nil nil))
            "registered"))
    (let ((remote-operation-providers
           (list (remote-operation-provider-create
                  :id "replacement" :operations '(file-attributes)
                  :invoke-function (lambda (&rest _) nil)))))
      (should
       (equal (remote-operation-provider-id
               (remote-operation-provider-for
                'file-attributes nil nil nil nil))
              "replacement")))
    (should-not
     (remote-operation-provider-for 'file-exists-p nil nil nil nil))))

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

(ert-deftest remote-tramp-rpc-upgrade-removes-unverified-private-shims ()
  "A newer client must not inherit advice written for the verified release."
  (should
   (remote-backend-tramp-rpc--verified-release-p
    '(:client-version "0.13.1" :git-checkout t
      :revision "e1d4632d576ecf2472c321de1e713b776ea2b78f"
      :release-checkout t)))
  (should-not
   (remote-backend-tramp-rpc--verified-release-p
    '(:client-version "0.14.0" :git-checkout t
      :revision "e1d4632d576ecf2472c321de1e713b776ea2b78f"
      :release-checkout t)))
  (should-not
   (remote-backend-tramp-rpc--verified-release-p
    '(:client-version "0.13.1" :git-checkout t
      :revision "e1d4632d576ecf2472c321de1e713b776ea2b78f"
      :release-checkout nil)))
  (should-not
   (remote-backend-tramp-rpc--verified-release-p
    '(:client-version "0.13.1" :git-checkout t
      :revision "different-commit" :release-checkout t)))
  (should-not
   (remote-backend-tramp-rpc--verified-release-p
    '(:client-version "0.13.1" :git-checkout nil
      :revision "e1d4632d576ecf2472c321de1e713b776ea2b78f"
      :release-checkout t)))
  (let ((original-require (symbol-function 'require))
        (pairs
         '((tramp-rpc-deploy--arch-to-rust-target
            . remote-backend-tramp-rpc--arch-to-rust-target-a)
           (tramp-rpc-handle-make-process
            . remote-backend-tramp-rpc--local-relay-cwd-a)
           (tramp-rpc--controlmaster-socket-path
            . remote-backend-tramp-rpc--local-controlmaster-path-a)
           (tramp-rpc--ensure-controlmaster-directory
            . remote-backend-tramp-rpc--local-controlmaster-path-a)
           (tramp-rpc--call
            . remote-backend-tramp-rpc--adapter-timeout-a)
           (tramp-rpc--acl-enabled-p
            . remote-backend-tramp-rpc--acl-enabled-a)
           (tramp-rpc--selinux-enabled-p
            . remote-backend-tramp-rpc--selinux-enabled-a)
           (tramp-rpc-handle-write-region
            . remote-backend-tramp-rpc--write-region-in-place-a)
           (file-extended-attributes
            . remote-backend-tramp-rpc--extended-attributes-in-place-a)
           (tramp-rpc--deliver-process-output
            . remote-backend-tramp-rpc--closed-relay-exit-a)
           (tramp-rpc--compute-remote-path
            . remote-backend-tramp-rpc--compute-remote-path-a))))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest arguments)
                 (pcase feature
                   ('msgpack nil)
                   ('tramp-rpc-deploy t)
                   (_ (apply original-require feature arguments)))))
              ((symbol-function 'remote-backend-tramp-rpc-release-contract)
               (lambda () '(:client-version "0.14.0"
                            :release-checkout t)))
              ((symbol-function 'tramp-rpc-deploy--arch-to-rust-target)
               (lambda (_architecture) nil))
              ((symbol-function 'tramp-rpc-handle-make-process)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'tramp-rpc--controlmaster-socket-path)
               (lambda (_vector) nil))
              ((symbol-function 'tramp-rpc--ensure-controlmaster-directory)
               (lambda () nil))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vector _method _params &optional _connection) nil))
              ((symbol-function 'tramp-rpc--call-with-timeout)
               (lambda (_vector _method _params _timeout _poll
                        &optional _connection)
                 nil))
              ((symbol-function 'tramp-rpc--acl-enabled-p)
               (lambda (_vector) nil))
              ((symbol-function 'tramp-rpc--selinux-enabled-p)
               (lambda (_vector) nil))
              ((symbol-function 'tramp-rpc-handle-write-region)
               (lambda (_start _end _filename &optional _append _visit
                        _lockname _mustbenew)
                 nil))
              ((symbol-function 'tramp-rpc--deliver-process-output)
               (lambda (_process _stdout _stderr _buffer) nil))
              ((symbol-function 'tramp-rpc--compute-remote-path)
               (lambda (_vector) nil)))
      (dolist (pair pairs)
        (advice-add (car pair) :around (cdr pair))
        (should (advice-member-p (cdr pair) (car pair))))
      (remote-backend-tramp-rpc-install)
      (dolist (pair pairs)
        (should-not (advice-member-p (cdr pair) (car pair)))))))

(ert-deftest remote-tramp-rpc-in-place-metadata-scope-is-exact ()
  "Only the current RPC write can bypass redundant metadata commands."
  (let ((file "/tmp/current")
        (calls nil))
    (should
     (eq
      (remote-backend-tramp-rpc--write-region-in-place-a
       (lambda (_start _end _filename &rest _options)
         (should-not
          (remote-backend-tramp-rpc--extended-attributes-in-place-a
           (lambda (_path) (push 'current calls) 'metadata)
           file))
         (should
          (eq
           (remote-backend-tramp-rpc--extended-attributes-in-place-a
            (lambda (_path) (push 'other calls) 'metadata)
            "/tmp/other")
           'metadata))
         'sent)
       "content" nil file)
      'sent))
    (should (equal calls '(other)))
    (should-not remote-backend-tramp-rpc--in-place-write-file)
    (let ((remote-backend-tramp-rpc--in-place-write-file file)
          (remote-backend-tramp-rpc-skip-in-place-metadata-roundtrip nil))
      (should
       (eq
        (remote-backend-tramp-rpc--extended-attributes-in-place-a
         (lambda (_path) 'metadata) file)
        'metadata)))))

(ert-deftest remote-tramp-rpc-closed-relay-exit-race-is-narrow ()
  "Only a closed write end on an exited relay is safe to discard."
  (let ((process (make-pipe-process
                  :name "remote-closed-relay-test" :noquery t)))
    (unwind-protect
        (let ((calls 0)
              (closed
               (format "Output file descriptor of %s is closed"
                       (process-name process))))
          (cl-labels ((deliver (_process _stdout _stderr _buffer)
                        (cl-incf calls)
                        (error "%s" closed)))
            (should-error
            (remote-backend-tramp-rpc--closed-relay-exit-a
              #'deliver process "data" nil nil))
            (process-put process :tramp-rpc-exited t)
            (should-not
             (remote-backend-tramp-rpc--closed-relay-exit-a
              #'deliver process "data" nil nil))
            (should (= calls 2))
            (should-error
             (remote-backend-tramp-rpc--closed-relay-exit-a
              (lambda (&rest _) (error "different failure"))
              process "data" nil nil))))
      (delete-process process))))

(ert-deftest remote-tramp-rpc-attribute-probes-cache-per-live-generation ()
  "A successful probe is reused only on its own live RPC transport."
  (let* ((remote-backend-tramp-rpc-attribute-probe-ttl 60)
         (first (make-pipe-process
                 :name "remote-attribute-first-test" :noquery t))
         (second (make-pipe-process
                  :name "remote-attribute-second-test" :noquery t))
         (current first)
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                   (lambda (_vector) (list :process current)))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (_vector method params &optional _connection)
                     (should (equal method "process.run"))
                     (should (equal (alist-get 'cmd params) "getfacl"))
                     (cl-incf calls)
                     '((exit_code . 0)))))
          (should (remote-backend-tramp-rpc--acl-enabled-a
                   #'ignore 'vector))
          (should (remote-backend-tramp-rpc--acl-enabled-a
                   #'ignore 'vector))
          (should (= calls 1))
          (setq current second)
          (should (remote-backend-tramp-rpc--acl-enabled-a
                   #'ignore 'vector))
          (should (= calls 2))
          (process-put second 'remote-backend-tramp-rpc--acl-available
                       (cons (- (float-time) 61) t))
          (should (remote-backend-tramp-rpc--acl-enabled-a
                   #'ignore 'vector))
          (should (= calls 3))
          (let ((remote-backend-tramp-rpc-attribute-probe-ttl 0))
            (should (remote-backend-tramp-rpc--acl-enabled-a
                     #'ignore 'vector)))
          (should (= calls 4)))
      (delete-process first)
      (delete-process second))))

(ert-deftest remote-tramp-rpc-attribute-probe-does-not-cache-on-replacement ()
  "A probe completed across a transport swap cannot populate the new cache."
  (let* ((remote-backend-tramp-rpc-attribute-probe-ttl 60)
         (first (make-pipe-process
                 :name "remote-attribute-race-first" :noquery t))
         (second (make-pipe-process
                  :name "remote-attribute-race-second" :noquery t))
         (current first)
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                   (lambda (_vector) (list :process current)))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (&rest _arguments)
                     (cl-incf calls)
                     (setq current second)
                     '((exit_code . 0)))))
          (should (remote-backend-tramp-rpc--acl-enabled-a
                   #'ignore 'vector))
          (should-not
           (process-get second 'remote-backend-tramp-rpc--acl-available))
          (should (remote-backend-tramp-rpc--acl-enabled-a
                   #'ignore 'vector))
          (should (= calls 2)))
      (delete-process first)
      (delete-process second))))

(ert-deftest remote-tramp-rpc-attribute-probes-cache-only-proven-missing-command ()
  "Missing-command results are cached; transport failures remain retryable."
  (dolist (missing
           '((file-missing "RPC" "No such file" "spawn failed"
                           ((os_errno . 2) (spawn_not_found . t)))
             (file-missing
              "RPC No such file Failed to spawn process: No such file or directory (os error 2) ((os_errno . 2) (spawn_not_found . t))")))
    (let ((process (make-pipe-process
                    :name "remote-attribute-missing-test" :noquery t))
          (remote-backend-tramp-rpc-attribute-probe-ttl 60)
          (calls 0))
      (unwind-protect
          (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                     (lambda (_vector) (list :process process)))
                    ((symbol-function 'tramp-rpc--call)
                     (lambda (&rest _arguments)
                       (cl-incf calls)
                       (signal (car missing) (cdr missing)))))
            (should-not
             (remote-backend-tramp-rpc--selinux-enabled-a
              #'ignore 'vector))
            (should-not
             (remote-backend-tramp-rpc--selinux-enabled-a
              #'ignore 'vector))
            (should (= calls 1)))
        (delete-process process))))
  (let ((process (make-pipe-process
                  :name "remote-attribute-error-test" :noquery t))
        (remote-backend-tramp-rpc-attribute-probe-ttl 60)
        (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                   (lambda (_vector) (list :process process)))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (&rest _arguments)
                     (cl-incf calls)
                     (signal 'remote-file-error '("transport lost")))))
          (should-not
           (remote-backend-tramp-rpc--selinux-enabled-a
            #'ignore 'vector))
          (should-not
           (remote-backend-tramp-rpc--selinux-enabled-a
            #'ignore 'vector))
          (should (= calls 2)))
      (delete-process process)))
  (let ((process (make-pipe-process
                  :name "remote-attribute-nonzero-test" :noquery t))
        (remote-backend-tramp-rpc-attribute-probe-ttl 60)
        (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                   (lambda (_vector) (list :process process)))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (&rest _arguments)
                     (cl-incf calls)
                     '((exit_code . 1)))))
          (should-not
           (remote-backend-tramp-rpc--selinux-enabled-a
            #'ignore 'vector))
          (should-not
           (remote-backend-tramp-rpc--selinux-enabled-a
            #'ignore 'vector))
          (should (= calls 2)))
      (delete-process process))))

(ert-deftest remote-tramp-rpc-timeout-advice-preserves-connection-generation ()
  "An RPC request pinned to an old connection must never use a new one."
  (let ((remote-current-adapter-id "environment")
        seen)
    (cl-letf (((symbol-function 'tramp-rpc--call-with-timeout)
               (lambda (&rest arguments)
                 (setq seen arguments)
                 'timed)))
      (should
       (eq (remote-backend-tramp-rpc--adapter-timeout-a
            (lambda (&rest _) (ert-fail "Unexpected ordinary RPC call"))
            'vector "process.run" 'params 'generation)
           'timed))
      (should
       (equal seen '(vector "process.run" params 60 0.1 generation))))
    (should
     (eq (remote-backend-tramp-rpc--adapter-timeout-a
          (lambda (&rest arguments)
            (setq seen arguments)
            'ordinary)
          'vector "system.info" 'params 'generation)
         'ordinary))
    (should (equal seen '(vector "system.info" params generation)))))

(ert-deftest remote-tramp-rpc-read-query-deadlines-keep-writes-unchanged ()
  "Only a routed retry-safe read receives the shorter RPC deadline."
  (let ((remote-fs-current-retry-safe-query t)
        (generation (list :process nil))
        seen)
    (cl-letf (((symbol-function 'tramp-rpc--call-with-timeout)
               (lambda (&rest arguments)
                 (setq seen arguments)
                 'bounded)))
      (should
       (eq (remote-backend-tramp-rpc--adapter-timeout-a
            (lambda (&rest _) (ert-fail "Expected a bounded RPC"))
            'vector "file.stat" 'params generation)
           'bounded))
      (should
       (equal seen (list 'vector "file.stat" 'params 5 0.1 generation)))
      (should
       (eq (remote-backend-tramp-rpc--adapter-timeout-a
            (lambda (&rest _) (ert-fail "Expected a bounded RPC"))
            'vector "dir.list" 'params generation)
           'bounded))
      (should
       (equal seen (list 'vector "dir.list" 'params 10 0.1 generation))))
    (should
     (eq (remote-backend-tramp-rpc--adapter-timeout-a
          (lambda (&rest arguments)
            (setq seen arguments)
            'upstream)
          'vector "file.write" 'params generation)
         'upstream))
    (should (equal seen (list 'vector "file.write" 'params generation)))
    (let ((remote-fs-current-retry-safe-query nil))
      (should
       (eq (remote-backend-tramp-rpc--adapter-timeout-a
            (lambda (&rest arguments)
              (setq seen arguments)
              'upstream)
            'vector "file.stat" 'params generation)
           'upstream))
      (should (equal seen (list 'vector "file.stat" 'params generation))))))

(ert-deftest remote-tramp-rpc-read-timeout-retires-only-current-generation ()
  "A timed-out request must never close a replacement SSH process."
  (let* ((remote-fs-current-retry-safe-query t)
         (process (make-pipe-process
                   :name "remote-read-timeout-test" :noquery t))
         (generation (list :process process))
         (current generation)
         (deletions 0))
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call-with-timeout)
                   (lambda (&rest _)
                     (signal 'remote-file-error
                             '("Timeout waiting for RPC response"))))
                  ((symbol-function 'tramp-rpc--get-connection)
                   (lambda (_vector) current))
                  ((symbol-function 'delete-process)
                   (lambda (_process) (cl-incf deletions))))
          (should-error
           (remote-backend-tramp-rpc--adapter-timeout-a
            #'ignore 'vector "file.stat" 'params generation)
           :type 'remote-file-error)
          (should (= deletions 1))
          (setq current (list :process process))
          (should-error
           (remote-backend-tramp-rpc--adapter-timeout-a
            #'ignore 'vector "file.stat" 'params generation)
           :type 'remote-file-error)
          (should (= deletions 1))
          (let ((remote-fs-current-retry-safe-query nil)
                (remote-current-adapter-id "environment"))
            (setq current generation)
            (should-error
             (remote-backend-tramp-rpc--adapter-timeout-a
              #'ignore 'vector "process.run" 'params generation)
             :type 'remote-file-error)
            (should (= deletions 2))))
      (delete-process process)))
  (should
   (eq (plist-get
        (remote-backend-tramp-rpc-classify-error
         '(remote-file-error "Timeout waiting for RPC response")
         'file-read)
        :scope)
       'transport)))

(ert-deftest remote-tramp-rpc-path-batch-keeps-order-and-falls-back ()
  "One target check preserves PATH order and rejects malformed responses."
  (let (request)
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vector method params &optional _connection)
                 (setq request (cons method params))
                 '((exit_code . 0) (stdout . "101"))))
              ((symbol-function 'tramp-rpc--decode-output)
               (lambda (data _encoding) data)))
      (should
       (equal
        (remote-backend-tramp-rpc--existing-paths
         'vector '("/bin" "/missing path" "/bin/odd;name"))
        '(t "/bin" "/bin/odd;name")))
      (should (equal (car request) "process.run"))
      (should
       (equal (append (alist-get 'args (cdr request)) nil)
              (list "-c" remote-backend-tramp-rpc--path-directory-script
                    "sh" "/bin" "/missing path" "/bin/odd;name")))))
  (cl-letf (((symbol-function 'tramp-rpc--call)
             (lambda (&rest _) '((exit_code . 0) (stdout . "bad"))))
            ((symbol-function 'tramp-rpc--decode-output)
             (lambda (data _encoding) data)))
    (should-not
     (remote-backend-tramp-rpc--existing-paths
      'vector '("/bin" "/missing"))))
  (let ((fallback 0))
    (cl-letf (((symbol-function 'tramp-rpc--effective-remote-path-spec)
               (lambda (_vector) '("/bin")))
              ((symbol-function 'tramp-rpc--expand-remote-path-entry)
               (lambda (_vector entry) entry))
              ((symbol-function 'tramp-rpc--append-path-entries)
               (lambda (entries result) (append result entries)))
              ((symbol-function
                'remote-backend-tramp-rpc--existing-paths)
               (lambda (_vector _paths) nil)))
      (should
       (equal
        (remote-backend-tramp-rpc--compute-remote-path-a
         (lambda (_vector)
           (cl-incf fallback)
           '("/upstream/bin"))
         'vector)
        '("/upstream/bin")))
      (should (= fallback 1)))))

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

(ert-deftest remote-inotify-records-preserve-newlines-and-partial-frames ()
  (let* ((process (make-pipe-process
                   :name "remote-inotify-framing-test" :noquery t))
         (watch (remote-file-watch-create
                 :generation 1 :recursive t))
         delivered)
    (unwind-protect
        (cl-letf (((symbol-function 'remote-fs--watch-deliver)
                   (lambda (_watch event) (push event delivered))))
          (remote-fs--inotify-filter
           watch 1 process "CREATE,ISDIR\0/tmp/new\nfolder\0\nMODIFY\0/tmp/file")
          (should (= (length delivered) 1))
          (remote-fs--inotify-filter watch 1 process "\nname\0\n")
          (should
           (equal (mapcar (lambda (event) (list (nth 1 event) (nth 2 event)))
                          (nreverse delivered))
                  '((created "/tmp/new\nfolder")
                    (changed "/tmp/file\nname")))))
      (delete-process process))))

(ert-deftest remote-native-metadata-cache-rechecks-mutable-route-and-workspace ()
  (let* ((remote-targets (copy-hash-table remote-targets))
         (remote-links (copy-hash-table remote-links))
         (remote-fs-native-route-cache (make-hash-table :test #'equal))
         (remote-fs-context-cache (make-hash-table :test #'equal))
         (target (copy-remote-target (remote-get-target "local")))
         (link-id (car (remote-target-links target)))
         (link (copy-remote-pipeline (remote-get-link link-id)))
         (path "/fs:local:/tmp/"))
    (puthash "local" target remote-targets)
    (puthash link-id link remote-links)
    (remote-fs--routes "emacs-file" 'metadata (remote-context path))
    (should (remote-fs--cached-native-query 'file-exists-p (list path)))
    (setf (remote-link-plugin-ids link) '("tramp"))
    (should-not (remote-fs--cached-native-query 'file-exists-p (list path)))
    (setf (remote-link-plugin-ids link) '("native")
          (remote-target-workspaces target)
          '(((id . "edited") (path . "/tmp/"))))
    (should-not (remote-fs--cached-native-query 'file-exists-p (list path)))
    (should (equal (remote-context-workspace-id
                    (remote-fs--context-for-file path))
                   "edited"))))

(ert-deftest remote-foreign-handler-normalizes-file-name-and-vector-inputs ()
  (let* ((file "/fs:lab:/srv/project")
         (vector (tramp-dissect-file-name file nil)))
    (should (remote-fs-foreign-p file))
    (should (remote-fs-foreign-p vector))
    (should-not (remote-fs-foreign-p "/ssh:lab:/srv/project"))
    (should-not (remote-fs-foreign-p '(not a file name)))))

(ert-deftest remote-logical-path-helpers-preserve-newline-names ()
  (let ((logical "/fs:local:/tmp/line\nbreak.txt"))
    (should (equal (remote-fs-target-id logical) "local"))
    (should (equal (remote-fs-localname logical)
                   "/tmp/line\nbreak.txt"))
    (should (equal (remote-canonicalize-file-name logical) logical))))

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

(ert-deftest remote-tramp-rpc-private-arity-sees-through-other-advice ()
  "Tracing advice must not hide the verified upstream call contract."
  (let ((remote-backend-tramp-rpc--private-contracts
         '((remote-compat--advised-function . (1 . 1))))
        (wrapper (lambda (original &rest arguments)
                   (apply original arguments))))
    (unwind-protect
        (progn
          (fset 'remote-compat--advised-function
                (lambda (_argument) nil))
          (advice-add 'remote-compat--advised-function :around wrapper)
          (should
           (remote-backend-tramp-rpc--private-compatible-p
            'remote-compat--advised-function))
          (should
           (equal (remote-backend-tramp-rpc--upstream-arity
                   'remote-compat--advised-function)
                  '(1 . 1)))
          (advice-remove 'remote-compat--advised-function wrapper)
          (fset 'remote-compat--advised-function
                (lambda (_one _two) nil))
          (advice-add 'remote-compat--advised-function :around wrapper)
          (should-not
           (remote-backend-tramp-rpc--private-compatible-p
            'remote-compat--advised-function)))
      (advice-remove 'remote-compat--advised-function wrapper)
      (fmakunbound 'remote-compat--advised-function))))

(ert-deftest remote-tramp-rpc-transport-exit-ignores-stale-generation ()
  "A late old SSH sentinel must not invalidate its replacement session."
  (let* ((remote-workspaces (make-hash-table :test #'equal))
         (route (remote-route-create
                 :target-id "box" :link-id "box/ssh"
                 :link-plugin-id "tramp-rpc"))
         (owner (remote-workspace-create :routes (list route)))
         (connection (remote-connection-create
                      :handle "/rpc:box:/tmp/"))
         (process (make-pipe-process
                   :name "remote-stale-generation-test" :noquery t))
         (current 'replacement)
         (reports 0))
    (unwind-protect
        (progn
          (puthash 'owner owner remote-workspaces)
          (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                     (lambda (_vec) (list :process current)))
                    ((symbol-function 'tramp-rpc--connection-key)
                     (lambda (_vec) 'same-connection))
                    ((symbol-function 'tramp-dissect-file-name)
                     (lambda (_name &optional _nodefault) 'vec))
                    ((symbol-function 'remote-connection-cached-p)
                     (lambda (_route) connection))
                    ((symbol-function 'remote-report-route-failure)
                     (lambda (_route _error) (cl-incf reports))))
            (remote-backend-tramp-rpc--transport-death-before-a
             process 'vec "old process exited")
            (should (= reports 0))
            (setq current process)
            (remote-backend-tramp-rpc--transport-death-before-a
             process 'vec "current process exited")
            (should (= reports 1))))
      (delete-process process))))

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
