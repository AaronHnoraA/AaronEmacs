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

(ert-deftest remote-tramp-file-name-brackets-an-ipv6-host ()
  "TRAMP has to be able to read the name back, port and all."
  (let* ((link (remote-link-create
                :id "v6/ssh" :target-id "v6"
                :config '(:host "2001:db8::1" :user "u" :port 2222)))
         (name (remote-backend-tramp-file-name "/tmp/x" link "ssh"))
         (vec (tramp-dissect-file-name name)))
    (should (equal name "/ssh:u@[2001:db8::1]#2222:/tmp/x"))
    (should (equal (tramp-file-name-host vec) "2001:db8::1"))
    (should (equal (tramp-file-name-port vec) "2222"))
    (should (equal (tramp-file-name-localname vec) "/tmp/x"))))

;; Startup alone installs the shims several times, and each install used to
;; spawn the three git commands of the release contract again.
(ert-deftest remote-tramp-rpc-git-answers-last-until-the-checkout-changes ()
  (let* ((root (file-name-as-directory (make-temp-file "rpc-git" t)))
         (remote-backend-tramp-rpc--git-cache nil)
         (spawns 0))
    (unwind-protect
        (cl-letf (((symbol-function 'remote-client-executable-find)
                   (lambda (&rest _) "git"))
                  ((symbol-function 'process-file)
                   (lambda (&rest _)
                     (cl-incf spawns)
                     (insert (format "answer %d\n" spawns))
                     0)))
          (make-directory (expand-file-name ".git" root))
          (write-region "ref" nil (expand-file-name ".git/HEAD" root)
                        nil 'silent)
          (should (equal (remote-backend-tramp-rpc--git-output root "tag")
                         "answer 1"))
          (should (equal (remote-backend-tramp-rpc--git-output root "tag")
                         "answer 1"))
          (should (= spawns 1))
          (should (equal (remote-backend-tramp-rpc--git-output root "rev")
                         "answer 2"))
          (set-file-times (expand-file-name ".git/HEAD" root) 0)
          (should (equal (remote-backend-tramp-rpc--git-output root "tag")
                         "answer 3")))
      (delete-directory root t))))

;;;; tramp-rpc connection generations

;; tramp-rpc 0.16.0 keeps each connection generation in a
;; `tramp-rpc-connection' record.  These tests run without tramp-rpc on the
;; load path, so they define the same slots the adapter reads.
(defvar tramp-rpc-protocol-error-file-not-found)

(unless (featurep 'tramp-rpc-connection)
  ;; Ask for the feature: expanding `cl-defstruct' at load time already
  ;; defines its predicate before this test runs.
  (cl-defstruct (tramp-rpc-connection
                 (:constructor tramp-rpc--make-connection)
                 (:copier nil))
    process buffer stderr-buffer vec cleanup-started probing))

(defmacro remote-tramp-rpc-test--readable (&rest body)
  "Run BODY with tramp-rpc connection generations readable."
  (declare (indent 0))
  `(let ((remote-backend-tramp-rpc--connection-shape t))
     ,@body))

(defun remote-tramp-rpc-test--generation (&rest slots)
  "Return a tramp-rpc connection generation with SLOTS."
  (apply #'tramp-rpc--make-connection slots))

(eval-and-compile
  (defun remote-tramp-rpc-test--arglist (arity)
    "Return a lambda list with ARITY."
    (let ((min (car arity)) (max (cdr arity)) args)
      (dotimes (i min) (push (intern (format "_a%d" i)) args))
      (cond
       ((eq max 'many) (push '&rest args) (push '_rest args))
       ((> max min)
        (push '&optional args)
        (dotimes (i (- max min))
          (push (intern (format "_o%d" i)) args))))
      (nreverse args))))

(defmacro remote-tramp-rpc-test--with-stub-seams (&rest body)
  "Run BODY with every shim target defined at its contracted arity."
  (declare (indent 0))
  `(cl-letf* ,(mapcar
               (lambda (entry)
                 `((symbol-function ',(car entry))
                   (lambda ,(remote-tramp-rpc-test--arglist (cdr entry))
                     (ignore ,@(seq-remove
                                (lambda (arg) (memq arg '(&optional &rest)))
                                (remote-tramp-rpc-test--arglist (cdr entry))))
                     nil)))
               (append
                (seq-remove
                 (lambda (entry)
                   (memq (car entry)
                         '(tramp-rpc-connection-p
                           tramp-rpc-connection-process
                           tramp-rpc-connection-buffer
                           tramp-rpc-connection-stderr-buffer
                           tramp-rpc-connection-cleanup-started)))
                 remote-backend-tramp-rpc--private-contracts)
                '((tramp-rpc-deploy--arch-to-rust-target . (1 . 1))
                  (tramp-rpc-handle-make-process . (0 . many))
                  (tramp-rpc--compute-remote-path . (1 . 1))
                  (tramp-rpc--deliver-process-output . (4 . 4)))))
     ,@body))

(defconst remote-tramp-rpc-test--verified
  (list :client-version remote-backend-tramp-rpc--verified-client-version
        :git-checkout t
        :revision remote-backend-tramp-rpc--verified-client-revision
        :release-checkout t)
  "A release contract describing exactly the verified client.")

(ert-deftest remote-tramp-rpc-upgrade-removes-unverified-private-shims ()
  "A newer client must not inherit advice written for the verified release."
  (should (equal remote-backend-tramp-rpc--verified-client-version "0.16.0"))
  (should
   (remote-backend-tramp-rpc--verified-release-p remote-tramp-rpc-test--verified))
  (dolist (change '((:client-version "0.17.0")
                    (:release-checkout nil)
                    (:revision "different-commit")
                    (:git-checkout nil)))
    (let ((release (copy-sequence remote-tramp-rpc-test--verified)))
      (setq release (plist-put release (car change) (cadr change)))
      (should-not (remote-backend-tramp-rpc--verified-release-p release))))
  (let ((original-require (symbol-function 'require))
        (pairs
         (append
          (mapcar (lambda (shim) (cons (nth 1 shim) (nth 3 shim)))
                  remote-backend-tramp-rpc--shims)
          remote-backend-tramp-rpc--retired-shims)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest arguments)
                 (pcase feature
                   ('msgpack nil)
                   ('tramp-rpc-deploy t)
                   (_ (apply original-require feature arguments)))))
              ((symbol-function 'remote-backend-tramp-rpc-release-contract)
               (lambda () '(:client-version "0.17.0"
                            :release-checkout t))))
      (remote-tramp-rpc-test--with-stub-seams
        (dolist (pair pairs)
          (advice-add (car pair) :around (cdr pair))
          (should (advice-member-p (cdr pair) (car pair))))
        (remote-backend-tramp-rpc-install)
        (dolist (pair pairs)
          (should-not (advice-member-p (cdr pair) (car pair))))))))

(ert-deftest remote-tramp-rpc-verified-install-drops-retired-shims ()
  "The verified release installs current shims and none fixed upstream."
  (let ((original-require (symbol-function 'require))
        (remote-backend-tramp-rpc--connection-shape nil)
        (remote-backend-tramp-rpc--probe-slot nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest arguments)
                 (pcase feature
                   ('msgpack nil)
                   ('tramp-rpc-deploy t)
                   (_ (apply original-require feature arguments)))))
              ((symbol-function 'remote-backend-tramp-rpc-release-contract)
               (lambda () remote-tramp-rpc-test--verified))
              ((symbol-function 'remote-backend-tramp-rpc-metadata-install)
               #'ignore))
      (remote-tramp-rpc-test--with-stub-seams
        (unwind-protect
            (progn
              (pcase-dolist (`(,symbol . ,advice)
                             remote-backend-tramp-rpc--retired-shims)
                (advice-add symbol :around advice))
              (remote-backend-tramp-rpc-install)
              (pcase-dolist (`(,symbol . ,advice)
                             remote-backend-tramp-rpc--retired-shims)
                (should-not (advice-member-p advice symbol)))
              (should remote-backend-tramp-rpc--connection-shape)
              (should (integerp remote-backend-tramp-rpc--probe-slot))
              (dolist (shim remote-backend-tramp-rpc--shims)
                (should (advice-member-p (nth 3 shim) (nth 1 shim))))
              ;; A second install, as a package reload runs, is idempotent.
              (remote-backend-tramp-rpc-install)
              (dolist (shim remote-backend-tramp-rpc--shims)
                (should (advice-member-p (nth 3 shim) (nth 1 shim)))))
          (dolist (shim remote-backend-tramp-rpc--shims)
            (advice-remove (nth 1 shim) (nth 3 shim))))))))

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

(ert-deftest remote-tramp-rpc-in-place-write-skips-only-its-chown ()
  "The verified write keeps its owner; every other chown still runs."
  (let ((file "/tmp/current")
        (calls nil))
    (remote-backend-tramp-rpc--write-region-in-place-a
     (lambda (_start _end _filename &rest _options)
       (should-not
        (remote-backend-tramp-rpc--uid-gid-in-place-a
         (lambda (&rest args) (push (cons 'current args) calls))
         file 1000 1000))
       (remote-backend-tramp-rpc--uid-gid-in-place-a
        (lambda (&rest args) (push (cons 'other args) calls))
        "/tmp/other" 1000 1000))
     "content" nil file)
    (should (equal calls '((other "/tmp/other" 1000 1000))))
    (remote-backend-tramp-rpc--uid-gid-in-place-a
     (lambda (&rest args) (push (cons 'outside args) calls))
     file nil 5)
    (should (equal (car calls) (list 'outside file nil 5)))
    (let ((remote-backend-tramp-rpc--in-place-write-file file)
          (remote-backend-tramp-rpc-skip-in-place-metadata-roundtrip nil))
      (remote-backend-tramp-rpc--uid-gid-in-place-a
       (lambda (&rest _) (push 'disabled calls)) file 1 1)
      (should (eq (car calls) 'disabled)))))

(ert-deftest remote-tramp-rpc-attribute-probes-cache-per-live-generation ()
  "A successful probe is reused only on its own live RPC transport."
  (remote-tramp-rpc-test--readable
   (let* ((remote-backend-tramp-rpc-attribute-probe-ttl 60)
          (first (make-pipe-process
                  :name "remote-attribute-first-test" :noquery t))
          (second (make-pipe-process
                   :name "remote-attribute-second-test" :noquery t))
          (current first)
          (calls 0))
     (unwind-protect
         (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                    (lambda (_vector) (remote-tramp-rpc-test--generation :process current)))
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
       (delete-process second)))))

(ert-deftest remote-tramp-rpc-attribute-probe-does-not-cache-on-replacement ()
  "A probe completed across a transport swap cannot populate the new cache."
  (remote-tramp-rpc-test--readable
   (let* ((remote-backend-tramp-rpc-attribute-probe-ttl 60)
          (first (make-pipe-process
                  :name "remote-attribute-race-first" :noquery t))
          (second (make-pipe-process
                   :name "remote-attribute-race-second" :noquery t))
          (current first)
          (calls 0))
     (unwind-protect
         (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                    (lambda (_vector) (remote-tramp-rpc-test--generation :process current)))
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
       (delete-process second)))))

(ert-deftest remote-tramp-rpc-attribute-probes-cache-only-proven-missing-command ()
  "Missing-command results are cached; transport failures remain retryable."
  (remote-tramp-rpc-test--readable
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
                      (lambda (_vector) (remote-tramp-rpc-test--generation :process process)))
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
                    (lambda (_vector) (remote-tramp-rpc-test--generation :process process)))
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
                    (lambda (_vector) (remote-tramp-rpc-test--generation :process process)))
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
       (delete-process process)))))

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
        (generation (remote-tramp-rpc-test--generation :process nil))
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

(ert-deftest remote-tramp-rpc-timeout-keeps-a-transport-that-still-answers ()
  "A timed-out bounded read closes its transport only when a ping also fails.
The bounded request suppresses upstream's own 10 s probe, so the whole
wait stays within the caller's deadline plus the short liveness ping."
  (remote-tramp-rpc-test--readable
    (let* ((remote-fs-current-retry-safe-query t)
           (remote-backend-tramp-rpc--probe-slot
            (cl-struct-slot-offset 'tramp-rpc-connection 'probing))
           (process (make-pipe-process
                     :name "remote-liveness-test" :noquery t))
           (generation (remote-tramp-rpc-test--generation :process process))
           (ping-answers t)
           (invalidations 0)
           calls)
      (unwind-protect
          (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                     (lambda (_vector) generation))
                    ((symbol-function 'tramp-rpc--invalidate-timed-out-connection)
                     (lambda (dead _vector _event)
                       (should (eq dead process))
                       (cl-incf invalidations)))
                    ((symbol-function 'tramp-rpc--call-with-timeout)
                     (lambda (_vector method _params timeout _poll &optional _c)
                       (push (list method timeout
                                   (tramp-rpc-connection-probing generation))
                             calls)
                       (if (and (equal method "system.ping") ping-answers)
                           t
                         (signal 'remote-file-error
                                 '("Timeout waiting for RPC response"))))))
            (should-error
             (remote-backend-tramp-rpc--adapter-timeout-a
              #'ignore 'vector "file.stat" 'params nil)
             :type 'remote-file-error)
            ;; Both waits ran with upstream's probe suppressed.
            (should (equal (reverse calls)
                           (list (list "file.stat" 5 t)
                                 (list "system.ping"
                                       remote-backend-tramp-rpc-liveness-probe-timeout
                                       t))))
            (should (= invalidations 0))
            (should-not (tramp-rpc-connection-probing generation))
            (setq ping-answers nil calls nil)
            (should-error
             (remote-backend-tramp-rpc--adapter-timeout-a
              #'ignore 'vector "file.stat" 'params nil)
             :type 'remote-file-error)
            (should (= invalidations 1))
            (should-not (tramp-rpc-connection-probing generation))
            (let ((remote-backend-tramp-rpc-liveness-probe-timeout 0))
              (setq calls nil)
              (should-error
               (remote-backend-tramp-rpc--adapter-timeout-a
                #'ignore 'vector "file.stat" 'params nil)
               :type 'remote-file-error)
              (should (equal (mapcar #'car calls) '("file.stat")))
              (should (= invalidations 2))))
        (delete-process process)))))

(ert-deftest remote-tramp-rpc-probe-guard-restores-a-running-probe ()
  "Suppressing upstream's probe never clears a probe already in progress."
  (remote-tramp-rpc-test--readable
    (let* ((remote-backend-tramp-rpc--probe-slot
            (cl-struct-slot-offset 'tramp-rpc-connection 'probing))
           (generation (remote-tramp-rpc-test--generation :probing 'running))
           (guard (remote-backend-tramp-rpc--probe-guard generation)))
      (should (integerp guard))
      (ignore-errors
        (remote-backend-tramp-rpc--with-probe-guard generation guard
          (should (eq (tramp-rpc-connection-probing generation) t))
          (error "Interrupted")))
      (should (eq (tramp-rpc-connection-probing generation) 'running))
      ;; Without the verified slot nothing is touched.
      (let ((remote-backend-tramp-rpc--probe-slot nil))
        (should-not (remote-backend-tramp-rpc--probe-guard generation))))))

(ert-deftest remote-tramp-rpc-directory-search-answer-lives-on-its-generation ()
  "A directory search answer is reused until its transport, TTL or a
filesystem event says otherwise; other access questions always go out."
  (require 'tramp)
  (remote-tramp-rpc-test--readable
    (let* ((remote-backend-tramp-rpc--directory-access
            (make-hash-table :test #'equal))
           (remote-backend-tramp-rpc--directory-access-hits 0)
           (first (make-pipe-process :name "remote-dir-access-1" :noquery t))
           (second (make-pipe-process :name "remote-dir-access-2" :noquery t))
           (current (remote-tramp-rpc-test--generation :process first))
           (valid t)
           (calls 0)
           (check (lambda (_filename _mode) (cl-incf calls) t)))
      (unwind-protect
          (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                     (lambda (_vec) current))
                    ((symbol-function 'tramp-rpc--cache-entry-valid-p)
                     (lambda (_timestamp) valid)))
            (dotimes (_ 3)
              (should (remote-backend-tramp-rpc--directory-access-a
                       check "/ssh:box:/w/./" 'directory)))
            (should (= calls 1))
            (should (= remote-backend-tramp-rpc--directory-access-hits 2))
            ;; Another question about the same name is not this one.
            (remote-backend-tramp-rpc--directory-access-a
             check "/ssh:box:/w/" "w")
            (should (= calls 2))
            ;; The TTL policy (and `remote-file-name-inhibit-cache') rules.
            (setq valid nil)
            (remote-backend-tramp-rpc--directory-access-a
             check "/ssh:box:/w/" 'directory)
            (should (= calls 3))
            (setq valid t)
            ;; A replacement transport starts empty.
            (setq current (remote-tramp-rpc-test--generation :process second))
            (remote-backend-tramp-rpc--directory-access-a
             check "/ssh:box:/w/" 'directory)
            (should (= calls 4))
            (remote-backend-tramp-rpc--directory-access-a
             check "/ssh:box:/w/sub/" 'directory)
            (should (= calls 5))
            ;; A change at /w drops /w and everything below it.
            (remote-backend-tramp-rpc--directory-access-events-h
             (tramp-dissect-file-name "/ssh:box:/") '("/ssh:box:/w"))
            (should (= (hash-table-count
                        remote-backend-tramp-rpc--directory-access)
                       0))
            (remote-backend-tramp-rpc--directory-access-a
             check "/ssh:box:/w/" 'directory)
            ;; Lost events forget the whole host.
            (remote-backend-tramp-rpc--directory-access-events-h
             (tramp-dissect-file-name "/ssh:box:/") nil)
            (should (= (hash-table-count
                        remote-backend-tramp-rpc--directory-access)
                       0)))
        (delete-process first)
        (delete-process second)))))

(ert-deftest remote-tramp-rpc-ancestor-prefetch-lists-each-ancestor-once ()
  "One batch lists every ancestor; only absent markers are seeded missing."
  (require 'tramp)
  (let ((remote-backend-tramp-rpc--ancestor-prefetch t)
        (route (remote-route-create :target-id "box" :link-id "box/ssh"
                                    :link-plugin-id "tramp-rpc"))
        requests seeded listings)
    (cl-letf (((symbol-function 'remote-backend-project-file-name)
               (lambda (_route _file) "/ssh:box:/w/p/"))
              ((symbol-function 'tramp-rpc--encode-path)
               (lambda (path) `((path . ,path))))
              ((symbol-function 'tramp-rpc--decode-filename)
               (lambda (entry) (alist-get 'name entry)))
              ((symbol-function 'tramp-rpc--batch-error-p)
               (lambda (result) (eq (car-safe result) :error)))
              ((symbol-function 'tramp-rpc--call-batch)
               (lambda (_vec batch)
                 (setq requests batch)
                 (list '(((name . ".git")) ((name . "a.c")))
                       '(:error -32000 :message "too large")
                       '(((name . ".projectile"))))))
              ((symbol-function 'tramp-rpc--cache-file-stat-result)
               (lambda (_vec file stat &optional lstat)
                 (push (list file stat lstat) seeded)))
              ((symbol-function 'remote-fs-remember-ancestor-listing)
               (lambda (directory names) (push (cons directory names) listings))))
      (should (remote-backend-tramp-rpc-prefetch-ancestor-markers
               route "/fs:box:/w/p/" '(".git" ".projectile")))
      (should (equal (mapcar (lambda (r) (cons (car r) (alist-get 'path (cdr r))))
                             requests)
                     '(("dir.list" . "/w/p/") ("dir.list" . "/w/")
                       ("dir.list" . "/"))))
      ;; /w/p/ has .git but not .projectile; /w/ was refused; / has .projectile.
      (should (equal (sort (mapcar #'car seeded) #'string<)
                     '("/.git" "/.git" "/w/p/.projectile" "/w/p/.projectile")))
      (should (cl-every (lambda (entry) (null (nth 1 entry))) seeded))
      (should (equal (mapcar #'car listings) '("/fs:box:/" "/fs:box:/w/p/"))))
    ;; Another backend's route, or an unverified release, is not served here.
    (should-not (remote-backend-tramp-rpc-prefetch-ancestor-markers
                 (remote-route-create :target-id "box" :link-id "box/ssh"
                                      :link-plugin-id "tramp")
                 "/fs:box:/w/" '(".git")))
    (let ((remote-backend-tramp-rpc--ancestor-prefetch nil))
      (should-not (remote-backend-tramp-rpc-prefetch-ancestor-markers
                   route "/fs:box:/w/" '(".git"))))))

(ert-deftest remote-fs-records-the-search-path-each-target-hands-out ()
  "`exec-path' answers accumulate per target and go with the session."
  (let ((remote-fs--target-search-paths (make-hash-table :test #'equal)))
    (remote-fs--remember-search-path "box" '("/usr/bin" "/opt/p/bin"))
    (remote-fs--remember-search-path "box" '("/usr/bin" "/home/u/.local/bin"))
    (remote-fs--remember-search-path "other" '("/bin"))
    (should (equal (remote-fs-target-search-path "box")
                   '("/usr/bin" "/opt/p/bin" "/home/u/.local/bin")))
    (remote-fs-clear-target-cache "box")
    (should-not (remote-fs-target-search-path "box"))
    (should (remote-fs-target-search-path "other"))))

(ert-deftest remote-tramp-rpc-path-index-answers-absent-programs ()
  "One batch lists the session PATH; only absence is answered locally."
  (require 'tramp)
  (remote-tramp-rpc-test--readable
    (let* ((remote-backend-tramp-rpc--path-index (make-hash-table :test #'equal))
           (remote-backend-tramp-rpc--path-index-hits 0)
           (remote-backend-tramp-rpc-path-index-ttl 30)
           (tramp-rpc-protocol-error-file-not-found -32001)
           (first (make-pipe-process :name "remote-path-index-1" :noquery t))
           (second (make-pipe-process :name "remote-path-index-2" :noquery t))
           (current (remote-tramp-rpc-test--generation :process first))
           (batches 0)
           (checks nil)
           (check (lambda (filename mode) (push (cons filename mode) checks) t)))
      (unwind-protect
          (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                     (lambda (_vec) current))
                    ((symbol-function 'tramp-rpc--cache-entry-valid-p)
                     (lambda (_timestamp) t))
                    ((symbol-function 'remote-backend-tramp-rpc--session-path)
                     (lambda (_vec) '("/usr/bin/" "/opt/missing/bin/")))
                    ((symbol-function 'tramp-rpc--encode-path)
                     (lambda (path) `((path . ,path))))
                    ((symbol-function 'tramp-rpc--decode-filename)
                     (lambda (entry) (alist-get 'name entry)))
                    ((symbol-function 'tramp-rpc--batch-error-p)
                     (lambda (result) (eq (car-safe result) :error)))
                    ((symbol-function 'tramp-rpc--call-batch)
                     (lambda (_vec requests)
                       (cl-incf batches)
                       (should (equal (mapcar (lambda (r) (alist-get 'path (cdr r)))
                                              requests)
                                      '("/usr/bin/" "/opt/missing/bin/")))
                       (list '(((name . "git")) ((name . "ls")))
                             '(:error -32001 :message "missing")))))
            (cl-flet ((x (file) (remote-backend-tramp-rpc--path-index-a
                                 check file "x")))
              ;; Absent from a listed directory, or from one that is missing.
              (should-not (x "/ssh:box:/usr/bin/rg"))
              (should-not (x "/ssh:box:/opt/missing/bin/git"))
              ;; Present: the kernel decides.
              (should (x "/ssh:box:/usr/bin/git"))
              ;; Off the PATH, and other access modes, are never judged.
              (should (x "/ssh:box:/w/node_modules/.bin/tsc"))
              (should (remote-backend-tramp-rpc--path-index-a
                       check "/ssh:box:/usr/bin/rg" "r"))
              (should (= batches 1))
              (should (= remote-backend-tramp-rpc--path-index-hits 2))
              (should (equal (mapcar #'car (reverse checks))
                             '("/ssh:box:/usr/bin/git"
                               "/ssh:box:/w/node_modules/.bin/tsc"
                               "/ssh:box:/usr/bin/rg")))
              ;; A replacement transport lists again.
              (setq current (remote-tramp-rpc-test--generation :process second))
              (should-not (x "/ssh:box:/usr/bin/rg"))
              (should (= batches 2))
              ;; Disabled, every check is the kernel's.
              (let ((remote-backend-tramp-rpc-path-index-ttl 0))
                (should (x "/ssh:box:/usr/bin/rg")))))
        (delete-process first)
        (delete-process second)))))

(ert-deftest remote-tramp-rpc-executable-find-follows-path-order ()
  "The first listed directory holding the program wins, after one check;
absence costs nothing, and an unlisted directory leaves it to the probe."
  (require 'tramp)
  (remote-tramp-rpc-test--readable
    (let* ((remote-backend-tramp-rpc--path-index (make-hash-table :test #'equal))
           (remote-backend-tramp-rpc-path-index-ttl 30)
           (process (make-pipe-process :name "remote-exe-find" :noquery t))
           (generation (remote-tramp-rpc-test--generation :process process))
           (listings '(("/a/bin/" . (((name . "tool") (type . "directory"))))
                       ("/b/bin/" . (((name . "tool") (type . "file"))
                                     ((name . "other") (type . "file"))))
                       ("/c/bin/" . (((name . "tool") (type . "file"))))))
           (refuse nil)
           (checked nil))
      (unwind-protect
          (cl-letf (((symbol-function 'advice-member-p) (lambda (&rest _) t))
                    ((symbol-function 'tramp-rpc--get-connection)
                     (lambda (_vec) generation))
                    ((symbol-function 'tramp-rpc--cache-entry-valid-p)
                     (lambda (_timestamp) t))
                    ((symbol-function 'tramp-rpc--encode-path)
                     (lambda (path) `((path . ,path))))
                    ((symbol-function 'tramp-rpc--decode-filename)
                     (lambda (entry) (alist-get 'name entry)))
                    ((symbol-function 'tramp-rpc--batch-error-p)
                     (lambda (result) (eq (car-safe result) :error)))
                    ((symbol-function 'tramp-rpc--call-batch)
                     (lambda (_vec requests)
                       (mapcar (lambda (request)
                                 (let ((path (alist-get 'path (cdr request))))
                                   (if (member path refuse)
                                       '(:error -32000 :message "too large")
                                     (cdr (assoc path listings)))))
                               requests)))
                    ((symbol-function 'file-executable-p)
                     (lambda (file) (push file checked) t)))
            (cl-flet ((find (program dirs)
                        (remote-backend-tramp-rpc-executable-find
                         nil "/ssh:box:/w/" program dirs)))
              ;; /a/bin/tool is a directory; /b/bin/tool is the answer.
              (should (equal (find "tool" '("/a/bin" "/b/bin" "/c/bin"))
                             '(t . "/b/bin/tool")))
              (should (equal checked '("/ssh:box:/b/bin/tool")))
              (setq checked nil)
              (should (equal (find "absent" '("/a/bin" "/b/bin")) '(t)))
              (should-not checked)
              (should-not (find "a/b" '("/b/bin")))
              ;; A directory the server would not list may hold the program.
              (clrhash remote-backend-tramp-rpc--path-index)
              (setq refuse '("/a/bin/"))
              (should-not (find "absent" '("/a/bin" "/b/bin")))))
        (delete-process process)))))

(ert-deftest remote-executable-find-asks-the-backend-first ()
  "A backend answer is used; a backend without one leaves the probe."
  (let ((backend (remote-backend-create :id "x"))
        (route (remote-route-create :target-id "box" :link-plugin-id "x")))
    (cl-letf (((symbol-function 'remote-route-backend) (lambda (_r) backend)))
      (should-not (remote-backend-executable-find route "/d/" "git" nil))
      (setf (remote-backend-executable-find-function backend)
            (lambda (_route directory program dirs)
              (list directory program dirs)))
      (should (equal (remote-backend-executable-find route "/d/" "git" '("/b"))
                     '("/d/" "git" ("/b"))))
      (setf (remote-backend-executable-find-function backend)
            (lambda (&rest _) (error "Backend failure")))
      (should-not (remote-backend-executable-find route "/d/" "git" nil))))
  (should (equal (remote--environment-path-directories
                  '(("PATH" . "/a/bin:/b/bin") ("HOME" . "/h")))
                 '("/a/bin" "/b/bin")))
  (should-not (remote--environment-path-directories '(("HOME" . "/h")))))

(ert-deftest remote-tramp-rpc-only-read-only-commands-keep-caches ()
  "Commands that provably write nothing run without the cache flush."
  (dolist (argv '(("git" "ls-files" "-zco" "--exclude-standard")
                  ("/usr/bin/git" "--no-pager" "-c" "core.x=1" "-C" "/w" "log")
                  ("git" "diff" "HEAD")
                  ("rg" "--files")
                  ("find" "." "-name" "*.el")
                  ("sh" "-c" "git ls-files -z -- ':(exclude)x' | head -n 3")
                  ("/bin/bash" "-c" "rg -l TODO")))
    (should (remote-backend-tramp-rpc--argv-read-only-p argv)))
  (dolist (argv '(("git" "status")
                  ("git" "commit" "-m" "x")
                  ("git" "-c" "log" "checkout")
                  ("git" "diff" "--output=patch")
                  ("git")
                  ("find" "." "-delete")
                  ("fd" "-x" "rm")
                  ("make")
                  ("sh" "-c" "rg x > out")
                  ("sh" "-c" "git log; rm -f x")
                  ("sh" "-c" "echo $(rm x)")
                  ("sh" "-c" "ls && touch y")
                  ("sh" "-c" "ls\nrm x")
                  ("sh" "-c" "ls | tee out")
                  ("sh" "script.sh")))
    (should-not (remote-backend-tramp-rpc--argv-read-only-p argv)))
  (let (seen)
    (cl-flet ((run (program &rest arguments)
                (apply #'remote-backend-tramp-rpc--read-only-command-a
                       (lambda (&rest _) (push process-file-side-effects seen))
                       program arguments)))
      (let ((process-file-side-effects t))
        (run "git" nil t nil "ls-files")
        (run "git" nil t nil "commit")
        (run "/bin/sh" nil t nil "-c" "rg x | wc -l"))
      ;; A caller that already said "no side effects" keeps that.
      (let ((process-file-side-effects nil))
        (run "make" nil t nil))
      (should (equal (reverse seen) '(nil t nil nil))))))

(ert-deftest remote-fs-forgets-ancestor-scans-per-target ()
  "Dropping a backend's seeded answers lets the next walk scan again."
  (let ((remote-fs--ancestor-prefetched (make-hash-table :test #'equal)))
    (puthash "/fs:box:/w/" 1.0 remote-fs--ancestor-prefetched)
    (puthash "/fs:box:/v/" 1.0 remote-fs--ancestor-prefetched)
    (puthash "/fs:other:/w/" 1.0 remote-fs--ancestor-prefetched)
    (cl-letf (((symbol-function 'remote-file-name-target)
               (lambda (name) (nth 1 (split-string name ":")))))
      (remote-fs-forget-ancestor-scans "box")
      (should (equal (hash-table-keys remote-fs--ancestor-prefetched)
                     '("/fs:other:/w/")))
      (remote-fs-forget-ancestor-scans)
      (should (= (hash-table-count remote-fs--ancestor-prefetched) 0)))))

(ert-deftest remote-tramp-rpc-read-timeout-retires-only-current-generation ()
  "A timed-out request must never close a replacement SSH process."
  (remote-tramp-rpc-test--readable
   (let* ((remote-fs-current-retry-safe-query t)
          (process (make-pipe-process
                    :name "remote-read-timeout-test" :noquery t))
          (generation (remote-tramp-rpc-test--generation :process process))
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
           (setq current (remote-tramp-rpc-test--generation :process process))
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
        'transport))))

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
  (remote-tramp-rpc-test--readable
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
                      (lambda (_vec) (remote-tramp-rpc-test--generation :process current)))
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
       (delete-process process)))))

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

;;;; Executable lookup environment

(ert-deftest remote-rpc-executable-probe-keeps-buffer-local-capsule ()
  "A direnv capsule bound over a buffer-local environment reaches the probe.
`remote-environment-apply' makes `process-environment' buffer-local, so the
probe's temporary output buffer must not fall back to the global value."
  (with-temp-buffer
    (setq-local process-environment
                (cons "PATH=/login/bin" (default-value 'process-environment)))
    (setq-local exec-path '("/login/bin"))
    (let ((process-environment
           (cons "PATH=/project/.conda/bin:/login/bin" process-environment))
          (exec-path '("/project/.conda/bin" "/login/bin"))
          seen)
      (cl-letf (((symbol-function 'process-file)
                 (lambda (&rest _)
                   (setq seen (list (getenv "PATH") exec-path))
                   (insert "/project/.conda/bin/python3" 0)
                   0)))
        (should (equal (remote--rpc-executable-find "python3")
                       '(t . "/project/.conda/bin/python3")))
        (should (equal seen
                       '("/project/.conda/bin:/login/bin"
                         ("/project/.conda/bin" "/login/bin"))))))))

;;;; Notification cost and modtime precision

(defvar tramp-time-dont-know)

(ert-deftest remote-builtin-backends-declare-watch-cost-and-mtime ()
  "Push-capable, precise backends say so; shell TRAMP keeps its window."
  (dolist (expected '(("native" push exact)
                      ("tramp-rpc" push exact)
                      ("tramp" process window)))
    (let ((description (remote-backend-describe (car expected))))
      (should (eq (plist-get description :file-watch-cost)
                  (nth 1 expected)))
      (should (eq (plist-get description :mtime-compare)
                  (nth 2 expected))))))

(ert-deftest remote-builtin-backends-declare-process-write-cost ()
  "Only a pipe Emacs owns is written to directly."
  (dolist (expected '(("native" . direct)
                      ("tramp-rpc" . queued)
                      ("tramp" . queued)))
    (should (eq (plist-get (remote-backend-describe (car expected))
                           :process-write-cost)
                (cdr expected))))
  (should (eq (remote-process-write-cost "/fs:local:/tmp/a.c") 'direct))
  (cl-letf (((symbol-function 'remote-fs--backend-property)
             (lambda (&rest _) nil)))
    ;; A backend that says nothing is assumed to relay.
    (should (eq (remote-process-write-cost "/fs:devbox:/tmp/a.c") 'queued))))

(ert-deftest remote-fs-lock-failure-does-not-abort-the-edit ()
  "A backend that cannot ask about the file must not stop the first edit."
  ;; `userlock' defines these as plain `file-error's; it provides no feature.
  (dolist (condition '(file-locked file-supersession))
    (unless (get condition 'error-conditions)
      (define-error condition (symbol-name condition) 'file-error)))
  (should-not
   (remote-fs--lock-file-a
    (lambda (_file)
      (signal 'remote-file-error '("`stat' returns with error")))
    "/ssh:lab:/srv/new.h"))
  ;; The scope the supersession check relies on is still active inside.
  (should (remote-fs--lock-file-a (lambda (_file) remote-fs--lock-check)
                                  "/ssh:lab:/srv/new.h"))
  ;; Someone else's lock and a file changed on disk are the user's call.
  (should-error
   (remote-fs--lock-file-a
    (lambda (file) (signal 'file-locked (list file "someone")))
    "/ssh:lab:/srv/new.h")
   :type 'file-locked)
  (should-error
   (remote-fs--lock-file-a
    (lambda (file) (signal 'file-supersession (list file)))
    "/ssh:lab:/srv/new.h")
   :type 'file-supersession))

(ert-deftest remote-fs-visiting-a-missing-file-keeps-its-logical-identity ()
  "A new file is still a `/fs:' buffer after the backend reported it missing."
  (with-temp-buffer
    (cl-letf (((symbol-function 'remote-fs--call-routed)
               (lambda (_operation arguments)
                 ;; What a backend does before it signals: it has already
                 ;; recorded its own name for the file.
                 (when (nth 1 arguments)
                   (setq buffer-file-name "/ssh:lab:/srv/new.h"
                         default-directory "/ssh:lab:/srv/"))
                 (signal 'file-missing
                         (list "Opening input file" "No such file or directory"
                               "/ssh:lab:/srv/new.h"))))
              ((symbol-function 'file-truename) #'identity))
      (should-error
       (remote-fs-handle-insert-file-contents "/fs:lab:/srv/new.h" t)
       :type 'file-missing)
      (should (equal buffer-file-name "/fs:lab:/srv/new.h"))
      (should (equal default-directory "/fs:lab:/srv/"))
      (should (equal buffer-file-truename "/fs:lab:/srv/new.h")))
    ;; Without VISIT the buffer is not the file and stays untouched.
    (setq buffer-file-name nil)
    (cl-letf (((symbol-function 'remote-fs--call-routed)
               (lambda (&rest _) (signal 'file-missing '("x")))))
      (should-error
       (remote-fs-handle-insert-file-contents "/fs:lab:/srv/other.h")
       :type 'file-missing)
      (should-not buffer-file-name))))

(ert-deftest remote-tramp-ancestor-scan-reports-existing-markers ()
  "One shell loop answers every marker in a directory and its ancestors."
  (let* ((root (file-truename (make-temp-file "remote-ancestor-scan-" t)))
         (deep (expand-file-name "a/b c/d" root)))
    (unwind-protect
        (progn
          (make-directory deep t)
          (make-directory (expand-file-name "a/.git" root))
          (make-directory (expand-file-name "a/debian" root))
          (with-temp-file (expand-file-name "a/debian/control" root))
          (with-temp-file (expand-file-name "a/b c/.envrc" root))
          (make-symbolic-link "/nonexistent-target"
                              (expand-file-name "a/b c/d/Makefile" root))
          (let* ((scan
                  (with-temp-buffer
                    (should
                     (zerop
                      (call-process
                       "/bin/sh" nil t nil "-c"
                       remote-backend-tramp--ancestor-scan-script "sh"
                       deep "3" ".git" ".envrc" "debian/control" "Makefile"
                       ".projectile")))
                    (remote-backend-tramp--parse-ancestor-scan
                     (buffer-string))))
                 (found (seq-filter (lambda (file) (string-prefix-p root file))
                                    (car scan)))
                 (listings (cdr scan)))
            (should
             (equal (sort found #'string<)
                    (sort (list (expand-file-name "a/.git" root)
                                (expand-file-name "a/debian/control" root)
                                (expand-file-name "a/b c/.envrc" root)
                                ;; A dangling link is still a name that is
                                ;; taken, as `file-symlink-p' would say.
                                (expand-file-name "a/b c/d/Makefile" root))
                          #'string<)))
            ;; Each ancestor comes back with its own entries.
            (should (equal (cdr (assoc deep listings)) '("Makefile")))
            (should (equal (sort (copy-sequence
                                  (cdr (assoc (expand-file-name "a/b c" root)
                                              listings)))
                                 #'string<)
                           '(".envrc" "d")))
            (should (equal (sort (copy-sequence
                                  (cdr (assoc (expand-file-name "a" root)
                                              listings)))
                                 #'string<)
                           '(".git" "b c" "debian")))
            ;; A directory above the limit is left for the consumer to list.
            (dotimes (index 5)
              (with-temp-file (expand-file-name (format "f%d" index) root)))
            (let ((crowded
                   (with-temp-buffer
                     (call-process
                      "/bin/sh" nil t nil "-c"
                      remote-backend-tramp--ancestor-scan-script "sh"
                      root "3" ".git")
                     (remote-backend-tramp--parse-ancestor-scan
                      (buffer-string)))))
              (should-not (assoc root (cdr crowded))))))
      (delete-directory root t))))

(ert-deftest remote-ancestor-listing-is-remembered-briefly ()
  (let ((remote-fs--ancestor-listings (make-hash-table :test #'equal))
        (remote-ancestor-prefetch-lifetime 10)
        (clock 100.0))
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) clock)))
      (should-not (remote-ancestor-directory-names "/fs:lab:/srv/"))
      (remote-fs-remember-ancestor-listing "/fs:lab:/srv" '("Makefile" ".git"))
      (remote-fs-remember-ancestor-listing "/fs:lab:/empty/" nil)
      (should (equal (remote-ancestor-directory-names "/fs:lab:/srv/")
                     '(t "Makefile" ".git")))
      ;; A directory known to be empty is an answer, not a miss.
      (should (equal (remote-ancestor-directory-names "/fs:lab:/empty")
                     '(t)))
      (setq clock 111.0)
      (should-not (remote-ancestor-directory-names "/fs:lab:/srv/")))))

(ert-deftest remote-ancestor-prefetch-is-bounded-to-costly-open-routes ()
  "The scan runs once per directory, and only where lookups are round trips."
  (let* ((remote-ancestor-marker-names '(".git" ".envrc"))
        (remote-fs--ancestor-prefetched (make-hash-table :test #'equal))
        (remote-ancestor-prefetch-lifetime 10)
        (cost 'round-trip)
        (open t)
        calls
        (remote-ancestor-prefetch-functions
         (list (lambda (_route directory names)
                 (push (list directory names) calls)
                 t))))
    (cl-letf (((symbol-function 'remote-file-operation-cost)
               (lambda (&rest _) cost))
              ((symbol-function 'remote-connection-target-open-p)
               (lambda (_target) open))
              ((symbol-function 'remote-fs--placement-route)
               (lambda (&rest _) 'route)))
      (remote-prefetch-ancestor-markers "/fs:lab:/usr/include")
      (remote-prefetch-ancestor-markers "/fs:lab:/usr/include/")
      (should (equal calls '(("/fs:lab:/usr/include/" (".git" ".envrc")))))
      (remote-prefetch-ancestor-markers "/fs:lab:/srv/")
      (should (= (length calls) 2))
      ;; Cheap lookups need no scan, and a closed target is not reopened.
      (setq cost 'batched)
      (remote-prefetch-ancestor-markers "/fs:lab:/opt/")
      (setq cost 'round-trip open nil)
      (remote-prefetch-ancestor-markers "/fs:lab:/var/")
      (should (= (length calls) 2))
      ;; A failing backend never breaks the lookup that asked.
      (setq open t)
      (let ((remote-ancestor-prefetch-functions
             (list (lambda (&rest _) (error "Injected scan failure")))))
        (should-not (remote-prefetch-ancestor-markers "/fs:lab:/home/")))
      ;; Wildcards and absolute names are not existence tests.
      (let ((remote-ancestor-marker-names nil))
        (remote-register-ancestor-markers '("Makefile" "?*.sln" "/etc/x" ""))
        (should (equal remote-ancestor-marker-names '("Makefile")))))))

(ert-deftest remote-tramp-session-replacement-keeps-host-facts ()
  "A reconnect must not re-learn the host; a cleanup by name still resets."
  (require 'tramp-cmds)
  (let* ((tramp-cache-data (make-hash-table :test #'equal))
         (tramp-cache-data-changed nil)
         (physical "/ssh:lab-host-facts:/")
         (vec (tramp-dissect-file-name physical nil))
         (connection (remote-connection-create :handle physical)))
    (cl-letf (((symbol-function 'tramp-cleanup-connection)
               (lambda (cleaned &rest _)
                 (tramp-flush-connection-properties cleaned))))
      (dolist (explicit '(nil t))
        (tramp-set-connection-property vec "stat" "/usr/bin/stat")
        (tramp-set-connection-property vec "uid-integer" 1000)
        ;; Not a host fact: it describes the session that is going away.
        (tramp-set-connection-property vec "session-only" 'stale)
        (let ((remote-backend-tramp-explicit-cleanup explicit))
          (remote-backend-tramp-disconnect connection nil))
        (should-not (tramp-get-connection-property vec "session-only"))
        (if explicit
            (progn
              (should-not (tramp-get-connection-property vec "stat"))
              (should-not (tramp-get-connection-property vec "uid-integer")))
          (should (equal (tramp-get-connection-property vec "stat")
                         "/usr/bin/stat"))
          (should (equal (tramp-get-connection-property vec "uid-integer")
                         1000)))))))

(ert-deftest remote-tramp-timer-cannot-talk-to-an-opening-connection ()
  "A timer that fires during a handshake must not send into that connection."
  (let ((lab (tramp-dissect-file-name "/ssh:lab:/" nil))
        (other (tramp-dissect-file-name "/ssh:other:/" nil)))
    ;; Nothing is opening: every caller is left alone.
    (let ((remote-backend-tramp--connecting nil))
      (cl-letf (((symbol-function 'remote-backend-tramp--timer-intrusion-p)
                 (lambda () t)))
        (should-not (remote-backend-tramp--guard-opening-connection-a lab))))
    (let ((remote-backend-tramp--connecting lab))
      ;; The handshake's own commands pass.
      (cl-letf (((symbol-function 'remote-backend-tramp--timer-intrusion-p)
                 (lambda () nil)))
        (should-not (remote-backend-tramp--guard-opening-connection-a lab)))
      (cl-letf (((symbol-function 'remote-backend-tramp--timer-intrusion-p)
                 (lambda () t)))
        (should-error (remote-backend-tramp--guard-opening-connection-a lab)
                      :type 'remote-connection-busy)
        ;; Another host's connection is none of this handshake's business.
        (should-not
         (remote-backend-tramp--guard-opening-connection-a other))))
    ;; The stack decides who is inside whom.
    (should-not (remote-backend-tramp--timer-intrusion-p))))

(ert-deftest remote-client-process-queries-ignore-the-current-directory ()
  "Asking about this machine's processes never becomes a target question."
  (let ((default-directory "/fs:lab:/srv/")
        seen)
    (cl-letf (((symbol-function 'list-system-processes)
               (lambda () (push default-directory seen) '(1)))
              ((symbol-function 'process-attributes)
               (lambda (_pid) (push default-directory seen) '((ppid . 0)))))
      (should (equal (remote-client-list-system-processes) '(1)))
      (should (equal (remote-client-process-attributes 1) '((ppid . 0))))
      (should (equal seen (list temporary-file-directory
                                temporary-file-directory))))))

(ert-deftest remote-tramp-connect-does-not-start-over-past-its-deadline ()
  "One framework connection is one bounded attempt, not one per TRAMP step."
  (let ((vec (tramp-dissect-file-name "/ssh:lab:/" nil))
        live)
    (cl-letf (((symbol-function 'tramp-get-connection-process)
               (lambda (_vec) live)))
      ;; Outside a framework connection TRAMP is left alone.
      (let ((remote-backend-tramp--connect-deadline nil))
        (should-not (remote-backend-tramp--bounded-open-connection-a vec)))
      ;; Before the deadline another attempt may start.
      (let ((remote-backend-tramp--connect-deadline (+ (float-time) 60)))
        (should-not (remote-backend-tramp--bounded-open-connection-a vec)))
      (let ((remote-backend-tramp--connect-deadline (- (float-time) 1)))
        (should-error (remote-backend-tramp--bounded-open-connection-a vec)
                      :type 'remote-connection-timeout)
        ;; A transport failure: no other backend on this link is tried.
        (should
         (eq (plist-get
              (remote-backend-default-classify-error
               (should-error
                (remote-backend-tramp--bounded-open-connection-a vec))
               'connect)
              :scope)
             'transport))
        ;; A slow handshake that did succeed stays valid past the deadline.
        (setq live (make-pipe-process :name "remote-tramp-deadline-test"
                                      :noquery t))
        (unwind-protect
            (should-not (remote-backend-tramp--bounded-open-connection-a vec))
          (delete-process live))))))

(ert-deftest remote-tramp-rpc-bootstrap-inherits-the-route-ssh-options ()
  "The connection that deploys the RPC server needs the same `-F' and options."
  (let ((tramp-rpc-deploy-bootstrap-method "scpx"))
    (should (equal (remote-backend-tramp--bootstrap-prefix "/rpc:me@lab#2222:")
                   "/scpx:me@lab#2222:"))
    (should-not (remote-backend-tramp--bootstrap-prefix "/ssh:lab:"))
    (let* ((properties
            (remote-backend-tramp--bootstrap-properties
             "/rpc:lab:" '("ConnectTimeout=5") '("-F" "/client/ssh_config")))
           (login (cdr (assoc "login-args" properties)))
           (copy (cdr (assoc "copy-args" properties))))
      ;; Both the shell login and the scp transfer of the binary.
      (should (equal (seq-take login 2)
                     '(("-F" "/client/ssh_config") ("-o" "ConnectTimeout=5"))))
      (should (equal (seq-take copy 2)
                     '(("-F" "/client/ssh_config") ("-o" "ConnectTimeout=5"))))
      (should (> (length login) 2))
      (should (> (length copy) 2)))
    (should-not (remote-backend-tramp--bootstrap-properties "/rpc:lab:" nil nil))
    (should-not (remote-backend-tramp--bootstrap-properties
                 "/ssh:lab:" nil '("-F" "/client/ssh_config")))))

(ert-deftest remote-require-on-client-escapes-the-target-context ()
  "Loading a package or its native module must never reach a target."
  (let ((default-directory "/fs:box:/work/")
        (remote-current-adapter-id "language-server")
        (remote-current-route 'target-route)
        (remote-current-workspace 'target-workspace)
        observed)
    (cl-letf (((symbol-function 'remote-client-process-environment)
               (lambda () '("CLIENT_ENV=1")))
              ((symbol-function 'remote-client-exec-path)
               (lambda () '("/client/bin")))
              ((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (setq observed
                       (list feature filename noerror
                             default-directory process-environment
                             exec-path remote-current-adapter-id
                             remote-current-route remote-current-workspace))
                 feature)))
      (should (eq (remote-require-on-client 'test-client-package nil t)
                  'test-client-package)))
    (should
     (equal observed
            (list 'test-client-package nil t
                  temporary-file-directory '("CLIENT_ENV=1")
                  '("/client/bin") nil nil nil)))))

(ert-deftest remote-placement-queries-reuse-the-validated-route ()
  "Cost and placement are asked per buffer; they must not reselect a route."
  (let ((file (remote-canonicalize-file-name
               (expand-file-name "init.el" user-emacs-directory)))
        (selections 0))
    (clrhash remote-fs-native-route-cache)
    (let ((cost (remote-file-operation-cost file))
          (watch (remote-file-watch-cost file))
          (client (remote-client-file-name file))
          (original (symbol-function 'remote-routes)))
      (should (eq cost 'batched))
      (should (eq watch 'push))
      (should (equal client (remote-file-local-name file)))
      (cl-letf (((symbol-function 'remote-routes)
                 (lambda (&rest arguments)
                   (cl-incf selections)
                   (apply original arguments))))
        (dotimes (_ 20)
          (should (eq (remote-file-operation-cost file) cost))
          (should (eq (remote-file-watch-cost file) watch))
          (should (equal (remote-client-file-name file) client))))
      (should (zerop selections))
      ;; The reuse is guarded: a changed registration selects again.
      (clrhash remote-fs-native-route-cache)
      (cl-letf (((symbol-function 'remote-routes)
                 (lambda (&rest arguments)
                   (cl-incf selections)
                   (apply original arguments))))
        (should (eq (remote-file-operation-cost file) cost)))
      (should (= selections 1)))))

(ert-deftest remote-file-watch-cost-defaults-to-none ()
  "A backend that declares nothing cannot be relied on for notifications."
  (cl-letf (((symbol-function 'remote-fs--backend-property)
             (lambda (_file property &optional _adapter)
               (and (eq property :file-watch-cost) nil))))
    (should (eq (remote-file-watch-cost "/fs:box:/a") 'none)))
  (cl-letf (((symbol-function 'remote-fs--backend-property)
             (lambda (_file property &optional _adapter)
               (and (eq property :file-watch-cost) 'push))))
    (should (eq (remote-file-watch-cost "/fs:box:/a") 'push))))

(ert-deftest remote-fs-verify-modtime-honors-backend-precision ()
  "Exact backends see a change one second after the visited modtime."
  (let ((visited (seconds-to-time 1000))
        (disk (seconds-to-time 1001))
        compare)
    (cl-letf (((symbol-function 'visited-file-modtime) (lambda () visited))
              ((symbol-function 'file-attributes)
               (lambda (&rest _) (list nil 1 0 0 disk disk disk 1)))
              ((symbol-function 'remote-fs--backend-property)
               (lambda (_file property &optional _adapter)
                 (and (eq property :mtime-compare) compare))))
      (with-temp-buffer
        (setq buffer-file-name "/fs:box:/work/a.c")
        (unwind-protect
            (progn
              (setq compare 'exact)
              (should-not (remote-fs-handle-verify-visited-file-modtime))
              (setq compare 'window)
              (should (remote-fs-handle-verify-visited-file-modtime))
              (setq compare 'exact disk visited)
              (should (remote-fs-handle-verify-visited-file-modtime)))
          (setq buffer-file-name nil))))))

(ert-deftest remote-fs-single-file-watch-uses-push-backend ()
  "A push route never pays an inotifywait lookup or process per buffer."
  (let ((watch (remote-file-watch-create
                :file "/fs:box:/work/a.c" :flags '(change)
                :adapter-id "emacs-file"))
        (cost 'push)
        direct)
    (cl-letf (((symbol-function 'remote-file-watch-cost)
               (lambda (&rest _) cost))
              ((symbol-function 'remote-fs--watch-add-direct-inotify)
               (lambda (&rest _) (push 'direct direct) 'inotify))
              ((symbol-function 'remote-fs--call-routed)
               (lambda (&rest _) 'backend)))
      (should (eq (remote-fs--watch-add-physical watch) 'backend))
      (should-not direct)
      (setq cost 'process)
      (should (eq (remote-fs--watch-add-physical watch) 'inotify))
      (should (equal direct '(direct)))
      (setq cost 'push)
      (setf (remote-file-watch-recursive watch) t)
      (should (eq (remote-fs--watch-add-physical watch) 'inotify)))))

(ert-deftest remote-tramp-rpc-verify-modtime-is-exact ()
  "RPC buffers detect a write made one second after their modtime."
  (let ((visited (seconds-to-time 1000))
        (disk (seconds-to-time 1001))
        (remote-backend-tramp-rpc-exact-mtime t)
        (tramp-time-dont-know '(0 0 0 1000)))
    (cl-letf (((symbol-function 'visited-file-modtime) (lambda () visited))
              ((symbol-function 'tramp-rpc-file-name-p) (lambda (_) t))
              ((symbol-function 'file-remote-p) (lambda (&rest _) t))
              ((symbol-function 'file-attributes)
               (lambda (&rest _) (list nil 1 0 0 disk disk disk 1))))
      (with-temp-buffer
        (setq buffer-file-name "/rpc:box:/work/a.c")
        (unwind-protect
            (progn
              (should-not (remote-backend-tramp-rpc-metadata--verify-a
                           (lambda (&rest _) 'tramp)))
              (setq disk visited)
              (should (remote-backend-tramp-rpc-metadata--verify-a
                       (lambda (&rest _) 'tramp)))
              (let ((remote-backend-tramp-rpc-exact-mtime nil))
                (should (eq (remote-backend-tramp-rpc-metadata--verify-a
                             (lambda (&rest _) 'tramp))
                            'tramp))))
          (setq buffer-file-name nil))))))

;;;; TRAMP signal logging

(ert-deftest remote-compat-quiet-file-missing-keeps-other-errors ()
  "Handled missing files stay out of *Messages*; other errors are logged."
  (let (logged)
    (let ((hook (lambda (symbol _data) (push symbol logged))))
      (remote-compat--quiet-file-missing-a hook 'file-missing '("gone"))
      (remote-compat--quiet-file-missing-a hook 'file-error '("denied"))
      (should (equal logged '(file-error)))
      (let ((remote-compat-quiet-file-missing nil))
        (remote-compat--quiet-file-missing-a hook 'file-missing '("gone")))
      (should (equal logged '(file-missing file-error))))))

;;;; Cross-target default directory

(ert-deftest remote-fs-other-target-default-directory-is-not-projected ()
  "A buffer on one target may operate on another target's files."
  (remote-fs-install)
  (let* ((projected nil)
         (expected (concat "/fs:local:" (file-truename "/tmp/")))
         (spy (lambda (function file &rest arguments)
                (push file projected)
                (apply function file arguments))))
    (advice-add 'remote-project-file-name :around spy)
    (unwind-protect
        (let ((default-directory "/fs:nowhere:/work/"))
          (should (equal (file-truename "/fs:local:/tmp/") expected)))
      (advice-remove 'remote-project-file-name spy))
    (should projected)
    (should-not (member "/fs:nowhere:/work/" projected))))

;;;; Notification-backed supersession

(defvar auto-revert-notify-watch-descriptor)
(defvar auto-revert-notify-modified-p)

(ert-deftest remote-fs-verified-stat-arms-only-an-undisturbed-watch ()
  "Only a proof made under one watch with no new event clears its flag."
  (cl-letf (((symbol-function 'file-notify-valid-p) (lambda (_) t)))
    (with-temp-buffer
      (setq-local auto-revert-notify-watch-descriptor 'watch)
      (setq-local auto-revert-notify-modified-p t)
      (should-not (remote-fs-call-verified-stat (lambda () nil)))
      (should auto-revert-notify-modified-p)
      (remote-fs-call-verified-stat
       (lambda () (remote-fs--count-notify-a) t))
      (should auto-revert-notify-modified-p)
      (remote-fs-call-verified-stat
       (lambda () (setq auto-revert-notify-watch-descriptor 'new) t))
      (should auto-revert-notify-modified-p)
      (should (remote-fs-call-verified-stat (lambda () t)))
      (should-not auto-revert-notify-modified-p)))
  (cl-letf (((symbol-function 'file-notify-valid-p) (lambda (_) nil)))
    (with-temp-buffer
      (setq-local auto-revert-notify-watch-descriptor 'watch)
      (setq-local auto-revert-notify-modified-p t)
      (remote-fs-call-verified-stat (lambda () t))
      (should auto-revert-notify-modified-p))))

(ert-deftest remote-fs-notification-proof-is-lock-scoped ()
  "The first-edit check may trust a quiet watch; nothing else may."
  (cl-letf (((symbol-function 'file-notify-valid-p) (lambda (_) t)))
    (with-temp-buffer
      (setq-local auto-revert-notify-watch-descriptor 'watch)
      (setq-local auto-revert-notify-modified-p nil)
      (should-not (remote-fs-notification-proves-unchanged-p))
      (should (remote-fs--lock-file-a
               (lambda (_file) (remote-fs-notification-proves-unchanged-p))
               "/fs:box:/a"))
      (setq auto-revert-notify-modified-p t)
      (should-not (remote-fs--lock-file-a
                   (lambda (_file)
                     (remote-fs-notification-proves-unchanged-p))
                   "/fs:box:/a"))
      (setq auto-revert-notify-modified-p nil)
      (let ((remote-fs-notify-backed-lock-check nil))
        (should-not (remote-fs--lock-file-a
                     (lambda (_file)
                       (remote-fs-notification-proves-unchanged-p))
                     "/fs:box:/a"))))))

;;;; tramp-rpc metadata scope

(defvar tramp-rpc--file-stat-cache)
(defvar tramp-rpc--file-truename-cache)
(defvar remote-backend-tramp-rpc--in-place-write-file)
(defvar tramp-rpc-protocol-error-file-not-found)

(defmacro remote-tramp-rpc-metadata-test--with-stubs (batches &rest body)
  "Run BODY with tramp-rpc's private cache seams replaced by local tables.
BATCHES names a variable that collects every batch request list; the fake
server answers from `remote-tramp-rpc-metadata-test--server'."
  (declare (indent 1))
  `(let ((tramp-rpc--file-stat-cache (make-hash-table :test #'equal))
         (tramp-rpc--file-truename-cache (make-hash-table :test #'equal))
         (tramp-rpc-protocol-error-file-not-found -32001)
         (remote-backend-tramp-rpc-metadata--locate-cache
          (make-hash-table :test #'equal))
         (remote-backend-tramp-rpc-metadata--counters
          (list :batches 0 :fresh-reuse 0 :modtime-reuse 0 :access-reuse 0
                :locate-hits 0 :locate-misses 0))
         (remote-backend-tramp-rpc-metadata-scope t)
         (remote-backend-tramp-rpc-locate-cache t)
         (remote-file-name-inhibit-cache 10)
         (,batches nil))
     (cl-letf (((symbol-function 'tramp-rpc--encode-path)
                (lambda (path) `((path . ,path))))
               ((symbol-function 'tramp-rpc--decode-string) #'identity)
               ((symbol-function 'tramp-rpc--file-stat-cache-key)
                (lambda (vec localname lstat)
                  (cons (tramp-make-tramp-file-name vec localname)
                        (and lstat t))))
               ((symbol-function 'tramp-rpc--cache-entry-valid-p)
                (lambda (_timestamp) t))
               ((symbol-function 'tramp-rpc--cache-put)
                (lambda (cache key value)
                  (puthash key (cons (float-time) value) cache)))
               ((symbol-function 'tramp-rpc--cache-file-stat-result)
                (lambda (vec localname stat &optional lstat)
                  (puthash (cons (tramp-make-tramp-file-name vec localname)
                                 (and lstat t))
                           (cons (float-time) stat)
                           tramp-rpc--file-stat-cache)))
               ((symbol-function 'tramp-rpc--convert-file-attributes)
                (lambda (stat _id-format)
                  (list nil nil nil nil nil (alist-get 'mtime stat))))
               ((symbol-function 'tramp-rpc--call-batch)
                (lambda (_vec requests)
                  (push (mapcar (lambda (request)
                                  (list (car request)
                                        (alist-get 'path (cdr request))
                                        (alist-get 'lstat (cdr request))))
                                requests)
                        ,batches)
                  (mapcar #'remote-tramp-rpc-metadata-test--server
                          requests))))
       ,@body)))

(defun remote-tramp-rpc-metadata-test--server (request)
  "Answer one fake batch REQUEST for a regular file below /tmp/p/."
  (let ((path (alist-get 'path (cdr request))))
    (cond
     ((equal (car request) "file.access")
      (if (string-prefix-p "/tmp/ro/" path)
          '((errno . 13) (message . "Permission denied"))
        '((errno . 0))))
     ;; The real server answers a missing file's stat with nil and its
     ;; truename with a file-not-found error.
     ((equal path "/tmp/p/missing.c")
      (if (equal (car request) "file.truename")
          '(:error -32001 :message "missing")
        nil))
     ((equal (car request) "file.truename") path)
     ((string-suffix-p "/" path) '((type . "directory") (mtime . dir)))
     (t '((type . "file") (mtime . disk))))))

(ert-deftest remote-tramp-rpc-metadata-scope-batches-first-miss ()
  "A visit's metadata costs one batch; later queries hit the seeded caches."
  (require 'tramp)
  (remote-tramp-rpc-metadata-test--with-stubs batches
    (let* ((file "/ssh:box:/tmp/p/a.c")
           (vec (tramp-dissect-file-name file))
           (calls 0)
           (stat (lambda (vec localname &optional lstat)
                   (cl-incf calls)
                   (ignore vec localname lstat)
                   'network)))
      (remote-backend-tramp-rpc-metadata--scope-a
       (lambda ()
         (remote-backend-tramp-rpc-metadata--stat-a stat vec "/tmp/p/a.c")
         ;; Seeded answers are served by tramp-rpc's own cache lookup,
         ;; which this adapter never reaches; only forced stats come here.
         (let ((remote-file-name-inhibit-cache t))
           (should
            (equal (remote-backend-tramp-rpc-metadata--stat-a
                    stat vec "/tmp/p/a.c" t)
                   '((type . "file") (mtime . disk)))))))
      (should (equal (length batches) 1))
      (should (equal (car batches)
                     '(("file.stat" "/tmp/p/a.c" nil)
                       ("file.stat" "/tmp/p/a.c" t)
                       ("file.truename" "/tmp/p/a.c" nil)
                       ("file.stat" "/tmp/p/" nil)
                       ("file.stat" "/tmp/p/" t)
                       ("file.access" "/tmp/p/a.c" nil)
                       ("file.access" "/tmp/p/" nil))))
      ;; The first stat still asks its own function after the batch; it now
      ;; hits the cache in production.  The forced stat never went out.
      (should (= calls 1))
      (dolist (key (list (cons file nil) (cons file t)
                         (cons "/ssh:box:/tmp/p/" nil)
                         (cons "/ssh:box:/tmp/p/" t)))
        (should (gethash key tramp-rpc--file-stat-cache)))
      (should (equal (cdr (gethash file tramp-rpc--file-truename-cache))
                     file))
      (should (= (plist-get remote-backend-tramp-rpc-metadata--counters
                            :fresh-reuse)
                 1)))))

(ert-deftest remote-tramp-rpc-metadata-scope-answers-save-write-checks ()
  "A save's two `file-writable-p' checks come from the scope's one batch.
The answer is the server's own kernel decision, denials included, and a
write to the file sends its next check to the server again."
  (require 'tramp)
  (remote-tramp-rpc-metadata-test--with-stubs batches
    (let* ((calls nil)
           (access (lambda (_vec localname mode &optional check-parent)
                     (push (list localname mode check-parent) calls)
                     '((errno . 0)))))
      (dolist (case '(("/ssh:box:/tmp/p/a.c" . 0) ("/ssh:box:/tmp/ro/a.c" . 13)))
        (let* ((vec (tramp-dissect-file-name (car case)))
               (localname (tramp-file-name-localname vec))
               (directory (file-name-directory localname)))
          (setq calls nil)
          (remote-backend-tramp-rpc-metadata--scope-a
           (lambda ()
             ;; The first write check opens the scope's batch itself.
             (should (eql (alist-get
                           'errno
                           (remote-backend-tramp-rpc-metadata--access-a
                            access vec localname "w" t))
                          (cdr case)))
             (should (eql (alist-get
                           'errno
                           (remote-backend-tramp-rpc-metadata--access-a
                            access vec directory "w" t))
                          (cdr case)))
             ;; Another question is not the one the batch asked.
             (remote-backend-tramp-rpc-metadata--access-a
              access vec localname "r")
             (should (equal calls (list (list localname "r" nil))))
             (remote-backend-tramp-rpc-metadata--invalidate-path-a
              #'ignore (car case))
             (remote-backend-tramp-rpc-metadata--access-a
              access vec localname "w" t)
             (should (equal (car calls) (list localname "w" t)))))))
      (should (= (length batches) 2))
      (should (= (plist-get remote-backend-tramp-rpc-metadata--counters
                            :access-reuse)
                 4))
      ;; Without a scope every check is the server's.
      (setq calls nil)
      (remote-backend-tramp-rpc-metadata--access-a
       access (tramp-dissect-file-name "/ssh:box:/tmp/p/a.c") "/tmp/p/a.c"
       "w" t)
      (should (= (length calls) 1)))))

(ert-deftest remote-tramp-rpc-metadata-scope-rejects-malformed-access ()
  "An access answer of an unexpected shape is never served from the scope."
  (should (remote-backend-tramp-rpc-metadata--access-result-p '((errno . 0))))
  (should (remote-backend-tramp-rpc-metadata--access-result-p
           '((errno . 13) (message . "Permission denied"))))
  (dolist (bad '(nil t ((errno . 13)) ((errno . -1)) ((errno . "0"))
                 (:error -32001 :message "missing")))
    (should-not (remote-backend-tramp-rpc-metadata--access-result-p bad))))

(ert-deftest remote-tramp-rpc-metadata-dired-reads-a-directory-once ()
  "Within one Dired read, `directory-files' reuses the complete listing.
Its answer keeps `directory-files' semantics; outside a scope, after an
invalidation, or from a filtered listing, the server is asked."
  (let ((remote-backend-tramp-rpc-metadata-scope t)
        (remote-backend-tramp-rpc-metadata--counters
         (list :listing-reuse 0))
        (listings 0)
        (asked 0)
        (dir "/ssh:box:/w/"))
    (cl-flet ((list-all (_directory &optional _full match _nosort _id count)
                (cl-incf listings)
                (ignore match count)
                '(("." . a) (".." . a) ("b" . a) ("a\nz" . a) ("c" . a))))
      (let ((files (lambda (&rest _) (cl-incf asked) 'server)))
        (remote-backend-tramp-rpc-metadata--listing-scope-a
         (lambda ()
           (remote-backend-tramp-rpc-metadata--listing-record-a
            #'list-all dir)
           (should (equal (remote-backend-tramp-rpc-metadata--listing-answer-a
                           files dir nil "\n")
                          '("a\nz")))
           (should (equal (remote-backend-tramp-rpc-metadata--listing-answer-a
                           files "/ssh:box:/w" t "\\`[bc]" nil 1)
                          '("/ssh:box:/w/b")))
           (should (equal (remote-backend-tramp-rpc-metadata--listing-answer-a
                           files dir nil nil t)
                          '("." ".." "b" "a\nz" "c")))
           ;; Another directory is not this one.
           (should (eq (remote-backend-tramp-rpc-metadata--listing-answer-a
                        files "/ssh:box:/other/")
                       'server))
           ;; A change inside the directory forgets its listing.
           (remote-backend-tramp-rpc-metadata--forget-listing
            "/ssh:box:/w/new")
           (should (eq (remote-backend-tramp-rpc-metadata--listing-answer-a
                        files dir)
                       'server))
           ;; A filtered listing is not complete and is never recorded.
           (remote-backend-tramp-rpc-metadata--listing-record-a
            #'list-all dir nil "b")
           (should (eq (remote-backend-tramp-rpc-metadata--listing-answer-a
                        files dir)
                       'server))))
        (should (= listings 2))
        (should (= asked 3))
        (should (= (plist-get remote-backend-tramp-rpc-metadata--counters
                              :listing-reuse)
                   3))
        ;; No scope, no reuse.
        (remote-backend-tramp-rpc-metadata--listing-record-a #'list-all dir)
        (should (eq (remote-backend-tramp-rpc-metadata--listing-answer-a
                     files dir)
                    'server))))))

(ert-deftest remote-tramp-rpc-metadata-scope-forgets-written-file ()
  "After a write, a forced stat in the same save is a real round trip."
  (require 'tramp)
  (remote-tramp-rpc-metadata-test--with-stubs batches
    (let* ((file "/ssh:box:/tmp/p/a.c")
           (vec (tramp-dissect-file-name file))
           (calls 0)
           (stat (lambda (_vec _localname &optional _lstat)
                   (cl-incf calls) 'fresh)))
      (remote-backend-tramp-rpc-metadata--scope-a
       (lambda ()
         (let ((remote-file-name-inhibit-cache t))
           (should (equal (remote-backend-tramp-rpc-metadata--stat-a
                           stat vec "/tmp/p/a.c" t)
                          '((type . "file") (mtime . disk))))
           (remote-backend-tramp-rpc-metadata--invalidate-path-a #'ignore file)
           (should (eq (remote-backend-tramp-rpc-metadata--stat-a
                        stat vec "/tmp/p/a.c" t)
                       'fresh)))))
      (should (= (length batches) 1))
      (should (= calls 1)))))

(ert-deftest remote-tramp-rpc-metadata-no-scope-is-transparent ()
  "Outside a visit or save the adapter neither batches nor answers."
  (require 'tramp)
  (remote-tramp-rpc-metadata-test--with-stubs batches
    (let ((vec (tramp-dissect-file-name "/ssh:box:/tmp/p/a.c"))
          (remote-file-name-inhibit-cache t))
      (should (eq (remote-backend-tramp-rpc-metadata--stat-a
                   (lambda (&rest _) 'direct) vec "/tmp/p/a.c")
                  'direct))
      (should-not batches))
    (let ((remote-backend-tramp-rpc-metadata-scope nil)
          (vec (tramp-dissect-file-name "/ssh:box:/tmp/p/a.c")))
      (remote-backend-tramp-rpc-metadata--scope-a
       (lambda ()
         (should (eq (remote-backend-tramp-rpc-metadata--stat-a
                      (lambda (&rest _) 'direct) vec "/tmp/p/a.c")
                     'direct))))
      (should-not batches))))

(ert-deftest remote-tramp-rpc-metadata-missing-file-and-batch-failure ()
  "A missing file caches nil; a failed batch leaves the per-query path."
  (require 'tramp)
  (remote-tramp-rpc-metadata-test--with-stubs batches
    (let ((vec (tramp-dissect-file-name "/ssh:box:/tmp/p/missing.c")))
      (remote-backend-tramp-rpc-metadata--scope-a
       (lambda ()
         (let ((remote-file-name-inhibit-cache t))
           (should-not (remote-backend-tramp-rpc-metadata--stat-a
                        (lambda (&rest _) 'network)
                        vec "/tmp/p/missing.c")))))
      (should (equal (gethash (cons "/ssh:box:/tmp/p/missing.c" nil)
                              tramp-rpc--file-stat-cache)
                     (cons (car (gethash (cons "/ssh:box:/tmp/p/missing.c"
                                               nil)
                                         tramp-rpc--file-stat-cache))
                           nil)))
      ;; A missing file has no symlink to chase: its truename is its name,
      ;; which saves the failing `file.truename' round trip and its noise.
      (should (equal (cdr (gethash "/ssh:box:/tmp/p/missing.c"
                                   tramp-rpc--file-truename-cache))
                     "/ssh:box:/tmp/p/missing.c")))
    (cl-letf (((symbol-function 'tramp-rpc--call-batch)
               (lambda (&rest _) (signal 'remote-file-error '("down")))))
      (let ((vec (tramp-dissect-file-name "/ssh:box:/tmp/p/b.c")))
        (remote-backend-tramp-rpc-metadata--scope-a
         (lambda ()
           (let ((remote-file-name-inhibit-cache t))
             (should (eq (remote-backend-tramp-rpc-metadata--stat-a
                          (lambda (&rest _) 'network) vec "/tmp/p/b.c")
                         'network)))))))))

(ert-deftest remote-tramp-rpc-metadata-visit-modtime-is-pre-read-stat ()
  "Only a visit's own stat may stand in for the post-read modtime stat."
  (require 'tramp)
  (remote-tramp-rpc-metadata-test--with-stubs batches
    (let* ((file "/ssh:box:/tmp/p/a.c")
           (vec (tramp-dissect-file-name file))
           (modtime (lambda (&optional time) (list 'set time))))
      (with-temp-buffer
        (setq buffer-file-name file)
        (should (equal (remote-backend-tramp-rpc-metadata--modtime-a modtime)
                       '(set nil)))
        (remote-backend-tramp-rpc-metadata--scope-a
         (lambda ()
           (remote-backend-tramp-rpc-metadata--stat-a
            #'ignore vec "/tmp/p/a.c")
           (should (equal (remote-backend-tramp-rpc-metadata--modtime-a
                           modtime)
                          '(set disk)))
           (should (equal (remote-backend-tramp-rpc-metadata--modtime-a
                           modtime 'explicit)
                          '(set explicit)))
           (remote-backend-tramp-rpc-metadata--invalidate-path-a #'ignore file)
           (should (equal (remote-backend-tramp-rpc-metadata--modtime-a
                           modtime)
                          '(set nil)))))
        (setq buffer-file-name nil)))))

(ert-deftest remote-tramp-rpc-metadata-truename-starts-the-batch ()
  "`write-region' asks for a truename first; that miss starts the batch."
  (require 'tramp)
  (remote-tramp-rpc-metadata-test--with-stubs batches
    (remote-backend-tramp-rpc-metadata--scope-a
     (lambda ()
       (should (eq (remote-backend-tramp-rpc-metadata--truename-a
                    (lambda (_file) 'answered) "/ssh:box:/tmp/p/a.c")
                   'answered))
       (remote-backend-tramp-rpc-metadata--truename-a
        (lambda (_file) 'again) "/ssh:box:/tmp/p/b.c")))
    (should (= (length batches) 1))
    (should (equal (cadr (car (car batches))) "/tmp/p/a.c"))
    (should (gethash "/ssh:box:/tmp/p/a.c" tramp-rpc--file-truename-cache))
    (dolist (spelling '("/ssh:box:/tmp/p/" "/ssh:box:/tmp/p"))
      (should (gethash (cons spelling t) tramp-rpc--file-stat-cache)))))

(ert-deftest remote-tramp-rpc-metadata-in-place-rewrite-keeps-directory ()
  "Rewriting an existing file keeps its directory's stat; nothing else does."
  (require 'tramp)
  (remote-tramp-rpc-metadata-test--with-stubs batches
    (let* ((file "/ssh:box:/tmp/p/a.c")
           (vec (tramp-dissect-file-name file))
           (drop (lambda (_file) (clrhash tramp-rpc--file-stat-cache))))
      (remote-backend-tramp-rpc-metadata--scope-a
       (lambda ()
         (remote-backend-tramp-rpc-metadata--stat-a #'ignore vec "/tmp/p/a.c")
         ;; A write outside the verified in-place scope drops everything.
         (let ((remote-backend-tramp-rpc--in-place-write-file nil))
           (remote-backend-tramp-rpc-metadata--invalidate-path-a drop file))
         (should-not (gethash (cons "/ssh:box:/tmp/p" t)
                              tramp-rpc--file-stat-cache))))
      (clrhash tramp-rpc--file-stat-cache)
      (remote-backend-tramp-rpc-metadata--scope-a
       (lambda ()
         (remote-backend-tramp-rpc-metadata--stat-a #'ignore vec "/tmp/p/a.c")
         (let ((remote-backend-tramp-rpc--in-place-write-file file))
           (remote-backend-tramp-rpc-metadata--invalidate-path-a drop file))
         (dolist (spelling '("/ssh:box:/tmp/p/" "/ssh:box:/tmp/p"))
           (dolist (lstat '(nil t))
             (should (gethash (cons spelling lstat)
                              tramp-rpc--file-stat-cache))))
         ;; The rewritten file itself is still forgotten.
         (should-not (gethash (cons file t) tramp-rpc--file-stat-cache))))
      ;; A file the scope never saw as regular (a new file) keeps nothing.
      (clrhash tramp-rpc--file-stat-cache)
      (remote-backend-tramp-rpc-metadata--scope-a
       (lambda ()
         (let ((remote-backend-tramp-rpc--in-place-write-file
                "/ssh:box:/tmp/p/missing.c"))
           (remote-backend-tramp-rpc-metadata--stat-a
            #'ignore (tramp-dissect-file-name "/ssh:box:/tmp/p/missing.c")
            "/tmp/p/missing.c")
           (remote-backend-tramp-rpc-metadata--invalidate-path-a
            drop "/ssh:box:/tmp/p/missing.c"))
         (should-not (gethash (cons "/ssh:box:/tmp/p" t)
                              tramp-rpc--file-stat-cache)))))))

(ert-deftest remote-tramp-rpc-metadata-locate-cache-scope ()
  "Marker searches are shared by siblings and dropped only by markers."
  (require 'tramp)
  (remote-tramp-rpc-metadata-test--with-stubs batches
    (let* ((searches 0)
           (locate (lambda (_file _name)
                     (cl-incf searches) "/ssh:box:/tmp/")))
      (dolist (name '("a.c" "b.c"))
        (puthash (cons (concat "/ssh:box:/tmp/p/" name) nil)
                 (cons (float-time) '((type . "file")))
                 tramp-rpc--file-stat-cache))
      (dotimes (_ 2)
        (should (equal (remote-backend-tramp-rpc-metadata--locate-a
                        locate "/ssh:box:/tmp/p/a.c" ".git")
                       "/ssh:box:/tmp/")))
      (remote-backend-tramp-rpc-metadata--locate-a
       locate "/ssh:box:/tmp/p/b.c" ".git")
      (should (= searches 1))
      (remote-backend-tramp-rpc-metadata--invalidate-path-a
       #'ignore "/ssh:box:/tmp/p/a.c")
      (remote-backend-tramp-rpc-metadata--flush-file-a nil "/tmp/p/b.c")
      (remote-backend-tramp-rpc-metadata--locate-a
       locate "/ssh:box:/tmp/p/a.c" ".git")
      (should (= searches 1))
      (remote-backend-tramp-rpc-metadata--flush-file-a nil "/tmp/p/.git")
      (remote-backend-tramp-rpc-metadata--locate-a
       locate "/ssh:box:/tmp/p/a.c" ".git")
      (should (= searches 2))
      (remote-backend-tramp-rpc-metadata--locate-a
       locate "/ssh:box:/tmp/p/a.c" #'ignore)
      (should (= searches 3))
      (let ((remote-file-name-inhibit-cache t))
        (remote-backend-tramp-rpc-metadata--locate-a
         locate "/ssh:box:/tmp/p/a.c" ".git"))
      (should (= searches 4)))))

(ert-deftest remote-tramp-rpc-metadata-install-is-gated ()
  "The adapter installs only on the verified release with intact seams."
  (require 'tramp)
  (let ((tramp-rpc--file-stat-cache (make-hash-table :test #'equal))
        (tramp-rpc--file-truename-cache (make-hash-table :test #'equal))
        (tramp-rpc-protocol-error-file-not-found -32001))
    (unwind-protect
        (progn
          (should-not (remote-backend-tramp-rpc-metadata-install
                       nil (lambda (_contract) t)))
          (should-not (remote-backend-tramp-rpc-metadata-install
                       t (lambda (contract)
                           (not (eq (car contract)
                                    'tramp-rpc--call-batch)))))
          (should-not (advice-member-p
                       #'remote-backend-tramp-rpc-metadata--scope-a
                       'find-file-noselect))
          (should (remote-backend-tramp-rpc-metadata-install
                   t (lambda (_contract) t)))
          (should (advice-member-p
                   #'remote-backend-tramp-rpc-metadata--scope-a
                   'basic-save-buffer)))
      (remote-backend-tramp-rpc-metadata-uninstall)
      (should-not (advice-member-p
                   #'remote-backend-tramp-rpc-metadata--scope-a
                   'find-file-noselect)))))

;;;; Explicit TRAMP cleanup lifecycle

(defun remote-compat-test--open-session (target handle plugin)
  "Return an open session for TARGET with physical HANDLE through PLUGIN."
  (remote-connection-create
   :key (list target plugin) :target-id target
   :link-id (concat target "/ssh") :plugin-id plugin :handle handle
   :state 'open
   :pipeline-runtime (remote-pipeline-runtime-create :state 'open)))

(ert-deftest remote-connection-target-open-p-reads-pool-state-only ()
  (let ((remote-connection-pool (make-hash-table :test #'equal))
        (session (remote-compat-test--open-session
                  "box" "/ssh:box:/" "tramp")))
    (should-not (remote-connection-target-open-p "box"))
    (puthash (remote-connection-key session) session remote-connection-pool)
    (should (remote-connection-target-open-p "box"))
    (should-not (remote-connection-target-open-p "other"))
    (setf (remote-pipeline-runtime-state
           (remote-connection-pipeline-runtime session))
          'closed)
    (should-not (remote-connection-target-open-p "box"))))

(ert-deftest remote-fs-file-remote-p-connected-follows-open-session ()
  "Background callers must not see a closed target as connected."
  (let (open)
    (cl-letf (((symbol-function 'remote-connection-target-open-p)
               (lambda (_target) open)))
      (should (equal (remote-fs-handle-file-remote-p "/fs:box:/tmp/x")
                     "/fs:box:"))
      (should-not (remote-fs-handle-file-remote-p "/fs:box:/tmp/x" nil t))
      (setq open t)
      (should (equal (remote-fs-handle-file-remote-p "/fs:box:/tmp/x" nil t)
                     "/fs:box:"))
      (should (equal (remote-fs-handle-file-remote-p
                      "/fs:box:/tmp/x" 'host t)
                     "box"))
      (should-not (remote-fs-handle-file-remote-p "/fs:local:/tmp/x")))))

(ert-deftest remote-tramp-cleanup-disconnects-owning-target-first ()
  "An explicit TRAMP cleanup closes the framework target, not reconnects it."
  (let ((remote-connection-pool (make-hash-table :test #'equal))
        (remote-workspaces (make-hash-table :test #'equal))
        (remote-links (make-hash-table :test #'equal))
        (remote-backend-tramp-explicit-cleanup nil)
        disconnected cleaned)
    (dolist (session (list (remote-compat-test--open-session
                            "box" "/ssh:box:/" "tramp")
                           (remote-compat-test--open-session
                            "other" "/ssh:other:/" "tramp")
                           (remote-compat-test--open-session
                            "near" "/fs:near:/" "native")))
      (puthash (remote-connection-key session) session
               remote-connection-pool))
    (cl-letf (((symbol-function 'remote-workspace-disconnect-target)
               (lambda (target reason)
                 (should remote-backend-tramp-explicit-cleanup)
                 (push (cons target reason) disconnected))))
      (remote-backend-tramp--cleanup-connection-a
       (lambda (vec &rest _)
         (should remote-backend-tramp-explicit-cleanup)
         (push (tramp-file-name-host vec) cleaned))
       (tramp-dissect-file-name "/ssh:box:/"))
      (should (equal disconnected '(("box" . tramp-cleanup))))
      (should (equal cleaned '("box")))
      ;; The backend closer re-enters cleanup; it must not recurse.
      (setq disconnected nil cleaned nil)
      (let ((remote-backend-tramp-explicit-cleanup t))
        (remote-backend-tramp--cleanup-connection-a
         (lambda (vec &rest _) (push (tramp-file-name-host vec) cleaned))
         (tramp-dissect-file-name "/ssh:box:/")))
      (should-not disconnected)
      (should (equal cleaned '("box")))
      (setq disconnected nil cleaned nil)
      (remote-backend-tramp--cleanup-connection-a
       (lambda (vec &rest flags)
         (should (equal flags '(keep-debug nil keep-processes)))
         (push (tramp-file-name-host vec) cleaned))
       (tramp-dissect-file-name "/ssh:box:/") 'keep-debug nil 'keep-processes)
      (should-not disconnected)
      (should (equal cleaned '("box")))
      (setq disconnected nil)
      (remote-backend-tramp--cleanup-all-connections-a #'ignore)
      (should (equal (sort (mapcar #'car disconnected) #'string<)
                     '("box" "other"))))
    (should-not remote-backend-tramp-explicit-cleanup)))

(ert-deftest remote-tramp-rpc-explicit-cleanup-does-not-report-failure ()
  "Transport death caused by an explicit cleanup must not schedule reconnect."
  (remote-tramp-rpc-test--readable
   (let* ((remote-workspaces (make-hash-table :test #'equal))
          (route (remote-route-create
                  :target-id "box" :link-id "box/ssh"
                  :link-plugin-id "tramp-rpc"))
          (owner (remote-workspace-create :routes (list route)))
          (connection (remote-connection-create :handle "/rpc:box:/tmp/"))
          (process (make-pipe-process
                    :name "remote-explicit-cleanup-test" :noquery t))
          (reports 0))
     (unwind-protect
         (progn
           (puthash 'owner owner remote-workspaces)
           (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                      (lambda (_vec) (remote-tramp-rpc-test--generation :process process)))
                     ((symbol-function 'tramp-rpc--connection-key)
                      (lambda (_vec) 'same-connection))
                     ((symbol-function 'tramp-dissect-file-name)
                      (lambda (_name &optional _nodefault) 'vec))
                     ((symbol-function 'remote-connection-cached-p)
                      (lambda (_route) connection))
                     ((symbol-function 'remote-report-route-failure)
                      (lambda (_route _error) (cl-incf reports))))
             (let ((remote-backend-tramp-explicit-cleanup t))
               (remote-backend-tramp-rpc--transport-death-before-a
                process 'vec "killed: 9"))
             (should (= reports 0))
             (remote-backend-tramp-rpc--transport-death-before-a
              process 'vec "killed: 9")
             (should (= reports 1))))
       (delete-process process)))))

(ert-deftest remote-tramp-internal-session-disconnect-keeps-target-buffers ()
  "Reconnect/stale-session cleanup must not invoke user-level teardown."
  (require 'tramp-cmds)
  (let ((remote-backend-tramp-explicit-cleanup nil)
        (connection (remote-compat-test--open-session
                     "box" "/ssh:box:/" "tramp"))
        cleaned)
    (cl-letf (((symbol-function 'remote-workspace-disconnect-target)
               (lambda (&rest _) (ert-fail "Internal disconnect closed target")))
              ((symbol-function 'tramp-cleanup-connection)
               (lambda (_vec &rest _)
                 (should remote-backend-tramp-explicit-cleanup)
                 (setq cleaned t))))
      (remote-backend-tramp-disconnect connection nil)
      (should cleaned))))

(ert-deftest remote-tramp-cleanup-finds-buffer-owner-without-a-session ()
  "Protected edits remain discoverable on a subsequent cleanup."
  (let ((remote-targets (make-hash-table :test #'equal))
        (remote-links (make-hash-table :test #'equal))
        (remote-connection-pool (make-hash-table :test #'equal))
        (remote-workspaces (make-hash-table :test #'equal))
        (buffer (generate-new-buffer " *legacy-cleanup-file*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq buffer-file-name "/ssh:cleanup-box:/tmp/a.c"))
          (should (member "cleanup-box"
                          (remote-backend-tramp--targets-for-vec
                           (tramp-dissect-file-name "/ssh:cleanup-box:/")))))
      (kill-buffer buffer))))

(ert-deftest remote-tramp-rpc-watch-descriptor-leaves-no-buffer ()
  "`make-pipe-process' always creates a buffer; descriptors must drop it."
  (when-let* ((stale (get-buffer "tramp-rpc")))
    (kill-buffer stale))
  (let ((descriptor (make-pipe-process :name "tramp-rpc" :noquery t)))
    (unwind-protect
        (progn
          (should (get-buffer "tramp-rpc"))
          (should (eq (remote-backend-tramp-rpc--detach-descriptor-buffer-a
                       descriptor)
                      descriptor))
          (should-not (process-buffer descriptor))
          (should-not (get-buffer "tramp-rpc")))
      (delete-process descriptor))))

(ert-deftest remote-tramp-rpc-cleanup-kills-stderr-buffer-and-relay ()
  (let* ((vec (tramp-dissect-file-name "/ssh:box:/"))
         (buffer (get-buffer-create "*tramp/ssh box* stderr"))
         (relay (make-pipe-process :name "remote-stderr-relay-test"
                                   :buffer buffer :noquery t)))
    (unwind-protect
        (cl-letf (((symbol-function 'remote-backend-tramp-rpc--current-stderr-buffer)
                   (lambda (_vec) nil)))
          (remote-backend-tramp-rpc--cleanup-connection-buffers-h vec)
          (should-not (buffer-live-p buffer))
          (should-not (process-live-p relay)))
      (when (process-live-p relay) (delete-process relay))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest remote-tramp-rpc-generation-cleanup-spares-replacement-relay ()
  "A finished generation drops its relay but never the live replacement's."
  (remote-tramp-rpc-test--readable
   (let* ((buffer (get-buffer-create " remote-generation-stderr-test"))
          (relay (make-pipe-process :name "remote-generation-relay-test"
                                    :buffer buffer :noquery t))
          (transport (make-pipe-process :name "remote-generation-test"
                                        :noquery t))
          current)
     (unwind-protect
         (cl-letf (((symbol-function 'remote-backend-tramp-rpc--current-stderr-buffer)
                    (lambda (_vec) current))
                   ((symbol-function 'tramp-rpc--process-connection)
                    (lambda (process)
                      (process-get process :tramp-rpc-connection))))
           (process-put transport :tramp-rpc-connection
                        (remote-tramp-rpc-test--generation :stderr-buffer buffer))
           (setq current buffer)
           (remote-backend-tramp-rpc--generation-cleanup-after-a
            transport 'vec "late\n" :transport-death)
           (should (process-live-p relay))
           (setq current nil)
           (remote-backend-tramp-rpc--generation-cleanup-after-a
            transport 'vec "killed\n" :transport-death)
           (should-not (process-live-p relay))
           ;; The buffer stays for diagnostics until TRAMP cleanup.
           (should (buffer-live-p buffer)))
       (dolist (process (list relay transport))
         (when (process-live-p process) (delete-process process)))
       (kill-buffer buffer)))))

(ert-deftest remote-tramp-rpc-cleanup-removes-numbered-stderr-buffers ()
  (let* ((vec (tramp-dissect-file-name "/ssh:cleanup-box:/"))
         (base (concat (tramp-buffer-name vec) " stderr"))
         (buffers (mapcar #'get-buffer-create
                          (list base (concat base "<1>") (concat base "<2>"))))
         (other (get-buffer-create "*tramp/ssh cleanup-other* stderr"))
         (processes (mapcar (lambda (buffer)
                             (make-pipe-process :name "numbered-stderr"
                                                :buffer buffer :noquery t))
                           buffers)))
    (unwind-protect
        (progn
          (remote-backend-tramp-rpc--cleanup-connection-buffers-h vec)
          (dolist (buffer buffers) (should-not (buffer-live-p buffer)))
          (dolist (process processes) (should-not (process-live-p process)))
          (should (buffer-live-p other)))
      (dolist (process processes)
        (when (process-live-p process) (delete-process process)))
      (dolist (buffer (cons other buffers))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest remote-tramp-rpc-explicit-cleanup-sweeps-own-orphaned-main-buffers ()
  "Single-target cleanup must also find transports no longer in any table."
  (let* ((tramp-methods (cons '("rpc") tramp-methods))
         (vec (tramp-dissect-file-name "/rpc:cleanup-box:/"))
         (base (tramp-buffer-name vec))
         (buffers (mapcar #'get-buffer-create
                          (list base (concat base "<1>")
                                (concat base " stderr<2>"))))
         (other (get-buffer-create "*tramp/rpc cleanup-other*")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'tramp-rpc--get-connection)
                     (lambda (_vec) nil)))
            ;; Ordinary keep-debug/internal cleanup must preserve main buffers.
            (let ((remote-backend-tramp-explicit-cleanup nil))
              (remote-backend-tramp-rpc--cleanup-connection-buffers-h vec))
            (should (buffer-live-p (car buffers)))
            (let ((remote-backend-tramp-explicit-cleanup t))
              (remote-backend-tramp-rpc--cleanup-connection-buffers-h vec)))
          (dolist (buffer buffers) (should-not (buffer-live-p buffer)))
          (should (buffer-live-p other)))
      (dolist (buffer (cons other buffers))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest remote-tramp-rpc-global-cleanup-removes-orphaned-buffers ()
  "A missing connection-table entry must not hide transport/relay buffers."
  (let* ((buffers (mapcar #'get-buffer-create
                         '("*tramp/rpc cleanup-orphan*"
                           "*tramp/rpc cleanup-orphan* stderr"
                           "*tramp/rpc cleanup-orphan* stderr<1>")))
         (processes (mapcar (lambda (buffer)
                             (make-pipe-process :name "orphaned-rpc-relay"
                                                :buffer buffer :noquery t))
                           buffers)))
    (unwind-protect
        (progn
          (remote-backend-tramp-rpc--cleanup-all-buffers-h)
          (dolist (buffer buffers) (should-not (buffer-live-p buffer)))
          (dolist (process processes) (should-not (process-live-p process))))
      (dolist (process processes)
        (when (process-live-p process) (delete-process process)))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'remote-compat-tests)
;;; remote-compat-tests.el ends here
