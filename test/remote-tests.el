;;; remote-tests.el --- Logical target and routing tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with:
;;   emacs --batch -Q -L lisp -L lisp/remote -L lisp/remote/backend \
;;     -l test/remote-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-process)
(require 'remote-connection)
(require 'remote-environment)
(require 'remote-path)
(require 'remote-backend-tramp-rpc)
(require 'remote-framework)
(require 'direnv)

(remote-fs-install)

(defmacro remote-test-with-registry (&rest body)
  "Evaluate BODY with an isolated remote registry."
  (declare (indent 0) (debug t))
  `(let ((remote-targets (make-hash-table :test #'equal))
         (remote-links (make-hash-table :test #'equal))
         (remote-link-plugins (make-hash-table :test #'equal))
         (remote-adapters (make-hash-table :test #'equal))
         (remote-route-health (make-hash-table :test #'equal))
         (remote-route-log nil)
         (remote-connection-pool (make-hash-table :test #'equal))
         (remote-pipeline-runtime-pool (make-hash-table :test #'equal))
         (remote-environment-cache (make-hash-table :test #'equal))
         (remote-environments-by-id (make-hash-table :test #'equal))
         (remote-environment-providers nil))
     (remote-reset-registries)
     (remote-fs-register-link-plugins)
     (remote-register-adapter
      "process"
      :capabilities '(process-sync process-async environment)
      :preferences '((default . ("native" "tramp-rpc" "tramp"))))
     (remote-register-adapter
      "exec"
      :capabilities '(process-sync environment)
      :preferences '((default . ("tramp-rpc" "tramp" "native"))))
     (remote-register-adapter
      "environment"
      :capabilities '(process-sync environment)
      :preferences '((default . ("tramp-rpc" "tramp" "native"))))
     ,@body))

(ert-deftest remote-public-api-is-present ()
  (dolist (function
           '(remote-canonicalize-file-name
             remote-expand-file-name
             remote-file-name-to-uri
             remote-uri-to-file-name
             remote-file-name-target
             remote-file-local-name
             remote-project-file-name
             remote-file-equal-p
             remote-register-link-plugin
             remote-register-adapter
             remote-register-pipeline
             remote-get-pipeline
             remote-pipeline-resolve
             remote-register-backend
             remote-get-backend
             remote-backend-prepare-execution
             remote-connection-ensure
             remote-connection-warm
             remote-connection-pool-status
             remote-session-acquire
             remote-session-warm
             remote-session-list
             remote-context
             remote-resolve
             remote-make-process
             remote-process-file
             remote-exec
             remote-exec-async
             remote-executable-find
             remote-environment-ensure
             remote-environment-derive
             remote-register-environment-maintainer
             remote-get-environment
             remote-path-decorate
             remote-register-path-profile
             remote-path-candidates
             remote-path-probe
             remote-make-network-process
             remote-open-network-stream
             remote-port-forward))
    (should (fboundp function)))
  (should (macrop 'remote-with-route)))

(ert-deftest remote-canonicalize-tilde-default-does-not-reenter-target-inference ()
  (remote-test-with-registry
    (let* ((default-directory "~/.config/emacs/")
           (canonical
            (remote-canonicalize-file-name default-directory)))
      (should (equal (remote-fs-target-id canonical) "local"))
      (should
       (equal (remote-fs-localname canonical)
              (file-name-as-directory
               (expand-file-name "~/.config/emacs/")))))))

(ert-deftest remote-connection-pool-opens-once-and-reuses ()
  (remote-test-with-registry
    (let ((opens 0))
      (remote-register-link-plugin
       "pooled"
       :capabilities '(process-sync)
       :project-file-name (lambda (_file _link _route) "/pooled/")
       :connect
       (lambda (_route _context)
         (cl-incf opens)
         'handle)
       :connection-live-p
       (lambda (_connection _route _context) t))
      (remote-register-target "lab" :trusted t)
      (remote-register-link "lab" "ssh" "pooled")
      (remote-register-adapter
       "test" :capabilities '(process-sync)
       :preferences '((default . ("pooled"))))
      (let* ((context
              (remote-context-create
               :target-id "lab" :localname "/work/a"
               :workspace-root "/fs:lab:/work/"))
             (route (remote-resolve "test" 'process-sync context))
             (first (remote-connection-ensure route context))
             (second (remote-connection-ensure route context)))
        (should (eq first second))
        (should (= opens 1))
        (should (= (remote-connection-use-count second) 2))
        (should (= (length (remote-connection-pool-status)) 1))))))

(ert-deftest remote-connection-open-has-framework-deadline ()
  (remote-test-with-registry
    (remote-register-link-plugin
     "blocking"
     :capabilities '(process-sync)
     :project-file-name (lambda (_file _link _route) "/blocking/")
     :connect
     (lambda (_route _context)
       (while t
         (accept-process-output nil 0.005))))
    (remote-register-target "offline" :trusted t)
    (remote-register-link "offline" "ssh" "blocking")
    (remote-register-adapter
     "test" :capabilities '(process-sync)
     :preferences '((default . ("blocking"))))
    (let* ((context
            (remote-context-create
             :target-id "offline" :localname "/work/a"
             :workspace-root "/fs:offline:/work/"))
           (route (remote-resolve "test" 'process-sync context))
           (remote-connection-open-timeout 0.03)
           (started (float-time)))
      (should-error
       (remote-connection-ensure route context)
       :type 'remote-connection-timeout)
      (should (< (- (float-time) started) 0.5))
      (should (zerop (hash-table-count remote-connection-pool)))
      (should-not (remote-pipeline-runtime-list)))))

(ert-deftest remote-path-and-uri-round-trip ()
  (remote-test-with-registry
    (should
     (equal (remote-canonicalize-file-name "/tmp/a b")
            "/fs:local:/tmp/a b"))
    (should
     (equal (remote-file-name-to-uri "/tmp/a b")
            "fs://local/tmp/a%20b"))
    (should
     (equal (remote-uri-to-file-name "fs://local/tmp/a%20b")
            "/fs:local:/tmp/a b"))
    (should (equal (remote-make-file-name "box" "")
                   "/fs:box:/"))
    (should (equal (remote-canonicalize-file-name "/ssh:box:")
                   "/fs:box:/"))
    (should (equal (remote-file-name-target "/tmp/a") "local"))
    (should (equal (remote-file-local-name "/fs:local:/tmp/a")
                   "/tmp/a"))
    (should (remote-file-equal-p
             "/tmp/../tmp/a" "fs://local/tmp/a"))))

(ert-deftest remote-canonicalize-tramp-home-on-the-target ()
  (remote-test-with-registry
    (remote-register-target "box" :trusted t)
    (remote-register-link
     "box" "ssh" '("tramp" "tramp-rpc")
     :config '(:host "box" :method "ssh"))
    (let ((original (symbol-function 'expand-file-name)))
      (cl-letf
          (((symbol-function 'expand-file-name)
            (lambda (name &optional directory)
              (if (equal name "/ssh:box:~/work/")
                  "/ssh:box:/home/me/work/"
                (funcall original name directory)))))
        (should
         (equal
          (remote-canonicalize-file-name "/ssh:box:~/work/")
          "/fs:box:/home/me/work/"))))
    (should-error
     (remote-make-file-name "box" "~/work/")
     :type 'error)))

(ert-deftest remote-expand-local-home-is-target-aware ()
  (remote-test-with-registry
    (let* ((spelling "~/Documents/AaronNote/")
           (expected
            (file-name-as-directory
             (expand-file-name spelling))))
      (should
       (equal
        (remote-expand-file-name spelling nil "local")
        (remote-make-file-name "local" expected)))
      (should
       (equal
        (remote-canonicalize-file-name spelling)
        (remote-make-file-name "local" expected)))
      (should
       (equal
        (abbreviate-file-name
         (remote-make-file-name "local" expected))
        (remote-make-file-name "local" expected)))
      (should
       (equal
        (remote-target-file-name
         (remote-get-target "local") spelling)
        (remote-make-file-name "local" expected))))))

(ert-deftest remote-expand-home-is-owned-by-selected-backend ()
  (remote-test-with-registry
    (remote-register-backend
     "target-home"
     :capabilities '(metadata)
     :project (lambda (file _link _route) file)
     :expand-localname
     (lambda (name _directory _link _route)
       (if (string-prefix-p "~/" name)
           (concat "/home/remote/" (string-remove-prefix "~/" name))
         name)))
    (remote-register-target
     "box" :trusted t
     :workspaces
     '(((id . "main") (path . "~/work/"))))
    (remote-register-link "box" "home" "target-home")
    (should
     (equal
      (remote-expand-file-name "~/work/" nil "box")
      "/fs:box:/home/remote/work/"))
    (should
     (equal
      (remote-canonicalize-file-name "~/work/" "/fs:box:/")
      "/fs:box:/home/remote/work/"))
    (should
     (equal
      (gethash
       '("box" "~/work/") remote-fs-path-expansion-cache)
      "/home/remote/work/"))
    (let* ((logical
            (remote-target-file-name (remote-get-target "box")))
           (context (remote-context logical)))
      (should (equal logical "/fs:box:/home/remote/work/"))
      (should (equal (remote-context-workspace-id context) "main"))
      (should
       (equal
        (remote-context-workspace-root context)
        "/fs:box:/home/remote/work/")))))

(ert-deftest remote-symlink-api-preserves-native-target-spelling ()
  (remote-test-with-registry
    (let* ((root (make-temp-file "remote-symlink-" t))
           (target (expand-file-name "target.txt" root))
           (relative-link (expand-file-name "relative-link" root))
           (absolute-link (expand-file-name "absolute-link" root))
           (logical-target (remote-canonicalize-file-name target))
           (logical-truename
            (remote-canonicalize-file-name (file-truename target)))
           (logical-relative
            (remote-canonicalize-file-name relative-link))
           (logical-absolute
            (remote-canonicalize-file-name absolute-link)))
      (unwind-protect
          (progn
            (with-temp-file target (insert "target"))
            (make-symbolic-link "target.txt" logical-relative)
            (should
             (equal (file-symlink-p logical-relative) "target.txt"))
            (should
             (equal (file-truename logical-relative) logical-truename))
            (should (file-equal-p logical-relative logical-target))
            (make-symbolic-link logical-target logical-absolute)
            (should
             (equal (file-symlink-p logical-absolute) target))
            (should
             (equal (file-truename logical-absolute) logical-truename))
            (delete-file target)
            (should
             (equal (file-symlink-p logical-relative) "target.txt")))
        (ignore-errors (delete-file relative-link))
        (ignore-errors (delete-file absolute-link))
        (ignore-errors (delete-file target))
        (ignore-errors (delete-directory root))))))

(ert-deftest remote-symlink-logical-target-never-becomes-tramp-text ()
  (remote-test-with-registry
    (remote-register-target "box" :trusted t)
    (remote-register-link
     "box" "ssh" "tramp"
     :config '(:host "box" :method "ssh"))
    (let* ((context
            (remote-context-create
             :target-id "box"
             :localname "/home/me/link"
             :workspace-root "/fs:box:/home/me/"))
           (route
            (remote-resolve "emacs-file" 'file-write context)))
      (should
       (equal
        (remote-fs--translate-args
         'make-symbolic-link
         '("/fs:box:/home/me/target"
           "/fs:box:/home/me/link")
         route)
        '("/home/me/target"
          "/ssh:box:/home/me/link")))
      (should-error
       (remote-fs--translate-args
        'make-symbolic-link
        '("/fs:local:/tmp/target"
          "/fs:box:/home/me/link")
        route)))))

(ert-deftest remote-direnv-skips-tramp-connection-buffer-prefix ()
  (remote-test-with-registry
    (let ((default-directory "/ssh:box:")
          buffer-file-name)
      (should (direnv--transport-connection-path-p default-directory))
      (should-not (direnv--directory))
      (should-not (direnv--envrc-root)))
    (with-temp-buffer
      (rename-buffer "*tramp/ssh box*" t)
      (setq default-directory "/ssh:box:/home/me/")
      (should (direnv--transport-connection-path-p default-directory))
      (should-not (direnv--directory)))))

(ert-deftest remote-direnv-defers-all-discovery-while-tramp-is-busy ()
  (remote-test-with-registry
    (with-temp-buffer
      (let ((tramp-current-connection '(busy))
            scheduled)
        (cl-letf
            (((symbol-function 'direnv--envrc-root)
              (lambda (&rest _)
                (ert-fail "Busy refresh must not inspect .envrc")))
             ((symbol-function 'direnv--schedule-buffer-refresh)
              (lambda (buffer delay)
                (setq scheduled (list buffer delay)))))
          (direnv--refresh-buffer (current-buffer))
          (should (equal scheduled
                         (list (current-buffer)
                               direnv-transport-busy-retry-delay))))))))

(ert-deftest remote-direnv-contains-discovery-errors-inside-timer ()
  (remote-test-with-registry
    (with-temp-buffer
      (let (logged)
        (cl-letf
            (((symbol-function 'direnv--envrc-root)
              (lambda (&rest _)
                (signal 'remote-file-error '("stale connection"))))
             ((symbol-function 'remote-log)
              (lambda (&rest event) (setq logged event))))
          (direnv--refresh-buffer (current-buffer))
          (should (eq (car logged) 'direnv-error))
          (should (string-match-p
                   "stale connection"
                   (plist-get (cdr logged) :error))))))))

(ert-deftest remote-route-prefers-plugin-and-falls-back-on-health ()
  (remote-test-with-registry
    (remote-register-link-plugin
     "slow" :capabilities '(file-read)
     :project-file-name (lambda (file _link _route) file))
    (remote-register-link-plugin
     "fast" :capabilities '(file-read)
     :project-file-name (lambda (file _link _route) file))
    (remote-register-target
     "lab" :preferences '((default . ("fast" "slow"))) :trusted t)
    (remote-register-link "lab" "primary" "fast" :priority 1)
    (remote-register-link "lab" "fallback" "slow" :priority 100)
    (remote-register-adapter "test" :capabilities '(file-read))
    (let* ((context (remote-context-create
                     :target-id "lab" :localname "/work/a"
                     :workspace-root "/fs:lab:/work/"))
           (route (remote-resolve "test" 'file-read context)))
      (should (equal (remote-route-link-plugin-id route) "fast"))
      (remote-report-route-failure
       route '(file-error "Connection refused"))
      (should
       (equal
        (remote-route-link-plugin-id
         (remote-resolve "test" 'file-read context))
        "slow")))))

(ert-deftest remote-one-link-can-offer-multiple-backend-plugins ()
  (remote-test-with-registry
    (remote-register-link-plugin
     "slow" :capabilities '(file-read)
     :project-file-name (lambda (file _link _route) file))
    (remote-register-link-plugin
     "fast" :capabilities '(file-read)
     :project-file-name (lambda (file _link _route) file))
    (remote-register-target "lab" :trusted t)
    (remote-register-link "lab" "ssh" "slow" :priority 10)
    (remote-register-link "lab" "ssh" "fast" :priority 10)
    (remote-register-adapter
     "test" :capabilities '(file-read)
     :preferences '((default . ("fast" "slow"))))
    (let* ((context
            (remote-context-create
             :target-id "lab" :localname "/work/a"
             :workspace-root "/fs:lab:/work/"))
           (routes (remote-routes "test" 'file-read context)))
      (should (= (length (remote-links-for-target "lab")) 1))
      (should (= (length routes) 2))
      (should
       (equal (mapcar #'remote-route-link-id routes)
              '("lab/ssh" "lab/ssh")))
      (should
       (equal (remote-route-link-plugin-id (car routes)) "fast")))))

(ert-deftest remote-backend-incompatibility-falls-back-on-the-same-link ()
  (remote-test-with-registry
    (dolist (plugin '("tramp-rpc" "tramp"))
      (remote-register-link-plugin
       plugin
       :capabilities '(process-sync)
       :project-file-name
       (lambda (_file _link _route) temporary-file-directory)))
    (remote-register-target "pi" :trusted t)
    (remote-register-link
     "pi" "ssh" '("tramp-rpc" "tramp") :priority 10)
    (remote-register-adapter
     "test" :capabilities '(process-sync)
     :preferences '((default . ("tramp-rpc" "tramp"))))
    (let* ((context
            (remote-context-create
             :target-id "pi" :localname "/home/hc/a"
             :workspace-root "/fs:pi:/home/hc/"))
           (remote-environment-inhibit t)
           attempts result)
      (setq result
            (remote--call-with-process-route
             "test" 'process-sync context nil
             (lambda (route _directory _environment)
               (push (remote-route-link-plugin-id route) attempts)
               (if (equal (remote-route-link-plugin-id route)
                          "tramp-rpc")
                   (signal
                    'remote-file-error
                    '("Unknown architecture armv7l-linux"))
                 'fallback-ok))))
      (should (eq result 'fallback-ok))
      (should (equal (nreverse attempts) '("tramp-rpc" "tramp")))
      (let* ((route
              (car (remote-routes "test" 'process-sync context)))
             (backend-health
              (remote-route-backend-health
               (remote-route-create
                :target-id "pi"
                :link-id "pi/ssh"
                :link-plugin-id "tramp-rpc"
                :capability 'process-sync
                :adapter-id "test"))))
        (should (equal (remote-route-link-plugin-id route) "tramp"))
        (should
         (eq (plist-get backend-health :status) 'incompatible))))))

(ert-deftest remote-connection-failure-cools-the-whole-pipeline ()
  (remote-test-with-registry
    (let ((attempts 0))
      (dolist (plugin '("one" "two"))
        (remote-register-link-plugin
         plugin
         :capabilities '(process-sync)
         :project-file-name
         (lambda (_file _link _route) "/unreachable/")
         :connect
         (lambda (_route _context)
           (cl-incf attempts)
           (signal 'file-error '("Connection refused")))))
      (remote-register-target "offline" :trusted t)
      (remote-register-link
       "offline" "ssh" '("one" "two"))
      (remote-register-adapter
       "test" :capabilities '(process-sync)
       :preferences '((default . ("one" "two"))))
      (let ((context
             (remote-context-create
              :target-id "offline" :localname "/work/a"
              :workspace-root "/fs:offline:/work/"))
            (remote-environment-inhibit t))
        (should-error
         (remote--call-with-process-route
          "test" 'process-sync context nil
          (lambda (&rest _)
            (ert-fail "A failed connection must not run the operation")))
         :type 'file-error)
        ;; Both backends use the same physical pipeline.  A transport failure
        ;; must not retry that endpoint under another backend name.
        (should (= attempts 1))
        (should
         (eq
          (plist-get
           (remote-link-health
            (remote-get-link "offline/ssh") 'process-sync)
           :status)
          'failed))))))

(ert-deftest remote-tramp-rpc-maps-published-armv7-release ()
  (should
   (equal
    (remote-backend-tramp-rpc--arch-to-rust-target-a
     #'identity "armv7l-linux")
    "armv7-unknown-linux-musleabihf")))

(ert-deftest remote-tramp-ssh-options-preserve-argv-boundaries ()
  (should
   (equal
    (remote-backend-tramp--ssh-raw-args
     '("ConnectTimeout=8" "ConnectionAttempts=1"))
    '("-o" "ConnectTimeout=8" "-o" "ConnectionAttempts=1"))))

(ert-deftest remote-tramp-rpc-local-relays-use-a-local-directory ()
  (let ((default-directory "/rpc:box:/work/")
        seen-directory)
    (cl-letf
        (((symbol-function 'start-process)
          (lambda (&rest _arguments)
            (setq seen-directory default-directory)
            'relay)))
      (should
       (eq
        (remote-backend-tramp-rpc--local-relay-cwd-a
         (lambda () (start-process "relay" nil "cat")))
        'relay)))
    (should (equal seen-directory temporary-file-directory))))

(ert-deftest remote-tramp-rpc-encodes-large-process-environments ()
  (skip-unless (require 'msgpack nil t))
  (remote-backend-tramp-rpc-install)
  (let* ((environment
          (cl-loop for index below 78
                   collect (cons (format "REMOTE_TEST_%02d" index)
                                 (format "value-%02d" index))))
         (encoded (msgpack-encode environment))
         (msgpack-map-type 'alist)
         (msgpack-key-type 'string)
         (decoded (msgpack-read-from-string encoded)))
    (should (= (length decoded) 78))
    (should (equal (cdr (assoc "REMOTE_TEST_00" decoded)) "value-00"))
    (should (equal (cdr (assoc "REMOTE_TEST_77" decoded)) "value-77"))))

(ert-deftest remote-project-file-name-accepts-explicit-link ()
  (remote-test-with-registry
    (remote-register-link-plugin
     "prefix"
     :capabilities '(file-read)
     :project-file-name
     (lambda (file link _route)
       (concat (plist-get (remote-link-config link) :prefix)
               (remote-file-local-name file))))
    (remote-register-target "box" :trusted t)
    (remote-register-link
     "box" "one" "prefix" :config '(:prefix "/transport"))
    (should
     (equal
      (remote-project-file-name "/fs:box:/work/a" "one")
      "/transport/work/a"))))

(ert-deftest remote-local-visit-keeps-logical-buffer-identity ()
  (remote-test-with-registry
    (let* ((native (make-temp-file "remote-visit-" nil ".txt" "hello"))
           (logical (remote-canonicalize-file-name native))
           buffer)
      (unwind-protect
          (progn
            (setq buffer (find-file-noselect logical))
            (with-current-buffer buffer
              (should (equal buffer-file-name logical))
              (should (remote-fs-file-name-p buffer-file-truename))
              (should (equal (buffer-string) "hello"))
              (should (file-exists-p buffer-file-name))
              (should (verify-visited-file-modtime buffer))
              (set-visited-file-modtime)
              (should (verify-visited-file-modtime buffer))
              (should (stringp (make-auto-save-file-name)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when (file-exists-p native) (delete-file native))))))

(ert-deftest remote-ordinary-local-visit-keeps-native-buffer-identity ()
  (let ((file (make-temp-file "remote-native-visit-" nil ".el"))
        buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should-not (remote-fs-file-name-p buffer-file-name))
            (should-not (file-remote-p buffer-file-name))
            (should
             (equal
              (expand-file-name buffer-file-name)
              (expand-file-name file)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest remote-native-process-status-does-not-reenter-fs-handler ()
  (remote-test-with-registry
    (let* ((buffer (generate-new-buffer " *remote-native-process*"))
           (process (make-pipe-process
                     :name "remote-native-process"
                     :buffer buffer
                     :noquery t)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq default-directory
                    (remote-canonicalize-file-name
                     temporary-file-directory)))
            (let ((tramp-file-name-for-operation-external
                   (cons '(process-status . process)
                         tramp-file-name-for-operation-external)))
              (should
               (eq (remote-fs-file-name-handler
                    'process-status process)
                   'open))))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest remote-mock-target-routes-file-operations ()
  (remote-test-with-registry
    (let* ((root (make-temp-file "remote-target-" t))
           (native (expand-file-name "note.txt" root))
           (logical "/fs:mock:/workspace/note.txt"))
      (unwind-protect
          (progn
            (with-temp-file native (insert "routed"))
            (remote-register-link-plugin
             "mock"
             :capabilities remote-capabilities
             :project-file-name
             (lambda (file _link _route)
               (expand-file-name
                (string-remove-prefix
                 "/workspace/" (remote-file-local-name file))
                root)))
            (remote-register-target "mock" :trusted t)
            (remote-register-link "mock" "only" "mock")
            (should (file-exists-p logical))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents logical)
                (buffer-string))
              "routed"))
            (should (equal (file-remote-p logical 'host) "mock"))
            (should (equal (file-local-name logical)
                           "/workspace/note.txt")))
        (when (file-exists-p native) (delete-file native))
        (when (file-directory-p root) (delete-directory root))))))

(ert-deftest remote-fs-install-restores-the-outer-tramp-dispatcher ()
  (remote-test-with-registry
    ;; Reproduce daemon/reload startup with TRAMP already loaded but its
    ;; top-level file-name handler temporarily removed.
    (let ((file-name-handler-alist nil))
      (remote-fs-install)
      (should
       (eq (find-file-name-handler
            "/fs:local:/tmp/" 'file-directory-p)
           #'tramp-file-name-handler))
      (should (file-directory-p "/fs:local:/tmp/")))))

(ert-deftest remote-process-and-executable-use-logical-context ()
  (remote-test-with-registry
    (let ((default-directory
           (remote-canonicalize-file-name temporary-file-directory)))
      (with-temp-buffer
        (should
         (zerop
          (remote-process-file
           "sh" nil t nil "-c" "printf routed")))
        (should (equal (buffer-string) "routed")))
      (should (file-name-absolute-p
               (remote-executable-find "sh"))))))

(ert-deftest remote-exec-returns-structured-route-result ()
  (remote-test-with-registry
    (let* ((default-directory
            (remote-canonicalize-file-name temporary-file-directory))
           (result
            (remote-exec
             "sh" :args '("-c" "printf stdout; printf stderr >&2")
             :check t)))
      (should (zerop (remote-exec-result-status result)))
      (should (equal (remote-exec-result-stdout result) "stdout"))
      (should (equal (remote-exec-result-stderr result) "stderr"))
      (should
       (equal (remote-route-link-plugin-id
               (remote-exec-result-route result))
              "native")))))

(ert-deftest remote-exec-async-uses-routed-make-process ()
  (remote-test-with-registry
    (let* ((default-directory
            (remote-canonicalize-file-name temporary-file-directory))
           result
           (process
            (remote-exec-async
             "sh"
             :args '("-c" "printf async; printf problem >&2")
             :callback (lambda (value) (setq result value)))))
      (while (and (process-live-p process) (not result))
        (accept-process-output process 0.1))
      (unless result
        (accept-process-output process 0.1))
      (should (remote-exec-result-p result))
      (should (zerop (remote-exec-result-status result)))
      (should (equal (remote-exec-result-stdout result) "async"))
      (should (equal (remote-exec-result-stderr result) "problem"))
      (should
       (equal
        (remote-route-link-plugin-id
        (remote-exec-result-route result))
        "native")))))

(ert-deftest remote-tramp-stderr-frame-roundtrips-streams ()
  (let* ((token "frame-token")
         (combined (concat "stdout\036" token "\037stderr"))
         (streams (remote--split-stderr-frame combined token)))
    (should (equal (car streams) "stdout"))
    (should (equal (cdr streams) "stderr"))
    (should
     (equal (remote--split-stderr-frame "partial output" token)
            '("partial output" . "")))))

(ert-deftest remote-standard-tramp-process-uses-direct-ssh-stderr ()
  (let* ((route
          (remote-route-create
           :target-id "box"
           :pipeline-id "box/ssh"
           :backend-id "tramp"
           :capability 'process-async
           :adapter-id "exec"))
         (context
          (remote-context-create
           :target-id "box"
           :localname "/tmp/"
           :workspace-root "/fs:box:/tmp/"))
         (process
          (make-pipe-process
           :name "remote-test-tramp-stderr" :noquery t))
         (stdout (generate-new-buffer " *remote-test-stdout*"))
         (stderr (generate-new-buffer " *remote-test-stderr*"))
         captured)
    (unwind-protect
        (cl-letf
            (((symbol-function 'remote--call-with-process-route)
              (lambda (_adapter _capability _context _constraints function)
                (funcall function route temporary-file-directory nil)))
             ((symbol-function 'remote--prepare-backend-execution)
             (lambda (_route _context command _environment directory
                              &optional _logical-directory)
                (remote-backend-execution-create
                 :physical-directory directory
                 :command command)))
             ((symbol-function
              'remote-backend-tramp-direct-async-command)
              (lambda (_route command _environment directory)
                (should (equal directory "/tmp/"))
                (append '("/usr/bin/ssh" "-T" "box") command)))
             ((symbol-function 'make-process)
              (lambda (&rest arguments)
                (setq captured arguments)
                process)))
          (remote-make-process
           :name "remote-test-tramp-stderr"
           :buffer stdout
           :stderr stderr
           :command '("sh" "-c" "printf output")
           :remote-context context
           :remote-stderr-token "token")
          (should (eq (plist-get captured :stderr) stderr))
          (should
           (equal (car (plist-get captured :command)) "/usr/bin/ssh"))
          (should-not (process-get process 'remote-stderr-token))
          (should (process-get process 'remote-direct-ssh)))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p stdout)
        (kill-buffer stdout))
      (when (buffer-live-p stderr)
        (kill-buffer stderr)))))

(ert-deftest remote-direnv-starting-sentinel-coalesces-nested-refresh ()
  (let ((direnv--export-processes (make-hash-table :test #'equal))
        (direnv--export-waiters (make-hash-table :test #'equal))
        (root "/fs:local:/tmp/project/")
        (context
         (remote-context-create
          :target-id "local"
          :localname "/tmp/project/"
          :workspace-root "/fs:local:/tmp/project/"))
        (calls 0))
    (cl-letf (((symbol-function 'remote-exec-async)
               (lambda (_program &rest _options)
                 (cl-incf calls)
                 (direnv--start-export
                  context root '(fingerprint) (current-buffer))
                 'mock-process)))
      (direnv--start-export
       context root '(fingerprint) (current-buffer))
      (should (= calls 1))
      (should (eq (gethash root direnv--export-processes)
                  'mock-process)))))

(ert-deftest remote-direnv-detects-locked-tramp-connection ()
  (let ((process
         (make-pipe-process
          :name "remote-test-locked-tramp" :noquery t)))
    (unwind-protect
        (progn
          (process-put process 'tramp-vector 'mock-vector)
          (cl-letf (((symbol-function 'tramp-get-connection-property)
                     (lambda (candidate property &optional _default)
                       (and (eq candidate process)
                            (equal property "locked")))))
            (should (direnv--transport-busy-p))))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest remote-internal-process-buffer-skips-user-kill-hooks ()
  (let ((buffer
         (generate-new-buffer " *remote-internal-process-cleanup*"))
        (hook-ran nil))
    (with-current-buffer buffer
      (add-hook
       'kill-buffer-hook
       (lambda ()
         (setq hook-ran t)
         ;; Reproduce global cleanup integrations which assume a file buffer.
         (file-name-directory buffer-file-name))
       nil t))
    (should (remote--kill-internal-process-buffer buffer))
    (should-not (buffer-live-p buffer))
    (should-not hook-ran)))

(ert-deftest remote-official-make-process-is-decorated-at-fs-boundary ()
  (remote-test-with-registry
    (let* ((default-directory
            (remote-canonicalize-file-name temporary-file-directory))
           (buffer (generate-new-buffer " *remote-official-process*"))
           (process
            (make-process
             :name "remote-official-process"
             :buffer buffer
             :command '("sh" "-c" "printf official")
             :file-handler t
             :noquery t)))
      (unwind-protect
          (progn
            (while (process-live-p process)
              (accept-process-output process 0.1))
            (with-current-buffer buffer
              (should (string-prefix-p "official" (buffer-string))))
            (should
             (equal
              (remote-route-link-plugin-id
               (process-get process 'remote-route))
              "native"))
            (should
             (equal
              (remote-backend-execution-backend-id
               (process-get process 'remote-backend-execution))
              "native")))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest remote-process-boundary-reenables-the-physical-tramp-handler ()
  (let ((inhibit-file-name-handlers
         '(tramp-file-name-handler another-handler))
        (inhibit-file-name-operation 'make-process)
        seen)
    (cl-letf
        (((symbol-function 'remote-make-process)
          (lambda (&rest _)
            (setq seen
                  (list inhibit-file-name-operation
                        (copy-sequence inhibit-file-name-handlers)))
            'mock-process)))
      (should
       (eq
        (remote-fs-handle-make-process
         :name "mock" :command '("mock"))
        'mock-process)))
    (should-not (car seen))
    (should-not (memq #'tramp-file-name-handler (cadr seen)))
    (should (memq 'another-handler (cadr seen)))))

(ert-deftest remote-exec-projects-route-into-its-output-buffer ()
  (remote-test-with-registry
    (remote-register-link-plugin
     "mock-process"
     :capabilities '(process-sync)
     :project-file-name
     (lambda (_file _link _route) "/mock-physical/"))
    (remote-register-target "mock" :trusted t)
    (remote-register-link "mock" "only" "mock-process")
    (remote-register-adapter
     "mock-exec" :capabilities '(process-sync)
     :preferences '((default . ("mock-process"))))
    (let ((context
           (remote-context-create
            :target-id "mock"
            :localname "/workspace/a"
            :workspace-root "/fs:mock:/workspace/"))
          seen-directory)
      (cl-letf (((symbol-function 'process-file)
                 (lambda (&rest _args)
                   (setq seen-directory default-directory)
                   (insert "mock")
                   0)))
        (let ((result
               (remote-exec
                "demo" :context context :adapter "mock-exec")))
          (should (zerop (remote-exec-result-status result)))
          (should (equal (remote-exec-result-stdout result) "mock"))
          (should (equal seen-directory "/mock-physical/")))))))

(ert-deftest remote-path-probe-discovers-real-target-facts ()
  (remote-test-with-registry
    (let* ((default-directory
            (remote-canonicalize-file-name temporary-file-directory))
           (remote-path-facts-cache (make-hash-table :test #'equal))
           (facts (remote-path-probe nil t)))
      (should (equal (remote-path-facts-target-id facts) "local"))
      (should (stringp (remote-path-facts-system facts)))
      (should (consp (remote-path-facts-path facts)))
      (should
       (equal (car (remote-path-candidates))
              (car (remote-path-facts-path facts)))))))

(ert-deftest remote-path-probe-ignores-remote-login-banner ()
  (should
   (equal
    (remote-path--parse-probe-output
     (concat
      "Wi-Fi is currently blocked by rfkill.\n"
      remote-path--probe-marker
      "Linux\0armv7l\0/bin/bash\0/home/hc\0/usr/bin:/bin\0"))
    '("Linux" "armv7l" "/bin/bash" "/home/hc" "/usr/bin:/bin"))))

(ert-deftest remote-environment-is-buffer-local-and-target-native ()
  (remote-test-with-registry
    (remote-register-environment-provider
     "test"
     :priority 10
     :predicate (lambda (_context) t)
     :fingerprint (lambda (_context) 1)
     :load (lambda (_context)
             '(("PATH" . "/target/bin:/usr/bin")
               ("REMOTE_TEST" . "yes"))))
    (with-temp-buffer
      (setq default-directory
            (remote-canonicalize-file-name temporary-file-directory))
      (let ((environment (remote-environment-ensure)))
        (should (remote-environment-p environment))
        (should (string-prefix-p "local@" (remote-environment-id environment)))
        (should (equal (getenv "REMOTE_TEST") "yes"))
        (should (equal (car exec-path) "/target/bin"))))))

(ert-deftest remote-environment-resolve-does-not-mutate-caller-buffer ()
  (remote-test-with-registry
    (remote-register-environment-provider
     "resolve-only"
     :priority 10
     :predicate (lambda (_context) t)
     :fingerprint (lambda (_context) 1)
     :load (lambda (_context)
             '(("REMOTE_RESOLVE_ONLY" . "yes"))))
    (with-temp-buffer
      (setq default-directory
            (remote-canonicalize-file-name temporary-file-directory))
      (let ((before (copy-sequence process-environment))
            (environment (remote-environment-resolve)))
        (should
         (equal
          (cdr
           (assoc-string
            "REMOTE_RESOLVE_ONLY"
            (remote-environment-vars environment)
            t))
          "yes"))
        (should (equal process-environment before))
        (should-not (local-variable-p 'process-environment))
        (should-not remote-buffer-environment)))))

(ert-deftest remote-path-layers-inherit-and-decorate-without-mutation ()
  (remote-test-with-registry
    (remote-register-target
     "local"
     :trusted t
     :environment
     '((providers "base" "workspace")
       (path (prepend "/target/bin"))))
    (remote-register-environment-provider
     "base" :scope 'host :priority 0
     :load
     (lambda (_context)
       '(:vars (("BASE" . "yes"))
         :path ("/base/bin" "/usr/bin")
         :path-mode replace)))
    (remote-register-environment-provider
     "workspace" :scope 'workspace :priority 10
     :load
     (lambda (_context)
       '(:vars (("WORKSPACE" . "yes"))
         :path ("/workspace/bin" "/target/bin" "/usr/bin")
         :path-mode replace)))
    (with-temp-buffer
      (setq default-directory
            (remote-canonicalize-file-name temporary-file-directory))
      (let* ((base (remote-environment-ensure))
             (toolchain
              (remote-environment-derive
               base "lean"
               :scope 'toolchain
               :path-prepend '("/lake/bin")))
             (invocation
              (remote-path-decorate
               toolchain "test-run"
               :path-remove '("/usr/bin")
               :path-append '("/test/bin"))))
        (should
         (equal
          (remote-path-state-resolved
           (remote-environment-path-state base))
          '("/workspace/bin" "/target/bin" "/usr/bin")))
        (should
         (equal
          (remote-path-state-resolved
           (remote-environment-path-state invocation))
          '("/lake/bin" "/workspace/bin" "/target/bin" "/test/bin")))
        (should
         (equal (remote-environment-parent-id invocation)
                (remote-environment-id toolchain)))
        (should (eq (remote-get-environment
                     (remote-environment-id invocation))
                    invocation))
        (should
         (equal
          (remote-path-state-resolved
           (remote-environment-path-state base))
          '("/workspace/bin" "/target/bin" "/usr/bin")))))))

(ert-deftest remote-environment-id-is-workspace-scoped-not-link-scoped ()
  (remote-test-with-registry
    (remote-register-target
     "lab"
     :trusted t
     :workspaces
     '(((id . "one") (path . "/work/one"))
       ((id . "two") (path . "/work/two")))
     :environment '((providers "workspace")))
    (remote-register-environment-provider
     "workspace"
     :scope 'workspace
     :load
     (lambda (context)
       (list
        :vars
        (list
         (cons "WORKSPACE"
               (remote-context-workspace-id context)))
        :path
        (list
         (concat
          (remote-file-local-name
           (remote-context-workspace-root context))
          "bin"))
        :path-mode 'replace)))
    (let* ((one-context (remote-context "/fs:lab:/work/one/a"))
           (two-context (remote-context "/fs:lab:/work/two/b"))
           (one
            (with-temp-buffer
              (remote-environment-ensure one-context)))
           (two
            (with-temp-buffer
              (remote-environment-ensure two-context))))
      (should (equal (remote-environment-id one) "lab@one"))
      (should (equal (remote-environment-id two) "lab@two"))
      (should-not (eq one two))
      (should
       (equal
        (remote-path-state-resolved
         (remote-environment-path-state one))
        '("/work/one/bin")))
      (should
       (equal
        (remote-path-state-resolved
         (remote-environment-path-state two))
        '("/work/two/bin"))))))

(ert-deftest remote-direnv-adapter-prefers-rpc ()
  (let ((preferences
         (remote-adapter-preferences
          (remote-get-adapter "direnv"))))
    (should
     (equal (cdr (assq 'default preferences))
            '("tramp-rpc" "tramp" "native")))))

(ert-deftest remote-direnv-export-waiters-keep-process-boundary-callbacks ()
  (let ((direnv--export-waiters (make-hash-table :test #'equal))
        (root "/fs:local:/tmp/project/")
        (context 'context)
        (environment 'environment)
        (events nil)
        (buffer (generate-new-buffer " *remote-direnv-waiter*")))
    (unwind-protect
        (progn
          ;; Eglot may register first and the automatic file/window refresh may
          ;; enqueue the same buffer immediately afterwards.  The latter must
          ;; not erase the callback which resumes process startup.
          (direnv--queue-export-waiter
           root buffer
           (lambda (result error)
             (push (list 'eglot result error) events)))
          (direnv--queue-export-waiter root buffer nil)
          ;; Independent process boundaries in one buffer must both resume.
          (direnv--queue-export-waiter
           root buffer
           (lambda (result error)
             (push (list 'task result error) events)))
          (cl-letf (((symbol-function 'remote-environment-ensure)
                     (lambda (seen-context &optional _force)
                       (should (eq seen-context context))
                       environment)))
            (direnv--apply-export-waiters root context nil))
          (should
           (equal
            (sort events
                  (lambda (left right)
                    (string< (symbol-name (car left))
                             (symbol-name (car right)))))
            '((eglot environment nil)
              (task environment nil))))
          (should-not (gethash root direnv--export-waiters)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest remote-direnv-lifecycle-coalesces-enter-and-reports-leave ()
  (let ((direnv--reported-selection nil)
        (messages nil)
        (root "/fs:local:/tmp/project/")
        (environment
         (remote-environment-create
          :id "local@project"
          :key '(local project)
          :target-id "local"
          :workspace-id "project"
          :workspace-root "/fs:local:/tmp/project/"
          :vars '(("DEMO" . "yes"))
          :sources
          '((host-path "native")
            (direnv "/fs:local:/tmp/project/" "native" "local/native")))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'direnv--selected-buffer-p)
                 (lambda () t))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (direnv--announce-environment environment)
        (direnv--announce-environment environment)
        (should (equal direnv--active-root root))
        (should (= (length messages) 1))
        (should (string-match-p "direnv: entered" (car messages)))
        (direnv-clear-environment)
        (should-not direnv--active-root)
        (should-not direnv--reported-selection)
        (should (= (length messages) 2))
        (should (string-match-p
                 "direnv: left"
                 (car messages)))))))

(ert-deftest remote-direnv-refresh-clears-buffer-outside-envrc-tree ()
  (let ((clears 0))
    (with-temp-buffer
      (cl-letf (((symbol-function 'direnv--transport-connection-path-p)
                 (lambda (_path) nil))
                ((symbol-function 'direnv--transport-busy-p)
                 (lambda () nil))
                ((symbol-function 'direnv--envrc-root)
                 (lambda (&optional _path) nil))
                ((symbol-function 'direnv-clear-environment)
                 (lambda () (cl-incf clears))))
        (direnv--refresh-buffer (current-buffer))
        (should (= clears 1))))))

(ert-deftest remote-direnv-export-uses-routed-process-api ()
  (let* ((root (make-temp-file "remote-direnv-" t))
         (envrc (expand-file-name ".envrc" root))
         (default-directory
          (remote-canonicalize-file-name
           (file-name-as-directory root)))
         seen-adapter)
    (unwind-protect
        (progn
          (with-temp-file envrc (insert "export DEMO=yes\n"))
          (cl-letf
              (((symbol-function 'remote-executable-find)
                (lambda (_program _context) "/usr/bin/direnv"))
               ((symbol-function 'remote-exec)
                (lambda (_program &rest options)
                  (setq seen-adapter (plist-get options :adapter))
                  (remote-exec-result-create
                   :status 0
                   :stdout
                   "{\"PATH\":\"/remote/bin:/usr/bin\",\"DEMO\":\"yes\"}"
                   :stderr ""))))
            (let* ((result (direnv--export (remote-context)))
                   (vars (plist-get result :vars)))
              (should (equal seen-adapter "direnv"))
              (should (equal (cdr (assoc "DEMO" vars)) "yes"))
              (should
               (equal (cdr (assoc "PATH" vars))
                      "/remote/bin:/usr/bin")))))
      (when (file-exists-p envrc) (delete-file envrc))
      (when (file-directory-p root) (delete-directory root)))))

(provide 'remote-tests)
;;; remote-tests.el ends here
