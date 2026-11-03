;;; agent-remote-performance-tests.el --- ACP placement and hot paths -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tools/agent/config)
(require 'tools/agent/acp)
(require 'tools/agent/activity)
(require 'tools/agent/api)
(unless (featurep 'agent-flow-e2e)
  (load (expand-file-name "test/agent-flow-e2e.el" user-emacs-directory) nil t))

(defmacro agent-remote-test-with-title-client (&rest body)
  "Run BODY with a live synthetic client, capturing title QUERIES."
  (declare (indent 0))
  `(with-temp-buffer
     (setq major-mode 'agent-shell-mode default-directory temporary-file-directory)
     (let* ((process (make-pipe-process :name "agent-title-test" :noquery t))
            (client (list (cons :process process)))
            queries)
       (setq-local agent-shell--state (agent-shell--make-state :buffer (current-buffer)))
       (setf (map-elt agent-shell--state :client) client
             (map-elt agent-shell--state :supports-session-list) t
             (map-elt agent-shell--state :init-finished) t
             (map-elt (map-elt agent-shell--state :session) :id) "session")
       (unwind-protect
           (cl-letf (((symbol-function 'acp-send-request)
                      (lambda (&rest args) (setq queries (append queries (list args))))))
             ,@body)
         (delete-process process)))))

(defun agent-remote-test-title-response (title &optional id)
  "Return a native title response containing TITLE for ID."
  (list (cons 'sessions
              (vector (list (cons 'sessionId (or id "session")) (cons 'title title))))))

(defun agent-remote-test-project-session (name root &optional origin)
  "Create a registry fixture NAME at ROOT without starting a provider."
  (let ((buffer (generate-new-buffer (concat " *agent-project-" name "*"))))
    (with-current-buffer buffer
      (setq major-mode 'agent-shell-mode)
      (setq-local default-directory root +agent-acp-session-root root
                  +agent-acp-session-name name +agent-acp-session-origin origin
                  agent-shell--state (agent-shell--make-state :buffer buffer)))
    buffer))

(ert-deftest agent-remote-c45-exact-status-reads-only-the-identified-session ()
  (let* ((wanted (agent-remote-test-project-session "api-wanted" "/tmp/agent-api-a/"))
         (other (agent-remote-test-project-session "api-other" "/tmp/agent-api-b/"))
         (private (agent-remote-test-project-session "api-private" "/tmp/agent-api-a/" 'private))
         (ordinary (generate-new-buffer " *agent-api-ordinary*"))
         (buffers (list wanted other private ordinary))
         (+agent-session-private-functions nil)
         (+agent-session-private-buffer-functions
          (list (lambda (buffer) (eq (buffer-local-value '+agent-acp-session-origin buffer) 'private))))
         (original (symbol-function '+agent-session-snapshot)) seen)
    (unwind-protect
        (progn
          (dolist (buffer (list wanted other private))
            (with-current-buffer buffer
              (setf (map-elt (map-elt agent-shell--state :session) :id) "shared-native-id")))
          (cl-letf (((symbol-function '+agent-session-snapshot)
                     (lambda (buffer) (push buffer seen) (funcall original buffer))))
            (let ((reply (+agent/api `((action . "status") (buffer . ,(buffer-name wanted))
                                      (nativeSessionId . "shared-native-id")))))
              (should (equal seen (list wanted)))
              (should (equal (alist-get 'buffer (alist-get 'session reply)) (buffer-name wanted))))
            (with-temp-buffer
              (setq default-directory "/fs:api-caller:/tmp/agent-api-a/")
              (should (alist-get 'ok (+agent/api `((action . "status") (buffer . ,(buffer-name wanted))
                                                  (nativeSessionId . "shared-native-id")
                                                  (root . "/tmp/agent-api-a/"))))))
            (should-error (+agent/api `((action . "status") (buffer . ,(buffer-name private))
                                       (nativeSessionId . "shared-native-id"))) :type 'user-error)
            (setq seen nil)
            (should-error (+agent/api `((action . "status") (buffer . ,(buffer-name ordinary))
                                       (nativeSessionId . "shared-native-id"))) :type 'user-error)
            (should-not seen)
            (with-current-buffer wanted
              (setf (map-elt (map-elt agent-shell--state :session) :id) "replacement-id"))
            (cl-letf (((symbol-function '+agent-acp-stop)
                       (lambda (_) (ert-fail "A stale identity stopped the replacement session"))))
              (should-error (+agent/api `((action . "stop") (buffer . ,(buffer-name wanted))
                                         (nativeSessionId . "shared-native-id"))) :type 'user-error))))
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-remote-c45-list-filters-project-target-and-privacy-before-paging ()
  (let* ((root "/tmp/agent-api-page/")
         (a (agent-remote-test-project-session "api-page-a" root))
         (b (agent-remote-test-project-session "api-page-b" "/fs:local:/tmp/agent-api-page/" 'pi-project))
         (c (agent-remote-test-project-session "api-page-c" root))
         (private (agent-remote-test-project-session "api-page-private" root 'private))
         (other (agent-remote-test-project-session "api-page-other" "/tmp/agent-api-other/"))
         (foreign (agent-remote-test-project-session "api-page-foreign" "/fs:api-page-target:/tmp/agent-api-page/"))
         (buffers (list a b c private other foreign))
         (+agent-session-private-functions nil)
         (+agent-session-private-buffer-functions
          (list (lambda (buffer) (eq (buffer-local-value '+agent-acp-session-origin buffer) 'private))))
         (original (symbol-function '+agent-session-snapshot)) seen)
    (unwind-protect
        (let ((expected (+agent-pool-buffers root "local" 'all)))
          (should (= (length expected) 3))
          (cl-letf (((symbol-function '+agent-session-snapshot)
                     (lambda (buffer) (push buffer seen) (funcall original buffer))))
            ;; An explicit client root remains on the client even when the
            ;; request originates from another execution target's buffer.
            (with-current-buffer foreign
              (let ((reply (+agent/api `((action . "list") (root . ,root) (offset . 1) (limit . 1)))))
                (should (= (alist-get 'total reply) 3))
                (should (= (alist-get 'nextOffset reply) 2))
                (should (equal (alist-get 'buffer (aref (alist-get 'sessions reply) 0))
                               (buffer-name (nth 1 expected))))
                (should (equal seen (list (nth 1 expected))))))
            (setq seen nil)
            (let ((reply (+agent/api `((action . "list") (root . ,root) (offset . 3) (limit . 1)))))
              (should (= (alist-get 'total reply) 3))
              (should (= (length (alist-get 'sessions reply)) 0))
              (should-not (alist-get 'nextOffset reply))
              (should-not seen))
            (let ((reply (+agent/api '((action . "list") (target . "api-page-target") (limit . 1)))))
              (should (= (alist-get 'total reply) 1))
              (should (equal seen (list foreign))))))
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-remote-c45-list-preserves-legacy-snapshot-privacy ()
  (let* ((root "/tmp/agent-api-private/")
         (public (agent-remote-test-project-session "api-legacy-public" root))
         (private (agent-remote-test-project-session "api-legacy-private" root))
         (+agent-session-private-buffer-functions nil)
         (+agent-session-private-functions
          (list (lambda (row)
                  (should (plist-member row :usage))
                  (equal (plist-get row :last-prompt) "private last prompt")))))
    (unwind-protect
        (progn
          (with-current-buffer private
            (setq-local comint-input-ring (make-ring 1))
            (ring-insert comint-input-ring "private last prompt"))
          (let ((reply (+agent/api `((action . "list") (root . ,root) (limit . 1)))))
            (should (= (alist-get 'total reply) 1))
            (should (equal (alist-get 'buffer (aref (alist-get 'sessions reply) 0)) (buffer-name public)))))
      (dolist (buffer (list public private)) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-remote-c23-pool-identity-does-not-borrow-the-current-target ()
  (let ((expected (+agent-pool-key '(:target "local" :root "/tmp/project/"))))
    (with-temp-buffer
      (setq default-directory "/fs:elsewhere:/srv/project/")
      (should (equal expected (+agent-pool-key '(:target "local" :root "/tmp/project/")))))
    (should (equal expected (+agent-pool-key '(:target "local" :root "/fs:local:/tmp/project/"))))))

(ert-deftest agent-remote-c23-project-discovery-keeps-an-explicit-client-directory ()
  (let* (seen
         (+agent-project-root-functions
          (list (lambda (directory) (setq seen directory) directory))))
    (with-temp-buffer
      (setq default-directory "/fs:elsewhere:/srv/project/")
      (should (equal (+agent-acp-project-root "/tmp/client-project/") "/tmp/client-project/"))
      (should (equal seen "/tmp/client-project/")))))

(ert-deftest agent-remote-c23-project-query-reads-only-its-member-snapshots ()
  (let* ((a (agent-remote-test-project-session "a" "/tmp/project-a/"))
         (alias (agent-remote-test-project-session "alias" "/fs:local:/tmp/project-a/"))
         (other (agent-remote-test-project-session "other" "/tmp/project-b/"))
         (foreign (agent-remote-test-project-session "foreign" "/fs:other:/tmp/project-a/"))
         (private (agent-remote-test-project-session "private" "/tmp/project-a/" 'private))
         (pi (agent-remote-test-project-session "pi" "/tmp/project-a/" 'pi-project))
         (buffers (list a alias other foreign private pi))
         (+agent-session-private-functions
          (list (lambda (row) (eq (plist-get row :origin) 'private))))
         seen)
    (unwind-protect
        (cl-letf (((symbol-function 'remote-client-file-name)
                   (lambda (path &rest _)
                     (should-not (string-prefix-p "/fs:other:" path))
                     (and (string-prefix-p "/fs:local:" path) (substring path 10))))
                  ((symbol-function '+agent-session-snapshot)
                   (lambda (buffer)
                     (push buffer seen)
                     (list :buffer buffer
                           :origin (buffer-local-value '+agent-acp-session-origin buffer)))))
          (let ((rows (+agent-pool-sessions "/tmp/project-a/" "local")))
            (should (equal (sort (mapcar (lambda (row) (plist-get row :buffer)) rows)
                                (lambda (a b) (string-lessp (buffer-name a) (buffer-name b))))
                           (sort (list a alias) (lambda (a b) (string-lessp (buffer-name a) (buffer-name b)))))))
          (should (= (length seen) 3))
          (should-not (seq-intersection seen (list other foreign pi)))
          (kill-buffer a)
          (setq seen nil)
          (should (= 1 (length (+agent-pool-sessions "/tmp/project-a/" "local"))))
          (should-not (memq a seen)))
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-remote-c23-retention-groups-by-canonical-project-without-snapshots ()
  (let* ((a (agent-remote-test-project-session "warm-a" "/tmp/project/"))
         (old (agent-remote-test-project-session "warm-old" "/fs:local:/tmp/project/"))
         (other (agent-remote-test-project-session "warm-other" "/tmp/other-project/"))
         (foreign (agent-remote-test-project-session "warm-foreign" "/fs:other:/tmp/project/"))
         (buffers (list a old other foreign)) killed)
    (unwind-protect
        (progn
          (dolist (buffer buffers)
            (with-current-buffer buffer (setq +agent-acp-last-used-at (float-time))))
          (with-current-buffer old (cl-decf +agent-acp-last-used-at 10))
          (cl-letf (((symbol-function 'remote-client-file-name)
                     (lambda (path &rest _) (and (string-prefix-p "/fs:local:" path)
                                                 (substring path 10))))
                    ((symbol-function '+agent-acp-resumable-p) (lambda (buffer) (memq buffer buffers)))
                    ((symbol-function '+agent-acp-auto-stoppable-p) (lambda (buffer) (memq buffer buffers)))
                    ((symbol-function '+agent-session-snapshot)
                     (lambda (&rest _) (ert-fail "Full snapshot used for project grouping")))
                    ((symbol-function '+agent-acp-kill) (lambda (buffer) (push buffer killed))))
            (+agent-acp-sweep-warm-buffers 1 1000000)
            (should (equal killed (list old)))))
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-remote-c23-tabs-read-current-members-without-full-snapshots ()
  (require 'noema-agent-acp)
  (let* ((root "/tmp/tab-project/")
         (a (agent-remote-test-project-session "a" root))
         (b (agent-remote-test-project-session "b" root))
         (other (agent-remote-test-project-session "other" "/tmp/other-tab-project/"))
         (foreign (agent-remote-test-project-session "foreign" "/fs:other:/tmp/tab-project/"))
         (private (agent-remote-test-project-session "private" root 'latex-export))
         (pi (agent-remote-test-project-session "pi" root 'pi-project))
         (buffers (list a b other foreign private pi))
         (+agent-session-private-functions nil)
         (+agent-session-private-buffer-functions (list #'noema-agent-export-session-p)))
    (unwind-protect
        (with-current-buffer a
          (cl-letf (((symbol-function '+agent-session-snapshot)
                     (lambda (&rest _) (ert-fail "Tab query built a full session snapshot"))))
            (should (equal (+agent-acp--project-buffers) (list a b)))
            (let ((added (agent-remote-test-project-session "added" root)))
              (push added buffers)
              (should (equal (+agent-acp--project-buffers) (list a added b))))
            (with-current-buffer b
              (setq default-directory "/tmp/moved-tab-project/"
                    +agent-acp-session-root default-directory))
            (should-not (memq b (+agent-acp--project-buffers)))
            (with-current-buffer private (setq +agent-acp-session-origin 'manual))
            (should (memq private (+agent-acp--project-buffers)))
            (kill-buffer private)
            (should-not (memq private (+agent-acp--project-buffers)))))
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-remote-c23-legacy-privacy-still-receives-full-member-snapshots ()
  (let* ((root "/tmp/legacy-private-project/")
         (a (agent-remote-test-project-session "public" root))
         (private (agent-remote-test-project-session "private" root))
         (other (agent-remote-test-project-session "other" "/tmp/legacy-other/"))
         (buffers (list a private other))
         (+agent-session-private-buffer-functions nil)
         (+agent-session-private-functions
          (list (lambda (row)
                  (should (plist-member row :usage))
                  (should (plist-member row :process-state))
                  (equal (plist-get row :last-prompt) "private prompt"))))
         (original (symbol-function '+agent-session-snapshot))
         seen)
    (unwind-protect
        (progn
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (setq-local comint-input-ring (make-ring 1))
              (ring-insert comint-input-ring
                           (concat (if (eq buffer private) "private" "public") " prompt\ndetails"))))
          (cl-letf (((symbol-function '+agent-session-snapshot)
                     (lambda (buffer) (push buffer seen) (funcall original buffer))))
            (should (equal (+agent-pool-buffers root "local") (list a)))
            (should (= (length seen) 2))
            (should-not (memq other seen))
            (setq seen nil)
            (should (equal (mapcar (lambda (row) (plist-get row :buffer))
                                  (+agent-pool-sessions root "local")) (list a)))
            (should (= (length seen) 2))))
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-remote-c23-buffer-privacy-also-applies-to-manager-snapshots ()
  (require 'noema-agent-acp)
  (require 'tools/agent/manager)
  (let* ((root "/tmp/privacy-project/")
         (a (agent-remote-test-project-session "public" root 'manual))
         (export (agent-remote-test-project-session "export" root 'latex-export))
         (legacy (agent-remote-test-project-session "old-export" "/tmp/latex-export--old/" 'side))
         (side (agent-remote-test-project-session "side" root 'side))
         (buffers (list a export legacy side))
         (+agent-session-private-functions nil)
         (+agent-session-private-buffer-functions (list #'noema-agent-export-session-p)))
    (unwind-protect
        (let* ((rows (mapcar #'+agent-session-snapshot buffers))
               (public (+agent-manager--sessions rows 'all nil "")))
          (dolist (buffer buffers)
            (should (eq (and (+agent-session-private-p buffer) t)
                        (and (+agent-session-private-p (+agent-session-snapshot buffer)) t))))
          (should (= (length public) 2))
          (should (memq a (mapcar (lambda (row) (plist-get row :buffer)) public)))
          (should (memq side (mapcar (lambda (row) (plist-get row :buffer)) public)))
          (should (noema-agent-export-session-p (+agent-session-snapshot legacy)))
          (should-not (noema-agent-export-session-p (+agent-session-snapshot side))))
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-remote-c46-title-refreshes-share-only-the-inflight-query ()
  (agent-remote-test-with-title-client
    (dotimes (_ 10) (agent-shell--refresh-session-title))
    (should (= 1 (length queries)))
    (should-not (+agent-acp-busy-p (current-buffer)))
    (funcall (plist-get (car queries) :on-success)
             (agent-remote-test-title-response "Provider title"))
    (should (equal (map-nested-elt agent-shell--state '(:session :title)) "Provider title"))
    (should-not +agent-acp--title-query)
    (agent-shell--refresh-session-title)
    (should (= 2 (length queries)))))

(ert-deftest agent-remote-c46-title-refreshes-fold-newer-turns-into-one-read ()
  (agent-remote-test-with-title-client
    (agent-shell--refresh-session-title)
    (dotimes (_ 10)
      (cl-incf (map-elt agent-shell--state :request-count))
      (agent-shell--refresh-session-title))
    (should (= 1 (length queries)))
    (funcall (plist-get (car queries) :on-success)
             (agent-remote-test-title-response "Old turn"))
    (should-not (map-nested-elt agent-shell--state '(:session :title)))
    (should (= 2 (length queries)))
    (funcall (plist-get (cadr queries) :on-success)
             (agent-remote-test-title-response "Latest turn"))
    (should (equal (map-nested-elt agent-shell--state '(:session :title)) "Latest turn"))
    (should-not +agent-acp--title-query)
    (should (= 2 (length queries)))))

(ert-deftest agent-remote-c15-title-read-cannot-replace-a-newer-notification ()
  (agent-remote-test-with-title-client
    (agent-shell--refresh-session-title)
    (agent-shell--set-session-title "Pushed title")
    (funcall (plist-get (car queries) :on-success)
             (agent-remote-test-title-response "Older query title"))
    (should (equal (map-nested-elt agent-shell--state '(:session :title)) "Pushed title"))
    (should-not +agent-acp--title-query)))

(ert-deftest agent-remote-c15-title-callback-is-bound-to-all-execution-identity ()
  (dolist (changed '(state client process session directory))
    (agent-remote-test-with-title-client
      (agent-shell--refresh-session-title)
      (let ((replacement (make-pipe-process :name "agent-title-replacement" :noquery t)))
        (unwind-protect
            (progn
              (pcase changed
                ('state (setq agent-shell--state (copy-sequence agent-shell--state)))
                ('client (setf (map-elt agent-shell--state :client) (copy-sequence client)))
                ('process (setf (map-elt client :process) replacement))
                ('session (setf (map-elt (map-elt agent-shell--state :session) :id) "other"))
                ('directory (setq default-directory "/fs:other:/srv/")))
              (agent-shell--set-session-title "Current title")
              (funcall (plist-get (car queries) :on-success)
                       (agent-remote-test-title-response "Stale title"))
              (should (equal (map-nested-elt agent-shell--state '(:session :title)) "Current title"))
              (should-not +agent-acp--title-query))
          (delete-process replacement))))))

(ert-deftest agent-remote-c15-old-title-callback-leaves-a-new-query-intact ()
  (agent-remote-test-with-title-client
    (agent-shell--refresh-session-title)
    (setf (map-elt (map-elt agent-shell--state :session) :id) "other")
    (agent-shell--set-session-title "Current title")
    (agent-shell--refresh-session-title)
    (let ((new-query +agent-acp--title-query))
      (funcall (plist-get (car queries) :on-success)
               (agent-remote-test-title-response "Stale title"))
      (should (eq new-query +agent-acp--title-query))
      (funcall (plist-get (cadr queries) :on-success)
               (agent-remote-test-title-response "New title" "other"))
      (should (equal (map-nested-elt agent-shell--state '(:session :title)) "New title")))))

(ert-deftest agent-remote-c46-title-failure-releases-the-slot-without-retry ()
  (agent-remote-test-with-title-client
    (agent-shell--refresh-session-title)
    (funcall (plist-get (car queries) :on-failure) '((message . "Fixture error")) nil)
    (should-not +agent-acp--title-query)
    (should (= 1 (length queries)))
    (agent-shell--refresh-session-title)
    (should (= 2 (length queries)))
    (funcall (plist-get (cadr queries) :on-failure) '((message . "Fixture error")) nil)
    (cl-letf (((symbol-function 'acp-send-request) (lambda (&rest _) (error "Send failed"))))
      (should-error (agent-shell--refresh-session-title) :type 'error))
    (should-not +agent-acp--title-query)))

(ert-deftest agent-remote-c46-title-query-never-starts-a-dead-or-disconnected-client ()
  (agent-remote-test-with-title-client
    (let ((remote-buffer-disconnected-p t)) (agent-shell--refresh-session-title))
    (should-not queries)
    (delete-process process)
    (agent-shell--refresh-session-title)
    (should-not queries)
    (setf (map-elt client :process) nil)
    (agent-shell--refresh-session-title)
    (should-not queries)))

(ert-deftest agent-remote-c15-shutdown-releases-the-pending-title-query ()
  (agent-remote-test-with-title-client
    (agent-shell--refresh-session-title)
    (let ((+agent-acp-client-ended-functions nil))
      (+agent-acp--shutdown-a #'ignore))
    (should-not +agent-acp--title-query)
    (funcall (plist-get (car queries) :on-success)
             (agent-remote-test-title-response "Late title"))
    (should-not (map-nested-elt agent-shell--state '(:session :title)))))

(ert-deftest agent-remote-c46-cli-lookups-share-only-one-start ()
  "Repeated client construction shares a lookup, not a client or lifetime."
  (let ((calls 0)
        (arguments '(:command "codex-acp" :environment-variables ("A=1"))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (program &optional _remote)
                   (should (equal program "codex"))
                   (cl-incf calls)
                   "/tools/codex")))
        (dotimes (_ 2)
          (+agent-shell-startup-lookups-a
           (lambda ()
             (dotimes (_ 3)
               (should
                (equal (plist-get (+agent-shell-use-workspace-cli arguments)
                                  :environment-variables)
                       '("CODEX_PATH=/tools/codex" "A=1")))))))
        (should (= calls 2))
        (should (equal arguments
                       '(:command "codex-acp" :environment-variables ("A=1")))))
      (should-not +agent-shell--startup-lookups))))

(ert-deftest agent-remote-c46-cli-lookups-observe-context-and-failure ()
  "Changing PATH, environment, cwd or buffer cannot reuse another lookup."
  (let ((calls 0) (missing t))
    (+agent-shell-startup-lookups-a
     (lambda ()
       (with-temp-buffer
         (let ((default-directory "/fs:box:/srv/"))
           (cl-letf (((symbol-function 'executable-find)
                      (lambda (&rest _)
                        (cl-incf calls) (unless missing "/tools/codex"))))
             (should-not (+agent-shell-workspace-executable "codex"))
             (setq missing nil)
             (should (+agent-shell-workspace-executable "codex"))
             (should (+agent-shell-workspace-executable "codex"))
             (let ((exec-path '("/other/bin")))
               (should (+agent-shell-workspace-executable "codex")))
             (let ((process-environment (cons "NEW=1" process-environment)))
               (should (+agent-shell-workspace-executable "codex")))
             (let ((default-directory "/fs:other:/srv/"))
               (should (+agent-shell-workspace-executable "codex")))
             (with-temp-buffer
               (should (+agent-shell-workspace-executable "codex")))
             (should (= calls 6)))))))))

(ert-deftest agent-remote-c46-client-start-binds-its-owner-and-route ()
  "Even a callback in another buffer starts on the client's own target."
  (let ((owner (generate-new-buffer " *agent-route-owner*")))
    (unwind-protect
        (progn
          (with-current-buffer owner
            (setq major-mode 'agent-shell-mode
                  default-directory "/fs:box:/srv/work/"))
          (with-temp-buffer
            (setq default-directory "/fs:wrong:/other/")
            (+agent-shell-start-client-on-target-a
             (lambda (&rest _)
               (should (eq (current-buffer) owner))
               (should (equal default-directory "/fs:box:/srv/work/"))
               (should (equal remote-current-adapter-id "agent-shell")))
             :client `((:context-buffer . ,owner)))
            (should (equal default-directory "/fs:wrong:/other/"))))
      (kill-buffer owner)))
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (+agent-shell-start-client-on-target-a
       (lambda (&rest _) (should (eq (current-buffer) buffer)))
       :client nil))))

(ert-deftest agent-remote-c46-local-client-uses-the-same-route ()
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode
          default-directory temporary-file-directory)
    (+agent-shell-start-client-on-target-a
     (lambda (&rest _)
       (should (equal (remote-file-name-target default-directory) "local"))
       (should (string-prefix-p "/fs:" default-directory))
       (should (equal remote-current-adapter-id "agent-shell")))
     :client `((:context-buffer . ,(current-buffer)))))
  (let ((adapter (remote-get-adapter "agent-shell")))
    (should (eq (remote-adapter-placement adapter) 'target))
    (should (eq (remote-adapter-process-class adapter) 'interactive))))

(ert-deftest agent-remote-c46-process-factory-leaves-nested-spawns-alone ()
  "A backend's own spawn and other buffers never re-enter the ACP factory."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode
          default-directory temporary-file-directory)
    (let ((client `((:context-buffer . ,(current-buffer)) (:command . "fixture")
                    (:instance-count . 1)))
          (native 0) (routed 0))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _) (cl-incf native)))
                ((symbol-function 'remote-make-process)
                 (lambda (&rest args)
                   (cl-incf routed)
                   (should (equal (plist-get args :remote-adapter) "agent-shell"))
                   ;; The backend ultimately calls the native primitive.
                   (make-process :name "acp-client(fixture)-1"))))
        (+agent-shell-start-client-on-target-a
         (lambda (&rest _)
           (with-temp-buffer (make-process :name "acp-client(fixture)-1"))
           (make-process :name "unrelated-client-helper")
           (make-process :name "acp-client(fixture)-1"))
         :client client)
        (should (= native 3))
        (should (= routed 1))))))

(ert-deftest agent-remote-c46-client-environment-preserves-acp-precedence ()
  (let* ((client '((:environment-variables . ("A=first=value" "A=second" "UNSET"))))
         (overrides (+agent-shell-client-environment client)))
    (should (equal overrides '(("UNSET") ("A" . "second") ("A" . "first=value"))))
    (should (equal (remote--apply-environment '("A=old" "UNSET=old") overrides)
                   '("A=first=value")))))

(ert-deftest agent-remote-c56-capture-crosses-only-the-file-boundary ()
  "The UI tool uses client paths and environment; only the saved file moves."
  (let (captured copied)
    (with-temp-buffer
      (setq default-directory "/fs:box:/srv/work/")
      (cl-letf (((symbol-function 'remote-client-file-name) (lambda (&rest _) nil))
                ((symbol-function 'copy-file)
                 (lambda (source target &rest _)
                   (setq copied (list source target))
                   (should (file-exists-p source))
                   (should (equal target "/fs:box:/srv/work/screenshots/value.png")))))
        (should
         (equal
          (+agent-shell-capture-on-client-a
           (lambda (&rest args)
             (let ((directory (plist-get args :destination-dir)))
               (setq captured directory)
               (should-not (file-remote-p directory))
               (should-not (file-remote-p default-directory))
               (with-temp-file (expand-file-name "value.png" directory) (insert "image"))
               (expand-file-name "value.png" directory)))
           :destination-dir "/fs:box:/srv/work/screenshots/")
          "/fs:box:/srv/work/screenshots/value.png"))
        (should copied)
        (should-not (file-exists-p captured))))))

(ert-deftest agent-remote-c56-native-capture-needs-no-copy ()
  (let* ((directory (file-name-as-directory (make-temp-file "agent-native-capture-" t)))
         (logical (concat "/fs:local:" directory)))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory "/fs:box:/other/")
          (cl-letf (((symbol-function 'copy-file)
                     (lambda (&rest _) (ert-fail "Copied a client-accessible image"))))
            (should
             (equal
              (+agent-shell-capture-on-client-a
               (lambda (&rest args)
                 (should (equal (plist-get args :destination-dir) directory))
                 (should-not (file-remote-p default-directory))
                 (concat directory "value.png"))
               :destination-dir logical)
              (concat directory "value.png")))))
      (delete-directory directory t))))

(ert-deftest agent-remote-c56-capture-failure-cleans-up-and-honors-no-error ()
  (let (temporary)
    (cl-letf (((symbol-function 'remote-client-file-name) (lambda (&rest _) nil)))
      (dolist (no-error '(nil t))
        (let ((capture (lambda (&rest args)
                         (setq temporary (plist-get args :destination-dir))
                         (error "Fixture capture failure"))))
          (if no-error
              (should-not (+agent-shell-capture-on-client-a
                           capture :destination-dir "/fs:box:/images/" :no-error t))
            (should-error (+agent-shell-capture-on-client-a
                           capture :destination-dir "/fs:box:/images/") :type 'error))
          (should-not (file-exists-p temporary)))))))

(ert-deftest agent-remote-c46-activity-event-is-local-and-stable ()
  "An event for A neither enumerates B nor rebuilds an unchanged activity."
  (let ((+agent-activity--agents (make-hash-table :test #'eq))
        (updates 0) seen
        (agent (generate-new-buffer " *agent-activity-test*")))
    (unwind-protect
        (progn
          (with-current-buffer agent
            (setq major-mode 'agent-shell-mode
                  +agent-acp-session-name "test" +agent-acp-session-agent "fixture"))
          (cl-letf (((symbol-function '+agent-acp-busy-p)
                     (lambda (buffer) (push buffer seen) t))
                    ((symbol-function '+agent-acp-sessions)
                     (lambda (&rest _) (ert-fail "Enumerated every session")))
                    ((symbol-function '+agent-session-snapshot)
                     (lambda (&rest _) (ert-fail "Constructed a full snapshot")))
                    ((symbol-function '+agent-activity--changed)
                     (lambda () (cl-incf updates))))
            (+agent-activity--update agent)
            (let* ((activity (gethash agent +agent-activity--agents))
                   (started (+activity-started-at activity)))
              (+agent-activity--update agent)
              (should (eq activity (gethash agent +agent-activity--agents)))
              (should (= updates 1))
              (with-current-buffer agent (setq +agent-acp-session-name "renamed"))
              (+agent-activity--update agent)
              (should (= updates 2))
              (should (equal started (+activity-started-at
                                      (gethash agent +agent-activity--agents)))))
            (should (equal seen (list agent agent agent)))
            (kill-buffer agent)
            (+agent-activity--update agent)
            (should (= updates 3))
            (should-not (hash-table-values +agent-activity--agents))))
      (when (buffer-live-p agent) (kill-buffer agent)))))

(ert-deftest agent-remote-c46-activity-poll-reads-only-execution-facts ()
  (let ((+agent-activity--agents (make-hash-table :test #'eq))
        (agent (generate-new-buffer " *agent-activity-poll*")))
    (unwind-protect
        (progn
          (with-current-buffer agent (setq major-mode 'agent-shell-mode))
          (cl-letf (((symbol-function '+agent-acp-busy-p) (lambda (_) t))
                    ((symbol-function '+agent-session-snapshot)
                     (lambda (&rest _) (ert-fail "Constructed a full snapshot")))
                    ((symbol-function '+agent-activity--changed) #'ignore))
            (+agent-activity--update)
            (should (gethash agent +agent-activity--agents))
            (with-current-buffer agent (setq +agent-acp-session-origin 'latex-export))
            (+agent-activity--update)
            (should-not (gethash agent +agent-activity--agents))))
      (kill-buffer agent))))

(ert-deftest agent-remote-c46-native-stdio-has-placement-and-one-handshake ()
  "The real local process uses the same adapter and protocol as SSH."
  (let* ((directory (file-name-as-directory (make-temp-file "agent-route-" t)))
         (store (concat directory "sessions.json"))
         (+agent-acp-notify-function nil)
         (config (agent-flow-e2e--config store))
         (maker (map-elt config :client-maker))
         buffer)
    (setf (map-elt config :client-maker)
          (lambda (owner)
            (let ((client (funcall maker owner)))
              (setf (map-elt client :environment-variables)
                    '("AGENT_REMOTE_MARKER=native-route"))
              client)))
    (unwind-protect
        (progn
          (setq buffer (+agent-acp-start :config config :directory directory :focus nil))
          (agent-flow-e2e--wait (lambda () (+agent-acp-state-value buffer '(:init-finished))))
          (let* ((process (+agent-acp-state-value buffer '(:client :process)))
                 (route (process-get process 'remote-route)))
            (should (remote-route-p route))
            (should (equal (remote-route-target-id route) "local"))
            (should (equal (remote-route-adapter-id route) "agent-shell"))
            (should (equal (remote-buffer-target buffer) "local"))
            (should (eq (process-get process 'remote-process-class) 'interactive))
            (should-not (process-get process 'remote-process-adaptive-read-buffering)))
          (agent-flow-e2e--send buffer "runtime")
          (agent-flow-e2e--idle buffer)
          (let ((text (agent-flow-e2e--text buffer)))
            (should (string-match-p "native-route" text))
            (should (string-match-p (regexp-quote (directory-file-name (file-truename directory)))
                                    text)))
          (with-temp-buffer
            (insert-file-contents (concat store ".events"))
            (let ((events (mapcar (lambda (line) (json-parse-string line :object-type 'alist))
                                  (split-string (buffer-string) "\n" t))))
              (dolist (method '("initialize" "session/new" "session/prompt"))
                (should (= 1 (cl-count method events :key (lambda (event) (alist-get 'method event))
                                      :test #'equal)))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))
      (delete-directory directory t))))

(provide 'agent-remote-performance-tests)
;;; agent-remote-performance-tests.el ends here
