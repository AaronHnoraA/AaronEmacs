;;; remote-transfer-tests.el --- Machines and transfers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with:
;;   make remote-transfer-test
;;
;; The transfer tests start a real rsync between two folders of this machine,
;; which is the `local' half of the parity contract.  The remote half is
;; checked at the process boundary: the argv, environment and route a target
;; without a shared filesystem produces.  A live SSH run is opt-in through
;; REMOTE_TRANSFER_E2E_TARGET.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'auth-source)
(require 'remote-framework)
(require 'remote-config)
(require 'remote-machines)
(require 'remote-transfer)
(require 'remote-files)
(require 'remote-board)
(require 'emacs/dirvish/config)

(when (boundp 'comp-enable-subr-trampolines)
  (setq comp-enable-subr-trampolines nil))

(remote-fs-install)

(ert-deftest remote-files-menu-renders-with-transient ()
  "The transfer menu renders its heading, command, and grouped commands."
  (let ((transient--prefix nil)
        (transient--layout nil)
        (transient--suffixes nil))
    (cl-letf (((symbol-function 'remote-files--menu-title)
               (lambda () "Target folder <-> local")))
      (dolist (transient-force-single-column '(nil t))
        (transient--init-objects 'remote-files-dispatch nil nil)
        (with-temp-buffer
          (transient--insert-groups)
          (goto-char (point-min))
          (dolist (label '("Target folder <-> local" "Choose other machine"
                           "Files" "Send marked to the other side" "Folders"
                           "Sync this folder to the other" "More"
                           "Disconnect target"))
            (should (search-forward label nil t))
            (goto-char (point-min))))))))

(defmacro remote-transfer-test-with-registry (&rest body)
  "Evaluate BODY with isolated registries, machines and transfer state."
  (declare (indent 0) (debug t))
  `(let* ((directory (file-name-as-directory
                      (file-truename
                       (make-temp-file "remote-transfer-test-" t))))
          (remote-targets (make-hash-table :test #'equal))
          (remote-links (make-hash-table :test #'equal))
          (remote-adapters (make-hash-table :test #'equal))
          (remote-route-health (make-hash-table :test #'equal))
          (remote-route-log nil)
          (remote-connection-pool (make-hash-table :test #'equal))
          (remote-pipeline-runtime-pool (make-hash-table :test #'equal))
          (remote-fs-preferred-route-cache (make-hash-table :test #'equal))
          (remote-backend-contracts (make-hash-table :test #'equal))
          (remote-machines-directory directory)
          (remote-machines-file (expand-file-name "machines.json" directory))
          (remote-machines-auth-file
           (expand-file-name "machines.authinfo" directory))
          (remote-machines-askpass-file
           (expand-file-name "askpass.sh" directory))
          (remote-machines--list nil)
          (remote-machines--loaded nil)
          (remote-machines--revision nil)
          (remote-transfer-jobs nil)
          (remote-config-file (expand-file-name "remote.json" directory)))
     (unwind-protect
         (progn
           (remote-reset-registries)
           (remote-framework-bootstrap)
           (with-temp-file remote-config-file
             (insert "{\"version\": 2, \"targets\": []}"))
           ,@body)
       (dolist (job remote-transfer-jobs)
         (when (process-live-p (remote-transfer-job-process job))
           (delete-process (remote-transfer-job-process job)))
         (when (buffer-live-p (remote-transfer-job-log-buffer job))
           (kill-buffer (remote-transfer-job-log-buffer job))))
       (delete-directory directory t))))

(defun remote-transfer-test--machine (&rest overrides)
  "Return a machine plist with OVERRIDES applied."
  (let ((machine (list :id "lab" :label "Lab" :host "192.0.2.10"
                       :user "root" :port 2222 :password "s3 \"cr\\et")))
    (cl-loop for (key value) on overrides by #'cddr
             do (setq machine (plist-put machine key value)))
    machine))

(defun remote-transfer-test--file-digest (file)
  "Return a digest of FILE without exposing its stored credentials."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(defun remote-transfer-test--wait (job)
  "Wait until JOB has ended and return its state."
  (with-timeout (30 (error "Transfer did not finish"))
    (while (eq (remote-transfer-job-state job) 'running)
      (accept-process-output nil 0.05)))
  (remote-transfer-job-state job))

(ert-deftest remote-machines-are-a-reload-stable-target-source ()
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (let ((target (remote-get-target "lab")))
      (should (remote-machines-target-p target))
      (should-not (remote-target-trusted target))
      (should (equal (remote-target-label target) "Lab")))
    (let* ((pipeline (car (remote-pipelines-for-target "lab")))
           (config (remote-pipeline-config pipeline)))
      (should (equal (plist-get config :host) "192.0.2.10"))
      (should (equal (plist-get config :user) "root"))
      (should (equal (plist-get config :port) 2222))
      (should (member "StrictHostKeyChecking=accept-new"
                      (plist-get config :ssh-options))))
    ;; A reload rebuilds the registry from the file and from every source.
    (remote-config-reload)
    (should (remote-machines-target-p "lab"))
    ;; Another Emacs reads the same store.
    (setq remote-machines--list nil remote-machines--loaded nil)
    (remote-config-reload)
    (should (remote-machines-target-p "lab"))
    (remote-machines-remove "lab")
    (should-not (remote-get-target "lab"))
    (should-not (file-exists-p remote-machines-auth-file))))

(ert-deftest remote-machines-reject-stale-edits-until-disk-reload ()
  "An external edit is preserved until this Emacs explicitly reloads it."
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (let* ((outside (remote-transfer-test--machine
                     :id "outside" :label "Outside" :host "192.0.2.11"
                     :password nil))
           (local (remote-transfer-test--machine
                   :id "local-new" :label "Local New" :host "192.0.2.12"
                   :password nil))
           (outside-json
            (concat
             (json-serialize
              (list :version 1
                    :machines
                    (vconcat (mapcar #'remote-machines--compact
                                     (list (remote-machines-get "lab") outside)))))
             "\n")))
      ;; Another Emacs has saved a newer copy of the shared machine list.
      (remote-machines--private-write remote-machines-file outside-json)
      (let ((disk-digest (remote-transfer-test--file-digest
                          remote-machines-file)))
        (should (string-match-p
                 "remote-machines-reload-from-disk"
                 (error-message-string
                  (should-error (remote-machines-add local)
                                :type 'user-error))))
        (should (equal (remote-transfer-test--file-digest
                        remote-machines-file)
                       disk-digest))
        (should-not (remote-machines-get "local-new"))
        (should-not (remote-machines-target-p "local-new")))
      (remote-machines-reload-from-disk)
      (should (remote-machines-target-p "outside"))
      (remote-machines-add local)
      (dolist (id '("lab" "outside" "local-new"))
        (should (remote-machines-get id))
        (should (remote-machines-target-p id)))
      (should (= (length (plist-get
                          (json-parse-string
                           (with-temp-buffer
                             (insert-file-contents remote-machines-file)
                             (buffer-string))
                           :object-type 'plist :array-type 'list)
                          :machines))
                 3)))))

(ert-deftest remote-machines-native-lock-blocks-another-emacs ()
  "An independent Emacs holding the machine file lock prevents a commit."
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (let* ((ready (expand-file-name "other-emacs-ready" directory))
           (child-form
            `(let ((create-lockfiles t)
                   (lock-file-name-transforms nil))
               (lock-file ,remote-machines-file)
               (unwind-protect
                   (progn
                     (with-temp-file ,ready (insert "ready"))
                     (sleep-for 4))
                 (unlock-file ,remote-machines-file))))
           (child
            (make-process
             :name "remote-machines-lock-test"
             :buffer nil
             :noquery t
             :command
             (list (expand-file-name invocation-name invocation-directory)
                   "-Q" "--batch" "--eval" (prin1-to-string child-form)))))
      (unwind-protect
          (progn
            (with-timeout (5 (error "Other Emacs did not acquire the machine lock"))
              (while (and (process-live-p child)
                          (not (file-exists-p ready)))
                (accept-process-output child 0.05)))
            (should (file-exists-p ready))
            (should (process-live-p child))
            (let ((disk-digest
                   (remote-transfer-test--file-digest remote-machines-file)))
              (should (string-match-p
                       "being updated in another Emacs"
                       (error-message-string
                        (should-error
                         (remote-machines-add
                          (remote-transfer-test--machine
                           :id "local-new" :label "Local New"
                           :host "192.0.2.12"))
                         :type 'user-error))))
              (should (equal (remote-transfer-test--file-digest
                              remote-machines-file)
                             disk-digest))
              (should-not (remote-machines-get "local-new"))))
        (when (process-live-p child)
          (delete-process child))))))

(ert-deftest remote-machines-views-refresh-and-close-independently-by-frame ()
  "Edits refresh both visible lists; frame closing releases only its view."
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (let* ((real-frame (selected-frame))
           (other-frame (make-symbol "other-frame"))
           (other-parameters (make-hash-table :test #'eq))
           (original-frame-parameter (symbol-function 'frame-parameter))
           (original-set-frame-parameter (symbol-function 'set-frame-parameter))
           (original-frame-live-p (symbol-function 'frame-live-p))
           first second first-board second-board)
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (frame key)
                   (if (eq frame other-frame)
                       (gethash key other-parameters)
                     (funcall original-frame-parameter frame key))))
                ((symbol-function 'set-frame-parameter)
                 (lambda (frame key value)
                   (if (eq frame other-frame)
                       (puthash key value other-parameters)
                     (funcall original-set-frame-parameter frame key value))))
                ((symbol-function 'frame-live-p)
                 (lambda (frame)
                   (if (eq frame other-frame) t
                     (funcall original-frame-live-p frame))))
                ((symbol-function 'frame-list)
                 (lambda () (list real-frame other-frame)))
                ((symbol-function 'get-buffer-window)
                 (lambda (buffer frame)
                   (or (eq buffer (frame-parameter
                                   frame 'remote-machines-view-buffer))
                       (eq buffer (frame-parameter
                                   frame 'remote-board-view-buffer))))))
        (unwind-protect
            (progn
              (setq first (remote-machines--buffer real-frame)
                    second (remote-machines--buffer other-frame)
                    first-board (remote-board--buffer real-frame)
                    second-board (remote-board--buffer other-frame))
              (should-not (eq first second))
              (remote-machines-add
               (remote-transfer-test--machine
                :id "other" :label "Other" :host "192.0.2.11"))
              (dolist (buffer (list first second first-board second-board))
                (with-current-buffer buffer
                  (should (string-match-p "Other" (buffer-string)))))
              (remote-machines--frame-deleted other-frame)
              (remote-board--frame-deleted other-frame)
              (should-not (buffer-live-p second))
              (should-not (buffer-live-p second-board))
              (should (buffer-live-p first))
              (should (buffer-live-p first-board)))
          (when (buffer-live-p second-board) (kill-buffer second-board))
          (when (buffer-live-p first-board) (kill-buffer first-board))
          (when (buffer-live-p second) (kill-buffer second))
          (when (buffer-live-p first) (kill-buffer first)))))))

(ert-deftest remote-machines-password-edit-invalidates-old-session ()
  "Changing only a stored password also closes the old pooled SSH session."
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (let ((pipeline-id
           (remote-pipeline-id
            (car (remote-pipelines-for-target "lab"))))
          invalidated)
      (cl-letf (((symbol-function 'remote-machines--read-machine)
                 (lambda (old)
                   (plist-put (copy-sequence old) :password "new-secret")))
                ((symbol-function 'remote-connection-invalidate-link)
                 (lambda (id disconnect reason)
                   (push (list id disconnect reason) invalidated))))
        (remote-machines-edit "lab"))
      (should (equal (plist-get (remote-machines-get "lab") :password)
                     "new-secret"))
      (should (member (list pipeline-id t 'credentials-changed)
                      invalidated)))))

(ert-deftest remote-machines-add-restores-state-after-auth-write-error ()
  "An auth-file failure cannot leave an unregistered saved machine."
  (remote-transfer-test-with-registry
    (cl-letf (((symbol-function 'remote-machines--write-auth-file)
               (lambda () (error "injected auth write failure"))))
      (should-error (remote-machines-add (remote-transfer-test--machine))))
    (should-not (remote-machines-get "lab"))
    (should-not (remote-get-target "lab"))
    (should-not (file-exists-p remote-machines-file))
    (should-not (file-exists-p remote-machines-auth-file))))

(ert-deftest remote-machines-add-needs-a-loadable-target-config ()
  "A missing framework config cannot produce a saved but unusable machine."
  (remote-transfer-test-with-registry
    (delete-file remote-config-file)
    (should-error (remote-machines-add (remote-transfer-test--machine)))
    (should-not (remote-machines-get "lab"))
    (should-not (remote-get-target "lab"))
    (should-not (file-exists-p remote-machines-file))
    (should-not (file-exists-p remote-machines-auth-file))))

(ert-deftest remote-machines-edit-restores-record-after-config-error ()
  "A failed target reload restores memory and both private files."
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (let ((original (remote-machines-get "lab"))
          (json-digest (remote-transfer-test--file-digest
                        remote-machines-file))
          (auth-digest (remote-transfer-test--file-digest
                        remote-machines-auth-file)))
      (cl-letf (((symbol-function 'remote-machines--read-machine)
                 (lambda (old)
                   (plist-put (copy-sequence old) :password "replacement")))
                ((symbol-function 'remote-config-reload)
                 (lambda () (error "injected config reload failure"))))
        (should-error (remote-machines-edit "lab")))
      (should (eq (remote-machines-get "lab") original))
      (should (remote-machines-target-p "lab"))
      (should (equal (remote-transfer-test--file-digest
                      remote-machines-file)
                     json-digest))
      (should (equal (remote-transfer-test--file-digest
                      remote-machines-auth-file)
                     auth-digest)))))

(ert-deftest remote-machines-edit-restores-committed-registry-after-hook-error ()
  "A hook failing after config commit cannot leave a new target behind."
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (let ((json-digest (remote-transfer-test--file-digest
                        remote-machines-file))
          (auth-digest (remote-transfer-test--file-digest
                        remote-machines-auth-file))
          failed-once)
      (let ((remote-config-after-load-hook
             (list (lambda ()
                     (unless failed-once
                       (setq failed-once t)
                       (error "injected after-load failure"))))))
        (cl-letf (((symbol-function 'remote-machines--read-machine)
                   (lambda (old)
                     (plist-put (copy-sequence old) :host "192.0.2.20"))))
          (should-error (remote-machines-edit "lab"))))
      (should failed-once)
      (should (equal (plist-get (remote-machines-get "lab") :host)
                     "192.0.2.10"))
      (should (equal (plist-get
                      (remote-pipeline-config
                       (car (remote-pipelines-for-target "lab")))
                      :host)
                     "192.0.2.10"))
      (should (equal (remote-transfer-test--file-digest
                      remote-machines-file)
                     json-digest))
      (should (equal (remote-transfer-test--file-digest
                      remote-machines-auth-file)
                     auth-digest)))))

(ert-deftest remote-machines-edit-recovers-from-invalidation-error ()
  "A reload failing before generation increments still restores the target."
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (let ((invalidate (symbol-function 'remote-config--invalidate-pipelines))
          failed-once)
      (cl-letf (((symbol-function 'remote-machines--read-machine)
                 (lambda (old)
                   (plist-put (copy-sequence old) :host "192.0.2.20")))
                ((symbol-function 'remote-config--invalidate-pipelines)
                 (lambda (ids &optional reason)
                   (if (and ids (not failed-once))
                       (progn
                         (setq failed-once t)
                         (error "injected invalidation failure"))
                     (funcall invalidate ids reason)))))
        (should-error (remote-machines-edit "lab")))
      (should failed-once)
      (should (equal (plist-get (remote-machines-get "lab") :host)
                     "192.0.2.10"))
      (should (equal (plist-get
                      (remote-pipeline-config
                       (car (remote-pipelines-for-target "lab")))
                      :host)
                     "192.0.2.10")))))

(ert-deftest remote-machines-remove-does-not-disconnect-before-save ()
  "Forgetting a machine only tears down its target after persistence works."
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (let ((json-digest (remote-transfer-test--file-digest
                        remote-machines-file))
          (auth-digest (remote-transfer-test--file-digest
                        remote-machines-auth-file))
          disconnected)
      (cl-letf (((symbol-function 'remote-machines--write-auth-file)
                 (lambda () (error "injected auth write failure")))
                ((symbol-function 'remote-workspace-disconnect-target)
                 (lambda (&rest _) (setq disconnected t))))
        (should-error (remote-machines-remove "lab")))
      (should-not disconnected)
      (should (remote-machines-get "lab"))
      (should (remote-machines-target-p "lab"))
      (should (equal (remote-transfer-test--file-digest
                      remote-machines-file)
                     json-digest))
      (should (equal (remote-transfer-test--file-digest
                      remote-machines-auth-file)
                     auth-digest)))))

(ert-deftest remote-read-target-offers-to-remember-a-machine ()
  "Every target prompt is also the entry for a machine typed by hand."
  (remote-transfer-test-with-registry
    (let ((remote-target-history nil)
          offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq offered collection)
                   (push remote-read-target-new-machine remote-target-history)
                   remote-read-target-new-machine))
                ((symbol-function 'remote-machines--read-machine)
                 (lambda (&rest _) (remote-transfer-test--machine))))
        (let ((target (remote-read-target "Open target: " t)))
          (should (member remote-read-target-new-machine offered))
          (should (equal (remote-target-id target) "lab"))
          (should (remote-machines-target-p target))
          (should (file-exists-p remote-machines-file))
          (should-not remote-target-history)))
      ;; Next time it is listed as an ordinary, marked, choice.
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (seq-find (lambda (name) (string-prefix-p "Lab" name))
                             collection))))
        (should (equal (remote-target-id (remote-read-target)) "lab"))))))

(ert-deftest remote-machines-do-not-replace-a-configured-target ()
  (remote-transfer-test-with-registry
    (with-temp-file remote-config-file
      (insert "{\"version\": 2, \"targets\": [{\"id\": \"lab\", \"label\": \"Configured\"}]}"))
    (setq remote-machines--list (list (remote-transfer-test--machine))
          remote-machines--loaded t)
    (remote-config-reload)
    (should (equal (remote-target-label (remote-get-target "lab"))
                   "Configured"))
    (should-not (remote-machines-target-p "lab"))
    (should-error (remote-machines-add (remote-transfer-test--machine))
                  :type 'user-error)))

(ert-deftest remote-machines-keep-credentials-private-and-findable ()
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (dolist (file (list remote-machines-file remote-machines-auth-file))
      (should (= (logand (file-modes file) #o077) 0)))
    ;; TRAMP asks auth-source by host, or host#port off the default port.
    (let ((auth-sources (list remote-machines-auth-file))
          (auth-source-do-cache nil))
      (dolist (host '("192.0.2.10" "192.0.2.10#2222"))
        (let* ((found (car (auth-source-search
                            :host host :user "root" :port "ssh" :max 1)))
               (secret (plist-get found :secret)))
          (should (equal (if (functionp secret) (funcall secret) secret)
                         "s3 \"cr\\et")))))
    (should (equal (remote-machines--netrc-token "plain") "plain"))
    (should (equal (remote-machines--netrc-token "#lead") "\"#lead\""))
    (should (equal (remote-machines--netrc-token "it's ok") "\"it's ok\""))
    (should-not (remote-machines--netrc-token "a 'b' \"c\""))
    (remote-machines-forget-password "lab")
    (should (remote-machines-get "lab"))
    (should-not (plist-get (remote-machines-get "lab") :password))
    (should-not (file-exists-p remote-machines-auth-file))))

(ert-deftest remote-machines-reject-unusable-input ()
  (remote-transfer-test-with-registry
    (should (equal (remote-machines--parse-address "root@192.0.2.10:2222")
                   '("192.0.2.10" "root" 2222)))
    (should (equal (remote-machines--parse-address "host.example")
                   '("host.example" nil nil)))
    (should (equal (remote-machines--parse-address "u@[2001:db8::1]:22")
                   '("2001:db8::1" "u" 22)))
    (dolist (machine
             (list (remote-transfer-test--machine :host "bad host")
                   (remote-transfer-test--machine :user "a b")
                   (remote-transfer-test--machine :port 70000)
                   (remote-transfer-test--machine
                    :password nil :identity "/nonexistent/key")))
      (should-error (remote-machines-add machine) :type 'user-error))
    (should-not (remote-machines-list))))

(ert-deftest remote-transfer-client-shell-is-a-separate-authenticated-link ()
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (let* ((endpoint (remote-transfer-endpoint "/fs:lab:/srv/data/"))
           (shell (plist-get endpoint :shell))
           (command (plist-get shell :command))
           (environment (plist-get shell :environment)))
      (should (equal (plist-get endpoint :target) "lab"))
      (should (equal (plist-get endpoint :path) "/srv/data/"))
      (should (equal (plist-get shell :destination) "root@192.0.2.10"))
      (should (remote-route-p (plist-get shell :route)))
      ;; Its own connection: never the editing session's control socket.
      (should (member "ControlMaster=no" command))
      (should (member "ControlPath=none" command))
      (should (member "2222" command))
      (should-not (member "root@192.0.2.10" command))
      (should (equal (cdr (assoc "SSH_ASKPASS_REQUIRE" environment)) "force"))
      (should (equal (cdr (assoc remote-machines-secret-variable environment))
                     "s3 \"cr\\et"))
      (should (file-executable-p (cdr (assoc "SSH_ASKPASS" environment)))))
    ;; The helper answers from the environment, whatever the secret contains.
    (let ((process-environment
           (cons (concat remote-machines-secret-variable "=s3 \"cr\\et")
                 process-environment)))
      (should (equal (with-temp-buffer
                       (call-process remote-machines-askpass-file nil t)
                       (buffer-string))
                     "s3 \"cr\\et\n")))
    ;; A machine that signs in with a key hands the tool no secret.
    (remote-machines-forget-password "lab")
    (should-not
     (plist-get (plist-get (remote-transfer-endpoint "/fs:lab:/srv/") :shell)
                :environment))))

(ert-deftest remote-transfer-command-follows-placement-not-target-names ()
  (remote-transfer-test-with-registry
    (remote-machines-add (remote-transfer-test--machine))
    (remote-machines-add
     (remote-transfer-test--machine :id "other" :label "Other"
                                    :host "192.0.2.11"))
    (let ((remote-transfer--rsync-version-cache
           (cons (remote-client-executable-find "rsync") '(3 4 1))))
      ;; Both sides on this machine: plain paths, no remote shell.
      (pcase-let ((`(,command ,environment ,route ,targets)
                   (remote-transfer-command
                    '("/fs:local:/tmp/a dir/x") "/tmp/out")))
        (should-not (member "-e" command))
        (should (equal (last command 2) '("/tmp/a dir/x" "/tmp/out/")))
        (should-not environment)
        (should-not route)
        (should (equal targets '("local"))))
      ;; Upload: the destination is addressed through the client shell.
      (pcase-let ((`(,command ,environment ,route ,targets)
                   (remote-transfer-command
                    '("/tmp/a" "/tmp/b") "/fs:lab:/srv/data" :delete t)))
        (should (member "--delete" command))
        (should (string-match-p "ControlMaster=no"
                                (cadr (member "-e" command))))
        (should (equal (last command 3)
                       '("/tmp/a" "/tmp/b" "root@192.0.2.10:/srv/data/")))
        (should environment)
        (should (remote-route-p route))
        (should (equal (sort (copy-sequence targets) #'string<)
                       '("lab" "local"))))
      ;; Download of a folder's contents, as a dry run.
      (pcase-let ((`(,command . ,_)
                   (remote-transfer-command
                    '("/fs:lab:/srv/data") "/tmp/out"
                    :contents t :dry-run t)))
        (should (member "-n" command))
        (should-not (member "--info=progress2" command))
        (should (equal (last command 2)
                       '("root@192.0.2.10:/srv/data/" "/tmp/out/"))))
      (should-error
       (remote-transfer-command '("/fs:lab:/a") "/fs:other:/b")
       :type 'user-error)
      (should-error (remote-transfer-command nil "/tmp/out")
                    :type 'user-error))))

(ert-deftest remote-transfer-refuses-a-backend-without-a-client-shell ()
  (remote-transfer-test-with-registry
    (remote-register-backend
     "api-only"
     :capabilities '(metadata file-read process-sync process-async)
     :project (lambda (_file _link) nil))
    (remote-register-target "box" :label "Box")
    (remote-register-pipeline "box" "api" '("api-only"))
    ;; Never served from this machine's own disk instead.
    (should-error (remote-transfer-endpoint "/fs:box:/srv/")
                  :type 'remote-backend-unsupported)))

(ert-deftest remote-transfer-options-follow-the-client-tool ()
  (cl-flet ((options (version dry-run)
              (let ((remote-transfer--rsync-version-cache
                     (cons "rsync" version)))
                (remote-transfer--rsync-options "rsync" dry-run))))
    ;; A current client escapes remote names itself; `-s' would only break a
    ;; server that is not rsync proper.
    (should (equal (options '(3 4 1) nil)
                   '("--info=progress2" "--no-inc-recursive")))
    (should (equal (options '(3 2 3) nil)
                   '("-s" "--info=progress2" "--no-inc-recursive")))
    (should (equal (options '(3 0 9) nil) '("-s" "--progress")))
    ;; openrsync reports no version: nothing it may not know.
    (should (equal (options nil nil) '("--progress")))
    (should (equal (options '(3 4 1) t) '("-n" "-i")))))

(ert-deftest remote-transfer-argument-quoting ()
  (should (equal (remote-transfer--rsh-string
                  '("ssh" "-o" "IdentityFile=/a b/key" "-p" "22"))
                 "ssh -o 'IdentityFile=/a b/key' -p 22"))
  (should-error (remote-transfer--rsh-string '("ssh" "it's"))
                :type 'remote-backend-unsupported)
  (should (equal (remote-transfer--destination-host "u@2001:db8::1")
                 "u@[2001:db8::1]"))
  (should (equal (remote-transfer--destination-host "u@host") "u@host")))

(ert-deftest remote-transfer-progress-is-parsed-across-chunks ()
  (let ((job (remote-transfer-job--create :id 1 :state 'running)))
    (remote-transfer--filter job "  1.05M  12%  2.10MB/s    0:0")
    (should-not (remote-transfer-job-percent job))
    (remote-transfer--filter job "0:07 (xfr#1, to-chk=3/9)\r  9.00M  87%")
    (should (= (remote-transfer-job-percent job) 12))
    (should (equal (remote-transfer-job-rate job) "2.10MB/s"))
    (should (equal (remote-transfer-job-eta job) "0:00:07"))
    (remote-transfer--filter job " 512.00kB/s    0:00:01\r")
    (should (= (remote-transfer-job-percent job) 87))))

(ert-deftest remote-transfer-runs-without-blocking-and-reports ()
  (skip-unless (remote-client-executable-find "rsync"))
  (remote-transfer-test-with-registry
    (let* ((source (expand-file-name "src/" directory))
           (destination (expand-file-name "dst/" directory))
           finished)
      (make-directory source)
      (make-directory destination)
      (with-temp-file (expand-file-name "a.txt" source) (insert "alpha"))
      (with-temp-file (expand-file-name "b c.txt" source)
        (insert (make-string 200000 ?x)))
      (let ((job (remote-transfer-start
                  (list (expand-file-name "a.txt" source)
                        (expand-file-name "b c.txt" source))
                  (remote-canonicalize-file-name destination)
                  :callback (lambda (job) (setq finished job)))))
        ;; Starting returned at once, with the job still owned by a process.
        (should (eq (remote-transfer-job-state job) 'running))
        (should (equal (remote-transfer--running) (list job)))
        (should (timerp remote-transfer--refresh-timer))
        (should (eq (remote-transfer-test--wait job) 'done))
        (should (eq finished job))
        (should (= (remote-transfer-job-percent job) 100))
        (should-not (remote-transfer-job-process job))
        (should (equal (with-temp-buffer
                         (insert-file-contents
                          (expand-file-name "a.txt" destination))
                         (buffer-string))
                       "alpha"))
        (should (file-exists-p (expand-file-name "b c.txt" destination)))
        (remote-transfer--refresh)
        (should-not remote-transfer--refresh-timer))
      ;; A mirror preview lists the extra file and changes nothing.
      (with-temp-file (expand-file-name "extra.txt" destination) (insert "x"))
      (let ((preview (remote-transfer-start
                      (list source) destination
                      :contents t :delete t :dry-run t)))
        (should (eq (remote-transfer-test--wait preview) 'done))
        (should (string-match-p
                 "deleting +extra\\.txt"
                 (with-current-buffer (remote-transfer-job-log-buffer preview)
                   (buffer-string))))
        (should (file-exists-p (expand-file-name "extra.txt" destination))))
      (let ((mirror (remote-transfer-start
                     (list source) destination :contents t :delete t)))
        (should (eq (remote-transfer-test--wait mirror) 'done))
        (should-not (file-exists-p
                     (expand-file-name "extra.txt" destination))))
      ;; A failure is a state with the tool's own words, not a signal.
      (let ((failed (remote-transfer-start
                     (list (expand-file-name "missing" source)) destination)))
        (should (eq (remote-transfer-test--wait failed) 'failed))
        (should (/= (remote-transfer-job-exit-status failed) 0))
        (let ((again (remote-transfer-retry failed)))
          (should (eq (remote-transfer-test--wait again) 'failed))
          (should (/= (remote-transfer-job-id again)
                      (remote-transfer-job-id failed)))))
      (remote-transfer-list-clear)
      (should-not remote-transfer-jobs))))

(ert-deftest remote-transfer-target-side-operations-use-one-routed-lifecycle ()
  "Copy, move and all link methods work through the same local target route."
  (remote-transfer-test-with-registry
    (dolist (method '(copy move symlink relative-symlink hardlink))
      (let* ((source (expand-file-name (format "%s.txt" method) directory))
             (destination (expand-file-name (format "out-%s" method) directory))
             (output (expand-file-name (file-name-nondirectory source)
                                       destination)))
        (make-directory destination)
        (with-temp-file source (insert (format "%s\n" method)))
        (let* ((plan (remote-transfer-plan-file-operation
                      method (list source) destination :overwrite 'ask))
               (job (remote-transfer-start-file-operation plan)))
          (should (eq (remote-transfer-job-execution job) 'target))
          (should (eq (remote-transfer-test--wait job) 'done))
          (should (file-exists-p output))
          (pcase method
            ('move (should-not (file-exists-p source)))
            ((or 'symlink 'relative-symlink)
             (should (file-symlink-p output))
             (should (equal (file-truename output) (file-truename source)))
             (when (eq method 'relative-symlink)
               (should-not (file-name-absolute-p (file-symlink-p output)))))
            ('hardlink
             (should (= (file-attribute-link-number (file-attributes source)) 2)))
            ('copy
             (should-not (file-symlink-p output)))))))
    ;; A directory copy must preserve the subtree, too.
    (let* ((source (expand-file-name "tree" directory))
           (destination (expand-file-name "out-tree" directory)))
      (make-directory source)
      (make-directory destination)
      (with-temp-file (expand-file-name "child.txt" source) (insert "child"))
      (should
       (eq (remote-transfer-test--wait
            (remote-transfer-start-file-operation
             (remote-transfer-plan-file-operation
              'copy (list source) destination)))
           'done))
      (should (file-exists-p
               (expand-file-name "tree/child.txt" destination))))))

(ert-deftest remote-transfer-target-side-preflight-rejects-unsafe-batches ()
  "An invalid operation never enters the queue; overwrite is explicit."
  (remote-transfer-test-with-registry
    (let* ((source (expand-file-name "source.txt" directory))
           (other (expand-file-name "elsewhere/source.txt" directory))
           (destination (expand-file-name "out" directory))
           (output (expand-file-name "source.txt" destination)))
      (make-directory destination)
      (make-directory (file-name-directory other))
      (with-temp-file source (insert "new"))
      (with-temp-file other (insert "other"))
      (with-temp-file output (insert "old"))
      (should-error
       (remote-transfer-plan-file-operation 'copy (list source) destination)
       :type 'user-error)
      (should-error
       (remote-transfer-plan-file-operation 'copy (list source other)
                                            destination :overwrite t)
       :type 'user-error)
      (should-error
       (remote-transfer-plan-file-operation 'symlink (list source)
                                            destination :overwrite t)
       :type 'user-error)
      (should-error
       (remote-transfer-plan-file-operation
        'hardlink (list (file-name-directory other)) destination)
       :type 'user-error)
      (should-not remote-transfer-jobs)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (let ((job (remote-transfer-start-file-operation
                    (remote-transfer-plan-file-operation
                     'copy (list source) destination :overwrite 'ask))))
          (should (eq (remote-transfer-test--wait job) 'done))
          (should (equal (with-temp-buffer
                           (insert-file-contents output)
                           (buffer-string))
                         "new")))))))

(ert-deftest remote-files-refreshes-expanded-dired-destination ()
  "A completed target operation updates the subtree shown under its parent."
  (remote-transfer-test-with-registry
    (let* ((nested (expand-file-name "nested" directory))
           (new-file (expand-file-name "new.txt" nested))
           (buffer nil))
      (unwind-protect
          (progn
            (make-directory nested)
            (setq buffer (dired-noselect
                          (remote-canonicalize-file-name directory)))
            (with-current-buffer buffer
              (dired-insert-subdir (remote-canonicalize-file-name nested)))
            (with-temp-file new-file (insert "new"))
            (remote-files--revert-directory
             (remote-canonicalize-file-name nested))
            (with-current-buffer buffer
              (should (dired-goto-file
                       (remote-canonicalize-file-name new-file)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest remote-transfer-queue-q-returns-to-its-origin ()
  "Opening the persistent queue twice does not make q fall to scratch."
  (remote-transfer-test-with-registry
    (let ((origin (generate-new-buffer " *transfer-origin*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer origin)
            (let ((queue (remote-transfer-list-buffer)))
              (dotimes (_ 2)
                (remote-transfer-list)
                (should (eq (window-buffer) queue))
                (with-current-buffer queue
                  (should (eq (key-binding "q") #'quit-window))
                  (call-interactively (key-binding "q")))
                (should (eq (window-buffer) origin)))))
        (kill-buffer origin)
        (when-let* ((queue (get-buffer "*Remote Transfers*")))
          (kill-buffer queue))))))

(ert-deftest remote-transfer-queue-views-are-frame-owned ()
  "The global job queue redraws two independent frame views."
  (let* ((real-frame (selected-frame))
         (other-frame (make-symbol "other-frame"))
         (other-parameters (make-hash-table :test #'eq))
         (original-frame-parameter (symbol-function 'frame-parameter))
         (original-set-frame-parameter (symbol-function 'set-frame-parameter))
         (original-frame-live-p (symbol-function 'frame-live-p))
         (remote-transfer-jobs
          (list (remote-transfer-job--create
                 :id 901 :name "frame probe" :state 'running :started 1)))
         (remote-transfer--refresh-timer nil)
         first second)
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (frame key)
                 (if (eq frame other-frame)
                     (gethash key other-parameters)
                   (funcall original-frame-parameter frame key))))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame key value)
                 (if (eq frame other-frame)
                     (puthash key value other-parameters)
                   (funcall original-set-frame-parameter frame key value))))
              ((symbol-function 'frame-live-p)
               (lambda (frame)
                 (if (eq frame other-frame) t
                   (funcall original-frame-live-p frame)))))
      (unwind-protect
          (progn
            (setq first (remote-transfer-list-buffer real-frame)
                  second (remote-transfer-list-buffer other-frame))
            (should-not (eq first second))
            (should (eq first (remote-transfer-list-buffer real-frame)))
            (with-current-buffer first
              (setq-local tabulated-list-sort-key '("Operation" . nil)))
            (with-current-buffer second
              (should-not tabulated-list-sort-key))
            (setf (remote-transfer-job-state (car remote-transfer-jobs)) 'done
                  (remote-transfer-job-finished (car remote-transfer-jobs)) 2)
            (cl-letf (((symbol-function 'frame-list)
                       (lambda () (list real-frame other-frame)))
                      ((symbol-function 'get-buffer-window)
                       (lambda (buffer frame)
                         (and (eq buffer
                                  (frame-parameter frame
                                                   'remote-transfer-queue-buffer))
                              t))))
              (remote-transfer--refresh))
            (with-current-buffer first
              (should (string-match-p "done" (buffer-string))))
            (with-current-buffer second
              (should (string-match-p "done" (buffer-string))))
            (cl-letf (((symbol-function 'frame-list)
                       (lambda () (list real-frame other-frame)))
                      ((symbol-function 'get-buffer-window)
                       (lambda (buffer frame)
                         (and (eq buffer
                                  (frame-parameter frame
                                                   'remote-transfer-queue-buffer))
                              t))))
              (with-current-buffer first
                (remote-transfer-list-clear)))
            (should-not remote-transfer-jobs)
            (with-current-buffer first
              (should-not tabulated-list-entries))
            (with-current-buffer second
              (should-not tabulated-list-entries))
            (remote-transfer--frame-deleted other-frame)
            (should-not (buffer-live-p second))
            (should (buffer-live-p first)))
        (when (buffer-live-p second) (kill-buffer second))
        (when (buffer-live-p first) (kill-buffer first))))))

(ert-deftest remote-transfer-visible-log-survives-trim-and-clear-returns ()
  "Automatic history trim leaves a visible log; clear restores its window."
  (remote-transfer-test-with-registry
    (let* ((log (generate-new-buffer " *transfer-log*"))
           (queue (remote-transfer-list-buffer))
           (job (remote-transfer-job--create
                 :id 901 :name "finished" :state 'done
                 :started 1 :finished 2 :log-buffer log))
           (remote-transfer-history-limit 0)
           (remote-transfer-jobs (list job)))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (with-current-buffer log (remote-transfer-log-mode))
            (switch-to-buffer queue)
            (switch-to-buffer log)
            (with-current-buffer log
              (should (eq (key-binding "q") #'quit-window))
              (call-interactively (key-binding "q")))
            (should (eq (window-buffer) queue))
            (switch-to-buffer log)
            (remote-transfer--trim-history)
            (should (equal remote-transfer-jobs (list job)))
            (should (buffer-live-p log))
            (with-current-buffer queue
              (remote-transfer-list-clear))
            (should-not (buffer-live-p log))
            (should-not remote-transfer-jobs)
            (should (eq (window-buffer) queue)))
        (when (buffer-live-p log) (kill-buffer log))
        (when (buffer-live-p queue) (kill-buffer queue))))))

(ert-deftest remote-transfer-crosses-a-client-shell-both-ways ()
  "Upload and download through a backend that shares no filesystem.
The remote shell is a stand-in that, like ssh, hands the far command to a
shell on this machine, so
rsync really speaks its protocol over the command the backend handed out."
  (skip-unless (remote-client-executable-find "rsync"))
  (remote-transfer-test-with-registry
    (let* ((rsh (expand-file-name "fake-rsh" directory))
           (marker (expand-file-name "rsh-env" directory))
           (far (expand-file-name "far/" directory))
           (near (expand-file-name "near/" directory))
           (back (expand-file-name "back/" directory)))
      (with-temp-file rsh
        (insert "#!/bin/sh\nprintf '%s' \"$FAKE_SECRET\" > " marker
                "\nshift\nexec sh -c \"$*\"\n"))
      (set-file-modes rsh #o700)
      (dolist (folder (list far near back)) (make-directory folder))
      (with-temp-file (expand-file-name "note one.txt" near) (insert "hello"))
      (remote-register-backend
       "shell-only"
       :capabilities '(metadata file-read process-sync process-async)
       :project (lambda (_file _link) nil)
       :client-shell
       (lambda (_route)
         (list :command (list rsh) :destination "far-host"
               :environment '(("FAKE_SECRET" . "from-backend")))))
      (remote-register-target "box" :label "Box")
      (remote-register-pipeline "box" "api" '("shell-only"))
      (let* ((remote (concat "/fs:box:" far))
             (up (remote-transfer-start (list near) remote :contents t)))
        (should (equal (car (last (remote-transfer-job-command up)))
                       (concat "far-host:" far)))
        (should (remote-route-p
                 (process-get (remote-transfer-job-process up)
                              'remote-route)))
        (should (eq (remote-transfer-test--wait up) 'done))
        (should (file-exists-p (expand-file-name "note one.txt" far)))
        ;; The backend's variables reached the tool's own connection.
        (should (equal (with-temp-buffer (insert-file-contents marker)
                                         (buffer-string))
                       "from-backend"))
        (let ((down (remote-transfer-start
                     (list (concat remote "note one.txt")) back)))
          (should (eq (remote-transfer-test--wait down) 'done))
          (should (equal (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "note one.txt" back))
                           (buffer-string))
                         "hello")))))))

(ert-deftest remote-transfer-ends-with-its-target ()
  (remote-transfer-test-with-registry
    (let* ((job (remote-transfer--launch
                 (remote-transfer-job--create
                  :id 900 :name "sleeper" :target-ids '("lab" "local")
                  :command '("sleep" "60"))))
           (process (remote-transfer-job-process job)))
      (should (process-live-p process))
      (should (equal (remote-transfer-jobs-for-target "lab" t) (list job)))
      ;; Another target's disconnect leaves it alone.
      (remote-transfer--target-disconnected "elsewhere" 'target-disconnect)
      (should (eq (remote-transfer-job-state job) 'running))
      (remote-transfer--target-disconnected "lab" 'target-disconnect)
      (should (eq (remote-transfer-test--wait job) 'cancelled))
      (with-timeout (5 (error "Process survived"))
        (while (process-live-p process)
          (accept-process-output nil 0.05)))
      (should-not (remote-transfer-job-kill-timer job))
      (should-not (remote-transfer-jobs-for-target "lab" t)))))

(ert-deftest remote-files-pop-up-chooses-the-other-side ()
  "Send and fetch go through a pop-up folder that cleans up after itself."
  (skip-unless (remote-client-executable-find "rsync"))
  (remote-transfer-test-with-registry
    (let* ((rsh (expand-file-name "fake-rsh" directory))
           (near (expand-file-name "near/" directory))
           (far (expand-file-name "far/" directory))
           (remote (concat "/fs:box:" far))
           (remote-files-other-history (make-hash-table :test #'equal))
           (remote-files-local-directory near)
           (dired-kill-when-opening-new-dired-buffer nil)
           (before (current-window-configuration))
           (buffers (buffer-list)))
      (with-temp-file rsh (insert "#!/bin/sh\nshift\nexec sh -c \"$*\"\n"))
      (set-file-modes rsh #o700)
      (make-directory near)
      (make-directory far)
      (with-temp-file (expand-file-name "up.txt" near) (insert "up"))
      (with-temp-file (expand-file-name "down.txt" far) (insert "down"))
      ;; A target that shares no filesystem but whose folders Dired can list.
      (remote-register-backend
       "shell-only"
       :capabilities '(metadata file-read directory process-sync process-async)
       :project (lambda (file _link) (remote-file-local-name file))
       :client-shell
       (lambda (_route) (list :command (list rsh) :destination "far-host")))
      (remote-register-target "box" :label "Box")
      (remote-register-pipeline "box" "api" '("shell-only"))
      (should-not (remote-client-file-name remote))
      (unwind-protect
          (with-temp-buffer
            ;; Stand in for the target's Dired: the commands only need its
            ;; mode, folder and marked files.
            (setq default-directory remote)
            (delay-mode-hooks (dired-mode))
            (switch-to-buffer (current-buffer))
            (let ((origin (selected-window)))
              (should (equal (remote-files--here) remote))
              (should (string-match-p "this machine"
                                      (remote-files--menu-title)))
              ;; Fetch: the pop-up shows this machine, as a native folder,
              ;; although the command ran in a buffer of the target.
              (remote-files-fetch)
              (should-not (eq (selected-window) origin))
              (should (equal default-directory near))
              (should (eq (plist-get (remote-files--pick-state) :want)
                          'files))
              (should (string-match-p "confirm"
                                      (remote-files-mode-line-text)))
              (dired-goto-file (expand-file-name "up.txt" near))
              (remote-files-pick-confirm)
              (should (eq (selected-window) origin))
              (should (= (length (window-list nil 'no-mini)) 1))
              (let ((job (car remote-transfer-jobs)))
                (should (equal (remote-transfer-job-destination job) remote))
                (should (eq (remote-transfer-test--wait job) 'done)))
              (should (file-exists-p (expand-file-name "up.txt" far)))
              ;; Send: what is marked here goes to a folder chosen there.
              (cl-letf (((symbol-function 'dired-get-marked-files)
                         (lambda (&rest _)
                           (list (concat remote "down.txt")))))
                (remote-files-send))
              (should (eq (plist-get (remote-files--pick-state) :want)
                          'directory))
              (remote-files-pick-confirm)
              (let ((job (car remote-transfer-jobs)))
                (should (equal (remote-transfer-job-destination job)
                               (concat "/fs:local:" near)))
                (should (eq (remote-transfer-test--wait job) 'done)))
              (should (file-exists-p (expand-file-name "down.txt" near)))
              ;; Cancelling leaves nothing queued and nothing open.
              (let ((count (length remote-transfer-jobs)))
                (remote-files-fetch)
                (remote-files-pick-cancel)
                (should (= (length remote-transfer-jobs) count))
                (should (eq (selected-window) origin))
                (should (= (length (window-list nil 'no-mini)) 1)))
              (should-error (remote-files-pick-confirm) :type 'user-error)
              ;; Every Dired the pop-ups made is gone again.
              (should-not
               (seq-some
                (lambda (buffer)
                  (and (not (memq buffer buffers))
                       (not (eq buffer (current-buffer)))
                       (with-current-buffer buffer
                         (derived-mode-p 'dired-mode))))
                (buffer-list)))))
        (set-window-configuration before)))))

(ert-deftest remote-files-other-side-selection-and-history-are-isolated ()
  "Changing the other machine never reuses another pair's directory."
  (remote-transfer-test-with-registry
    (remote-register-target "box" :label "Box")
    (remote-register-target "lab" :label "Lab")
    (let* ((box "/fs:box:/work/")
           (lab "/fs:lab:/work/")
           (local-a "/fs:local:/tmp/source-a/")
           (local-b "/fs:local:/tmp/source-b/")
           (remote-files-local-directory directory)
           (remote-files-other-history (make-hash-table :test #'equal))
           (remote-files-other-selections (make-hash-table :test #'equal)))
      (should (equal (remote-file-name-target
                      (remote-files--other-start box)) "local"))
      (should (equal (remote-file-name-target
                      (remote-files--other-start lab)) "local"))
      (cl-letf (((symbol-function 'remote-files--here) (lambda () box))
                ((symbol-function 'remote-read-target)
                 (lambda (&rest _) (remote-get-target "lab"))))
        (remote-files-select-other-target)
        (should (string-match-p "<->  lab" (remote-files--menu-title))))
      (should (equal (remote-files--other-start box) "/fs:lab:/"))
      (should (equal (remote-file-name-target
                      (remote-files--other-start lab)) "local"))
      (remote-files--remember-other box "/fs:lab:/shared/")
      (remote-files--remember-other box "/fs:local:/tmp/box-downloads/")
      (should (equal (gethash '("box" . "lab") remote-files-other-history)
                     "/fs:lab:/shared/"))
      (should (equal (gethash '("box" . "local") remote-files-other-history)
                     "/fs:local:/tmp/box-downloads/"))
      (should (equal (remote-files--other-start box)
                     "/fs:local:/tmp/box-downloads/"))
      (should (equal (remote-file-name-target
                      (remote-files--other-start lab)) "local"))
      (puthash local-a "box" remote-files-other-selections)
      (remote-files--remember-other local-a "/fs:box:/out/")
      (should (equal (remote-files--other-start local-a) "/fs:box:/out/"))
      (cl-letf (((symbol-function 'remote-read-target)
                 (lambda (&rest _) (remote-get-target "lab"))))
        (should (equal (remote-files--other-start local-b) "/fs:lab:/")))
      (should (equal (remote-files--other-start local-a) "/fs:box:/out/")))))

(ert-deftest remote-files-picker-can-switch-from-local-to-another-target ()
  "The picker carries its source identity when choosing another machine."
  (remote-transfer-test-with-registry
    (remote-register-target "box" :label "Box")
    (remote-register-target "lab" :label "Lab")
    (let* ((source "/fs:box:/work/")
           (remote-files-other-history (make-hash-table :test #'equal))
           (remote-files-other-selections (make-hash-table :test #'equal))
           (window (selected-window))
           (callback #'ignore)
           closed reopened)
      (set-window-parameter
       window 'remote-files-pick
       (list :source source :want 'directory :description "Destination"
             :callback callback))
      (unwind-protect
          (cl-letf (((symbol-function 'remote-read-target)
                     (lambda (&rest _) (remote-get-target "lab")))
                    ((symbol-function 'remote-files--pick-close)
                     (lambda (_window) (setq closed t)))
                    ((symbol-function 'remote-files--pick)
                     (lambda (&rest args) (setq reopened args))))
            (remote-files-pick-other-target)
            (should closed)
            (should (equal reopened
                           (list "/fs:lab:/" 'directory "Destination"
                                 callback source)))
            (should (equal (gethash source remote-files-other-selections)
                           "lab")))
        (set-window-parameter window 'remote-files-pick nil)))))

(ert-deftest remote-files-copies-between-remote-targets-via-client ()
  "Both target transports are used and the temporary local folder is removed."
  (skip-unless (remote-client-executable-find "rsync"))
  (remote-transfer-test-with-registry
    (let* ((rsh (expand-file-name "fake-rsh" directory))
           (source-dir (expand-file-name "box/" directory))
           (destination-dir (expand-file-name "lab/" directory))
           (source (expand-file-name "payload.txt" source-dir))
           (destination (concat "/fs:lab:" destination-dir))
           (logical-source (concat "/fs:box:" source)))
      (with-temp-file rsh (insert "#!/bin/sh\nshift\nexec sh -c \"$*\"\n"))
      (set-file-modes rsh #o700)
      (make-directory source-dir)
      (make-directory destination-dir)
      (with-temp-file source (insert "from box"))
      (remote-register-backend
       "shell-only"
       :capabilities '(metadata file-read directory process-sync process-async)
       :project (lambda (file _link) (remote-file-local-name file))
       :client-shell
       (lambda (_route) (list :command (list rsh) :destination "fake-host")))
      (dolist (id '("box" "lab"))
        (remote-register-target id)
        (remote-register-pipeline id "api" '("shell-only")))
      (with-temp-buffer
        (setq default-directory (concat "/fs:box:" source-dir))
        (remote-files--queue (list logical-source) destination))
      (let ((download (car remote-transfer-jobs)))
        (should (equal (remote-transfer-job-target-ids download)
                       '("local" "box")))
        (should (equal (remote-route-target-id
                        (remote-transfer-job-route download)) "box"))
        (should (eq (remote-transfer-test--wait download) 'done))
        (let ((upload (car remote-transfer-jobs))
              (stage (remote-file-local-name
                      (remote-transfer-job-destination download))))
          (should-not (eq upload download))
          (should (equal (remote-transfer-job-target-ids upload)
                         '("lab" "local")))
          (should (equal (remote-route-target-id
                          (remote-transfer-job-route upload)) "lab"))
          (should (eq (remote-transfer-test--wait upload) 'done))
          (should (equal (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "payload.txt" destination-dir))
                           (buffer-string))
                         "from box"))
          (should-not (file-exists-p stage))))
      (with-temp-buffer
        (setq default-directory (concat "/fs:box:" source-dir))
        (remote-files--queue
         (list (concat "/fs:box:" (expand-file-name "missing" source-dir)))
         destination))
      (let* ((failed (car remote-transfer-jobs))
             (stage (remote-file-local-name
                     (remote-transfer-job-destination failed))))
        (should (eq (remote-transfer-test--wait failed) 'failed))
        (should-not (file-exists-p stage))
        (with-temp-file (expand-file-name "missing" source-dir)
          (insert "retry works"))
        (let ((retry (remote-transfer-retry failed)))
          (should-not (equal (remote-transfer-job-destination retry)
                             (remote-transfer-job-destination failed)))
          (should (eq (remote-transfer-test--wait retry) 'done))
          (should (eq (remote-transfer-test--wait
                       (car remote-transfer-jobs)) 'done))
          (should (file-exists-p
                   (expand-file-name "missing" destination-dir))))))))

(ert-deftest remote-dirvish-yank-keeps-unrelated-dired-marks ()
  "A copy snapshot and its cleanup belong only to the source Dired buffer."
  (remote-transfer-test-with-registry
    (let* ((first (expand-file-name "first/" directory))
           (second (expand-file-name "second/" directory))
           (file-a (expand-file-name "a.txt" first))
           (file-b (expand-file-name "b.txt" second))
           (+dirvish--paste-method nil)
           (+dirvish--paste-sources nil)
           (+dirvish--paste-source-buffer nil)
           buffer-a buffer-b)
      (make-directory first)
      (make-directory second)
      (with-temp-file file-a (insert "a"))
      (with-temp-file file-b (insert "b"))
      (unwind-protect
          (progn
            (setq buffer-a (dired-noselect first)
                  buffer-b (dired-noselect second))
            (with-current-buffer buffer-b
              (dired-goto-file file-b)
              (dired-mark 1))
            (with-current-buffer buffer-a
              (dired-goto-file file-a)
              (+dirvish/yank)
              (should (equal (+dirvish--marked-everywhere)
                             (list (remote-canonicalize-file-name file-a))))
              (+dirvish--clear-marks))
            (with-current-buffer buffer-b
              (should (equal (dired-map-over-marks (dired-get-filename) nil)
                             (list file-b)))))
        (when (buffer-live-p buffer-a) (kill-buffer buffer-a))
        (when (buffer-live-p buffer-b) (kill-buffer buffer-b))))))

(ert-deftest remote-dirvish-paste-between-machines-is-a-transfer ()
  "Copy here, paste there: across machines it queues instead of blocking."
  (skip-unless (remote-client-executable-find "rsync"))
  (remote-transfer-test-with-registry
    (let* ((rsh (expand-file-name "fake-rsh" directory))
           (near (expand-file-name "near/" directory))
           (far (expand-file-name "far/" directory))
           (+dirvish--paste-method nil)
           source)
      (with-temp-file rsh (insert "#!/bin/sh\nshift\nexec sh -c \"$*\"\n"))
      (set-file-modes rsh #o700)
      (make-directory near)
      (make-directory far)
      (with-temp-file (expand-file-name "doc.txt" near) (insert "doc"))
      (remote-register-backend
       "shell-only"
       :capabilities '(metadata file-read directory process-sync process-async)
       :project (lambda (file _link) (remote-file-local-name file))
       :client-shell
       (lambda (_route) (list :command (list rsh) :destination "far-host")))
      (remote-register-target "box" :label "Box")
      (remote-register-pipeline "box" "api" '("shell-only"))
      (unwind-protect
          (progn
            (setq source (dired-noselect near))
            (with-current-buffer source
              (dired-goto-file (expand-file-name "doc.txt" near))
              (+dirvish/yank)
              (should (eq +dirvish--paste-method 'copy)))
            (with-temp-buffer
              (setq default-directory (concat "/fs:box:" far))
              (delay-mode-hooks (dired-mode))
              ;; The marked file is named for its own machine, not for the
              ;; target whose folder receives it.
              (should (equal (+dirvish--marked-everywhere)
                             (list (concat "/fs:local:" near "doc.txt"))))
              (setq +dirvish--paste-method 'cut)
              (should-error (+dirvish/paste) :type 'user-error)
              (should-not remote-transfer-jobs)
              (setq +dirvish--paste-method 'copy)
              (+dirvish/paste))
            (let ((job (car remote-transfer-jobs)))
              (should (equal (remote-transfer-job-destination job)
                             (concat "/fs:box:" far)))
              (should (eq (remote-transfer-test--wait job) 'done)))
            (should (file-exists-p (expand-file-name "doc.txt" far)))
            (should-not +dirvish--paste-method)
            (with-current-buffer source
              (should-not (+dirvish--marked-everywhere))))
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest remote-files-sync-previews-before-it-changes-anything ()
  (skip-unless (remote-client-executable-find "rsync"))
  (remote-transfer-test-with-registry
    (let* ((near (expand-file-name "near/" directory))
           (far (expand-file-name "far/" directory))
           (before (current-window-configuration)))
      (make-directory near)
      (make-directory far)
      (with-temp-file (expand-file-name "new.txt" near) (insert "new"))
      (with-temp-file (expand-file-name "extra.txt" far) (insert "extra"))
      (unwind-protect
          (progn
            (remote-files--synchronize (concat "/fs:local:" near)
                                       (concat "/fs:local:" far) t t)
            (let ((preview (car remote-transfer-jobs)))
              (should (eq (remote-transfer-test--wait preview) 'done))
              ;; Listed, not done.
              (should (file-exists-p (expand-file-name "extra.txt" far)))
              (should-not (file-exists-p (expand-file-name "new.txt" far)))
              (with-current-buffer (remote-transfer-job-log-buffer preview)
                (should remote-files-compare-mode)
                (should (string-match-p "deleting +extra\\.txt"
                                        (buffer-string)))
                (remote-files-apply)))
            (let ((mirror (car remote-transfer-jobs)))
              (should (eq (remote-transfer-test--wait mirror) 'done)))
            (should (file-exists-p (expand-file-name "new.txt" far)))
            (should-not (file-exists-p (expand-file-name "extra.txt" far))))
        (set-window-configuration before)))))

(ert-deftest remote-files-pick-keeps-popup-when-action-cannot-start ()
  "A failed transfer leaves the chosen folder open for correction and retry."
  (remote-transfer-test-with-registry
    (save-window-excursion
      (let ((origin (selected-window))
            popup folder-buffer chosen)
        (unwind-protect
            (progn
              (remote-files--pick
               directory 'directory "Choose destination"
               (lambda (_destination) (user-error "route unavailable")))
              (setq popup (selected-window)
                    folder-buffer (window-buffer popup))
              (should (remote-files--pick-state popup))
              (should-error (remote-files-pick-confirm) :type 'user-error)
              (should (window-live-p popup))
              (should (eq (window-buffer popup) folder-buffer))
              (should (remote-files--pick-state popup))
              (set-window-parameter
               popup 'remote-files-pick
               (plist-put (remote-files--pick-state popup) :callback
                          (lambda (destination)
                            (setq chosen destination))))
              (remote-files-pick-confirm)
              (should chosen)
              (should (eq (selected-window) origin))
              (should-not (remote-files--pick-state popup)))
          (when (and (window-live-p popup)
                     (remote-files--pick-state popup))
            (remote-files--pick-close popup)))))))

(ert-deftest remote-files-apply-keeps-preview-when-task-cannot-start ()
  "A failed sync launch must leave the preview's apply action available."
  (with-temp-buffer
    (let (quit-called applied)
      (setq-local remote-files--apply
                  (lambda () (user-error "rsync unavailable")))
      (cl-letf (((symbol-function 'quit-window)
                 (lambda (&rest _) (setq quit-called t))))
        (should-error (remote-files-apply) :type 'user-error)
        (should remote-files--apply)
        (should-not quit-called)
        (setq-local remote-files--apply
                    (lambda () (setq applied t)))
        (remote-files-apply)
        (should applied)
        (should quit-called)
        (should-not remote-files--apply)))))

(ert-deftest remote-transfer-completes-once-after-a-late-relay-callback ()
  "A relay callback after transport loss cannot rewrite the task outcome."
  (let* ((log (generate-new-buffer " *remote-transfer-once*"))
         (calls 0)
         (job (remote-transfer-job--create
               :id 700 :name "late callback" :state 'running
               :started (float-time) :log-buffer log
               :callback (lambda (_job) (cl-incf calls)))))
    (unwind-protect
        (progn
          (remote-transfer--complete job 1 'failed)
          (remote-transfer--complete job 0 'done)
          (should (eq (remote-transfer-job-state job) 'failed))
          (should (= (remote-transfer-job-exit-status job) 1))
          (should (= calls 1)))
      (when (buffer-live-p log) (kill-buffer log)))))

(ert-deftest remote-transfer-live-target-cancel ()
  "Cancel a real target process through the transfer queue's disconnect hook."
  (let ((target (getenv "REMOTE_TRANSFER_E2E_TARGET")))
    (skip-unless (and target (not (string-empty-p target))))
    (remote-config-load
     (expand-file-name "etc/remote.json" user-emacs-directory))
    (let* ((job (remote-transfer--launch
                 (remote-transfer-job--create
                  :id (cl-incf remote-transfer--next-id)
                  :name "live cancellation probe"
                  :kind 'copy :execution 'target
                  :destination (format "/fs:%s:/tmp/" target)
                  :target-ids (list target)
                  :command '("sleep" "30"))))
           (process (remote-transfer-job-process job)))
      (should (process-live-p process))
      (remote-transfer--target-disconnected "unrelated" 'target-disconnect)
      (should (eq (remote-transfer-job-state job) 'running))
      (remote-transfer--target-disconnected target 'target-disconnect)
      (should (eq (remote-transfer-job-state job) 'cancelled))
      (with-timeout (10 (error "Target process survived cancellation"))
        (while (or (process-live-p process)
                   (remote-transfer-job-process job))
          (accept-process-output nil 0.05)))
      (should-not (remote-transfer-job-kill-timer job)))))

(ert-deftest remote-transfer-live-transport-death ()
  "A lost SSH transport finishes its queued target process without hanging."
  (let ((target (getenv "REMOTE_TRANSFER_E2E_TARGET")))
    (skip-unless (and target (not (string-empty-p target))))
    (remote-config-load
     (expand-file-name "etc/remote.json" user-emacs-directory))
    (let* ((job (remote-transfer--launch
                 (remote-transfer-job--create
                  :id (cl-incf remote-transfer--next-id)
                  :name "live transport loss probe"
                  :kind 'copy :execution 'target
                  :destination (format "/fs:%s:/tmp/" target)
                  :target-ids (list target)
                  :command '("sleep" "30"))))
           (relay (remote-transfer-job-process job))
           (route (process-get relay 'remote-route)))
      (unwind-protect
          (progn
            (unless (and route
                         (member (remote-route-link-plugin-id route)
                                 '("tramp" "tramp-rpc")))
              (ert-skip
               (format "Exec route uses %S"
                       (and route (remote-route-link-plugin-id route)))))
            (let* ((physical
                    (remote-project-file-name
                     (remote-transfer-job-destination job) route))
                   (vector (tramp-dissect-file-name physical))
                   (transport
                    (if (equal (remote-route-link-plugin-id route)
                               "tramp-rpc")
                        (plist-get (tramp-rpc--get-connection vector)
                                   :process)
                      (tramp-get-connection-process vector))))
              (should (process-live-p relay))
              (should (process-live-p transport))
              (should (eq (remote-transfer-job-transport-process job)
                          transport))
              ;; Only the connection owned by this batch Emacs is removed.
              (delete-process transport)
              (with-timeout
                  (10 (error "Transfer survived transport loss: state=%S relay=%S transport=%S"
                             (remote-transfer-job-state job)
                             (process-status relay)
                             (process-status transport)))
                (while (or (eq (remote-transfer-job-state job) 'running)
                           (remote-transfer-job-process job))
                  (accept-process-output nil 0.05)))
              (should (eq (remote-transfer-job-state job) 'failed))
              (should-not (remote-transfer-job-exit-status job))
              (should (equal (remote-transfer--state-text job)
                             "failed (status unknown)"))
              (should-not (process-live-p relay))
              (should-not (remote-transfer-job-kill-timer job))
              (with-current-buffer (remote-transfer-job-log-buffer job)
                (should (string-match-p "remote command outcome is unknown"
                                        (buffer-string))))
              (should-not (remote-transfer-jobs-for-target target t))))
        (remote-transfer-cancel job)))))

(ert-deftest remote-transfer-live-round-trip ()
  "Upload, download and compare against a real SSH target."
  (let ((target (getenv "REMOTE_TRANSFER_E2E_TARGET")))
    (skip-unless (and target (not (string-empty-p target))))
    (remote-config-load
     (expand-file-name "etc/remote.json" user-emacs-directory))
    (let* ((local (file-name-as-directory
                   (make-temp-file "remote-transfer-live-" t)))
           (back (file-name-as-directory
                  (make-temp-file "remote-transfer-back-" t)))
           (name (format "emacs-transfer-%d" (emacs-pid)))
           (remote (format "/fs:%s:/tmp/%s/" target name))
           dired-buffer)
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "payload.bin" local)
              (insert (make-string 400000 ?z)))
            (should (plist-get (remote-transfer-endpoint remote) :shell))
            (let ((up (remote-transfer-start (list local) remote
                                             :contents t)))
              (should (eq (remote-transfer-test--wait up) 'done)))
            (let ((down (remote-transfer-start (list remote) back
                                               :contents t)))
              (should (eq (remote-transfer-test--wait down) 'done)))
            (should (= (file-attribute-size
                        (file-attributes
                         (expand-file-name "payload.bin" back)))
                       400000))
            ;; Use the same marked-file and directory-at-point entry as
            ;; Dirvish.  The client cannot read these target paths directly,
            ;; so every operation must become a target-side queue job.
            (let ((source (expand-file-name "payload.bin" remote)))
              (dolist (method '(copy symlink relative-symlink hardlink move))
                (make-directory
                 (expand-file-name (format "out-%s" method) remote)))
              (setq dired-buffer (dired-noselect remote))
              (dolist (method '(copy symlink relative-symlink hardlink move))
                (let* ((destination
                        (expand-file-name (format "out-%s" method) remote))
                       (output (expand-file-name "payload.bin" destination))
                       (previous-job (car remote-transfer-jobs))
                       job)
                  ;; Preflight caches this absence.  Completion must evict it.
                  (should-not (file-exists-p output))
                  (with-current-buffer dired-buffer
                    (should (dired-goto-file source))
                    (dired-mark 1)
                    (should (dired-goto-file destination))
                    (funcall
                     (pcase method
                       ('copy #'+dirvish/copy-here)
                       ('move #'+dirvish/move-here)
                       ('symlink #'+dirvish/symlink)
                       ('relative-symlink #'+dirvish/relative-symlink)
                       ('hardlink #'+dirvish/hardlink))))
                  (setq job (car remote-transfer-jobs))
                  (should-not (eq job previous-job))
                  (should (eq (remote-transfer-job-execution job) 'target))
                  (should (equal (remote-transfer-job-target-ids job)
                                 (list target)))
                  (should (eq (remote-transfer-test--wait job) 'done))
                  (should (file-exists-p output))
                  (pcase method
                    ('copy
                     (should (= (file-attribute-size
                                 (file-attributes output)) 400000)))
                    ('move (should-not (file-exists-p source)))
                    ((or 'symlink 'relative-symlink)
                     (should (file-symlink-p output))
                     (should (equal (file-truename output)
                                    (file-truename source)))
                     (when (eq method 'relative-symlink)
                       (should-not
                        (file-name-absolute-p (file-symlink-p output)))))
                    ('hardlink
                     (should (= (file-attribute-link-number
                                 (file-attributes source)) 2))))))))
        (when (buffer-live-p dired-buffer)
          (kill-buffer dired-buffer))
        (ignore-errors
          (let* ((shell (plist-get (remote-transfer-endpoint remote) :shell))
                 (default-directory temporary-file-directory))
            (apply #'call-process (car (plist-get shell :command)) nil nil nil
                   (append (cdr (plist-get shell :command))
                           (list (plist-get shell :destination)
                                 (format "rm -rf /tmp/%s" name))))))
        (delete-directory local t)
        (delete-directory back t)))))

(provide 'remote-transfer-tests)
;;; remote-transfer-tests.el ends here
