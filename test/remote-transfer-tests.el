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

(when (boundp 'comp-enable-subr-trampolines)
  (setq comp-enable-subr-trampolines nil))

(remote-fs-install)

(defmacro remote-transfer-test-with-registry (&rest body)
  "Evaluate BODY with isolated registries, machines and transfer state."
  (declare (indent 0) (debug t))
  `(let* ((directory (file-name-as-directory
                      (make-temp-file "remote-transfer-test-" t)))
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
           (remote (format "/fs:%s:/tmp/%s/" target name)))
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
                       400000)))
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
