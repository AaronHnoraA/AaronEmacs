;;; remote-machines.el --- Remembered ad-hoc SSH machines -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A temporary machine is a target typed in by hand: an address, a user and
;; either a password or a key file.  It is a third source of targets beside
;; `etc/remote.json' and the SSH config imports, registered through
;; `remote-config-source-functions', so it is an ordinary target everywhere
;; else: the Remote board, routing, workspaces and the file manager.
;;
;; Machines are runtime state, not configuration.  They live in
;; `remote-machines-file' under var/, which also holds a stored password.
;; Two files are derived from it: a netrc file that TRAMP reads through
;; auth-source, and an askpass helper for client tools such as rsync that
;; open a connection of their own.  `remote-machines' is the
;; management entry.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-config)

(declare-function remote-workspace-disconnect-target
                  "remote-workspace" (target-id &optional reason))
(declare-function remote-open-target "remote-board" (&optional target))
(declare-function remote-board--refresh-visible-views "remote-board" ())
(declare-function remote-files "remote-files"
                  (&optional remote-directory local-directory))
(declare-function auth-source-forget-all-cached "auth-source" ())
(declare-function evil-set-initial-state "evil-core" (mode state))

(defvar auth-sources)
(defvar remote-backend-tramp-client-environment-functions)

(defvar remote-machines-directory
  (expand-file-name "var/remote/" user-emacs-directory)
  "Directory holding remembered machines and the files derived from them.")

(defvar remote-machines-file
  (expand-file-name "machines.json" remote-machines-directory)
  "JSON file remembering temporary machines, stored passwords included.")

(defvar remote-machines-auth-file
  (expand-file-name "machines.authinfo" remote-machines-directory)
  "Netrc file derived from `remote-machines-file' for auth-source.")

(defvar remote-machines-askpass-file
  (expand-file-name "askpass.sh" remote-machines-directory)
  "Askpass helper answering OpenSSH from the client tool's environment.")

(defconst remote-machines-secret-variable "REMOTE_MACHINE_SECRET"
  "Environment variable through which the askpass helper gets a password.")

(defvar remote-machines--list nil
  "Remembered machines, each a plist.
Keys are :id, :label, :host, :user, :port, :identity and :password.")

(defvar remote-machines--loaded nil
  "Non-nil once `remote-machines-file' has been read.")

(defvar remote-machines--revision nil
  "Digest of the machine file when `remote-machines--list' was last loaded.
The symbol `:missing' means no file existed at that time.")

(defvar-local remote-machines--owner-frame nil
  "Frame whose temporary-machine list this buffer displays.")

(defun remote-machines--private-write (file content &optional modes)
  "Atomically write CONTENT to FILE with MODES, by default owner-only."
  (let ((directory (file-name-directory file))
        (temporary nil))
    (unless (file-directory-p directory)
      (make-directory directory t)
      (set-file-modes directory #o700))
    (unwind-protect
        (progn
          (setq temporary
                (make-temp-file (expand-file-name ".machines-" directory)))
          (set-file-modes temporary (or modes #o600))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region content nil temporary nil 'silent))
          (rename-file temporary file t)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun remote-machines--file-revision ()
  "Return a content digest of the machine file, or `:missing'."
  (if (file-exists-p remote-machines-file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents remote-machines-file))
        (secure-hash 'sha256 (current-buffer)))
    :missing))

(defun remote-machines--read ()
  "Return the machines stored in `remote-machines-file'.
Also remember the exact revision read for later conflict detection."
  (if (file-exists-p remote-machines-file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents remote-machines-file))
        (let* ((revision (secure-hash 'sha256 (current-buffer)))
               (root
                (json-parse-buffer :object-type 'plist :array-type 'list
                                   :null-object nil :false-object nil))
               (machines
                (seq-filter
                 (lambda (machine)
                   (and (stringp (plist-get machine :id))
                        (stringp (plist-get machine :host))))
                 (plist-get root :machines))))
          (setq remote-machines--revision revision)
          machines))
    (setq remote-machines--revision :missing)
    nil))

(defun remote-machines-list ()
  "Return the remembered machines, reading them on first use."
  (unless remote-machines--loaded
    (setq remote-machines--list (remote-machines--read)
          remote-machines--loaded t))
  remote-machines--list)

(defun remote-machines-get (id)
  "Return the remembered machine whose target ID is ID."
  (seq-find (lambda (machine) (equal (plist-get machine :id) id))
            (remote-machines-list)))

(defun remote-machines--compact (machine)
  "Return MACHINE without its empty fields."
  (cl-loop for (key value) on machine by #'cddr
           when (and value (not (equal value "")))
           append (list key value)))

(defun remote-machines--auth-hosts (machine)
  "Return the host spellings TRAMP may look MACHINE's password up under."
  (let ((host (plist-get machine :host))
        (port (plist-get machine :port)))
    (if (and port (/= port 22))
        (list host (format "%s#%d" host port))
      (list host))))

(defun remote-machines--netrc-token (value)
  "Return VALUE spelled as one netrc token, or nil when it has no spelling.
The netrc reader knows two quoting styles and no escapes."
  (cond
   ((string-match-p "[\n\r]" value) nil)
   ((not (string-match-p "\\`[\"'#]\\|[[:space:]]" value)) value)
   ((not (string-search "\"" value)) (concat "\"" value "\""))
   ((not (string-search "'" value)) (concat "'" value "'"))))

(defun remote-machines--write-auth-file ()
  "Regenerate `remote-machines-auth-file' from the remembered machines."
  (let ((lines
         (cl-loop
          for machine in remote-machines--list
          for password = (plist-get machine :password)
          for token = (and password (remote-machines--netrc-token password))
          when token
          append
          (mapcar
           (lambda (host)
             (format "machine %s login %s password %s\n"
                     host (plist-get machine :user) token))
           (remote-machines--auth-hosts machine)))))
    (if lines
        (remote-machines--private-write
         remote-machines-auth-file (apply #'concat lines))
      (when (file-exists-p remote-machines-auth-file)
        (delete-file remote-machines-auth-file)))
    (when (fboundp 'auth-source-forget-all-cached)
      (auth-source-forget-all-cached))))

(defun remote-machines--save ()
  "Persist the remembered machines and the files derived from them."
  (remote-machines--private-write
   remote-machines-file
   (concat
    (json-serialize
     (list :version 1
           :machines
           (vconcat (mapcar #'remote-machines--compact
                            remote-machines--list))))
    "\n"))
  (remote-machines--write-auth-file))

(defun remote-machines--backup-file (file)
  "Make a private rollback copy of FILE, or return nil if it does not exist."
  (when (file-exists-p file)
    (let ((backup
           (make-temp-file
            (expand-file-name ".machines-backup-"
                              (file-name-directory file)))))
      (condition-case error
          (progn
            (copy-file file backup t)
            (set-file-modes backup #o600)
            backup)
        (error
         (delete-file backup)
         (signal (car error) (cdr error)))))))

(defun remote-machines--restore-file (file backup)
  "Restore FILE from BACKUP, or remove it if it did not exist before."
  (if backup
      (rename-file backup file t)
    (when (file-exists-p file)
      (delete-file file))))

(defun remote-machines--with-lock (function)
  "Call FUNCTION while holding Emacs's native lock for the machine file."
  (let ((create-lockfiles t)
        ;; This one shared state file must use the same lock path even when
        ;; different Emacs instances configure their editor lock directories
        ;; differently.
        (lock-file-name-transforms nil)
        (directory (file-name-directory remote-machines-file))
        locked)
    (unless (file-directory-p directory)
      (make-directory directory t)
      (set-file-modes directory #o700))
    (unwind-protect
        (progn
          (when (eq (file-locked-p remote-machines-file) t)
            (user-error "Temporary machines are already being updated by this Emacs"))
          (with-temp-buffer
            (cl-letf (((symbol-function 'ask-user-about-lock)
                       (lambda (_file _opponent)
                         (user-error "Temporary machines are being updated in another Emacs"))))
              (lock-file remote-machines-file)))
          (setq locked t)
          (funcall function))
      (when locked
        (unlock-file remote-machines-file)))))

(defun remote-machines--commit-locked (machines &optional after-save)
  "Persist MACHINES and reload their targets as one user action.
The caller holds the native lock on `remote-machines-file'.
AFTER-SAVE runs after the files are written but before the registry reload;
removing a machine uses it to disconnect while the old target still exists.
If saving or reloading fails, restore the previous in-memory list and both
private files.  A failed rollback keeps the private backups for recovery."
  (let ((previous remote-machines--list)
        (generation remote-config-generation)
        (targets remote-targets)
        (pipelines remote-pipelines)
        (files (list remote-machines-file remote-machines-auth-file))
        backups retain-backups)
    (unwind-protect
        (progn
          (dolist (file files)
            (push (cons file (remote-machines--backup-file file)) backups))
          (condition-case original-error
              (progn
                (let ((remote-machines--list machines))
                  (remote-machines--save))
                (setq remote-machines--list machines)
                (when after-save
                  (funcall after-save))
                (remote-machines--reload))
            (error
             (setq remote-machines--list previous)
             (condition-case rollback-error
                 (progn
                   (dolist (entry backups)
                     (remote-machines--restore-file (car entry) (cdr entry)))
                   (when (fboundp 'auth-source-forget-all-cached)
                     (auth-source-forget-all-cached))
                   ;; A reload hook may fail after the registry has committed.
                   ;; Reapply the restored machine list in that case.
                   (when (or (/= generation remote-config-generation)
                             (not (eq targets remote-targets))
                             (not (eq pipelines remote-pipelines)))
                     (remote-config-reload)))
               (error
                (setq retain-backups t)
                (error "Machine update failed: %s; rollback failed: %s (backups: %S)"
                       (error-message-string original-error)
                       (error-message-string rollback-error)
                       (delq nil (mapcar #'cdr backups)))))
             (signal (car original-error) (cdr original-error)))))
      (unless retain-backups
        (dolist (entry backups)
          (when (and (cdr entry) (file-exists-p (cdr entry)))
            (delete-file (cdr entry))))))))

(defun remote-machines--commit (machines &optional after-save)
  "Persist MACHINES after checking for edits from another Emacs session."
  (remote-machines-list)
  (remote-machines--with-lock
   (lambda ()
     (unless (equal remote-machines--revision
                    (remote-machines--file-revision))
       (user-error
        "Temporary machines changed in another Emacs; run remote-machines-reload-from-disk"))
     (remote-machines--commit-locked machines after-save)
     (setq remote-machines--revision
           (remote-machines--file-revision)))))

(defun remote-machines--askpass ()
  "Return the askpass helper, writing it when it is missing."
  (unless (file-executable-p remote-machines-askpass-file)
    (remote-machines--private-write
     remote-machines-askpass-file
     (format "#!/bin/sh\nprintf '%%s\\n' \"$%s\"\n"
             remote-machines-secret-variable)
     #o700))
  remote-machines-askpass-file)

(defun remote-machines--ssh-options (machine)
  "Return the OpenSSH options MACHINE's pipeline carries."
  (append
   ;; A machine typed in by hand has no known_hosts entry yet; a changed key
   ;; is still refused.
   (list "StrictHostKeyChecking=accept-new")
   (when-let* ((identity (plist-get machine :identity)))
     (list (concat "IdentityFile=" identity) "IdentitiesOnly=yes"))
   (when (plist-get machine :password)
     ;; Keys offered by an agent first can exhaust the server's attempts.
     (list "PreferredAuthentications=password,keyboard-interactive"
           "PubkeyAuthentication=no"
           "NumberOfPasswordPrompts=1"))))

(defun remote-machines-register ()
  "Register every remembered machine as a target with one SSH pipeline.
Run from `remote-config-source-functions'.  A machine whose ID a configured
or imported target already uses is skipped: the configuration wins."
  (dolist (machine (remote-machines-list))
    (let ((id (plist-get machine :id)))
      (if (remote-get-target id)
          (remote-log
           'config-warning
           :message (format "Temporary machine %s is shadowed by a target" id))
        (remote-register-target
         id
         :label (or (plist-get machine :label) id)
         :preferences '((pty "ssh-pty" "tramp") (default "tramp"))
         :trusted nil
         :source 'remote-machines)
        (remote-register-pipeline
         id "ssh" '("ssh-pty" "tramp")
         :priority 50
         :config
         (append
          (list :host (plist-get machine :host) :method "ssh")
          (when-let* ((user (plist-get machine :user)))
            (list :user user))
          (when-let* ((port (plist-get machine :port))
                      ((/= port 22)))
            (list :port port))
          (list :ssh-options (remote-machines--ssh-options machine)))
         :source 'remote-machines)))))

(defun remote-machines-target-p (target)
  "Return non-nil when TARGET is a registered temporary machine."
  (when-let* ((target (if (remote-target-p target)
                          target
                        (remote-get-target target))))
    (eq (remote-target-source target) 'remote-machines)))

(defun remote-machines-client-environment (route)
  "Return the askpass environment for ROUTE's temporary machine, if any.
A client tool that opens its own SSH connection has no terminal to ask on."
  (when-let* ((id (remote-route-target-id route))
              ((remote-machines-target-p id))
              (machine (remote-machines-get id))
              (password (plist-get machine :password)))
    `(("SSH_ASKPASS" . ,(remote-machines--askpass))
      ("SSH_ASKPASS_REQUIRE" . "force")
      (,remote-machines-secret-variable . ,password))))

(defun remote-machines--reload ()
  "Reload targets after the remembered machines changed."
  (remote-config-reload)
  (when (fboundp 'remote-board--refresh-visible-views)
    (remote-board--refresh-visible-views))
  (remote-machines--refresh-visible-views))

;;;###autoload
(defun remote-machines-reload-from-disk ()
  "Read machine changes made by another Emacs and refresh Remote views."
  (interactive)
  (remote-machines--with-lock
   (lambda ()
     (setq remote-machines--list (remote-machines--read)
           remote-machines--loaded t)
     (remote-machines--reload)))
  (message "Temporary machines reloaded from disk"))

(defun remote-machines--validate (machine &optional replacing)
  "Signal a `user-error' unless MACHINE can be stored.
REPLACING is the ID of the machine being edited, which may keep its ID."
  (let ((id (plist-get machine :id))
        (host (plist-get machine :host))
        (user (plist-get machine :user))
        (port (plist-get machine :port))
        (identity (plist-get machine :identity)))
    (unless (and (stringp host)
                 (string-match-p "\\`[[:alnum:]][[:alnum:]._:%-]*\\'" host))
      (user-error "Invalid host name or address: %S" host))
    (unless (and (stringp user)
                 (string-match-p "\\`[[:alnum:]_][[:alnum:]._@-]*\\'" user))
      (user-error "Invalid user name: %S" user))
    (unless (and (integerp port) (<= 1 port 65535))
      (user-error "SSH port must be between 1 and 65535: %S" port))
    (when identity
      (when (string-match-p "[[:space:]\"'\\\\]" identity)
        (user-error "Key file name may not contain spaces or quotes: %s"
                    identity))
      (unless (file-readable-p identity)
        (user-error "Key file is not readable: %s" identity)))
    (unless (equal id replacing)
      (when (or (remote-machines-get id) (remote-get-target id))
        (user-error "A target named %s already exists" id)))))

(defun remote-machines--parse-address (address)
  "Split ADDRESS, spelled [USER@]HOST[:PORT], into (HOST USER PORT)."
  (let ((address (string-trim address)) user port)
    (when (string-match "\\`\\([^@]+\\)@\\(.+\\)\\'" address)
      (setq user (match-string 1 address)
            address (match-string 2 address)))
    (cond
     ((string-match "\\`\\[\\(.+\\)\\]\\(?::\\([0-9]+\\)\\)?\\'" address)
      (setq port (match-string 2 address)
            address (match-string 1 address)))
     ((string-match "\\`\\([^:]+\\):\\([0-9]+\\)\\'" address)
      (setq port (match-string 2 address)
            address (match-string 1 address))))
    (list address user (and port (string-to-number port)))))

(defun remote-machines--read-machine (&optional old)
  "Read a machine from the minibuffer, starting from OLD when editing."
  (pcase-let*
      ((`(,host ,user ,port)
        (remote-machines--parse-address
         (read-string
          "Address ([user@]host[:port]): "
          (and old
               (format "%s@%s%s" (plist-get old :user) (plist-get old :host)
                       (if-let* ((port (plist-get old :port))
                                 ((/= port 22)))
                           (format ":%d" port)
                         ""))))))
       (user (or user (read-string "User: " (user-login-name))))
       (port (or port (read-number "SSH port: " 22)))
       (method
        (completing-read
         "Sign in with: " '("password" "key file" "agent or SSH defaults")
         nil t nil nil
         (cond ((and old (plist-get old :identity)) "key file")
               ((and old (not (plist-get old :password)))
                "agent or SSH defaults")
               (t "password"))))
       (password
        (when (equal method "password")
          (let ((typed
                 (read-passwd
                  (if (and old (plist-get old :password))
                      "Password (empty keeps the stored one): "
                    "Password: "))))
            (if (string-empty-p typed)
                (or (and old (plist-get old :password))
                    (user-error "No password given"))
              typed))))
       (identity
        (when (equal method "key file")
          (expand-file-name
           (read-file-name "Key file: " "~/.ssh/"
                           (and old (plist-get old :identity)) t
                           (and old (plist-get old :identity))))))
       (label (read-string "Name: " (or (and old (plist-get old :label))
                                        host))))
    (list :id (or (and old (plist-get old :id)) (remote-fs--slug label))
          :label label :host host :user user :port port
          :identity identity :password password)))

;;;###autoload
(defun remote-machines-add (machine)
  "Remember MACHINE, a plist, as a temporary target and return the target.
Interactively, ask for its address and how to sign in.  A password is kept
in `remote-machines-file', readable by its owner only."
  (interactive (list (remote-machines--read-machine)))
  (remote-machines-list)
  (remote-machines--validate machine)
  (remote-machines--commit
   (append remote-machines--list (list machine)))
  (message "Remembered %s as target %s%s"
           (plist-get machine :label) (plist-get machine :id)
           (if (and (plist-get machine :password)
                    (not (remote-machines--netrc-token
                          (plist-get machine :password))))
               "; its password mixes both quote characters, so opening files will ask for it"
             ""))
  (remote-get-target (plist-get machine :id)))

(defun remote-machines--read-id (prompt)
  "Read a remembered machine's ID with PROMPT."
  (or (and (derived-mode-p 'remote-machines-mode) (tabulated-list-get-id))
      (let ((table
             (mapcar
              (lambda (machine)
                (cons (format "%-24s %s" (plist-get machine :label)
                              (remote-machines--address machine))
                      (plist-get machine :id)))
              (or (remote-machines-list)
                  (user-error "No temporary machine is remembered")))))
        (cdr (assoc (completing-read prompt table nil t) table)))))

(defun remote-machines-edit (id)
  "Change the remembered machine ID.
A changed address or credential closes the sessions that used the old one."
  (interactive (list (remote-machines--read-id "Edit machine: ")))
  (let* ((old (or (remote-machines-get id)
                  (user-error "No temporary machine named %s" id)))
         (new (remote-machines--read-machine old)))
    (remote-machines--validate new id)
    (remote-machines--commit
     (mapcar (lambda (machine) (if (eq machine old) new machine))
             remote-machines--list))
    ;; A new password can leave the pipeline configuration byte-for-byte
    ;; unchanged.  Reload alone then keeps a session authenticated with the
    ;; previous credential.
    (when (and (not (equal (plist-get old :password)
                           (plist-get new :password)))
               (remote-machines-target-p id))
      (remote-config-invalidate-target-sessions id))
    (message "Updated %s" (plist-get new :label))))

(defun remote-machines-forget-password (id)
  "Drop the stored password of machine ID, keeping the machine."
  (interactive (list (remote-machines--read-id "Forget password of: ")))
  (let ((machine (or (remote-machines-get id)
                     (user-error "No temporary machine named %s" id))))
    (remote-machines--commit
     (mapcar (lambda (entry)
               (if (eq entry machine)
                   (plist-put (copy-sequence entry) :password nil)
                 entry))
             remote-machines--list))
    (message "Forgot the password of %s" (plist-get machine :label))))

(defun remote-machines-remove (id)
  "Forget machine ID: save its removal, then disconnect its target."
  (interactive
   (let ((id (remote-machines--read-id "Forget machine: ")))
     (unless (yes-or-no-p (format "Forget machine %s and its credentials? "
                                  id))
       (user-error "Kept %s" id))
     (list id)))
  (let ((machine (or (remote-machines-get id)
                     (user-error "No temporary machine named %s" id))))
    (remote-machines--commit
     (seq-remove (lambda (entry) (eq entry machine))
                 remote-machines--list)
     (lambda ()
       (when (and (remote-machines-target-p id)
                  (fboundp 'remote-workspace-disconnect-target))
         (remote-workspace-disconnect-target id 'machine-removed))))
    (message "Forgot %s" (plist-get machine :label))))

;;; Management list

(defun remote-machines--address (machine)
  "Return MACHINE's address as USER@HOST[:PORT]."
  (format "%s@%s%s" (plist-get machine :user) (plist-get machine :host)
          (if-let* ((port (plist-get machine :port))
                    ((/= port 22)))
              (format ":%d" port)
            "")))

(defun remote-machines--entries ()
  "Return `tabulated-list-entries' for the remembered machines."
  (mapcar
   (lambda (machine)
     (let ((id (plist-get machine :id)))
       (list
        id
        (vector
         (or (plist-get machine :label) id)
         id
         (remote-machines--address machine)
         (cond ((plist-get machine :password) "password (stored)")
               ((plist-get machine :identity)
                (abbreviate-file-name (plist-get machine :identity)))
               (t "agent / SSH defaults"))
         (if (remote-machines-target-p id) "" "shadowed by a target")))))
   (remote-machines-list)))

(defun remote-machines-refresh ()
  "Redraw the machine list from the remembered machines."
  (interactive)
  (when (derived-mode-p 'remote-machines-mode)
    (setq tabulated-list-entries (remote-machines--entries))
    (tabulated-list-print t)))

(defun remote-machines--visible-views ()
  "Return temporary-machine lists visible in their owning frames."
  (cl-loop for frame in (frame-list)
           for buffer = (frame-parameter frame 'remote-machines-view-buffer)
           when (and (buffer-live-p buffer)
                     (get-buffer-window buffer frame))
           collect buffer))

(defun remote-machines--refresh-visible-views ()
  "Refresh all visible temporary-machine lists from the shared records."
  (dolist (buffer (remote-machines--visible-views))
    (with-current-buffer buffer
      (remote-machines-refresh))))

(defun remote-machines-open (id)
  "Open the default folder of machine ID."
  (interactive (list (remote-machines--read-id "Open machine: ")))
  (remote-open-target
   (or (remote-get-target id)
       (user-error "Machine %s is not registered as a target" id))))

(defun remote-machines-files (id)
  "Open the file manager on machine ID."
  (interactive (list (remote-machines--read-id "Manage files on: ")))
  (remote-files
   (remote-target-file-name
    (or (remote-get-target id)
        (user-error "Machine %s is not registered as a target" id)))))

(defvar-keymap remote-machines-mode-map
  :parent tabulated-list-mode-map
  "q" #'quit-window
  "RET" #'remote-machines-open
  "o" #'remote-machines-open
  "f" #'remote-machines-files
  "a" #'remote-machines-add
  "e" #'remote-machines-edit
  "p" #'remote-machines-forget-password
  "k" #'remote-machines-remove
  "D" #'remote-machines-remove
  "g" #'remote-machines-reload-from-disk)

(define-derived-mode remote-machines-mode tabulated-list-mode "Machines"
  "Mode for the temporary machines remembered under var/.
Add a machine with a, change it with e, forget it with k and only its
password with p.  RET opens its folder and f its file manager."
  (setq tabulated-list-format
        [("Name" 22 t)
         ("ID" 20 t)
         ("Address" 34 t)
         ("Sign-in" 30 nil)
         ("Note" 20 nil)])
  (setq tabulated-list-padding 2
        tabulated-list-entries (remote-machines--entries))
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'remote-machines--view-killed nil t))

(with-eval-after-load 'evil
  (evil-set-initial-state 'remote-machines-mode 'emacs))

(defun remote-machines--view-killed ()
  "Clear this list's frame reference when its buffer is killed."
  (when (and (frame-live-p remote-machines--owner-frame)
             (eq (frame-parameter remote-machines--owner-frame
                                  'remote-machines-view-buffer)
                 (current-buffer)))
    (set-frame-parameter remote-machines--owner-frame
                         'remote-machines-view-buffer nil)))

(defun remote-machines--frame-deleted (frame)
  "Release FRAME's machine list without changing remembered machines."
  (when-let* ((buffer (frame-parameter frame 'remote-machines-view-buffer)))
    (set-frame-parameter frame 'remote-machines-view-buffer nil)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(add-hook 'delete-frame-functions #'remote-machines--frame-deleted)

(defun remote-machines--buffer (&optional frame)
  "Return FRAME's temporary-machine list, creating it when needed."
  (let* ((frame (or frame (selected-frame)))
         (existing (frame-parameter frame 'remote-machines-view-buffer))
         (created (not (buffer-live-p existing)))
         (buffer (if created
                     (generate-new-buffer "*Remote Machines*")
                   existing)))
    (when created
      (set-frame-parameter frame 'remote-machines-view-buffer buffer))
    (condition-case error
        (with-current-buffer buffer
          (unless (derived-mode-p 'remote-machines-mode)
            (remote-machines-mode))
          (setq-local remote-machines--owner-frame frame)
          (remote-machines-refresh))
      (error
       (when created
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (when (eq (frame-parameter frame 'remote-machines-view-buffer)
                   buffer)
           (set-frame-parameter frame 'remote-machines-view-buffer nil)))
       (signal (car error) (cdr error))))
    buffer))

;;;###autoload
(defun remote-machines ()
  "List the remembered temporary machines and manage them."
  (interactive)
  (pop-to-buffer (remote-machines--buffer)
                 '(nil (reusable-frames . nil))))

(add-hook 'remote-config-source-functions #'remote-machines-register)
(add-hook 'remote-backend-tramp-client-environment-functions
          #'remote-machines-client-environment)

(with-eval-after-load 'auth-source
  (add-to-list 'auth-sources remote-machines-auth-file t))

(provide 'remote-machines)
;;; remote-machines.el ends here
