;;; init-aaronnote-jupyter-runtime.el --- Remote-owned Noema kernels -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Noema's Node service is the sole raw-ZMQ client and kernel registry.  This
;; module is only its Remote/process broker: it discovers kernelspecs, places a
;; target process, prepares connection files, and forwards five opaque ports.
;; It never decodes or sends a Jupyter message.  The same broker is available
;; for target `local'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'remote-channel)
(require 'remote-doctor)
(require 'remote-fs)
(require 'remote-gateway)
(require 'remote-process)
(require 'remote-workspace)
(require 'init-jupyter-command)

(cl-defstruct (my/noema-jupyter-runtime
               (:constructor my/noema-jupyter-runtime-create))
  id manager-id context workspace placement-file kernel spec pid connection-file
  target-connection client-connection channel-group generation state
  state-lost log-file resource-directory host-name)

(cl-defstruct (my/noema-jupyter-attachment
               (:constructor my/noema-jupyter-attachment-create))
  id token context workspace placement-file connection-file target-connection
  client-connection channel-group generation state)

(defvar my/noema-jupyter-runtimes (make-hash-table :test #'equal)
  "Noema Jupyter runtimes keyed by opaque runtime ID.")

(defvar my/noema-jupyter-attachments (make-hash-table :test #'equal)
  "Noema Jupyter attachment routes keyed by opaque attachment ID.")

(defvar my/noema-jupyter-pending-directory
  (expand-file-name "var/aaronnote/jupyter/pending-shutdown/"
                    user-emacs-directory)
  "Client-local records for brokered kernels whose shutdown is unconfirmed.")

(defvar my/noema-jupyter-shutdown-grace-seconds 3.0
  "Seconds to wait for a brokered kernel to exit after TERM.
An unconfirmed exit keeps its runtime and durable shutdown record for retry.")

(defvar my/noema-jupyter--pending-record-errors nil
  "Local pending-shutdown parse errors from the last passive snapshot.")

(defvar my/noema-jupyter-kernelspec-directory
  (expand-file-name
   "site-lisp/noema/jupyter/kernel-templates/"
   user-emacs-directory)
  "Noema kernelspec templates available to client-accessible targets.")

(defun my/noema-jupyter--substitute-template (value variables)
  "Recursively replace @NAME@ placeholders in VALUE from VARIABLES."
  (cond
   ((stringp value)
    (let ((result value))
      (dolist (entry variables result)
        (setq result
              (string-replace
               (format "@%s@" (car entry))
               (format "%s" (cdr entry)) result)))))
   ((vectorp value)
    (apply #'vector
           (mapcar
            (lambda (item)
              (my/noema-jupyter--substitute-template item variables))
            value)))
   ((consp value)
    (mapcar
     (lambda (item)
       (if (consp item)
           (cons (car item)
                 (my/noema-jupyter--substitute-template
                  (cdr item) variables))
         (my/noema-jupyter--substitute-template item variables)))
     value))
   (t value)))

(defconst my/noema-jupyter--port-helper
  (concat
   "import json,socket\n"
   "s=[]\n"
   "for _ in range(5):\n"
   " x=socket.socket();x.bind(('127.0.0.1',0));s.append(x)\n"
   "print(json.dumps([x.getsockname()[1] for x in s]))\n")
  "Target-side helper used to reserve five Jupyter ports.")

(defconst my/noema-jupyter--launch-helper
  (concat
   "import json,os,subprocess,sys\n"
   "argv=json.loads(sys.argv[1]); extra=json.loads(sys.argv[2])\n"
   "env=os.environ.copy();env.update({str(k):str(v) for k,v in extra.items()})\n"
   "log=open(sys.argv[4],'ab',buffering=0)\n"
   "p=subprocess.Popen(argv,cwd=sys.argv[3],env=env,stdin=subprocess.DEVNULL,"
   "stdout=log,stderr=subprocess.STDOUT,start_new_session=True,close_fds=True)\n"
   "print(p.pid)\n")
  "Target-side detached process launcher.")

(defun my/noema-jupyter--get (key alist)
  "Return KEY from decoded ALIST, accepting symbol and string keys."
  (or (alist-get key alist)
      (and (symbolp key)
           (alist-get (symbol-name key) alist nil nil #'string=))
      (and (stringp key)
           (alist-get (intern key) alist))))

(defun my/noema-jupyter--pending-file (runtime-id)
  "Return the client-local pending-shutdown file for RUNTIME-ID."
  (expand-file-name
   (concat (secure-hash 'sha256 runtime-id) ".json")
   my/noema-jupyter-pending-directory))

(defun my/noema-jupyter--record-pending-shutdown (runtime)
  "Durably record RUNTIME before an uncertain target shutdown.
Only brokered runtimes with a connection file have a unique target-side
identity suitable for a later retry.  The file contains no HMAC key."
  (when-let* ((connection-file
               (my/noema-jupyter-runtime-connection-file runtime)))
    (let* ((id (my/noema-jupyter-runtime-id runtime))
           (file (my/noema-jupyter--pending-file id))
           (directory my/noema-jupyter-pending-directory)
           (temporary nil)
           (entry
            `((version . 1)
              (runtimeId . ,id)
              (targetId . ,(remote-context-target-id
                            (my/noema-jupyter-runtime-context runtime)))
              (placementFile . ,(remote-canonicalize-file-name
                                 (my/noema-jupyter-runtime-placement-file runtime)))
              (pid . ,(my/noema-jupyter-runtime-pid runtime))
              (hostName . ,(my/noema-jupyter-runtime-host-name runtime))
              (connectionFile . ,(remote-canonicalize-file-name connection-file))
              (logFile . ,(if-let* ((log
                                    (my/noema-jupyter-runtime-log-file runtime)))
                             (remote-canonicalize-file-name log)
                           "")))))
      (with-file-modes #o700 (make-directory directory t))
      (set-file-modes directory #o700)
      (unwind-protect
          (progn
            (setq temporary (make-temp-file
                             (expand-file-name ".pending-" directory)))
            (set-file-modes temporary #o600)
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (concat (json-serialize entry) "\n")
                            nil temporary nil 'silent))
            (rename-file temporary file t)
            (setq temporary nil))
        (when (and temporary (file-exists-p temporary))
          (delete-file temporary))))))

(defun my/noema-jupyter--forget-pending-shutdown (runtime-id)
  "Remove the durable pending-shutdown record for RUNTIME-ID."
  (let ((file (my/noema-jupyter--pending-file runtime-id)))
    (when (file-exists-p file)
      (delete-file file))))

(defun my/noema-jupyter--read-pending-shutdown (file)
  "Read and validate one client-local pending-shutdown FILE."
  (when (file-symlink-p file)
    (error "Refusing symlinked Jupyter shutdown record: %s" file))
  (let* ((entry (with-temp-buffer
                  (insert-file-contents file)
                  (json-parse-buffer :object-type 'alist
                                     :array-type 'list :null-object nil
                                     :false-object nil)))
         (id (my/noema-jupyter--get 'runtimeId entry))
         (target (my/noema-jupyter--get 'targetId entry))
         (placement (my/noema-jupyter--get 'placementFile entry))
         (connection (my/noema-jupyter--get 'connectionFile entry))
         (pid (my/noema-jupyter--get 'pid entry))
         (host (my/noema-jupyter--get 'hostName entry))
         (log (my/noema-jupyter--get 'logFile entry)))
    (unless (and (equal (my/noema-jupyter--get 'version entry) 1)
                 (stringp id)
                 (string-match-p "\\`noema-[[:xdigit:]]\\{24\\}\\'" id)
                 (equal file (my/noema-jupyter--pending-file id))
                 (stringp target) (not (string-empty-p target))
                 (stringp placement) (remote-fs-file-name-p placement)
                 (equal (remote-fs-target-id placement) target)
                 (integerp pid) (> pid 0)
                 (or (null host)
                     (and (stringp host) (not (string-empty-p host))))
                 (stringp connection) (remote-fs-file-name-p connection)
                 (equal (remote-fs-target-id connection) target)
                 (equal (file-name-nondirectory connection)
                        (concat id ".json"))
                 (stringp log)
                 (or (string-empty-p log)
                     (and (remote-fs-file-name-p log)
                          (equal (remote-fs-target-id log) target)
                          (equal (file-name-directory log)
                                 (file-name-directory connection))
                          (equal (file-name-nondirectory log)
                                 (concat id ".log")))))
      (error "Invalid Jupyter shutdown record: %s" file))
    entry))

(defun my/noema-jupyter--pending-shutdowns ()
  "Return valid durable pending shutdowns without contacting any target."
  (setq my/noema-jupyter--pending-record-errors nil)
  (when (file-directory-p my/noema-jupyter-pending-directory)
    (let (entries)
      (dolist (file
               (directory-files my/noema-jupyter-pending-directory t
                                "\\`[[:xdigit:]]\\{64\\}\\.json\\'"))
        (condition-case error
            (push (my/noema-jupyter--read-pending-shutdown file) entries)
          (error
           (push (format "%s: %s" file (error-message-string error))
                 my/noema-jupyter--pending-record-errors))))
      (setq my/noema-jupyter--pending-record-errors
            (nreverse my/noema-jupyter--pending-record-errors))
      (nreverse entries))))

(defun my/noema-jupyter--file (params)
  "Return the canonical logical ipynb file from PARAMS."
  (let* ((raw (format "%s" (or (my/noema-jupyter--get 'file params) "")))
         (file (and (not (string-empty-p raw))
                    (remote-canonicalize-file-name raw))))
    (unless (and file
                 (remote-fs-file-name-p file)
                 (string-match-p "\\.ipynb\\'" file))
      (error "Jupyter document must be an ipynb: %s" raw))
    file))

(defun my/noema-jupyter--context (file)
  "Return the owning Remote context for FILE."
  (let* ((file (remote-canonicalize-file-name file))
         (context (remote-context file))
         (workspace (remote-workspace-for-path file)))
    ;; A folder opened through the Remote workbench is the project boundary,
    ;; even when its notebooks live in nested directories.  The static target
    ;; configuration may know only the SSH host, not every project folder.
    (when workspace
      (setf (remote-context-workspace-root context)
            (remote-workspace-root workspace)
            (remote-context-workspace-id context)
            (remote-workspace-workspace-id workspace)))
    (unless (remote-context-workspace-root context)
      (setf (remote-context-workspace-root context)
            (file-name-as-directory
             (file-name-directory file))))
    context))

(defun my/noema-jupyter--output (context program &rest args)
  "Run PROGRAM with ARGS in CONTEXT and return trimmed stdout."
  (let* ((default-directory
          (or (remote-context-workspace-root context)
              (remote-make-file-name
               (remote-context-target-id context) "/")))
         (buffer (generate-new-buffer " *noema-jupyter-remote*"))
         status)
    (unwind-protect
        (progn
          (setq status
                (let ((remote-current-adapter-id "process"))
                  (apply #'remote-process-file
                         program nil buffer nil args)))
          (let ((output
                 (with-current-buffer buffer
                   (string-trim (buffer-string)))))
            (unless (zerop status)
              (error "Target command failed (%s): %s %s"
                     status program output))
            output))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun my/noema-jupyter--json-output (context program &rest args)
  "Run PROGRAM with ARGS in CONTEXT and decode its JSON output."
  (json-parse-string
   (apply #'my/noema-jupyter--output context program args)
   :object-type 'alist :array-type 'list
   :null-object nil :false-object :json-false))

(defun my/noema-jupyter--normalize-kernelspecs (payload)
  "Return normalized kernelspec entries decoded from Jupyter PAYLOAD."
  (let ((specs (my/noema-jupyter--get 'kernelspecs payload))
        result)
    (dolist (entry specs)
      (let* ((name (format "%s" (car entry)))
             (value (cdr entry))
             (spec (my/noema-jupyter--get 'spec value)))
        (push
         `((name . ,name)
           (spec . ,spec)
           (resourceDir . ,(my/noema-jupyter--get 'resource_dir value)))
         result)))
    (nreverse result)))

(defun my/noema-jupyter--project-kernelspecs (file)
  "Return client-accessible project kernelspecs available for FILE."
  (when (and (remote-client-file-name file)
             (file-directory-p my/noema-jupyter-kernelspec-directory))
    (let (result)
      (dolist
          (directory
           (directory-files
            my/noema-jupyter-kernelspec-directory t
            directory-files-no-dot-files-regexp))
        (let ((kernel-json (expand-file-name "kernel.json" directory)))
          (when (file-readable-p kernel-json)
            (let ((name
                   (file-name-nondirectory
                    (directory-file-name directory))))
              (push
               `((name . ,name)
                 (spec .
                       ,(my/noema-jupyter--substitute-template
                         (json-parse-string
                          (with-temp-buffer
                            (insert-file-contents kernel-json)
                            (buffer-string))
                          :object-type 'alist :array-type 'list
                          :null-object nil :false-object :json-false)
                         `(("AARONNOTE_JUPYTER_ROOT" .
                            ,(expand-file-name
                              "site-lisp/noema/jupyter"
                              user-emacs-directory))
                           ("AARONNOTE_JUPYTER_STATE_ROOT" .
                            ,(expand-file-name
                              "var/aaronnote/jupyter"
                              user-emacs-directory))
                           ("NOEMA_USER_HOME" . ,(expand-file-name "~/"))
                           ("SAGE_VERSION" .
                            ,(or (getenv "AARONNOTE_SAGE_VERSION")
                                 "current")))))
                 (resourceDir . ,directory))
               result)))))
      (nreverse result))))

(defun my/noema-jupyter--merge-kernelspecs (target project)
  "Overlay PROJECT kernelspecs over TARGET kernelspecs by name."
  (let ((result (copy-sequence target)))
    (dolist (entry project)
      (let ((name (my/noema-jupyter--get 'name entry)))
        (setq result
              (cons
               entry
               (seq-remove
                (lambda (known)
                  (equal name (my/noema-jupyter--get 'name known)))
                result)))))
    (sort result
          (lambda (left right)
            (string-lessp
             (my/noema-jupyter--get 'name left)
             (my/noema-jupyter--get 'name right))))))

(defun my/noema-jupyter--kernelspecs (file)
  "Return target kernelspec entries for logical FILE."
  (let* ((context (my/noema-jupyter--context file))
         (command (my/jupyter-target-command context))
         (payload
          (apply #'my/noema-jupyter--json-output
                 context (car command)
                 (append (cdr command) '("kernelspec" "list" "--json"))))
         (result (my/noema-jupyter--normalize-kernelspecs payload)))
    ;; Project launchers are overlaid only when the selected backend explicitly
    ;; says this target is client-accessible.  Remote targets never receive a
    ;; client path by accident.
    (my/noema-jupyter--merge-kernelspecs
     result (my/noema-jupyter--project-kernelspecs file))))

(defconst my/noema-jupyter--connection-ports
  '((hb . hb_port) (control . control_port) (shell . shell_port)
    (stdin . stdin_port) (iopub . iopub_port))
  "Channel names and connection-file port keys in forwarding order.")

(defun my/noema-jupyter--valid-connection-p (connection)
  "Return non-nil when CONNECTION is a safe TCP Jupyter connection object."
  (and (listp connection)
       (equal (my/noema-jupyter--get 'transport connection) "tcp")
       (stringp (my/noema-jupyter--get 'ip connection))
       (stringp (my/noema-jupyter--get 'key connection))
       (stringp (my/noema-jupyter--get 'signature_scheme connection))
       (cl-every
        (lambda (mapping)
          (let ((port (my/noema-jupyter--get (cdr mapping) connection)))
            (and (integerp port) (> port 0) (< port 65536))))
        my/noema-jupyter--connection-ports)))

(defun my/noema-jupyter--read-connection-file (file)
  "Read and validate Jupyter connection FILE in its owning target namespace."
  (let ((connection
         (json-parse-string
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-string))
          :object-type 'alist :array-type 'list
          :null-object nil :false-object :json-false)))
    (unless (my/noema-jupyter--valid-connection-p connection)
      (error "Invalid Jupyter TCP connection file: %s" file))
    connection))

(defun my/noema-jupyter--target-runtime-directory (file)
  "Return the logical Jupyter runtime directory on FILE's owning target."
  (let* ((context (my/noema-jupyter--context file))
         (command (my/jupyter-target-command context))
         (target-directory
          (apply #'my/noema-jupyter--output context (car command)
                 (append (cdr command) '("--runtime-dir")))))
    (unless (and (stringp target-directory)
                 (not (string-empty-p target-directory)))
      (error "Target Jupyter returned no runtime directory"))
    (file-name-as-directory
     (remote-expand-file-name target-directory nil context))))

(defun my/noema-jupyter--attachable-connections (file)
  "Return validated connection-file choices on FILE's owning target."
  (condition-case error
      (let ((directory (my/noema-jupyter--target-runtime-directory file))
            result)
        (dolist (candidate
                 (directory-files directory t "\\.json\\'" t))
          (condition-case nil
              (let* ((_connection
                      (my/noema-jupyter--read-connection-file candidate))
                     (attributes (file-attributes candidate))
                     (mtime
                      (file-attribute-modification-time attributes)))
                (push
                 `((token . ,(file-name-nondirectory candidate))
                   (mtimeMs . ,(* 1000.0 (float-time mtime))))
                 result))
            (error nil)))
        (sort result
              (lambda (left right)
                (> (or (my/noema-jupyter--get 'mtimeMs left) 0)
                   (or (my/noema-jupyter--get 'mtimeMs right) 0)))))
    (error
     ;; Attaching is optional; a target with no Jupyter runtime directory must
     ;; not hide its otherwise valid kernelspecs.
     (message "Noema Jupyter: cannot list target connection files: %s"
              (error-message-string error))
     nil)))

(defun my/noema-jupyter--kernels (params _client)
  "List kernels available on the Target owning PARAMS file."
  (my/noema-jupyter--defer
   (lambda ()
     (let* ((file
             (remote-canonicalize-file-name
              (format "%s" (my/noema-jupyter--get 'file params))))
            (specs (my/noema-jupyter--kernelspecs file))
            (connections (when (eq t (my/noema-jupyter--get 'includeConnections params))
                           (my/noema-jupyter--attachable-connections file))))
       `((ok . t) (default . "python3") (specs . ,specs)
         (connections . ,connections))))))

(defun my/noema-jupyter--runtime-directory (workspace)
  "Return the target runtime directory for WORKSPACE."
  (remote-expand-file-name
   (format "~/.cache/noema/jupyter/%s/"
           (substring
            (secure-hash 'sha256 (remote-workspace-id workspace)) 0 16))
   nil (remote-workspace-context workspace)))

(defun my/noema-jupyter--signing-key ()
  "Return a fresh HMAC signing key for a kernel connection file.
Anyone holding this key can send the kernel arbitrary code, so it is drawn
from the OS CSPRNG.  Emacs\=' own `random\=' is not a cryptographic source, and
mixing in a timestamp does not make it one -- both inputs are guessable by
anyone who knows roughly when the kernel started."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (condition-case nil
        (progn
          (insert-file-contents-literally "/dev/urandom" nil 0 32)
          (mapconcat (lambda (byte) (format "%02x" byte))
                     (string-to-list (buffer-string)) ""))
      (error
       ;; Emacs can reject character devices as non-regular files, including
       ;; on macOS.  Use a client-local OS-backed generator in that case;
       ;; timestamps and Emacs `random' are not a substitute for a secret.
       (let ((default-directory temporary-file-directory)
             (process-environment (remote-client-process-environment))
             (exec-path (remote-client-exec-path))
             (remote-current-adapter-id nil)
             (remote-current-route nil)
             result)
         (dolist (command '(("openssl" "rand" "-hex" "32")
                            ("python3" "-c" "import secrets; print(secrets.token_hex(32))")))
           (when (and (not result) (executable-find (car command)))
             (erase-buffer)
             (when (and (equal 0 (apply #'call-process (car command) nil t nil (cdr command)))
                        (string-match-p "\\`[0-9a-f]\\{64\\}\\'" (string-trim (buffer-string))))
               (setq result (string-trim (buffer-string))))))
         (or result (error "No OS-backed random generator available for Jupyter HMAC key")))))))

(defun my/noema-jupyter--connection
    (kernel ports key)
  "Build a Jupyter connection object for KERNEL, PORTS and HMAC KEY."
  `((key . ,key)
    (signature_scheme . "hmac-sha256")
    (transport . "tcp")
    (ip . "127.0.0.1")
    (hb_port . ,(nth 0 ports))
    (control_port . ,(nth 1 ports))
    (shell_port . ,(nth 2 ports))
    (stdin_port . ,(nth 3 ports))
    (iopub_port . ,(nth 4 ports))
    (kernel_name . ,kernel)))

(defun my/noema-jupyter--client-connection
    (target group)
  "Rewrite TARGET using GROUP's client-side host and five ports."
  (let* ((result (copy-tree target))
         (locals (remote-channel-group-endpoints group 'local))
         (hosts
          (delete-dups
           (mapcar
            (lambda (mapping)
              (plist-get (alist-get (car mapping) locals) :host))
            my/noema-jupyter--connection-ports))))
    (unless (and (= (length hosts) 1) (stringp (car hosts))
                 (not (string-empty-p (car hosts))))
      (error "Jupyter channel group exposed inconsistent client hosts: %S"
             hosts))
    ;; The target may bind 0.0.0.0, ::, or a target-only hostname. Once the
    ;; ports are forwarded, Node must connect to the client listener instead.
    (setf (alist-get 'ip result) (car hosts))
    (dolist (mapping my/noema-jupyter--connection-ports)
      (let ((port
             (plist-get (alist-get (car mapping) locals) :port)))
        (unless (and (integerp port) (> port 0) (< port 65536))
          (error "Jupyter channel %s exposed invalid client port: %S"
                 (car mapping) port))
        (setf (alist-get (cdr mapping) result) port)))
    result))

(defun my/noema-jupyter--connection-endpoints (connection)
  "Return channel endpoints described by validated CONNECTION."
  (let* ((raw-host (my/noema-jupyter--get 'ip connection))
         ;; Wildcard bind addresses are not connectable destinations.  The
         ;; target-side kernel is nevertheless reachable through loopback.
         (host (cond ((member raw-host '("" "*" "0.0.0.0")) "127.0.0.1")
                     ((member raw-host '("::" "[::]")) "::1")
                     (t raw-host))))
    (mapcar
     (lambda (mapping)
       (cons (car mapping)
             (list :host host
                   :port (my/noema-jupyter--get (cdr mapping) connection))))
     my/noema-jupyter--connection-ports)))

(defun my/noema-jupyter--update-attachment-group
    (attachment replacement &optional target-connection)
  "Install recovered channel-group REPLACEMENT into ATTACHMENT.
When TARGET-CONNECTION is non-nil, also adopt its current ports and HMAC key."
  (when target-connection
    (setf (my/noema-jupyter-attachment-target-connection attachment)
          target-connection))
  (setf
   (my/noema-jupyter-attachment-channel-group attachment) replacement
   (my/noema-jupyter-attachment-client-connection attachment)
   (my/noema-jupyter--client-connection
    (my/noema-jupyter-attachment-target-connection attachment)
    replacement)
   (my/noema-jupyter-attachment-generation attachment)
   (1+ (my/noema-jupyter-attachment-generation attachment))
   (my/noema-jupyter-attachment-state attachment) 'open)
  (puthash (my/noema-jupyter-attachment-id attachment)
           attachment my/noema-jupyter-attachments)
  attachment)

(defun my/noema-jupyter--open-attachment-group
    (attachment &optional local-endpoints generation workspace)
  "Open ATTACHMENT's five routes.
LOCAL-ENDPOINTS and GENERATION preserve a previous client-side generation;
WORKSPACE defaults to the attachment's owning workspace."
  (let ((id (my/noema-jupyter-attachment-id attachment)))
    (remote-channel-group-open
     (my/noema-jupyter--connection-endpoints
      (my/noema-jupyter-attachment-target-connection attachment))
     :local-endpoints local-endpoints
     :context (my/noema-jupyter-attachment-context attachment)
     :workspace (or workspace
                    (my/noema-jupyter-attachment-workspace attachment))
     :key (list 'noema-jupyter-attach id)
     :register nil
     :generation generation
     :metadata (list :application "noema-jupyter-attach"
                     :attachment id))))

(defun my/noema-jupyter--close-attachment (attachment &optional reason)
  "Close ATTACHMENT's routes without stopping the external kernel.
REASON `transport-recovery' keeps its logical registry entry in place."
  (when (my/noema-jupyter-attachment-p attachment)
    (when-let* ((group
                 (my/noema-jupyter-attachment-channel-group attachment)))
      (ignore-errors (remote-channel-group-close group)))
    (unless (eq reason 'transport-recovery)
      (setf (my/noema-jupyter-attachment-state attachment) 'closed)
      (remhash (my/noema-jupyter-attachment-id attachment)
               my/noema-jupyter-attachments)))
  attachment)

(defun my/noema-jupyter--attach-runtime (params)
  "Open five routed channels to an existing target kernel from PARAMS."
  (let* ((placement-file
          (format "%s"
                  (or (my/noema-jupyter--get 'sourceFile params)
                      (my/noema-jupyter--get 'file params)
                      (error "Missing Jupyter attachment placement file"))))
         (token (format "%s" (or (my/noema-jupyter--get 'token params) "")))
         (context (my/noema-jupyter--context placement-file))
         (workspace (remote-workspace-open context :load-environment t))
         (directory (my/noema-jupyter--target-runtime-directory placement-file)))
    (when (or (string-empty-p token)
              (not (equal token (file-name-nondirectory token))))
      (error "Invalid Jupyter attachment token: %s" token))
    (let* ((connection-file (expand-file-name token directory))
           (target-connection
            (my/noema-jupyter--read-connection-file connection-file))
           (attachment-id
            (format "noema-attach-%s"
                    (substring
                     (secure-hash
                      'sha256
                      (format "%s:%s:%s" placement-file token (float-time)))
                     0 24)))
           attachment group)
      (condition-case error
          (progn
            (setq attachment
                  (my/noema-jupyter-attachment-create
                   :id attachment-id :token token :context context
                   :workspace workspace :placement-file placement-file
                   :connection-file connection-file
                   :target-connection target-connection
                   :generation 1 :state 'open))
            (setq group (my/noema-jupyter--open-attachment-group attachment))
            (setf
             (my/noema-jupyter-attachment-channel-group attachment) group
             (my/noema-jupyter-attachment-client-connection attachment)
             (my/noema-jupyter--client-connection target-connection group))
            (puthash attachment-id attachment my/noema-jupyter-attachments)
            ;; This keyed resource is the sole owner of the group. Recovery
            ;; preserves all five client listeners as one atomic generation.
            (remote-workspace-ensure-recoverable-resource
             workspace 'jupyter-attachment attachment-id attachment
             :close #'my/noema-jupyter--close-attachment
             :recover
             (lambda (resource owner)
               (let* ((old (remote-workspace-resource-value resource))
                      (old-group
                       (my/noema-jupyter-attachment-channel-group old))
                      (stable
                       (remote-channel-group-endpoints old-group 'local))
                      (replacement
                       (my/noema-jupyter--open-attachment-group
                        old stable
                        (1+ (remote-channel-group-generation old-group))
                        owner)))
                 (my/noema-jupyter--update-attachment-group old replacement)))
             :recovery 'auto
             :metadata (list :application "noema-jupyter-attach"
                             :attachment attachment-id))
            attachment)
        (error
         (remhash attachment-id my/noema-jupyter-attachments)
         (when group (ignore-errors (remote-channel-group-close group)))
         (signal (car error) (cdr error)))))))

(defun my/noema-jupyter--attachment-result (attachment)
  "Return JSON-safe connection information for ATTACHMENT."
  `((attachmentId . ,(my/noema-jupyter-attachment-id attachment))
    (generation . ,(my/noema-jupyter-attachment-generation attachment))
    (connectionFile . ,(my/noema-jupyter-attachment-connection-file attachment))
    (connectionInfo .
                    ,(my/noema-jupyter-attachment-client-connection attachment))))

(defun my/noema-jupyter--attachment (params)
  "Resolve an attachment from PARAMS."
  (let* ((id (format "%s"
                     (or (my/noema-jupyter--get 'attachmentId params) "")))
         (attachment (gethash id my/noema-jupyter-attachments)))
    (or attachment (error "Unknown Noema Jupyter attachment: %s" id))))

(defun my/noema-jupyter--exit-status (context program &rest args)
  "Run PROGRAM with ARGS in CONTEXT and return its exit status.
Unlike `my/noema-jupyter--output' a non-zero status is a value, not an
error, so callers can tell a target-side answer apart from a transport
failure.  Only a failure to carry out the routed call signals."
  (let* ((default-directory
          (or (remote-context-workspace-root context)
              (remote-make-file-name
               (remote-context-target-id context) "/")))
         (buffer (generate-new-buffer " *noema-jupyter-probe*")))
    (unwind-protect
        (let ((remote-current-adapter-id "process"))
          (apply #'remote-process-file program nil buffer nil args))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun my/noema-jupyter--probe-context (context)
  "Return CONTEXT rooted at target `/` for process identity probes.
The notebook's working directory may disappear or belong to a different
machine after a load-balanced SSH target reconnects."
  (let ((probe (copy-remote-context context)))
    (setf (remote-context-localname probe) "/"
          (remote-context-workspace-root probe)
          (remote-make-file-name (remote-context-target-id context) "/"))
    probe))

(defun my/noema-jupyter--current-host (context)
  "Return the physical machine answering CONTEXT's current route."
  (my/noema-jupyter--output
   (my/noema-jupyter--probe-context context) "hostname"))

(defun my/noema-jupyter--kill-command (runtime signal)
  "Return target argv to send SIGNAL only on RUNTIME's original host.
The hostname check and `kill' run in one routed command, so a reconnect
cannot switch hosts between the check and a process-group signal."
  (let ((group (format "-%s" (my/noema-jupyter-runtime-pid runtime)))
        (host (my/noema-jupyter-runtime-host-name runtime)))
    (if host
        (list "sh" "-c"
              "if [ \"$(hostname)\" != \"$1\" ]; then exit 77; fi; exec /bin/kill \"$2\" -- \"$3\""
              "sh" host (format "-%s" signal) group)
      (list "kill" (format "-%s" signal) "--" group))))

(defun my/noema-jupyter--alive-p (runtime)
  "Return the liveness of RUNTIME's target process group.
The value is t when the group exists, nil when the target answered that it
does not, and `unknown' when the probe itself could not be carried out.
Keeping the third state is the point: a dropped connection must never be
reported as a dead kernel, because callers respond to death by discarding
the kernel\='s in-memory state.  A load-balanced alias resolving to another
machine is also unknown, even if that machine does not have this PID."
  (let ((pid (my/noema-jupyter-runtime-pid runtime)))
    (if (not (and (integerp pid) (> pid 0)))
        nil
      (condition-case nil
          (let* ((command (my/noema-jupyter--kill-command runtime "0"))
                 (status
                  (apply #'my/noema-jupyter--exit-status
                         (my/noema-jupyter--probe-context
                          (my/noema-jupyter-runtime-context runtime))
                         command)))
            (pcase status
              (0 t)
              (1 nil)
              ;; 77 means a different physical host; 255 can mean SSH loss.
              (_ 'unknown)))
        (error 'unknown)))))

(defun my/noema-jupyter--signal (runtime signal)
  "Send SIGNAL to RUNTIME's target process group.
An unknown host or unreachable target leaves the durable shutdown record for
retry; neither can safely receive a signal for an old process-group ID."
  (pcase (my/noema-jupyter--alive-p runtime)
    ('t
     (let ((command (my/noema-jupyter--kill-command runtime signal)))
       (apply #'my/noema-jupyter--output
              (my/noema-jupyter--probe-context
               (my/noema-jupyter-runtime-context runtime))
              command)))
    ('nil nil)
    (_ (error "Cannot verify kernel %s on its original target host"
              (my/noema-jupyter-runtime-id runtime)))))

(defun my/noema-jupyter--await-process-exit (runtime)
  "Confirm that RUNTIME's process group exited after TERM.
An unreachable target or a still-live group is not a completed shutdown."
  (let ((deadline (+ (float-time)
                     (max 0 my/noema-jupyter-shutdown-grace-seconds)))
        state)
    (while (progn
             (setq state (my/noema-jupyter--alive-p runtime))
             (and (eq state t) (< (float-time) deadline)))
      (accept-process-output nil 0.1))
    (pcase state
      ('nil t)
      ('t (error "Kernel %s is still running after TERM"
                 (my/noema-jupyter-runtime-id runtime)))
      (_ (error "Cannot confirm kernel %s exited on its original host"
                (my/noema-jupyter-runtime-id runtime))))))

(defun my/noema-jupyter--delete-runtime-files (runtime)
  "Delete RUNTIME's connection and log files on its verified target host."
  (let* ((context (my/noema-jupyter-runtime-context runtime))
         (target (remote-context-target-id context))
         (files (seq-filter
                 (lambda (file)
                   (and (stringp file) (not (string-empty-p file))))
                 (list (my/noema-jupyter-runtime-connection-file runtime)
                       (my/noema-jupyter-runtime-log-file runtime)))))
    (when files
      (if (equal target "local")
          (dolist (file files)
            (when (file-exists-p file) (delete-file file)))
        (let ((host (my/noema-jupyter-runtime-host-name runtime)))
          (unless host
            (error "Cannot remove kernel files: original target host is unknown"))
          (apply #'my/noema-jupyter--output
                 (my/noema-jupyter--probe-context context)
                 "sh" "-c"
                 "if [ \"$(hostname)\" != \"$1\" ]; then exit 77; fi; shift; for file do rm -f -- \"$file\" || exit; done"
                 "sh" host
                 (mapcar #'remote-file-local-name files)))))))

(defun my/noema-jupyter--verify-pending-runtime (runtime)
  "Verify that RUNTIME's old PID still owns its unique connection file.
Return nil if the target confirms the process group is gone.  An unreachable
target or a reused PID signals, leaving the runtime available for retry."
  (let* ((id (my/noema-jupyter-runtime-id runtime))
         (pid (my/noema-jupyter-runtime-pid runtime))
         (connection (my/noema-jupyter-runtime-connection-file runtime))
         (context (my/noema-jupyter-runtime-context runtime))
         (host (my/noema-jupyter-runtime-host-name runtime)))
    (when (and (not host)
               (not (equal (remote-context-target-id context) "local")))
      (error "Cannot verify pending kernel %s: original target host is unknown" id))
    (pcase (my/noema-jupyter--alive-p runtime)
      ('unknown
       (error "Cannot verify pending kernel %s: target unavailable or on another host"
              id))
      ('nil
       (unless (or host
                   (equal (remote-context-target-id context) "local"))
         (error "Cannot confirm pending kernel %s died: original host is unknown"
                id))
       nil)
      ('t
       (unless connection
         (error "Pending kernel %s has no connection-file identity" id))
       (let ((command
              (if host
                  (my/noema-jupyter--output
                   (my/noema-jupyter--probe-context context)
                   "sh" "-c"
                   "if [ \"$(hostname)\" != \"$1\" ]; then exit 77; fi; exec ps -ww -p \"$2\" -o command="
                   "sh" host (number-to-string pid))
                (my/noema-jupyter--output
                 (my/noema-jupyter--probe-context context)
                 "ps" "-ww" "-p" (number-to-string pid)
                 "-o" "command="))))
         (unless (string-match-p
                  (regexp-quote (file-name-nondirectory connection)) command)
           (error "PID %s no longer names Noema kernel %s; record retained"
                  pid id)))
       t))))

(defun my/noema-jupyter--retry-pending-shutdown (entry)
  "Retry ENTRY shutdown after verifying the target process identity.
An old PID is never sufficient evidence.  Uncertain cases retain the record."
  (let* ((id (my/noema-jupyter--get 'runtimeId entry))
         (pid (my/noema-jupyter--get 'pid entry))
         (connection (my/noema-jupyter--get 'connectionFile entry))
         (host (my/noema-jupyter--get 'hostName entry))
         (log (my/noema-jupyter--get 'logFile entry))
         (context (remote-context
                   (my/noema-jupyter--get 'placementFile entry)))
         (runtime (my/noema-jupyter-runtime-create
                   :id id :pid pid :context context
                   :connection-file connection :log-file log
                   :host-name host
                   :state 'shutdown-pending)))
    (when (my/noema-jupyter--verify-pending-runtime runtime)
      (my/noema-jupyter--signal runtime "TERM"))
    (my/noema-jupyter--await-process-exit runtime)
    (my/noema-jupyter--delete-runtime-files runtime)
    (my/noema-jupyter--forget-pending-shutdown id)
    (setf (my/noema-jupyter-runtime-state runtime) 'closed)
    runtime))

(defun my/noema-jupyter--pending-shutdown-for (runtime-id)
  "Return the validated durable shutdown entry for RUNTIME-ID, if present."
  (let ((file (my/noema-jupyter--pending-file runtime-id)))
    (when (file-exists-p file)
      (my/noema-jupyter--read-pending-shutdown file))))

(defun my/noema-jupyter--update-group
    (runtime _old replacement)
  "Install recovered channel-group REPLACEMENT into RUNTIME."
  (setf
   (my/noema-jupyter-runtime-channel-group runtime) replacement
   (my/noema-jupyter-runtime-client-connection runtime)
   (my/noema-jupyter--client-connection
    (my/noema-jupyter-runtime-target-connection runtime)
    replacement)
   (my/noema-jupyter-runtime-generation runtime)
   (1+ (my/noema-jupyter-runtime-generation runtime))))

(defun my/noema-jupyter--refresh-recovered-runtime (runtime)
  "Update RUNTIME after transport recovery without guessing kernel death."
  (pcase (my/noema-jupyter--alive-p runtime)
    ('nil
     (setf (my/noema-jupyter-runtime-state runtime) 'dead
           (my/noema-jupyter-runtime-state-lost runtime) t))
    ('unknown
     (unless (eq (my/noema-jupyter-runtime-state runtime)
                 'shutdown-pending)
       (setf (my/noema-jupyter-runtime-state runtime) 'unknown)))
    ('t
     (when (eq (my/noema-jupyter-runtime-state runtime) 'unknown)
       (setf (my/noema-jupyter-runtime-state runtime) 'open))))
  runtime)

(defun my/noema-jupyter--launch-runtime (params)
  "Launch and return a target-owned runtime described by PARAMS."
  (let* ((manager-id
          (format "%s"
                  (or (my/noema-jupyter--get 'kernelId params)
                      (my/noema-jupyter--get 'key params)
                      (format "kernel-%s" (float-time)))))
         (placement-file
          (format
           "%s"
           (or
            (my/noema-jupyter--get 'sourceFile params)
            (my/noema-jupyter--get 'file params)
            (error "Missing Jupyter target placement file"))))
         (kernel
          (format "%s"
                  (or (my/noema-jupyter--get 'kernelName params)
                      "python3")))
         (context (my/noema-jupyter--context placement-file))
         (workspace
          (remote-workspace-open context :load-environment t))
         (spec-entry
          (seq-find
           (lambda (entry)
             (equal kernel
                    (my/noema-jupyter--get 'name entry)))
           (my/noema-jupyter--kernelspecs placement-file)))
         (spec (and spec-entry
                    (my/noema-jupyter--get 'spec spec-entry))))
    (unless spec
      (error "Unknown Jupyter kernel %s on target %s"
             kernel (remote-context-target-id context)))
    (let* ((host-name (my/noema-jupyter--current-host context))
           (runtime-id
            (format "noema-%s"
                    (substring
                     (secure-hash
                      'sha256
                      (format "%s:%s:%s" manager-id (float-time) (random)))
                     0 24)))
           (directory
            (my/noema-jupyter--runtime-directory workspace))
           (connection-file
            (expand-file-name (format "%s.json" runtime-id) directory))
           (log-file
            (expand-file-name (format "%s.log" runtime-id) directory))
           (ports
            (my/noema-jupyter--json-output
             context "python3" "-c"
             my/noema-jupyter--port-helper))
           (hmac-key (my/noema-jupyter--signing-key))
           (target-connection
            (my/noema-jupyter--connection kernel ports hmac-key))
           (argv
            (mapcar
             (lambda (argument)
               (if (equal argument "{connection_file}")
                   (remote-file-local-name connection-file)
                 (format "%s" argument)))
             (append (my/noema-jupyter--get 'argv spec) nil)))
           (env
            (or (my/noema-jupyter--get 'env spec) '()))
           group runtime pid)
      (condition-case error
          (progn
	    (with-file-modes #o700 (make-directory directory t))
	    (set-file-modes directory #o700)
	    ;; The connection file carries the HMAC signing key, so it must never
	    ;; exist readable — not even between the write and a later chmod.
	    (with-file-modes #o600
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region
                 (concat (json-serialize target-connection) "\n")
                 nil connection-file nil 'silent)))
	    (set-file-modes connection-file #o600)
	    (setq pid
		  (string-to-number
		   (my/noema-jupyter--output
		    context "python3" "-c"
		    my/noema-jupyter--launch-helper
		    (json-serialize (vconcat argv))
		    (json-serialize
		     (if env env (make-hash-table :test #'equal)))
		    (remote-file-local-name (remote-workspace-root workspace))
		    (remote-file-local-name log-file))))
	    (unless (> pid 0)
              (error "Target launcher returned an invalid kernel PID"))
	    (setq runtime
		  (my/noema-jupyter-runtime-create
		   :id runtime-id :manager-id manager-id
		   :context context :workspace workspace
		   ;; Placement is only a Remote target/workspace anchor.  It is not
		   ;; runtime identity and is never exposed as kernel ownership.
		   :placement-file placement-file
		   :kernel kernel :spec spec :pid pid :host-name host-name
		   :resource-directory
		   (my/noema-jupyter--get 'resourceDir spec-entry)
		   :connection-file connection-file
		   :target-connection target-connection
		   :generation 1 :state 'open :state-lost nil
		   :log-file log-file))
            ;; The detached process outlives Emacs.  Persist its identity as
            ;; soon as the launcher returns a PID, before channel setup can
            ;; fail or Emacs can exit without running shutdown hooks.  While
            ;; this session owns the runtime, snapshots hide the saved row.
            (my/noema-jupyter--record-pending-shutdown runtime)
            (setq group
                  (remote-channel-group-open
                   `((hb . (:host "127.0.0.1" :port ,(nth 0 ports)))
                     (control . (:host "127.0.0.1" :port ,(nth 1 ports)))
                     (shell . (:host "127.0.0.1" :port ,(nth 2 ports)))
                     (stdin . (:host "127.0.0.1" :port ,(nth 3 ports)))
                     (iopub . (:host "127.0.0.1" :port ,(nth 4 ports))))
                   :context context :workspace workspace
                   :key (list 'noema-jupyter runtime-id)
                   :metadata
                   (list
                    :application "noema-jupyter"
                    :runtime runtime-id
                    :on-recovered
                    (lambda (old replacement)
                      (my/noema-jupyter--update-group
                       runtime old replacement)))))
            (setf
             (my/noema-jupyter-runtime-channel-group runtime) group
             (my/noema-jupyter-runtime-client-connection runtime)
             (my/noema-jupyter--client-connection
              target-connection group))
            (puthash runtime-id runtime my/noema-jupyter-runtimes)
            (remote-workspace-ensure-recoverable-resource
             workspace 'jupyter-runtime runtime-id runtime
             :close
             (lambda (value reason)
               (unless (eq reason 'transport-recovery)
                 (my/noema-jupyter--shutdown-runtime value)))
             :recover
             (lambda (_resource _owner)
               (my/noema-jupyter--refresh-recovered-runtime runtime))
             :recovery 'auto
             :metadata (list :application "noema-jupyter"
                             :runtime runtime-id))
            runtime)
        (error
         ;; Once the detached process has a PID, a failed rollback must keep
         ;; its identity and connection file.  It can then be retried from the
         ;; current Board or from the durable record after Emacs restarts.
         (if (and (integerp pid) (> pid 0))
             (progn
               (unless runtime
                 (setq runtime
                       (my/noema-jupyter-runtime-create
                        :id runtime-id :manager-id manager-id
                        :context context :workspace workspace
                        :placement-file placement-file :kernel kernel
                        :pid pid :host-name host-name
                        :connection-file connection-file
                        :log-file log-file :channel-group group
                        :state 'open)))
               (puthash runtime-id runtime my/noema-jupyter-runtimes)
               (condition-case rollback-error
                   (my/noema-jupyter--shutdown-runtime runtime)
                 (error
                  (display-warning
                   'noema-jupyter
                   (format "Launched kernel %s could not be rolled back: %s"
                           runtime-id (error-message-string rollback-error))
                   :error))))
           (when group
             (ignore-errors (remote-channel-group-close group)))
           (ignore-errors (delete-file connection-file))
           (ignore-errors (delete-file log-file)))
         (signal (car error) (cdr error)))))))

(defun my/noema-jupyter--runtime-result (runtime)
  "Return JSON-safe connection information for RUNTIME."
  `((runtimeId . ,(my/noema-jupyter-runtime-id runtime))
    (pid . ,(my/noema-jupyter-runtime-pid runtime))
    (generation . ,(my/noema-jupyter-runtime-generation runtime))
    (stateLost .
               ,(if (my/noema-jupyter-runtime-state-lost runtime)
                    t :json-false))
    (connectionFile .
                    ,(my/noema-jupyter-runtime-connection-file runtime))
    (connectionInfo .
                    ,(my/noema-jupyter-runtime-client-connection runtime))))

(defun my/noema-jupyter--runtime (params)
  "Resolve a runtime from PARAMS."
  (let* ((id
          (format "%s"
                  (or (my/noema-jupyter--get 'runtimeId params) "")))
         (runtime (gethash id my/noema-jupyter-runtimes)))
    (or runtime (error "Unknown Noema Jupyter runtime: %s" id))))

(defun my/noema-jupyter--forget-runtime-resources (runtime)
  "Remove RUNTIME and its channel group from the workspace that owns them.
A workspace recovers every resource it still holds: left behind, a kernel that
was shut down would get its five forwards reopened on each reconnect."
  (when-let* ((workspace (my/noema-jupyter-runtime-workspace runtime))
              ((remote-workspace-p workspace)))
    (dolist (identity
             (list
              (cons 'jupyter-runtime (my/noema-jupyter-runtime-id runtime))
              (cons 'channel-group
                    (list 'noema-jupyter
                          (my/noema-jupyter-runtime-id runtime)))))
      (when-let* ((resource
                   (remote-workspace-find-resource
                    workspace (car identity) (cdr identity))))
        (remote-workspace-forget-resource workspace resource)))))

(defun my/noema-jupyter--shutdown-runtime (runtime)
  "Close and remove RUNTIME.
Shutting down a runtime twice does nothing the second time: its process group
ID is no longer its own and must not be signalled again.  If signalling fails,
keep the runtime registered so a later shutdown can retry."
  (when (and (my/noema-jupyter-runtime-p runtime)
             (not (eq (my/noema-jupyter-runtime-state runtime) 'closed)))
    ;; Write before the target call: if Emacs exits while it is unreachable,
    ;; the next session can still offer this kernel in Jupyter Board.
    ;; A failed target call leaves the detached kernel potentially alive.
    ;; Do not discard its identity or workspace resources until the process
    ;; is confirmed gone and its target files were removed.  A failed local
    ;; journal write also stops before signalling the target.
    (condition-case error
        (progn
          (my/noema-jupyter--record-pending-shutdown runtime)
          (when (eq (my/noema-jupyter-runtime-state runtime)
                    'shutdown-pending)
            (my/noema-jupyter--verify-pending-runtime runtime))
          (my/noema-jupyter--signal runtime "TERM")
          (my/noema-jupyter--await-process-exit runtime)
          (my/noema-jupyter--delete-runtime-files runtime))
      (error
       (setf (my/noema-jupyter-runtime-state runtime) 'shutdown-pending)
       (signal (car error) (cdr error))))
    (setf (my/noema-jupyter-runtime-state runtime) 'closed)
    (remhash
     (my/noema-jupyter-runtime-id runtime)
     my/noema-jupyter-runtimes)
    (my/noema-jupyter--forget-runtime-resources runtime)
    (when-let* ((group
                 (my/noema-jupyter-runtime-channel-group runtime)))
      (ignore-errors (remote-channel-group-close group)))
    (when (my/noema-jupyter-runtime-connection-file runtime)
      (condition-case error
          (my/noema-jupyter--forget-pending-shutdown
           (my/noema-jupyter-runtime-id runtime))
        (error
         (display-warning
          'noema-jupyter
          (format "Could not clear shutdown record for %s: %s"
                  (my/noema-jupyter-runtime-id runtime)
                  (error-message-string error))
          :warning)))))
  runtime)

(defun my/noema-jupyter-runtime-snapshot (&optional target-id)
  "Return a passive normalized snapshot of brokered Jupyter runtimes.
When TARGET-ID is non-nil, include only runtimes owned by that Remote target."
  (append
   (cl-loop
    for runtime being the hash-values of my/noema-jupyter-runtimes
    for context = (my/noema-jupyter-runtime-context runtime)
    for owner = (remote-context-target-id context)
    when (or (null target-id) (equal target-id owner))
    collect
    (list :id (format "runtime:noema-broker:%s"
                      (my/noema-jupyter-runtime-id runtime))
          :kind 'runtime :provider 'noema-broker
          :runtime-id (my/noema-jupyter-runtime-id runtime)
          :host-runtime-id (my/noema-jupyter-runtime-id runtime)
          :manager-kernel-id (my/noema-jupyter-runtime-manager-id runtime)
          :target-id owner
          :kernel (my/noema-jupyter-runtime-kernel runtime)
          ;; Snapshotting is intentionally passive: liveness probes belong to
          ;; Remote Doctor and must not block or start work when the Board opens.
          :status (or (my/noema-jupyter-runtime-state runtime) 'unknown)
          :pid (my/noema-jupyter-runtime-pid runtime)
          :host-name (my/noema-jupyter-runtime-host-name runtime)
          :generation (my/noema-jupyter-runtime-generation runtime)
          :state-lost (my/noema-jupyter-runtime-state-lost runtime)
          :connection-file (my/noema-jupyter-runtime-connection-file runtime)
          :log-file (my/noema-jupyter-runtime-log-file runtime)
          :resource-directory
          (my/noema-jupyter-runtime-resource-directory runtime)))
   (cl-loop
    for entry in (my/noema-jupyter--pending-shutdowns)
    for id = (my/noema-jupyter--get 'runtimeId entry)
    for owner = (my/noema-jupyter--get 'targetId entry)
    when (and (not (gethash id my/noema-jupyter-runtimes))
              (or (null target-id) (equal target-id owner)))
    collect
    (list :id (format "runtime:noema-broker:%s" id)
          :kind 'runtime :provider 'noema-broker
          :runtime-id id :host-runtime-id id :target-id owner
          :title "Broker kernel from previous Emacs session"
          :kernel "Unknown" :status 'shutdown-pending
          :pid (my/noema-jupyter--get 'pid entry)
          :host-name (my/noema-jupyter--get 'hostName entry)
          :source-file (my/noema-jupyter--get 'placementFile entry)
          :connection-file (my/noema-jupyter--get 'connectionFile entry)
          :log-file (my/noema-jupyter--get 'logFile entry)
          :journal-pending t))))

(defun my/noema-jupyter--restart-runtime (runtime)
  "Restart brokered RUNTIME and return its replacement.
The placement file is a target/workspace anchor only; the opaque manager ID
remains the kernel identity across provider replacement."
  (let ((launch-params
         `((kernelId . ,(my/noema-jupyter-runtime-manager-id runtime))
           (sourceFile . ,(my/noema-jupyter-runtime-placement-file runtime))
           (kernelName . ,(my/noema-jupyter-runtime-kernel runtime)))))
    (my/noema-jupyter--shutdown-runtime runtime)
    (let ((replacement (my/noema-jupyter--launch-runtime launch-params)))
      (setf
       (my/noema-jupyter-runtime-generation replacement)
       (1+ (my/noema-jupyter-runtime-generation runtime))
       (my/noema-jupyter-runtime-state-lost replacement) t)
      replacement)))

(defun my/noema-jupyter-runtime-control (runtime-id action callback)
  "Apply ACTION to broker RUNTIME-ID and invoke CALLBACK with (RESULT ERROR).
The operation is deferred so callers never run routed process work from a UI
button's redisplay stack."
  (run-at-time
   0 nil
   (lambda ()
     (condition-case error
         (let ((runtime (gethash runtime-id my/noema-jupyter-runtimes)))
           (if runtime
               (pcase action
                 ('interrupt (my/noema-jupyter--signal runtime "INT"))
                 ('restart (setq runtime (my/noema-jupyter--restart-runtime runtime)))
                 ('shutdown (my/noema-jupyter--shutdown-runtime runtime))
                 (_ (error "Unsupported Noema Jupyter action: %s" action)))
             (let ((pending
                    (my/noema-jupyter--pending-shutdown-for runtime-id)))
               (unless pending
                 (error "Unknown Noema Jupyter runtime: %s" runtime-id))
               (unless (eq action 'shutdown)
                 (error "Pending kernel %s can only be shut down" runtime-id))
               (setq runtime
                     (my/noema-jupyter--retry-pending-shutdown pending))))
           (funcall callback
                    (if (eq action 'shutdown)
                        '(:ok t)
                      (my/noema-jupyter--runtime-result runtime))
                    nil))
       (error (funcall callback nil (error-message-string error)))))))

(defun my/noema-jupyter--defer (function)
  "Run FUNCTION after dispatch and settle a deferred gateway request."
  (let ((deferred (remote-gateway-defer 60)))
    (run-at-time
     0 nil
     (lambda ()
       (condition-case error
           (remote-gateway-resolve deferred (funcall function))
         (error
          (remote-gateway-reject
           deferred -32603 (error-message-string error))))))
    deferred))

(defun my/noema-jupyter--launch (params _client)
  "Launch a brokered Jupyter runtime for PARAMS."
  (my/noema-jupyter--defer
   (lambda ()
     (my/noema-jupyter--runtime-result
      (my/noema-jupyter--launch-runtime params)))))

(defun my/noema-jupyter--runtime-status-payload (runtime)
  "Return RUNTIME's liveness payload for Noema.
Signals when the target could not be probed at all.  Noema reads
`alive: false\=' as permission to shut the kernel down and drop its state, so
an unanswerable probe must fail the request rather than pose as an answer."
  (let ((alive (my/noema-jupyter--alive-p runtime)))
    (when (eq alive 'unknown)
      (let* ((expected (my/noema-jupyter-runtime-host-name runtime))
             (current (and expected
                           (ignore-errors
                             (my/noema-jupyter--current-host
                              (my/noema-jupyter-runtime-context runtime))))))
        (if (and current (not (equal current expected)))
            (error "Kernel %s is on %s; target now connects to %s"
                   (my/noema-jupyter-runtime-id runtime) expected current)
          (error "Cannot verify target host and process for kernel %s"
                 (my/noema-jupyter-runtime-id runtime)))))
    (append
     `((alive . ,(if alive t :json-false))
       (message . ,(if alive "" "Target kernel process is not alive")))
     (my/noema-jupyter--runtime-result runtime))))

(defun my/noema-jupyter--status (params _client)
  "Return live status for PARAMS runtime."
  (my/noema-jupyter--defer
   (lambda ()
     (my/noema-jupyter--runtime-status-payload
      (my/noema-jupyter--runtime params)))))

(defun my/noema-jupyter--interrupt (params _client)
  "Interrupt PARAMS runtime."
  (my/noema-jupyter--defer
   (lambda ()
     (my/noema-jupyter--signal
      (my/noema-jupyter--runtime params) "INT")
     '((ok . t)))))

(defun my/noema-jupyter--restart (params _client)
  "Restart PARAMS runtime with the same target, key and kernelspec."
  (my/noema-jupyter--defer
   (lambda ()
     (my/noema-jupyter--runtime-result
      (my/noema-jupyter--restart-runtime
       (my/noema-jupyter--runtime params))))))

(defun my/noema-jupyter--shutdown (params _client)
  "Shutdown PARAMS runtime."
  (my/noema-jupyter--defer
   (lambda ()
     (my/noema-jupyter--shutdown-runtime
      (my/noema-jupyter--runtime params))
     '((ok . t)))))

(defun my/noema-jupyter--attach (params _client)
  "Attach to an existing target kernel described by PARAMS."
  (my/noema-jupyter--defer
   (lambda ()
     (my/noema-jupyter--attachment-result
      (my/noema-jupyter--attach-runtime params)))))

(defun my/noema-jupyter--attachment-status (params _client)
  "Return routed connection status for PARAMS attachment.
This reports transport/file availability, while Noema's heartbeat remains the
authority for whether the external kernel is servicing Jupyter messages."
  (my/noema-jupyter--defer
   (lambda ()
     (let* ((attachment (my/noema-jupyter--attachment params))
            (workspace (my/noema-jupyter-attachment-workspace attachment))
            (connection-file
             (my/noema-jupyter-attachment-connection-file attachment))
            (group (my/noema-jupyter-attachment-channel-group attachment)))
       ;; Repair the known route first. Connection-file I/O is a separate
       ;; diagnostic path and must not prevent recovery of the five sockets.
       (unless (remote-channel-group-live-p group)
         (let ((resource
                (remote-workspace-find-resource
                 workspace 'jupyter-attachment
                 (my/noema-jupyter-attachment-id attachment))))
           (unless (and resource
                        (remote-workspace-recover-resource workspace resource))
             (error "Cannot recover Jupyter attachment route %s"
                    (my/noema-jupyter-attachment-id attachment))))
         (setq group
               (my/noema-jupyter-attachment-channel-group attachment)))
       (let ((exists (file-exists-p connection-file)))
         ;; An external owner can restart a kernel and rewrite the same
         ;; connection file with a new key and ports. Preserve our five client
         ;; listeners, but route them to that new authoritative target tuple.
         (when exists
           (let ((current
                  (my/noema-jupyter--read-connection-file connection-file)))
             (unless (equal current
                            (my/noema-jupyter-attachment-target-connection
                             attachment))
               (let* ((stable (remote-channel-group-endpoints group 'local))
                      (generation
                       (1+ (remote-channel-group-generation group))))
                 (setf
                  (my/noema-jupyter-attachment-target-connection attachment)
                  current)
                 (remote-channel-group-close group)
                 (setq group
                       (my/noema-jupyter--open-attachment-group
                        attachment stable generation workspace))
                 (my/noema-jupyter--update-attachment-group
                  attachment group current)))))
         (append
          `((alive . "unknown")
            (connectionFilePresent . ,(if exists t :json-false))
            (message . ,(if exists ""
                          "Target connection file no longer exists; kernel liveness is unknown")))
          (my/noema-jupyter--attachment-result attachment)))))))

(defun my/noema-jupyter--release-attachment (params _client)
  "Release PARAMS attachment routes without stopping its external kernel."
  (my/noema-jupyter--defer
   (lambda ()
     (let* ((id (format "%s"
                        (or (my/noema-jupyter--get 'attachmentId params) "")))
            (attachment (gethash id my/noema-jupyter-attachments)))
       ;; A lost gateway response can retry this call. Cleanup succeeded even
       ;; when the logical resource is already absent, so release is idempotent.
       (when attachment
         (let* ((workspace
                 (my/noema-jupyter-attachment-workspace attachment))
                (resource
                 (remote-workspace-find-resource
                  workspace 'jupyter-attachment id)))
           (if resource
               (remote-workspace-close-resource
                workspace resource 'consumer-release)
             (my/noema-jupyter--close-attachment
              attachment 'consumer-release))))
       '((ok . t))))))

(defun my/noema-jupyter--read-nbextension (params _client)
  "Read a validated target nbextension asset for PARAMS runtime."
  (my/noema-jupyter--defer
   (lambda ()
     (let* ((runtime (my/noema-jupyter--runtime params))
            (relative
             (string-remove-prefix
              "/"
              (format
               "%s"
               (or
                (my/noema-jupyter--get 'relativePath params)
                "")))))
       (when (or (string-empty-p relative)
                 (string-match-p "\\(?:\\`\\|/\\)\\.\\.?\\(?:/\\|\\'\\)"
                                 relative))
         (error "Invalid nbextension path"))
       (let* ((context
               (my/noema-jupyter-runtime-context runtime))
              (resource
               (my/noema-jupyter-runtime-resource-directory runtime))
              (target-resource
               (and resource
                    (remote-make-file-name
                     (remote-context-target-id context) resource)))
              (data-root
               (and target-resource
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (directory-file-name target-resource))))))
              (base
               (and data-root
                    (expand-file-name "nbextensions/" data-root)))
              (candidates
               (and base
                    (list
                     (expand-file-name relative base)
                     (expand-file-name (concat relative ".js") base))))
              found)
         (dolist (candidate candidates)
           (when (and (not found)
                      (string-prefix-p base candidate)
                      (file-readable-p candidate))
             (setq found candidate)))
         (if (not found)
             '((found . :json-false))
           `((found . t)
             (contentType . "application/javascript; charset=utf-8")
             (content .
                      ,(with-temp-buffer
                         (insert-file-contents found)
                         (buffer-string))))))))))

(defun my/noema-jupyter--file-read (params _client)
  "Read a validated Jupyter notebook from PARAMS."
  (my/noema-jupyter--defer
   (lambda ()
     (let ((file (my/noema-jupyter--file params)))
       (if (not (file-readable-p file))
           `((exists . :json-false) (file . ,file))
         (let ((attributes (file-attributes file)))
           `((exists . t) (file . ,file)
             (content .
                      ,(with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))
             (size . ,(file-attribute-size attributes))
             (mtimeMs .
                      ,(* 1000.0
                          (float-time
                           (file-attribute-modification-time attributes)))))))))))

(defun my/noema-jupyter--file-write (params _client)
  "Atomically write a validated Jupyter notebook from PARAMS."
  (my/noema-jupyter--defer
   (lambda ()
     (let* ((file (my/noema-jupyter--file params))
            (content
             (format "%s"
                     (or (my/noema-jupyter--get 'content params) "")))
            (directory (file-name-directory file))
            temporary)
       (make-directory directory t)
       ;; A logical SSH buffer's `make-nearby-temp-file' can land in the
       ;; target's /tmp.  Keep the staging file beside the notebook so rename
       ;; is atomic even when /tmp and the project are different mounts.
       (setq temporary
             (make-temp-file
              (expand-file-name ".noema-notebook-" directory)))
       (unwind-protect
           (let ((coding-system-for-write 'utf-8-unix))
             (write-region content nil temporary nil 'silent)
             (rename-file temporary file t))
         (when (and temporary (file-exists-p temporary))
           (ignore-errors (delete-file temporary))))
       (let ((attributes (file-attributes file)))
         `((ok . t) (exists . t) (file . ,file)
           (size . ,(file-attribute-size attributes))
           (mtimeMs .
                    ,(* 1000.0
                        (float-time
                         (file-attribute-modification-time attributes))))))))))

(defun my/noema-jupyter--file-delete (params _client)
  "Delete a validated Jupyter notebook from PARAMS."
  (my/noema-jupyter--defer
   (lambda ()
     (let ((file (my/noema-jupyter--file params)))
       (when (file-exists-p file)
         (delete-file file))
       `((ok . t) (file . ,file))))))

(defun my/noema-jupyter--file-rename (params _client)
  "Rename one validated Jupyter notebook described by PARAMS."
  (my/noema-jupyter--defer
   (lambda ()
     (let* ((to (my/noema-jupyter--file params))
            (from
             (my/noema-jupyter--file
              `((file .
                      ,(my/noema-jupyter--get 'from params))))))
       (rename-file from to t)
       `((ok . t) (file . ,to))))))

(defun my/noema-jupyter--file-stat (params _client)
  "Stat a validated Jupyter notebook from PARAMS."
  (my/noema-jupyter--defer
   (lambda ()
     (let ((file (my/noema-jupyter--file params)))
       (if-let* ((attributes (file-attributes file)))
           `((exists . t) (file . ,file)
             (size . ,(file-attribute-size attributes))
             (mtimeMs .
                      ,(* 1000.0
                          (float-time
                           (file-attribute-modification-time attributes)))))
         `((exists . :json-false) (file . ,file)))))))

(defun my/noema-jupyter--doctor (target probe)
  "Return Noema Jupyter diagnostics for TARGET.
With PROBE, check target executables through the routed process boundary."
  (let* ((target-id (remote-target-id target))
         (runtimes
          (seq-filter
           (lambda (runtime)
             (equal
              target-id
              (remote-context-target-id
               (my/noema-jupyter-runtime-context runtime))))
           (hash-table-values my/noema-jupyter-runtimes)))
         (checks
          (list
           (list
            :name 'noema-jupyter-runtimes :status 'ok
            :detail (format "%d owned runtime(s)" (length runtimes))))))
    (when probe
      (let ((context
             (remote-context
              (remote-make-file-name target-id "/"))))
        (dolist (program '("python3" "jupyter"))
          (push
           (condition-case error
               (progn
                 (my/noema-jupyter--output
                  context "sh" "-lc"
                  (format "command -v %s" program))
                 (list :name (intern (format "noema:%s" program))
                       :status 'ok :detail "available on target"))
             (error
              (list
               :name (intern (format "noema:%s" program))
               :status 'error :detail (error-message-string error)
               :remedy
               (format
                "Install %s in the owning Remote workspace environment"
                program))))
           checks))))
    (nreverse checks)))

(defun my/noema-jupyter-shutdown-all ()
  "Shut down every brokered Jupyter runtime this Emacs placed on a target.
Brokered kernels are started detached (`start_new_session=True\='), so
without this they outlive Emacs, reparent to PID 1, and are invisible to
Noema\='s orphan sweep — which only recognises the connection files it names
itself.  Attached and server kernels are not brokered and are untouched."
  (dolist (runtime (hash-table-values my/noema-jupyter-runtimes))
    (condition-case error
        (progn
          (my/noema-jupyter--shutdown-runtime runtime)
          (remhash (my/noema-jupyter-runtime-id runtime)
                   my/noema-jupyter-runtimes))
      (error
       (message "Noema Jupyter: could not shut down runtime %s: %s"
                (my/noema-jupyter-runtime-id runtime)
                (error-message-string error))))))

(defun my/noema-jupyter-release-all-attachments ()
  "Release every brokered attachment route without stopping any kernel."
  (dolist (attachment (hash-table-values my/noema-jupyter-attachments))
    (condition-case error
        (let* ((workspace
                (my/noema-jupyter-attachment-workspace attachment))
               (resource
                (remote-workspace-find-resource
                 workspace 'jupyter-attachment
                 (my/noema-jupyter-attachment-id attachment))))
          (if resource
              (remote-workspace-close-resource workspace resource 'host-exit)
            (my/noema-jupyter--close-attachment attachment 'host-exit)))
      (error
       (message "Noema Jupyter: could not release attachment %s: %s"
                (my/noema-jupyter-attachment-id attachment)
                (error-message-string error)))))
  (clrhash my/noema-jupyter-attachments))

(defun my/noema-jupyter-shutdown-all-on-exit-h ()
  "Release brokered Jupyter runtimes while Emacs is still able to reach them."
  (ignore-errors (my/noema-jupyter-shutdown-all))
  (ignore-errors (my/noema-jupyter-release-all-attachments)))

(add-hook 'kill-emacs-hook #'my/noema-jupyter-shutdown-all-on-exit-h)

(dolist (entry
         `(("aaronnote.jupyter.kernels" . ,#'my/noema-jupyter--kernels)
           ("aaronnote.jupyter.launch" . ,#'my/noema-jupyter--launch)
           ("aaronnote.jupyter.status" . ,#'my/noema-jupyter--status)
           ("aaronnote.jupyter.interrupt" . ,#'my/noema-jupyter--interrupt)
           ("aaronnote.jupyter.restart" . ,#'my/noema-jupyter--restart)
           ("aaronnote.jupyter.shutdown" . ,#'my/noema-jupyter--shutdown)
           ("aaronnote.jupyter.attach" . ,#'my/noema-jupyter--attach)
           ("aaronnote.jupyter.attachment.status" .
            ,#'my/noema-jupyter--attachment-status)
           ("aaronnote.jupyter.attachment.release" .
            ,#'my/noema-jupyter--release-attachment)
           ("aaronnote.jupyter.read-nbextension" .
            ,#'my/noema-jupyter--read-nbextension)
           ("aaronnote.jupyter.file.read" . ,#'my/noema-jupyter--file-read)
           ("aaronnote.jupyter.file.write" . ,#'my/noema-jupyter--file-write)
           ("aaronnote.jupyter.file.rename" . ,#'my/noema-jupyter--file-rename)
           ("aaronnote.jupyter.file.delete" . ,#'my/noema-jupyter--file-delete)
           ("aaronnote.jupyter.file.stat" . ,#'my/noema-jupyter--file-stat)))
  (remote-gateway-register-method (car entry) (cdr entry)))

(remote-doctor-register-check #'my/noema-jupyter--doctor)

(provide 'init-aaronnote-jupyter-runtime)
;;; init-aaronnote-jupyter-runtime.el ends here
