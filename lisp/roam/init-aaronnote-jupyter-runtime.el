;;; init-aaronnote-jupyter-runtime.el --- Remote-owned Noema kernels -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Noema keeps the mature raw-ZMQ client on the Emacs machine.  This module
;; owns everything whose placement follows the note: kernelspec discovery,
;; target process lifetime, connection files, and the five forwarded Jupyter
;; channels.  The same broker is used for target `local'.

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

(cl-defstruct (my/noema-jupyter-runtime
               (:constructor my/noema-jupyter-runtime-create))
  id manager-id context workspace placement-file kernel spec pid connection-file
  target-connection client-connection channel-group generation state
  state-lost log-file resource-directory)

(defvar my/noema-jupyter-runtimes (make-hash-table :test #'equal)
  "Noema Jupyter runtimes keyed by opaque runtime ID.")

(defvar my/noema-jupyter-kernelspec-directory
  (expand-file-name
   "lisp/roam/Noema/jupyter/.jupyter/data/kernels/"
   user-emacs-directory)
  "Noema kernelspecs available to client-accessible targets.")

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

(defun my/noema-jupyter--file (params)
  "Return the canonical logical Jupyter sidecar file from PARAMS."
  (let* ((raw (format "%s" (or (my/noema-jupyter--get 'file params) "")))
         (file (and (not (string-empty-p raw))
                    (remote-canonicalize-file-name raw))))
    (unless (and file
                 (remote-fs-file-name-p file)
                 (string-match-p "/\\.cell/[^/]+\\.ipynb\\'" file))
      (error "Jupyter notebook must be an ipynb directly under .cell/: %s"
             raw))
    file))

(defun my/noema-jupyter--context (file)
  "Return the owning Remote context for FILE."
  (let ((context (remote-context (remote-canonicalize-file-name file))))
    (unless (remote-context-workspace-root context)
      (setf (remote-context-workspace-root context)
            (file-name-as-directory
             (file-name-directory (remote-canonicalize-file-name file)))))
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
                       ,(json-parse-string
                         (with-temp-buffer
                           (insert-file-contents kernel-json)
                           (buffer-string))
                         :object-type 'alist :array-type 'list
                         :null-object nil :false-object :json-false))
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
         (payload
          (my/noema-jupyter--json-output
           context "jupyter" "kernelspec" "list" "--json"))
         (result (my/noema-jupyter--normalize-kernelspecs payload)))
    ;; Project launchers are overlaid only when the selected backend explicitly
    ;; says this target is client-accessible.  Remote targets never receive a
    ;; client path by accident.
    (my/noema-jupyter--merge-kernelspecs
     result (my/noema-jupyter--project-kernelspecs file))))

(defun my/noema-jupyter--kernels (params _client)
  "List kernels available on the Target owning PARAMS file."
  (my/noema-jupyter--defer
   (lambda ()
     (let* ((file
             (remote-canonicalize-file-name
              (format "%s" (my/noema-jupyter--get 'file params))))
            (specs (my/noema-jupyter--kernelspecs file)))
       `((ok . t) (default . "python3") (specs . ,specs))))))

(defun my/noema-jupyter--runtime-directory (workspace)
  "Return the target runtime directory for WORKSPACE."
  (remote-expand-file-name
   (format "~/.cache/noema/jupyter/%s/"
           (substring
            (secure-hash 'sha256 (remote-workspace-id workspace)) 0 16))
   nil (remote-workspace-context workspace)))

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
  "Rewrite TARGET connection ports using GROUP's client endpoints."
  (let ((result (copy-tree target)))
    (dolist (mapping
             '((hb . hb_port) (control . control_port)
               (shell . shell_port) (stdin . stdin_port)
               (iopub . iopub_port)))
      (setf (alist-get (cdr mapping) result)
            (plist-get
             (alist-get (car mapping)
                        (remote-channel-group-endpoints group 'local))
             :port)))
    result))

(defun my/noema-jupyter--alive-p (runtime)
  "Return non-nil when RUNTIME's target process group exists."
  (condition-case nil
      (progn
        (my/noema-jupyter--output
         (my/noema-jupyter-runtime-context runtime)
         "kill" "-0" "--"
         (format "-%s" (my/noema-jupyter-runtime-pid runtime)))
        t)
    (error nil)))

(defun my/noema-jupyter--signal (runtime signal)
  "Send SIGNAL to RUNTIME's target process group."
  (when (my/noema-jupyter--alive-p runtime)
    (my/noema-jupyter--output
     (my/noema-jupyter-runtime-context runtime)
     "kill" (format "-%s" signal) "--"
     (format "-%s" (my/noema-jupyter-runtime-pid runtime)))))

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
    (let* ((runtime-id
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
           (hmac-key
            (secure-hash
             'sha256
             (format "%s:%s:%s" runtime-id (float-time) (random))))
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
	    (make-directory directory t)
	    (let ((coding-system-for-write 'utf-8-unix))
              (write-region
               (concat (json-serialize target-connection) "\n")
               nil connection-file nil 'silent))
	    (set-file-modes directory #o700)
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
		   :kernel kernel :spec spec :pid pid
		   :resource-directory
		   (my/noema-jupyter--get 'resourceDir spec-entry)
		   :connection-file connection-file
		   :target-connection target-connection
		   :generation 1 :state 'open :state-lost nil
		   :log-file log-file))
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
               (if (my/noema-jupyter--alive-p runtime)
                   runtime
                 (setf
                  (my/noema-jupyter-runtime-state runtime) 'dead
                  (my/noema-jupyter-runtime-state-lost runtime) t)
                 runtime))
             :recovery 'auto
             :metadata (list :application "noema-jupyter"
                             :runtime runtime-id))
            runtime)
        (error
         (when runtime
           (ignore-errors
             (my/noema-jupyter--signal runtime "TERM")))
         (ignore-errors (delete-file connection-file))
         (ignore-errors (delete-file log-file))
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

(defun my/noema-jupyter--shutdown-runtime (runtime)
  "Close and remove RUNTIME."
  (when (my/noema-jupyter-runtime-p runtime)
    (ignore-errors (my/noema-jupyter--signal runtime "TERM"))
    (when-let* ((group
                 (my/noema-jupyter-runtime-channel-group runtime)))
      (ignore-errors (remote-channel-group-close group)))
    (dolist (file
             (list
              (my/noema-jupyter-runtime-connection-file runtime)
              (my/noema-jupyter-runtime-log-file runtime)))
      (when (and file (file-exists-p file))
        (ignore-errors (delete-file file))))
    (setf (my/noema-jupyter-runtime-state runtime) 'closed)
    (remhash
     (my/noema-jupyter-runtime-id runtime)
     my/noema-jupyter-runtimes))
  runtime)

(defun my/noema-jupyter-runtime-snapshot (&optional target-id)
  "Return a passive normalized snapshot of brokered Jupyter runtimes.
When TARGET-ID is non-nil, include only runtimes owned by that Remote target."
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
         :generation (my/noema-jupyter-runtime-generation runtime)
         :state-lost (my/noema-jupyter-runtime-state-lost runtime)
         :connection-file (my/noema-jupyter-runtime-connection-file runtime)
         :log-file (my/noema-jupyter-runtime-log-file runtime)
         :resource-directory
         (my/noema-jupyter-runtime-resource-directory runtime))))

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
         (let ((runtime (or (gethash runtime-id my/noema-jupyter-runtimes)
                            (error "Unknown Noema Jupyter runtime: %s" runtime-id))))
           (pcase action
             ('interrupt (my/noema-jupyter--signal runtime "INT"))
             ('restart (setq runtime (my/noema-jupyter--restart-runtime runtime)))
             ('shutdown (my/noema-jupyter--shutdown-runtime runtime))
             (_ (error "Unsupported Noema Jupyter action: %s" action)))
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

(defun my/noema-jupyter--status (params _client)
  "Return live status for PARAMS runtime."
  (my/noema-jupyter--defer
   (lambda ()
     (let* ((runtime (my/noema-jupyter--runtime params))
            (alive (my/noema-jupyter--alive-p runtime)))
       (append
        `((alive . ,(if alive t :json-false))
          (message . ,(if alive "" "Target kernel process is not alive")))
        (my/noema-jupyter--runtime-result runtime))))))

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
  "Read a validated Jupyter sidecar from PARAMS."
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
  "Atomically write a validated Jupyter sidecar from PARAMS."
  (my/noema-jupyter--defer
   (lambda ()
     (let* ((file (my/noema-jupyter--file params))
            (content
             (format "%s"
                     (or (my/noema-jupyter--get 'content params) "")))
            (directory (file-name-directory file))
            temporary)
       (make-directory directory t)
       (setq temporary (make-nearby-temp-file ".noema-cell-"))
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
  "Delete a validated Jupyter sidecar from PARAMS."
  (my/noema-jupyter--defer
   (lambda ()
     (let ((file (my/noema-jupyter--file params)))
       (when (file-exists-p file)
         (delete-file file))
       `((ok . t) (file . ,file))))))

(defun my/noema-jupyter--file-rename (params _client)
  "Rename one validated Jupyter sidecar described by PARAMS."
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
  "Stat a validated Jupyter sidecar from PARAMS."
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

(dolist (entry
         `(("aaronnote.jupyter.kernels" . ,#'my/noema-jupyter--kernels)
           ("aaronnote.jupyter.launch" . ,#'my/noema-jupyter--launch)
           ("aaronnote.jupyter.status" . ,#'my/noema-jupyter--status)
           ("aaronnote.jupyter.interrupt" . ,#'my/noema-jupyter--interrupt)
           ("aaronnote.jupyter.restart" . ,#'my/noema-jupyter--restart)
           ("aaronnote.jupyter.shutdown" . ,#'my/noema-jupyter--shutdown)
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
