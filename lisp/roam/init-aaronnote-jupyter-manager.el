;;; init-aaronnote-jupyter-manager.el --- Global Jupyter sessions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This is the single live-state authority for Noema Jupyter.  It deliberately
;; follows Jupyter Server's split between kernels (global running resources),
;; sessions (document paths bound to a kernel), and clients (connections owned
;; by a view/controller).  Hidden `.cell' scripts never own kernel processes;
;; they register a session here and act as document controllers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'init-aaronnote-jupyter-runtime)
(require 'jupyter-client)

(cl-defstruct (my/noema-jupyter-kernel
               (:constructor my/noema-jupyter-kernel-create))
  id target-id kernelspec language runtime connection-file generation status
  session-ids running last-used state-lost owned)

(cl-defstruct (my/noema-jupyter-session
               (:constructor my/noema-jupyter-session-create))
  id script-file source-file session-name language kernelspec kernel-id client
  controller-buffers running last-used)

(cl-defstruct (my/noema-jupyter-task
               (:constructor my/noema-jupyter-task-create))
  id kernel-id session-id script-file cell-id status started-at finished-at
  error)

(defvar my/noema-jupyter-manager-kernels (make-hash-table :test #'equal)
  "Global Jupyter kernels keyed by opaque kernel ID.")

(defvar my/noema-jupyter-manager-sessions (make-hash-table :test #'equal)
  "Jupyter document sessions keyed by canonical script path.")

(defvar my/noema-jupyter-manager-sessions-by-id (make-hash-table :test #'equal)
  "Jupyter document sessions keyed by opaque session ID.")

(defvar my/noema-jupyter-manager-tasks (make-hash-table :test #'equal)
  "Live and recently completed Jupyter tasks keyed by opaque task ID.")

(defconst my/noema-jupyter-manager-task-history-limit 200
  "Maximum number of completed tasks retained for the management UI.")

(defun my/noema-jupyter-manager--id (prefix &optional seed)
  "Return a new opaque identifier beginning with PREFIX."
  (format "%s-%s" prefix
          (substring
           (secure-hash 'sha256
                        (format "%s:%s:%s:%s" prefix seed (float-time) (random)))
           0 24)))

(defun my/noema-jupyter-manager--canonical-script (file)
  "Return canonical FILE identity for a Jupyter script."
  (expand-file-name (format "%s" file)))

(defun my/noema-jupyter-manager--language-compatible-p (session kernel)
  "Return non-nil when SESSION can attach to KERNEL."
  (let ((session-language
         (downcase (or (my/noema-jupyter-session-language session) "")))
        (kernel-language
         (downcase (or (my/noema-jupyter-kernel-language kernel) ""))))
    (or (string-empty-p session-language)
        (string-empty-p kernel-language)
        (equal session-language kernel-language)
        (and (member session-language '("bash" "sh" "shell" "zsh"))
             (member kernel-language '("bash" "sh" "shell" "zsh"))))))

(defun my/noema-jupyter-manager-session (script-file &optional no-create metadata)
  "Return SCRIPT-FILE's global session.
When NO-CREATE is nil, create it from METADATA when necessary."
  (let* ((script (my/noema-jupyter-manager--canonical-script script-file))
         (known (gethash script my/noema-jupyter-manager-sessions)))
    (when (and (null known) (not no-create))
      (setq known
            (my/noema-jupyter-session-create
             :id (my/noema-jupyter-manager--id "session" script)
             :script-file script
             :source-file (plist-get metadata :source-file)
             :session-name (or (plist-get metadata :session) "default")
             :language (or (plist-get metadata :language) "python")
             :kernelspec (or (plist-get metadata :kernel) "")
             :controller-buffers nil :running 0 :last-used (float-time)))
      (puthash script known my/noema-jupyter-manager-sessions)
      (puthash (my/noema-jupyter-session-id known) known
               my/noema-jupyter-manager-sessions-by-id))
    (when (and known metadata)
      (setf (my/noema-jupyter-session-source-file known)
            (plist-get metadata :source-file)
            (my/noema-jupyter-session-session-name known)
            (or (plist-get metadata :session) "default")
            (my/noema-jupyter-session-language known)
            (or (plist-get metadata :language) "python")
            (my/noema-jupyter-session-kernelspec known)
            (or (plist-get metadata :kernel)
                (my/noema-jupyter-session-kernelspec known) "")))
    known))

(defun my/noema-jupyter-manager-register-controller (script-file buffer metadata)
  "Register BUFFER as a controller for SCRIPT-FILE using METADATA."
  (let ((session (my/noema-jupyter-manager-session script-file nil metadata)))
    (setf (my/noema-jupyter-session-controller-buffers session)
          (cons buffer
                (delq buffer
                      (seq-filter #'buffer-live-p
                                  (my/noema-jupyter-session-controller-buffers
                                   session)))))
    session))

(defun my/noema-jupyter-manager-release-controller (script-file buffer)
  "Release BUFFER's client role for SCRIPT-FILE without stopping its kernel."
  (when-let* ((session (my/noema-jupyter-manager-session script-file t)))
    (setf (my/noema-jupyter-session-controller-buffers session)
          (delq buffer
                (seq-filter #'buffer-live-p
                            (my/noema-jupyter-session-controller-buffers session))))
    session))

(defun my/noema-jupyter-manager--client-file (runtime)
  "Write and return a private Emacs connection file for RUNTIME."
  (let* ((directory (expand-file-name
                     "jupyter/emacs-client/"
                     (or (and (boundp 'my/noema--state-root)
                              my/noema--state-root)
                         user-emacs-directory)))
         (file (expand-file-name
                (format "%s.json" (my/noema-jupyter-runtime-id runtime))
                directory)))
    (make-directory directory t)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file file
        (insert (json-serialize
                 (my/noema-jupyter-runtime-client-connection runtime)
                 :null-object nil :false-object :json-false))
        (insert "\n")))
    (set-file-modes file #o600)
    file))

(defun my/noema-jupyter-manager--connect-client (session kernel)
  "Connect SESSION as an independent client of KERNEL."
  (when-let* ((old (my/noema-jupyter-session-client session)))
    (ignore-errors (jupyter-disconnect old)))
  (let ((client
         ;; A document session is a headless Jupyter client.  Creating a REPL
         ;; here would give the hidden controller buffer UI ownership and also
         ;; makes `jupyter-repl-mode' initialization part of the kernel path.
         (jupyter-client
          (jupyter-kernel
           :conn-info (my/noema-jupyter-kernel-connection-file kernel)
           :connect-p t))))
    (setf (my/noema-jupyter-session-client session) client)
    client))

(defun my/noema-jupyter-manager--detach-session (session &optional close-orphan)
  "Detach SESSION from its kernel.
When CLOSE-ORPHAN is non-nil, stop an owned kernel after its last session."
  (let* ((kernel-id (my/noema-jupyter-session-kernel-id session))
         (kernel (and kernel-id
                      (gethash kernel-id my/noema-jupyter-manager-kernels))))
    (when-let* ((client (my/noema-jupyter-session-client session)))
      (ignore-errors (jupyter-disconnect client)))
    (setf (my/noema-jupyter-session-client session) nil
          (my/noema-jupyter-session-kernel-id session) nil)
    (when kernel
      (setf (my/noema-jupyter-kernel-session-ids kernel)
            (delete (my/noema-jupyter-session-id session)
                    (my/noema-jupyter-kernel-session-ids kernel)))
      (when (and close-orphan
                 (null (my/noema-jupyter-kernel-session-ids kernel)))
        (my/noema-jupyter-manager-shutdown-kernel kernel-id)))
    kernel))

(defun my/noema-jupyter-manager-start-kernel (session &optional kernelspec)
  "Start KERNELSPEC for SESSION and return the global kernel object."
  (let* ((spec (string-trim
                (format "%s" (or kernelspec
                                    (my/noema-jupyter-session-kernelspec session)
                                    ""))))
         (script (my/noema-jupyter-session-script-file session)))
    (when (string-empty-p spec)
      (error "No kernelspec selected for %s" (file-name-nondirectory script)))
    (let* ((kernel-id (my/noema-jupyter-manager--id "kernel" spec))
           (runtime
            (my/noema-jupyter--launch-runtime
             `((kernelId . ,kernel-id)
               (sourceFile . ,script)
               (kernelName . ,spec))))
           (connection-file (my/noema-jupyter-manager--client-file runtime))
           (context (my/noema-jupyter-runtime-context runtime))
           (kernel
            (my/noema-jupyter-kernel-create
             :id kernel-id
             :target-id (remote-context-target-id context)
             :kernelspec spec
             :language (my/noema-jupyter-session-language session)
             :runtime runtime :connection-file connection-file
             :generation (my/noema-jupyter-runtime-generation runtime)
             :status 'idle :session-ids nil :running 0
             :last-used (float-time) :owned t)))
      (puthash kernel-id kernel my/noema-jupyter-manager-kernels)
      (my/noema-jupyter-manager-select session `(:kind start :kernel-id ,kernel-id)
                                        nil)
      kernel)))

(defun my/noema-jupyter-manager-select (session selection &optional close-old)
  "Apply kernel SELECTION to SESSION and return its selected kernel.
SELECTION is a plist whose :kind is `start', `connect', or `none'.  `start'
accepts :kernelspec or an already-created :kernel-id.  CLOSE-OLD defaults to
non-nil and only closes an owned old kernel after its final session detaches."
  (let ((kind (plist-get selection :kind)))
    ;; A plain `defun' has no implicit Common Lisp block.  Handle the
    ;; recursive start path structurally so this also works when the source is
    ;; interpreted (as it is after reloading a live Emacs configuration).
    (if (and (eq kind 'start)
             (null (plist-get selection :kernel-id)))
        (my/noema-jupyter-manager-start-kernel
         session (plist-get selection :kernelspec))
      (let* ((old-id (my/noema-jupyter-session-kernel-id session))
             (close-old (if (null close-old) t close-old))
             kernel)
        (pcase kind
          ('none
           (my/noema-jupyter-manager--detach-session session close-old))
          ('connect
           (setq kernel (gethash (plist-get selection :kernel-id)
                                 my/noema-jupyter-manager-kernels))
           (unless kernel (error "Unknown Jupyter kernel: %s"
                                 (plist-get selection :kernel-id))))
          ('start
           (let ((id (plist-get selection :kernel-id)))
             (setq kernel (or (gethash id my/noema-jupyter-manager-kernels)
                              (error "Unknown newly-created kernel: %s" id)))))
          (_ (error "Unsupported Jupyter session selection: %s" kind)))
        (when kernel
          (unless (my/noema-jupyter-manager--language-compatible-p session kernel)
            (error "Cannot attach %s session to %s kernel"
                   (my/noema-jupyter-session-language session)
                   (my/noema-jupyter-kernel-language kernel)))
          (unless (equal old-id (my/noema-jupyter-kernel-id kernel))
            (my/noema-jupyter-manager--detach-session session close-old))
          (setf (my/noema-jupyter-session-kernel-id session)
                (my/noema-jupyter-kernel-id kernel)
                (my/noema-jupyter-session-kernelspec session)
                (my/noema-jupyter-kernel-kernelspec kernel)
                (my/noema-jupyter-session-last-used session) (float-time)
                (my/noema-jupyter-kernel-session-ids kernel)
                (cl-adjoin (my/noema-jupyter-session-id session)
                           (my/noema-jupyter-kernel-session-ids kernel)
                           :test #'equal))
          (unless (and (my/noema-jupyter-session-client session)
                       (ignore-errors
                         (jupyter-connected-p
                          (my/noema-jupyter-session-client session))))
            (my/noema-jupyter-manager--connect-client session kernel)))
        kernel))))

(defun my/noema-jupyter-manager-ensure-session (metadata)
  "Return a live Jupyter session for document METADATA."
  (let* ((session
          (my/noema-jupyter-manager-session
           (plist-get metadata :script-file) nil metadata))
         (kernel-id (my/noema-jupyter-session-kernel-id session))
         (kernel (and kernel-id
                      (gethash kernel-id my/noema-jupyter-manager-kernels))))
    (cond
     ((and kernel
           (my/noema-jupyter-session-client session)
           (ignore-errors
             (jupyter-connected-p (my/noema-jupyter-session-client session))))
      session)
     (kernel
      (my/noema-jupyter-manager--connect-client session kernel)
      session)
     (t
      (my/noema-jupyter-manager-start-kernel
       session (my/noema-jupyter-session-kernelspec session))
      session))))

(defun my/noema-jupyter-manager-kernel-for-session (session)
  "Return SESSION's current global kernel, or nil."
  (and session
       (gethash (my/noema-jupyter-session-kernel-id session)
                my/noema-jupyter-manager-kernels)))

(defun my/noema-jupyter-manager-control (kernel-id action)
  "Apply lifecycle ACTION to global KERNEL-ID."
  (let* ((kernel (or (gethash kernel-id my/noema-jupyter-manager-kernels)
                     (error "Unknown Jupyter kernel: %s" kernel-id)))
         (session
          (seq-some (lambda (id)
                      (gethash id my/noema-jupyter-manager-sessions-by-id))
                    (my/noema-jupyter-kernel-session-ids kernel)))
         (client (and session (my/noema-jupyter-session-client session))))
    (unless client (error "Jupyter kernel %s has no connected client" kernel-id))
    (pcase action
      ('interrupt (jupyter-interrupt-kernel client))
      ('restart
       (jupyter-restart-kernel client)
       (cl-incf (my/noema-jupyter-kernel-generation kernel)))
      ('shutdown (my/noema-jupyter-manager-shutdown-kernel kernel-id))
      (_ (error "Unsupported Jupyter kernel action: %s" action)))
    (unless (eq action 'shutdown)
      (setf (my/noema-jupyter-kernel-last-used kernel) (float-time)))
    kernel))

(defun my/noema-jupyter-manager-shutdown-kernel (kernel-id)
  "Shutdown global KERNEL-ID and detach every associated session."
  (when-let* ((kernel (gethash kernel-id my/noema-jupyter-manager-kernels)))
    (let* ((ids (copy-sequence (my/noema-jupyter-kernel-session-ids kernel)))
           (first
            (seq-some (lambda (id)
                        (gethash id my/noema-jupyter-manager-sessions-by-id))
                      ids)))
      (when-let* ((client (and first
                              (my/noema-jupyter-session-client first))))
        (ignore-errors (jupyter-shutdown-kernel client)))
      (dolist (id ids)
        (when-let* ((session (gethash id my/noema-jupyter-manager-sessions-by-id)))
          (when-let* ((client (my/noema-jupyter-session-client session)))
            (ignore-errors (jupyter-disconnect client)))
          (setf (my/noema-jupyter-session-client session) nil
                (my/noema-jupyter-session-kernel-id session) nil)))
      (when-let* ((runtime (my/noema-jupyter-kernel-runtime kernel)))
        (ignore-errors (my/noema-jupyter--shutdown-runtime runtime)))
      (when-let* ((file (my/noema-jupyter-kernel-connection-file kernel)))
        (when (file-exists-p file) (ignore-errors (delete-file file))))
      (setf (my/noema-jupyter-kernel-status kernel) 'closed
            (my/noema-jupyter-kernel-session-ids kernel) nil)
      (remhash kernel-id my/noema-jupyter-manager-kernels))
    kernel))

(defun my/noema-jupyter-manager-task-start (session cell-id)
  "Create and return a running task for SESSION and CELL-ID."
  (let* ((kernel (my/noema-jupyter-manager-kernel-for-session session))
         (task
          (my/noema-jupyter-task-create
           :id (my/noema-jupyter-manager--id "task" cell-id)
           :kernel-id (and kernel (my/noema-jupyter-kernel-id kernel))
           :session-id (my/noema-jupyter-session-id session)
           :script-file (my/noema-jupyter-session-script-file session)
           :cell-id cell-id :status 'running :started-at (float-time))))
    (puthash (my/noema-jupyter-task-id task) task my/noema-jupyter-manager-tasks)
    (cl-incf (my/noema-jupyter-session-running session))
    (when kernel
      (cl-incf (my/noema-jupyter-kernel-running kernel))
      (setf (my/noema-jupyter-kernel-status kernel) 'busy
            (my/noema-jupyter-kernel-last-used kernel) (float-time)))
    task))

(defun my/noema-jupyter-manager-task-finish (task &optional error)
  "Finish TASK and record optional ERROR."
  (when (my/noema-jupyter-task-p task)
    (let* ((session (gethash (my/noema-jupyter-task-session-id task)
                             my/noema-jupyter-manager-sessions-by-id))
           (kernel (gethash (my/noema-jupyter-task-kernel-id task)
                            my/noema-jupyter-manager-kernels)))
      (setf (my/noema-jupyter-task-status task) (if error 'error 'completed)
            (my/noema-jupyter-task-finished-at task) (float-time)
            (my/noema-jupyter-task-error task) error)
      (when session
        (setf (my/noema-jupyter-session-running session)
              (max 0 (1- (or (my/noema-jupyter-session-running session) 0)))
              (my/noema-jupyter-session-last-used session) (float-time)))
      (when kernel
        (setf (my/noema-jupyter-kernel-running kernel)
              (max 0 (1- (or (my/noema-jupyter-kernel-running kernel) 0)))
              (my/noema-jupyter-kernel-status kernel)
              (if (> (or (my/noema-jupyter-kernel-running kernel) 0) 0)
                  'busy 'idle)
              (my/noema-jupyter-kernel-last-used kernel) (float-time))))
    (my/noema-jupyter-manager--trim-tasks)
    task))

(defun my/noema-jupyter-manager--trim-tasks ()
  "Bound retained completed task history."
  (let* ((completed
          (sort
           (cl-loop for task being the hash-values of my/noema-jupyter-manager-tasks
                    unless (eq (my/noema-jupyter-task-status task) 'running)
                    collect task)
           (lambda (a b)
             (> (or (my/noema-jupyter-task-finished-at a) 0)
                (or (my/noema-jupyter-task-finished-at b) 0))))))
    (dolist (task (nthcdr my/noema-jupyter-manager-task-history-limit completed))
      (remhash (my/noema-jupyter-task-id task) my/noema-jupyter-manager-tasks))))

(defun my/noema-jupyter-manager--kernel-object (kernel)
  "Return JSON-safe object for KERNEL."
  `((id . ,(my/noema-jupyter-kernel-id kernel))
    (kernelId . ,(my/noema-jupyter-kernel-id kernel))
    (kernelSpecName . ,(my/noema-jupyter-kernel-kernelspec kernel))
    (language . ,(my/noema-jupyter-kernel-language kernel))
    (targetId . ,(my/noema-jupyter-kernel-target-id kernel))
    (status . ,(symbol-name
                (or (my/noema-jupyter-kernel-status kernel) 'unknown)))
    (running . ,(or (my/noema-jupyter-kernel-running kernel) 0))
    (generation . ,(or (my/noema-jupyter-kernel-generation kernel) 1))
    (sessionIds . ,(vconcat (my/noema-jupyter-kernel-session-ids kernel)))
    (owned . ,(if (my/noema-jupyter-kernel-owned kernel) t :json-false))))

(defun my/noema-jupyter-manager--session-object (session)
  "Return JSON-safe object for SESSION."
  `((id . ,(my/noema-jupyter-session-id session))
    (sessionId . ,(my/noema-jupyter-session-id session))
    (scriptFile . ,(my/noema-jupyter-session-script-file session))
    (sourceFile . ,(or (my/noema-jupyter-session-source-file session) ""))
    (sessionName . ,(my/noema-jupyter-session-session-name session))
    (language . ,(my/noema-jupyter-session-language session))
    (kernelSpecName . ,(my/noema-jupyter-session-kernelspec session))
    (kernelId . ,(or (my/noema-jupyter-session-kernel-id session) ""))
    (running . ,(or (my/noema-jupyter-session-running session) 0))))

(defun my/noema-jupyter-manager--task-object (task)
  "Return JSON-safe object for TASK."
  `((id . ,(my/noema-jupyter-task-id task))
    (taskId . ,(my/noema-jupyter-task-id task))
    (kernelId . ,(or (my/noema-jupyter-task-kernel-id task) ""))
    (sessionId . ,(my/noema-jupyter-task-session-id task))
    (scriptFile . ,(my/noema-jupyter-task-script-file task))
    (cellId . ,(my/noema-jupyter-task-cell-id task))
    (status . ,(symbol-name (my/noema-jupyter-task-status task)))
    (error . ,(or (my/noema-jupyter-task-error task) ""))))

(defun my/noema-jupyter-manager-snapshot ()
  "Return a passive JSON-safe snapshot of kernels, sessions, and tasks."
  `((ok . t)
    (server . ((status . "emacs") (owned . t)))
    (servers . ,(vconcat
                 (mapcar
                  (lambda (entry)
                    `((id . ,(format "%s" (plist-get entry :id)))
                      (displayName . ,(format "%s" (or (plist-get entry :name)
                                                        (plist-get entry :id))))
                      (kind . ,(format "%s" (or (plist-get entry :kind)
                                                 "server")))
                      (target . ,(format "%s" (or (plist-get entry :target)
                                                   "local")))))
                  (if (boundp 'my/noema-jupyter-servers)
                      my/noema-jupyter-servers nil))))
    (kernels . ,(vconcat
                 (mapcar #'my/noema-jupyter-manager--kernel-object
                         (hash-table-values my/noema-jupyter-manager-kernels))))
    (sessions . ,(vconcat
                  (mapcar #'my/noema-jupyter-manager--session-object
                          (hash-table-values my/noema-jupyter-manager-sessions))))
    (tasks . ,(vconcat
               (mapcar #'my/noema-jupyter-manager--task-object
                       (hash-table-values my/noema-jupyter-manager-tasks))))))

(provide 'init-aaronnote-jupyter-manager)
;;; init-aaronnote-jupyter-manager.el ends here
