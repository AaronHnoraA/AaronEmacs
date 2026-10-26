;;; tools/jupyter/management.el --- Unified Jupyter management data -*- lexical-binding: t; -*-

;;; Commentary:
;; Passive discovery and normalized actions shared by the Jupyter Board.

;;; Code:

(require 'cl-lib)
(require 'config)
(require 'eieio)
(require 'json)
(require 'remote-board)
(require 'remote-fs)
(require 'remote-process)
(require 'tools/jupyter/command)
(require 'subr-x)

(declare-function jupyter-all-objects "jupyter-base" (tracking-symbol))
(declare-function jupyter-connect-repl "jupyter-repl" (file &optional repl-name associate-buffer client-class display))
(declare-function jupyter-interrupt-kernel "jupyter-client" (client))
(declare-function jupyter-kernel-language "jupyter-client" (&optional client))
(declare-function jupyter-restart-kernel "jupyter-client" (client))
(declare-function jupyter-run-repl "jupyter-repl" (kernel-name &optional repl-name associate-buffer client-class display))
(declare-function jupyter-shutdown-kernel "jupyter-client" (client))
(declare-function jupyter-run-with-state "jupyter-monads" (state mvalue))
(declare-function jupyter-push "jupyter-monads" (value))
(declare-function jupyter--gc-kernel-processes "jupyter-kernel-process" ())
(declare-function +noema-jupyter--substitute-template
                  "noema/jupyter/runtime" (value variables))

(config-defvar +jupyter-board-jupyter-command nil
  "Local Jupyter executable used by the management board."
  :type 'file)

(config-defvar +jupyter-board-python-command nil
  "Local Python executable used by Jupyter maintenance commands."
  :type 'file)

(config-defvar +jupyter-remote-ikernel-command nil
  "Local vendored remote_ikernel command."
  :type 'file)

(config-defvar +jupyter-remote-ikernel-source-directory nil
  "Vendored remote_ikernel source directory."
  :type 'directory)

(config-defvar +jupyter-remote-ikernel-install-script nil
  "Script that installs the vendored remote_ikernel source."
  :type 'file)

(defconst +jupyter-management-connection-stale-seconds (* 24 60 60)
  "Age after which an unused connection file is reported as stale.")

(defvar +jupyter-management--repairing-monads nil)

(defun +jupyter-management--check-monads ()
  "Recover Jupyter monad functions compiled before their macros were defined.
Some package byte-compilation orders leave `state' free in `jupyter-push'.
Reload the unchanged package source with its macros already available."
  (unless +jupyter-management--repairing-monads
    (condition-case err
        (jupyter-run-with-state nil (jupyter-push nil))
      (void-variable
       (unless (eq (cadr err) 'state) (signal (car err) (cdr err)))
       (let* ((+jupyter-management--repairing-monads t)
              (source (concat (file-name-sans-extension (locate-library "jupyter-monads")) ".el")))
         (load source nil t t)
         (jupyter-run-with-state nil (jupyter-push nil)))))))

(with-eval-after-load 'jupyter-client
  (+jupyter-management--check-monads))

(defun +jupyter-management-get (key object)
  "Return KEY from JSON OBJECT represented by an alist or hash table."
  (cond
   ((hash-table-p object)
    (or (gethash (if (symbolp key) (symbol-name key) key) object)
        (gethash key object)))
   ((listp object)
    (or (alist-get key object)
        (and (symbolp key)
             (alist-get (symbol-name key) object nil nil #'string=))))))

(defun +jupyter-management-target-id (target)
  "Return TARGET's stable ID."
  (if (remote-target-p target) (remote-target-id target) (format "%s" target)))

(defun +jupyter-management-target-label (target)
  "Return TARGET's display label."
  (if (remote-target-p target) (remote-target-label target) (format "%s" target)))

(defun +jupyter-management-local-target-p (target)
  "Return non-nil when TARGET is the machine Emacs runs on.
That is a question of placement, not of the target's name: its files are the
client's own, so client-side kernelspecs, commands and Emacs REPLs apply."
  (and (ignore-errors
         (remote-client-file-name
          (remote-make-file-name
           (+jupyter-management-target-id target) "/")))
       t))

(defun +jupyter-management-command (target kind)
  "Return the executable for KIND on TARGET."
  (if (+jupyter-management-local-target-p target)
      (pcase kind
        ('jupyter +jupyter-board-jupyter-command)
        ('python +jupyter-board-python-command)
        ('remote-ikernel +jupyter-remote-ikernel-command)
        (_ (error "Unknown Jupyter command kind: %s" kind)))
    (pcase kind
      ('jupyter "jupyter")
      ('python "python3")
      ('remote-ikernel "remote_ikernel")
      (_ (error "Unknown Jupyter command kind: %s" kind)))))

(defun +jupyter-management-context (target)
  "Return a Remote context rooted on TARGET."
  (remote-context (remote-target-file-name target)))

(defun +jupyter-management-logical-path (target path)
  "Project target-native PATH into Emacs logical space for TARGET."
  (if (or (not path) (string-empty-p path)
          (+jupyter-management-local-target-p target))
      path
    (remote-target-file-name target path)))

(defun +jupyter-management-argv-option (argv option)
  "Return OPTION's value from ARGV, accepting split and equals forms."
  (let ((tail argv) value)
    (while (and tail (not value))
      (let ((arg (format "%s" (car tail))))
        (cond
         ((string= arg option)
          (setq value (and (cdr tail) (format "%s" (cadr tail)))))
         ((string-prefix-p (concat option "=") arg)
          (setq value (substring arg (1+ (length option)))))))
      (setq tail (cdr tail)))
    value))

(defun +jupyter-management-remote-p (name argv raw)
  "Return non-nil when NAME, ARGV, or RAW identifies remote_ikernel."
  (or (string-prefix-p "rik_" name)
      (cl-some (lambda (arg)
                 (string-match-p "remote_ikernel" (format "%s" arg)))
               argv)
      (+jupyter-management-get 'remote_ikernel_argv raw)))

(defun +jupyter-management-remote-metadata (spec)
  "Return Aaron remote-kernel metadata from SPEC."
  (let* ((metadata (+jupyter-management-get 'metadata spec))
         (aaron (+jupyter-management-get 'aaron metadata)))
    (+jupyter-management-get 'remote_kernel aaron)))

(defun +jupyter-management-origin (resource-dir remote)
  "Classify RESOURCE-DIR and REMOTE into a kernelspec origin."
  (cond
   (remote 'remote-ikernel)
   ((and resource-dir
         (string-match-p "/Noema/jupyter/\.jupyter/data/kernels/" resource-dir))
    'noema-project)
   (t 'system)))

(defun +jupyter-management-launcher-health (target argv)
  "Return a health plist for ARGV on TARGET."
  (let ((launcher (and argv (format "%s" (car argv)))))
    (cond
     ((or (null launcher) (string-empty-p launcher))
      '(:status error :detail "missing argv launcher"))
     ((not (+jupyter-management-local-target-p target))
      (list :status 'unknown :detail (format "target launcher: %s" launcher)))
     ((file-name-absolute-p launcher)
      (if (file-executable-p launcher)
          (list :status 'ok :detail launcher)
        (list :status 'error :detail (format "launcher is not executable: %s" launcher))))
     ((executable-find launcher)
      (list :status 'ok :detail (executable-find launcher)))
     (t (list :status 'error :detail (format "launcher is not on PATH: %s" launcher))))))

(defun +jupyter-management-normalize-spec (target name object)
  "Normalize kernelspec NAME and JSON OBJECT owned by TARGET."
  (let* ((target-id (+jupyter-management-target-id target))
         (native-resource (+jupyter-management-get 'resource_dir object))
         (resource-dir (+jupyter-management-logical-path target native-resource))
         (spec (+jupyter-management-get 'spec object))
         (argv (+jupyter-management-get 'argv spec))
         (raw (and (+jupyter-management-local-target-p target)
                   resource-dir
                   (let ((file (expand-file-name "kernel.json" resource-dir)))
                     (when (file-readable-p file)
                       (condition-case nil
                           (json-parse-string
                            (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string))
                            :object-type 'alist :array-type 'list
                            :null-object nil :false-object nil)
                         (error nil))))))
         (remote (+jupyter-management-remote-p name argv raw))
         (remote-meta (+jupyter-management-remote-metadata spec))
         (group (and remote
                     (or (+jupyter-management-get 'group remote-meta) "core")))
         (origin (+jupyter-management-origin native-resource remote)))
    (list :id (format "spec:%s:%s:%s" target-id origin name)
          :kind 'kernelspec :target target :target-id target-id
          :name name
          :display-name (or (+jupyter-management-get 'display_name spec) name)
          :language (or (+jupyter-management-get 'language spec) "")
          :resource-dir resource-dir :native-resource-dir native-resource
          :spec spec :raw (or raw spec) :argv argv
          :remote remote :remote-meta remote-meta :group group
          :origin origin
          :health (+jupyter-management-launcher-health target argv)
          :interface (and remote (+jupyter-management-argv-option argv "--interface"))
          :host (and remote (+jupyter-management-argv-option argv "--host"))
          :kernel-command (and remote (+jupyter-management-argv-option argv "--kernel_cmd"))
          :workdir (and remote (+jupyter-management-argv-option argv "--workdir")))))

(defun +jupyter-management-parse-specs (target output)
  "Parse Jupyter kernelspec OUTPUT for TARGET."
  (let* ((data (json-parse-string output :object-type 'alist :array-type 'list
                                  :null-object nil :false-object nil))
         (specs (+jupyter-management-get 'kernelspecs data)))
    (sort
     (mapcar (lambda (cell)
               (+jupyter-management-normalize-spec
                target (if (symbolp (car cell)) (symbol-name (car cell)) (car cell))
                (cdr cell)))
             specs)
     (lambda (a b) (string-lessp (plist-get a :name) (plist-get b :name))))))

(defun +jupyter-management-project-specs (target)
  "Return Noema project-owned kernelspecs TARGET can actually reach.
The kernelspec directory ships with this config, so it exists on the client.
Whether a target can use it is a filesystem-sharing question -- the same one
`+noema-jupyter--project-kernelspecs\=' asks -- not a question about the
target\='s name."
  (when (remote-client-file-name (remote-target-file-name target "/"))
    (let ((directory
           (or (and (boundp '+noema-jupyter-kernelspec-directory)
                    +noema-jupyter-kernelspec-directory)
               (expand-file-name
                "site-lisp/noema/jupyter/kernel-templates/"
                user-emacs-directory))))
      (when (file-directory-p directory)
        (cl-loop
         for child in (directory-files directory t "\\`[^.]")
         for file = (expand-file-name "kernel.json" child)
         for parsed = (and
                    (file-readable-p file)
                    (condition-case nil
                        (json-parse-string
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil)
                      (error nil)))
         for raw = (and
                    parsed
                    (if (fboundp '+noema-jupyter--substitute-template)
                        (+noema-jupyter--substitute-template
                         parsed
                         `(("NOEMA_JUPYTER_ROOT" .
                            ,(expand-file-name
                              "site-lisp/noema/jupyter"
                              user-emacs-directory))
                           ("NOEMA_JUPYTER_STATE_ROOT" .
                            ,(expand-file-name
                              "var/noema/jupyter"
                              user-emacs-directory))
                           ("NOEMA_USER_HOME" . ,(expand-file-name "~/"))
                           ("SAGE_VERSION" .
                            ,(or (getenv "NOEMA_SAGE_VERSION")
                                 "current"))))
                      parsed))
         when raw
         collect
         (+jupyter-management-normalize-spec
          target (file-name-nondirectory (directory-file-name child))
          `((resource_dir . ,child) (spec . ,raw))))))))

(defun +jupyter-management-merge-project-specs (target entries)
  "Merge local Noema project specs into target ENTRIES without duplicates."
  (let ((projects (+jupyter-management-project-specs target)))
    (append entries
            (cl-remove-if
             (lambda (project)
               (cl-find (plist-get project :resource-dir) entries
                        :key (lambda (entry) (plist-get entry :resource-dir))
                        :test #'equal))
             projects))))

(defun +jupyter-management-discover-specs (target callback)
  "Discover TARGET kernelspecs asynchronously and invoke CALLBACK.
CALLBACK receives (ENTRIES ERROR)."
  (condition-case error
      (let* ((context (+jupyter-management-context target))
             (command (+jupyter-target-command
                       context (when (+jupyter-management-local-target-p target)
                                 +jupyter-board-jupyter-command))))
      (remote-exec-async
       (car command)
       :args (append (cdr command) '("kernelspec" "list" "--json"))
       :context context
       :filesystem-effects 'none
       :name (format "jupyter-specs-%s" (+jupyter-management-target-id target))
       :callback
       (lambda (result)
         (if (zerop (remote-exec-result-status result))
             (condition-case parse-error
                 (funcall callback
                          (+jupyter-management-merge-project-specs
                           target
                           (+jupyter-management-parse-specs
                            target (remote-exec-result-stdout result)))
                          nil)
               (error (funcall callback nil (error-message-string parse-error))))
           (funcall callback nil
                    (string-trim
                     (concat (remote-exec-result-stdout result) "\n"
                             (remote-exec-result-stderr result))))))))
    (error (funcall callback nil (error-message-string error)) nil)))

(defun +jupyter-management-connection-valid-p (payload)
  "Return non-nil when decoded connection PAYLOAD has the required fields."
  (and (+jupyter-management-get 'transport payload)
       (+jupyter-management-get 'ip payload)
       (+jupyter-management-get 'key payload)
       (cl-every (lambda (key) (numberp (+jupyter-management-get key payload)))
                 '(shell_port iopub_port stdin_port control_port hb_port))))

(defun +jupyter-management-scan-connections (directory)
  "Return normalized local connection files under DIRECTORY."
  (when (file-directory-p directory)
    (mapcar
     (lambda (file)
       (let* ((attributes (file-attributes file))
              (mtime (file-attribute-modification-time attributes))
              (payload
               (condition-case nil
                   (json-parse-string
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))
                    :object-type 'alist :array-type 'list
                    :null-object nil :false-object nil)
                 (error nil)))
              (valid (and payload (+jupyter-management-connection-valid-p payload))))
         (list :id (concat "connection:" file) :kind 'connection
               :file file :payload payload :valid valid
               :kernel (or (+jupyter-management-get 'kernel_name payload) "")
               :transport (or (+jupyter-management-get 'transport payload) "")
               :ip (or (+jupyter-management-get 'ip payload) "")
               :mtime mtime
               :stale (> (float-time (time-subtract nil mtime))
                         +jupyter-management-connection-stale-seconds))))
     (sort (directory-files directory t "\\(?:kernel-.*\\|.*connection.*\\)\\.json\\'")
           #'string-lessp))))

(defvar +jupyter-management--cleanup-times (make-hash-table :test #'equal))
(defvar +jupyter-management--cleanup-timer nil)

(defun +jupyter-management--prune-directory (target directory callback &optional force)
  "Maintain TARGET's native runtime DIRECTORY, then invoke CALLBACK.
Unless FORCE is non-nil, skip cleanup performed in the last 15 minutes."
  (let* ((key (cons (+jupyter-management-target-id target) directory))
         (last (gethash key +jupyter-management--cleanup-times 0)))
    (if (and (not force) (< (- (float-time) last) 900))
        (funcall callback nil)
      (puthash key (float-time) +jupyter-management--cleanup-times)
      (condition-case error
          (remote-exec-async
           (+jupyter-management-command target 'python)
           :args (list "-c" (with-temp-buffer
                              (insert-file-contents
                               (expand-file-name "etc/jupyter/prune-connections.py" user-emacs-directory))
                              (buffer-string))
                       "--runtime-dir" directory "--apply")
           :context (+jupyter-management-context target)
           :filesystem-effects 'content :name "jupyter-connection-cleanup"
           :callback
           (lambda (result)
             (if (eq (remote-exec-result-status result) 0)
                 (funcall callback nil)
               (remhash key +jupyter-management--cleanup-times)
               (funcall callback
                        (format "Connection cleanup exited %s: %s"
                                (remote-exec-result-status result)
                                (string-trim (or (remote-exec-result-stderr result) "")))))))
        (error
         (remhash key +jupyter-management--cleanup-times)
         (funcall callback (error-message-string error)))))))

(defun +jupyter-management-discover-connections (target callback &optional maintain)
  "Discover TARGET's Jupyter connection files asynchronously.
With MAINTAIN, prune abandoned files first; `force' bypasses the throttle."
  (condition-case error
      (let* ((context (+jupyter-management-context target))
             (command (+jupyter-target-command
                       context (when (+jupyter-management-local-target-p target)
                                 +jupyter-board-jupyter-command))))
      (remote-exec-async
       (car command)
       :args (append (cdr command) '("--runtime-dir"))
       :context context
       :filesystem-effects 'none
       :name "jupyter-runtime-dir"
       :callback
       (lambda (result)
         (if (zerop (remote-exec-result-status result))
             (let ((directory (string-trim (remote-exec-result-stdout result))))
               (let ((scan
                      (lambda (&optional cleanup-error)
                        (if cleanup-error
                            (funcall callback nil cleanup-error)
                          (condition-case scan-error
                              (funcall callback
                                       (+jupyter-management-scan-connections
                                        (+jupyter-management-logical-path target directory)) nil)
                            (error (funcall callback nil (error-message-string scan-error))))))))
                 (if maintain
                     (+jupyter-management--prune-directory
                      target directory scan (eq maintain 'force))
                   (funcall scan))))
           (funcall callback nil (string-trim (remote-exec-result-stderr result)))))))
    (error (funcall callback nil (error-message-string error)) nil)))

(defun +jupyter-management-maintain-connections ()
  "Run bounded local connection maintenance without blocking the editor."
  (when (and (stringp +jupyter-board-jupyter-command)
             (stringp +jupyter-board-python-command))
    (when (fboundp 'jupyter--gc-kernel-processes)
      (ignore-errors (jupyter--gc-kernel-processes)))
    (+jupyter-management-discover-connections
     "local" (lambda (&rest _) nil) t)))

(unless (or noninteractive (timerp +jupyter-management--cleanup-timer))
  (setq +jupyter-management--cleanup-timer
        (run-with-timer 60 900 #'+jupyter-management-maintain-connections)))

(defun +jupyter-management-object-slot (object slot)
  "Return OBJECT's SLOT, or nil when it is absent or unbound."
  (condition-case nil (slot-value object slot) (error nil)))

(defun +jupyter-management-emacs-clients ()
  "Return a passive snapshot of live emacs-jupyter clients."
  (when (and (require 'jupyter-repl nil t) (boundp 'jupyter--clients))
    (cl-loop
     for client in (ignore-errors (jupyter-all-objects 'jupyter--clients))
     for buffer = (+jupyter-management-object-slot client 'buffer)
     when (and client (buffer-live-p buffer))
     collect
     (list :id (format "runtime:emacs:%s" (sxhash-eq client))
           :kind 'runtime :provider 'emacs-repl :target-id "local"
           :client client :buffer buffer :title (buffer-name buffer)
           :kernel (or (ignore-errors (jupyter-kernel-language client)) "kernel")
           :status (or (+jupyter-management-object-slot
                        client 'execution-state) "unknown")))))

(defun +jupyter-management-run-repl (entry source-buffer)
  "Run ENTRY's kernelspec REPL, using SOURCE-BUFFER as context."
  (unless (and (eq (plist-get entry :kind) 'kernelspec)
               (or (ignore-errors
                     (remote-client-file-name
                      (remote-make-file-name
                       (or (plist-get entry :target-id) "local") "/")))
                   (plist-get entry :remote)))
    (user-error "This target kernelspec is not client-accessible; use remote_ikernel"))
  (require 'jupyter-repl)
  (let ((source (and (buffer-live-p source-buffer) source-buffer))
        (process-environment (copy-sequence process-environment)))
    (when (eq (plist-get entry :origin) 'noema-project)
      (let* ((data-dir (expand-file-name "../.." (plist-get entry :resource-dir)))
             (existing (getenv "JUPYTER_PATH")))
        (setenv "JUPYTER_PATH"
                (if (and existing (not (string-empty-p existing)))
                    (concat data-dir path-separator existing)
                  data-dir))))
    (with-current-buffer (or source (current-buffer))
      (jupyter-run-repl (plist-get entry :name) nil t nil t))))

(defun +jupyter-management-connect-repl (entry source-buffer)
  "Connect a REPL using connection ENTRY and SOURCE-BUFFER context."
  (unless (plist-get entry :valid) (user-error "Connection file is invalid"))
  (require 'jupyter-repl)
  (with-current-buffer (if (buffer-live-p source-buffer) source-buffer (current-buffer))
    (jupyter-connect-repl (plist-get entry :file) nil t nil t)))

(defun +jupyter-management-client-action (entry action)
  "Apply lifecycle ACTION to an Emacs runtime ENTRY."
  (let ((client (plist-get entry :client)))
    (unless client (user-error "Jupyter client is no longer available"))
    (pcase action
      ('interrupt (jupyter-interrupt-kernel client))
      ('restart (jupyter-restart-kernel client))
      ('shutdown (jupyter-shutdown-kernel client))
      (_ (user-error "Unsupported client action: %s" action)))))

(provide 'tools/jupyter/management)
;;; tools/jupyter/management.el ends here
