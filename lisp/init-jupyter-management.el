;;; init-jupyter-management.el --- Unified Jupyter management data -*- lexical-binding: t; -*-

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
(require 'subr-x)

(declare-function jupyter-all-objects "jupyter-base" (tracking-symbol))
(declare-function jupyter-connect-repl "jupyter-repl" (file &optional repl-name associate-buffer client-class display))
(declare-function jupyter-interrupt-kernel "jupyter-client" (client))
(declare-function jupyter-kernel-language "jupyter-client" (&optional client))
(declare-function jupyter-restart-kernel "jupyter-client" (client))
(declare-function jupyter-run-repl "jupyter-repl" (kernel-name &optional repl-name associate-buffer client-class display))
(declare-function jupyter-shutdown-kernel "jupyter-client" (client))

(config-defvar my/jupyter-board-jupyter-command nil
  "Local Jupyter executable used by the management board."
  :type 'file)

(config-defvar my/jupyter-board-python-command nil
  "Local Python executable used by Jupyter maintenance commands."
  :type 'file)

(config-defvar my/jupyter-remote-ikernel-command nil
  "Local vendored remote_ikernel command."
  :type 'file)

(config-defvar my/jupyter-remote-ikernel-source-directory nil
  "Vendored remote_ikernel source directory."
  :type 'directory)

(config-defvar my/jupyter-remote-ikernel-install-script nil
  "Script that installs the vendored remote_ikernel source."
  :type 'file)

(defconst my/jupyter-management-connection-stale-seconds (* 24 60 60)
  "Age after which an unused connection file is reported as stale.")

(defun my/jupyter-management-get (key object)
  "Return KEY from JSON OBJECT represented by an alist or hash table."
  (cond
   ((hash-table-p object)
    (or (gethash (if (symbolp key) (symbol-name key) key) object)
        (gethash key object)))
   ((listp object)
    (or (alist-get key object)
        (and (symbolp key)
             (alist-get (symbol-name key) object nil nil #'string=))))))

(defun my/jupyter-management-target-id (target)
  "Return TARGET's stable ID."
  (if (remote-target-p target) (remote-target-id target) (format "%s" target)))

(defun my/jupyter-management-target-label (target)
  "Return TARGET's display label."
  (if (remote-target-p target) (remote-target-label target) (format "%s" target)))

(defun my/jupyter-management-local-target-p (target)
  "Return non-nil when TARGET denotes the local machine."
  (equal (my/jupyter-management-target-id target) "local"))

(defun my/jupyter-management-command (target kind)
  "Return the executable for KIND on TARGET."
  (if (my/jupyter-management-local-target-p target)
      (pcase kind
        ('jupyter my/jupyter-board-jupyter-command)
        ('python my/jupyter-board-python-command)
        ('remote-ikernel my/jupyter-remote-ikernel-command)
        (_ (error "Unknown Jupyter command kind: %s" kind)))
    (pcase kind
      ('jupyter "jupyter")
      ('python "python3")
      ('remote-ikernel "remote_ikernel")
      (_ (error "Unknown Jupyter command kind: %s" kind)))))

(defun my/jupyter-management-context (target)
  "Return a Remote context rooted on TARGET."
  (remote-context (remote-target-file-name target)))

(defun my/jupyter-management-logical-path (target path)
  "Project target-native PATH into Emacs logical space for TARGET."
  (if (or (not path) (string-empty-p path)
          (my/jupyter-management-local-target-p target))
      path
    (remote-target-file-name target path)))

(defun my/jupyter-management-argv-option (argv option)
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

(defun my/jupyter-management-remote-p (name argv raw)
  "Return non-nil when NAME, ARGV, or RAW identifies remote_ikernel."
  (or (string-prefix-p "rik_" name)
      (cl-some (lambda (arg)
                 (string-match-p "remote_ikernel" (format "%s" arg)))
               argv)
      (my/jupyter-management-get 'remote_ikernel_argv raw)))

(defun my/jupyter-management-remote-metadata (spec)
  "Return Aaron remote-kernel metadata from SPEC."
  (let* ((metadata (my/jupyter-management-get 'metadata spec))
         (aaron (my/jupyter-management-get 'aaron metadata)))
    (my/jupyter-management-get 'remote_kernel aaron)))

(defun my/jupyter-management-origin (resource-dir remote)
  "Classify RESOURCE-DIR and REMOTE into a kernelspec origin."
  (cond
   (remote 'remote-ikernel)
   ((and resource-dir
         (string-match-p "/Noema/jupyter/\.jupyter/data/kernels/" resource-dir))
    'noema-project)
   (t 'system)))

(defun my/jupyter-management-launcher-health (target argv)
  "Return a health plist for ARGV on TARGET."
  (let ((launcher (and argv (format "%s" (car argv)))))
    (cond
     ((or (null launcher) (string-empty-p launcher))
      '(:status error :detail "missing argv launcher"))
     ((not (my/jupyter-management-local-target-p target))
      (list :status 'unknown :detail (format "target launcher: %s" launcher)))
     ((file-name-absolute-p launcher)
      (if (file-executable-p launcher)
          (list :status 'ok :detail launcher)
        (list :status 'error :detail (format "launcher is not executable: %s" launcher))))
     ((executable-find launcher)
      (list :status 'ok :detail (executable-find launcher)))
     (t (list :status 'error :detail (format "launcher is not on PATH: %s" launcher))))))

(defun my/jupyter-management-normalize-spec (target name object)
  "Normalize kernelspec NAME and JSON OBJECT owned by TARGET."
  (let* ((target-id (my/jupyter-management-target-id target))
         (native-resource (my/jupyter-management-get 'resource_dir object))
         (resource-dir (my/jupyter-management-logical-path target native-resource))
         (spec (my/jupyter-management-get 'spec object))
         (argv (my/jupyter-management-get 'argv spec))
         (raw (and (my/jupyter-management-local-target-p target)
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
         (remote (my/jupyter-management-remote-p name argv raw))
         (remote-meta (my/jupyter-management-remote-metadata spec))
         (group (and remote
                     (or (my/jupyter-management-get 'group remote-meta) "core")))
         (origin (my/jupyter-management-origin native-resource remote)))
    (list :id (format "spec:%s:%s:%s" target-id origin name)
          :kind 'kernelspec :target target :target-id target-id
          :name name
          :display-name (or (my/jupyter-management-get 'display_name spec) name)
          :language (or (my/jupyter-management-get 'language spec) "")
          :resource-dir resource-dir :native-resource-dir native-resource
          :spec spec :raw (or raw spec) :argv argv
          :remote remote :remote-meta remote-meta :group group
          :origin origin
          :health (my/jupyter-management-launcher-health target argv)
          :interface (and remote (my/jupyter-management-argv-option argv "--interface"))
          :host (and remote (my/jupyter-management-argv-option argv "--host"))
          :kernel-command (and remote (my/jupyter-management-argv-option argv "--kernel_cmd"))
          :workdir (and remote (my/jupyter-management-argv-option argv "--workdir")))))

(defun my/jupyter-management-parse-specs (target output)
  "Parse Jupyter kernelspec OUTPUT for TARGET."
  (let* ((data (json-parse-string output :object-type 'alist :array-type 'list
                                  :null-object nil :false-object nil))
         (specs (my/jupyter-management-get 'kernelspecs data)))
    (sort
     (mapcar (lambda (cell)
               (my/jupyter-management-normalize-spec
                target (if (symbolp (car cell)) (symbol-name (car cell)) (car cell))
                (cdr cell)))
             specs)
     (lambda (a b) (string-lessp (plist-get a :name) (plist-get b :name))))))

(defun my/jupyter-management-project-specs (target)
  "Return Noema project-owned kernelspecs TARGET can actually reach.
The kernelspec directory ships with this config, so it exists on the client.
Whether a target can use it is a filesystem-sharing question -- the same one
`my/noema-jupyter--project-kernelspecs\=' asks -- not a question about the
target\='s name."
  (when (remote-client-file-name (remote-target-file-name target "/"))
    (let ((directory
           (or (and (boundp 'my/noema-jupyter-kernelspec-directory)
                    my/noema-jupyter-kernelspec-directory)
               (expand-file-name
                "lisp/roam/Noema/jupyter/.jupyter/data/kernels/"
                user-emacs-directory))))
      (when (file-directory-p directory)
        (cl-loop
         for child in (directory-files directory t "\\`[^.]")
         for file = (expand-file-name "kernel.json" child)
         for raw = (and
                    (file-readable-p file)
                    (condition-case nil
                        (json-parse-string
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil)
                      (error nil)))
         when raw
         collect
         (my/jupyter-management-normalize-spec
          target (file-name-nondirectory (directory-file-name child))
          `((resource_dir . ,child) (spec . ,raw))))))))

(defun my/jupyter-management-merge-project-specs (target entries)
  "Merge local Noema project specs into target ENTRIES without duplicates."
  (let ((projects (my/jupyter-management-project-specs target)))
    (append entries
            (cl-remove-if
             (lambda (project)
               (cl-find (plist-get project :resource-dir) entries
                        :key (lambda (entry) (plist-get entry :resource-dir))
                        :test #'equal))
             projects))))

(defun my/jupyter-management-discover-specs (target callback)
  "Discover TARGET kernelspecs asynchronously and invoke CALLBACK.
CALLBACK receives (ENTRIES ERROR)."
  (condition-case error
      (remote-exec-async
       (my/jupyter-management-command target 'jupyter)
       :args '("kernelspec" "list" "--json")
       :context (my/jupyter-management-context target)
       :filesystem-effects 'none
       :name (format "jupyter-specs-%s" (my/jupyter-management-target-id target))
       :callback
       (lambda (result)
         (if (zerop (remote-exec-result-status result))
             (condition-case parse-error
                 (funcall callback
                          (my/jupyter-management-merge-project-specs
                           target
                           (my/jupyter-management-parse-specs
                            target (remote-exec-result-stdout result)))
                          nil)
               (error (funcall callback nil (error-message-string parse-error))))
           (funcall callback nil
                    (string-trim
                     (concat (remote-exec-result-stdout result) "\n"
                             (remote-exec-result-stderr result)))))))
    (error (funcall callback nil (error-message-string error)) nil)))

(defun my/jupyter-management-connection-valid-p (payload)
  "Return non-nil when decoded connection PAYLOAD has the required fields."
  (and (my/jupyter-management-get 'transport payload)
       (my/jupyter-management-get 'ip payload)
       (my/jupyter-management-get 'key payload)
       (cl-every (lambda (key) (numberp (my/jupyter-management-get key payload)))
                 '(shell_port iopub_port stdin_port control_port hb_port))))

(defun my/jupyter-management-scan-connections (directory)
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
              (valid (and payload (my/jupyter-management-connection-valid-p payload))))
         (list :id (concat "connection:" file) :kind 'connection
               :file file :payload payload :valid valid
               :kernel (or (my/jupyter-management-get 'kernel_name payload) "")
               :transport (or (my/jupyter-management-get 'transport payload) "")
               :ip (or (my/jupyter-management-get 'ip payload) "")
               :mtime mtime
               :stale (> (float-time (time-subtract nil mtime))
                         my/jupyter-management-connection-stale-seconds))))
     (sort (directory-files directory t "\\(?:kernel-.*\\|.*connection.*\\)\\.json\\'")
           #'string-lessp))))

(defun my/jupyter-management-discover-connections (target callback)
  "Discover TARGET's Jupyter connection files and invoke CALLBACK.
The runtime directory is read from TARGET's own `jupyter --runtime-dir\=' and
then scanned through ordinary file APIs, which reach it via the `/fs\=' handler.
Returning nothing for a non-local target used to make `attach:\=' unusable
anywhere but the client machine, even though every step here is already
routed."
  (condition-case error
      (remote-exec-async
       (my/jupyter-management-command target 'jupyter)
       :args '("--runtime-dir")
       :context (my/jupyter-management-context target)
       :filesystem-effects 'none
       :name "jupyter-runtime-dir"
       :callback
       (lambda (result)
         (if (zerop (remote-exec-result-status result))
             (condition-case scan-error
                 (funcall callback
                          (my/jupyter-management-scan-connections
                           (my/jupyter-management-logical-path
                            target
                            (string-trim (remote-exec-result-stdout result))))
                          nil)
               (error (funcall callback nil (error-message-string scan-error))))
           (funcall callback nil (string-trim (remote-exec-result-stderr result))))))
    (error (funcall callback nil (error-message-string error)) nil)))

(defun my/jupyter-management-object-slot (object slot)
  "Return OBJECT's SLOT, or nil when it is absent or unbound."
  (condition-case nil (slot-value object slot) (error nil)))

(defun my/jupyter-management-emacs-clients ()
  "Return a passive snapshot of live emacs-jupyter clients."
  (when (and (require 'jupyter-repl nil t) (boundp 'jupyter--clients))
    (cl-loop
     for client in (ignore-errors (jupyter-all-objects 'jupyter--clients))
     for buffer = (my/jupyter-management-object-slot client 'buffer)
     when (and client (buffer-live-p buffer))
     collect
     (list :id (format "runtime:emacs:%s" (sxhash-eq client))
           :kind 'runtime :provider 'emacs-repl :target-id "local"
           :client client :buffer buffer :title (buffer-name buffer)
           :kernel (or (ignore-errors (jupyter-kernel-language client)) "kernel")
           :status (or (my/jupyter-management-object-slot
                        client 'execution-state) "unknown")))))

(defun my/jupyter-management-run-repl (entry source-buffer)
  "Run ENTRY's kernelspec REPL, using SOURCE-BUFFER as context."
  (unless (and (eq (plist-get entry :kind) 'kernelspec)
               (or (equal (plist-get entry :target-id) "local")
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
      (jupyter-run-repl (plist-get entry :name) nil nil nil t))))

(defun my/jupyter-management-connect-repl (entry source-buffer)
  "Connect a REPL using connection ENTRY and SOURCE-BUFFER context."
  (unless (plist-get entry :valid) (user-error "Connection file is invalid"))
  (require 'jupyter-repl)
  (with-current-buffer (if (buffer-live-p source-buffer) source-buffer (current-buffer))
    (jupyter-connect-repl (plist-get entry :file) nil nil nil t)))

(defun my/jupyter-management-client-action (entry action)
  "Apply lifecycle ACTION to an Emacs runtime ENTRY."
  (let ((client (plist-get entry :client)))
    (unless client (user-error "Jupyter client is no longer available"))
    (pcase action
      ('interrupt (jupyter-interrupt-kernel client))
      ('restart (jupyter-restart-kernel client))
      ('shutdown (jupyter-shutdown-kernel client))
      (_ (user-error "Unsupported client action: %s" action)))))

(provide 'init-jupyter-management)
;;; init-jupyter-management.el ends here
