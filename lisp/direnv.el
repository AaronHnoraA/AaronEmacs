;;; direnv.el --- In-tree direnv integration for logical targets -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This replaces the third-party direnv.el package.  Both local and remote
;; workspaces execute direnv through the remote routing layer.  The dedicated
;; adapter prefers tramp-rpc and falls back to standard TRAMP.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'sh-script)
(require 'subr-x)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-process)
(require 'remote-environment)

(defgroup direnv nil
  "Buffer-local direnv environments."
  :group 'environment)

(defcustom direnv-always-show-summary nil
  "Whether environment refreshes should report a summary."
  :type 'boolean
  :group 'direnv)

(defcustom direnv-transport-busy-retry-delay 0.25
  "Seconds before retrying an automatic refresh while TRAMP is busy.
Automatic refreshes must not start file operations from a timer which has
interrupted an active TRAMP transaction."
  :type 'number
  :group 'direnv)

(defcustom direnv-export-cache-timeout 300
  "Seconds an unchanged asynchronous direnv export remains reusable."
  :type 'number
  :group 'direnv)

(defcustom direnv-export-failure-retry-delay 2
  "Seconds before automatically retrying an unchanged failed export.
Selection and buffer-list hooks can run many times per second.  A malformed
or temporarily failing `direnv export' must not turn those hooks into an
unbounded subprocess loop."
  :type 'number
  :group 'direnv)

(defvar direnv--hooks
  '(before-hack-local-variables-hook)
  "Hooks used while `direnv-mode' is enabled.")

(defvar direnv--last-error nil)
(defvar direnv--refresh-timers (make-hash-table :test #'eq :weakness 'key))
(defvar direnv--export-cache (make-hash-table :test #'equal))
(defvar direnv--export-processes (make-hash-table :test #'equal))
(defvar direnv--export-failures (make-hash-table :test #'equal)
  "Map envrc roots to their most recent fingerprinted export failure.")
(defvar direnv--export-waiters (make-hash-table :test #'equal)
  "Map an envrc root to `(BUFFER . CALLBACKS)' export waiters.")
(defvar direnv--reported-selection nil
  "Last direnv state reported for the selected buffer.
The value is `(TARGET ROOT LINK-PLUGIN LINK-ID)', or nil outside an envrc
tree.  Environment application remains buffer-local; this state only
coalesces user-facing enter/switch/leave reports.")
(defvar-local direnv--active-root nil
  "Canonical envrc root currently applied to this buffer.")

(declare-function tramp-get-connection-property
                  "tramp-cache" (key property &optional default))

(defmacro direnv--with-base-process-environment (&rest body)
  "Evaluate BODY from the buffer's pre-direnv process environment.
An already applied `DIRENV_DIFF' makes `direnv export json' emit no output,
because direnv correctly sees no delta.  Providers must always rebuild from
the saved base capsule rather than recursively inheriting their own result."
  (declare (indent 0) (debug t))
  `(let ((process-environment
          (copy-sequence
           (or remote--buffer-base-process-environment
               process-environment)))
         (exec-path
          (copy-sequence
           (or remote--buffer-base-exec-path exec-path))))
     ,@body))

(defun direnv--transport-busy-p ()
  "Return non-nil when a TRAMP file operation is in progress.
This consults only dynamic state and TRAMP's local connection cache."
  (or
   (and (boundp 'tramp-current-connection)
        tramp-current-connection)
   (and
    (fboundp 'tramp-get-connection-property)
    (seq-some
     (lambda (process)
       (and
        (process-get process 'tramp-vector)
        (ignore-errors
          (tramp-get-connection-property process "locked"))))
     (process-list)))))

(defun direnv--export-active-p (root)
  "Return non-nil while ROOT has an export starting or running."
  (let ((value (gethash root direnv--export-processes)))
    (or (eq value 'starting)
        (and (processp value) (process-live-p value)))))

(defun direnv--transport-connection-path-p (path)
  "Return non-nil when PATH is a transport's internal connection prefix.
TRAMP uses names such as `/ssh:host:' (with an empty localname) for its own
connection buffers.  They are not visiting a target directory and must not
trigger project or `.envrc' discovery while a connection is being established."
  (or
   (string-prefix-p "*tramp/" (buffer-name))
   (and
    (stringp path)
    (tramp-tramp-file-p path)
    (when-let* ((vector
                 (ignore-errors
                   (tramp-dissect-file-name path nil))))
      (string-empty-p
       (or (tramp-file-name-localname vector) ""))))))

(defun direnv--directory (&optional path)
  "Return canonical logical directory for PATH."
  (let ((path (or path buffer-file-name default-directory)))
    (unless (direnv--transport-connection-path-p path)
      (when-let* ((canonical
                   (and path (remote-canonicalize-file-name path))))
        (file-name-as-directory
         (if (file-directory-p canonical)
             canonical
           (file-name-directory canonical)))))))

(defun direnv--envrc-root (&optional path)
  "Return logical workspace root containing PATH's `.envrc'."
  (when-let* ((directory (direnv--directory path)))
    (when-let* ((root (locate-dominating-file directory ".envrc")))
      (file-name-as-directory
       (remote-canonicalize-file-name root)))))

(defun direnv--fingerprint (context)
  "Return `.envrc' state for CONTEXT."
  (when-let* ((root
               (direnv--envrc-root
                (remote-context-workspace-root context)))
              (envrc (expand-file-name ".envrc" root))
              (attributes (file-attributes envrc 'string)))
    (list root
          (file-attribute-size attributes)
          (file-attribute-modification-time attributes))))

(defun direnv--read-json-environment (text)
  "Return environment alist parsed from direnv JSON TEXT."
  (condition-case err
      (let ((object
             (json-parse-string
              text :object-type 'alist :array-type 'list
              :null-object nil :false-object nil)))
        (mapcar
         (lambda (entry)
           (cons (format "%s" (car entry)) (cdr entry)))
         object))
    (error
     (error "Invalid `direnv export json' output: %s"
            (error-message-string err)))))

(defun direnv--cached-export (root fingerprint)
  "Return cached export for ROOT matching FINGERPRINT."
  (when-let* ((entry (gethash root direnv--export-cache))
              ((equal fingerprint (plist-get entry :fingerprint)))
              (loaded-at (plist-get entry :loaded-at))
              ((< (- (float-time) loaded-at)
                  direnv-export-cache-timeout)))
    (plist-get entry :result)))

(defun direnv--cache-export (root fingerprint result)
  "Cache RESULT for ROOT and FINGERPRINT."
  (remhash root direnv--export-failures)
  (puthash
   root
   (list :fingerprint fingerprint
         :loaded-at (float-time)
         :result result)
   direnv--export-cache)
  result)

(defun direnv--record-export-failure (root fingerprint error)
  "Record ERROR for ROOT and FINGERPRINT and return ERROR."
  (puthash
   root
   (list :fingerprint fingerprint
         :failed-at (float-time)
         :error error)
   direnv--export-failures)
  error)

(defun direnv--recent-export-failure (root fingerprint)
  "Return ROOT's recent ERROR for FINGERPRINT, or nil when retry is due."
  (when-let* ((entry (gethash root direnv--export-failures)))
    (if (and
         (equal fingerprint (plist-get entry :fingerprint))
         (< (- (float-time) (plist-get entry :failed-at))
            direnv-export-failure-retry-delay))
        (plist-get entry :error)
      (remhash root direnv--export-failures)
      nil)))

(defun direnv--export (context)
  "Load direnv environment for CONTEXT."
  (when-let* ((root
               (direnv--envrc-root
                (remote-context-workspace-root context))))
    (let* ((context (remote-context root))
           (fingerprint (direnv--fingerprint context))
           (cached (direnv--cached-export root fingerprint))
           (remote-current-adapter-id "direnv"))
      (or
      cached
       (direnv--with-base-process-environment
         (unless (let ((remote-environment-inhibit t))
                   (remote-executable-find "direnv" context))
           (error "direnv is not installed on target %s"
                  (remote-context-target-id context)))
         (let* ((result
                 (remote-exec
                  "direnv"
                  :args '("export" "json")
                  :adapter "direnv"
                  :context context
                  :check t))
                (route (remote-exec-result-route result))
                (export
                 (list
                  :vars
                  (direnv--read-json-environment
                   (remote-exec-result-stdout result))
                  :source
                  (list 'direnv root
                        (and route
                             (remote-route-link-plugin-id route))
                        (and route
                             (remote-route-link-id route))))))
           (direnv--cache-export root fingerprint export)))))))

(defun direnv--queue-export-waiter (root buffer callback)
  "Queue BUFFER and optional CALLBACK for ROOT's current export."
  (let* ((waiters (gethash root direnv--export-waiters))
         (waiter (assq buffer waiters))
         (callbacks
          (when waiter
            (let ((value (cdr waiter)))
              ;; Normalize entries created by an older loaded definition,
              ;; whose cdr held one callback instead of a callback list.
              (cond
               ((null value) nil)
               ((functionp value) (list value))
               (t value))))))
    (when (and callback (not (memq callback callbacks)))
      (push callback callbacks))
    (if waiter
        (setcdr waiter callbacks)
      (push (cons buffer callbacks) waiters))
    (puthash root waiters direnv--export-waiters)))

(defun direnv--notify-export-callbacks
    (root callbacks environment error)
  "Notify CALLBACKS for ROOT with ENVIRONMENT and ERROR.
One faulty consumer must not prevent the remaining process-boundary waiters
from resuming."
  (dolist (callback callbacks)
    (condition-case callback-error
        (funcall callback environment error)
      (error
       (remote-log
        'direnv-callback-error
        :root root
        :error (error-message-string callback-error))))))

(defun direnv--apply-export-waiters (root context error)
  "Apply ROOT's cached environment to waiters, or deliver ERROR."
  (let ((waiters (prog1 (gethash root direnv--export-waiters)
                   (remhash root direnv--export-waiters))))
    (dolist (waiter waiters)
      (pcase-let ((`(,buffer . ,callbacks) waiter))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (if error
                (progn
                  (setq direnv--last-error error)
                  (direnv--notify-export-callbacks
                   root callbacks nil error))
              (condition-case err
                  (let ((environment
                         (remote-environment-ensure context)))
                    (setq direnv--last-error nil)
                    (direnv--notify-export-callbacks
                     root callbacks environment nil))
                (error
                 (setq direnv--last-error err)
                 (direnv--notify-export-callbacks
                  root callbacks nil err))))))))))

(defun direnv--finish-export-waiters (root context &optional error)
  "Apply ROOT's export waiters later, outside the backend receive loop."
  (run-with-idle-timer
   0.01 nil #'direnv--apply-export-waiters root context error))

(defun direnv--start-export
    (context root fingerprint buffer &optional callback)
  "Start one asynchronous export for CONTEXT at ROOT.
BUFFER receives the resulting environment.  CALLBACK, when non-nil, receives
two arguments: the environment and an error."
  (direnv--queue-export-waiter root buffer callback)
  (unless (direnv--export-active-p root)
    (let ((remote-environment-inhibit t)
          process)
      ;; Claim ROOT before process creation.  TRAMP startup can run timers and
      ;; hooks; a nested refresh must see this in-flight startup.
      (puthash root 'starting direnv--export-processes)
      (condition-case startup-error
          (progn
            (setq
             process
             (direnv--with-base-process-environment
               (remote-exec-async
                "direnv"
                :args '("export" "json")
                :adapter "direnv"
                :context context
                :name (format "direnv-%s"
                              (remote-context-target-id context))
                :callback
                (lambda (result)
		(remhash root direnv--export-processes)
		(if (zerop (remote-exec-result-status result))
		    (condition-case err
			(let* ((route (remote-exec-result-route result))
                               (export
				(list
				 :vars
				 (direnv--read-json-environment
				  (remote-exec-result-stdout result))
				 :source
				 (list
				  'direnv root
				  (and route
                                       (remote-route-link-plugin-id route))
				  (and route
                                       (remote-route-link-id route))))))
			  (direnv--cache-export root fingerprint export)
			  (remote-environment-invalidate
			   (remote-context-target-id context))
			  (direnv--finish-export-waiters root context))
                      (error
                       (direnv--record-export-failure
                        root fingerprint err)
                       (setq direnv--last-error err)
                       (direnv--finish-export-waiters root context err)
                       (remote-log
			'direnv-error
			:target (remote-context-target-id context)
			:error (error-message-string err))))
		  (let ((err
			 (list
			  'remote-file-error
			  (format "direnv exited with %s: %s"
				  (remote-exec-result-status result)
				  (remote-exec-result-stderr result)))))
		    (setq direnv--last-error err)
                    (direnv--record-export-failure
                     root fingerprint err)
		    (direnv--finish-export-waiters root context err)
		    (remote-log
		     'direnv-error
		     :target (remote-context-target-id context)
		     :error (cadr err))))))))
            ;; Do not resurrect an entry if a very short-lived process has
            ;; already completed and its callback removed the sentinel.
            (when (eq (gethash root direnv--export-processes) 'starting)
              (puthash root process direnv--export-processes))
            process)
        (error
         (when (eq (gethash root direnv--export-processes) 'starting)
           (remhash root direnv--export-processes))
         (direnv--record-export-failure
          root fingerprint startup-error)
         (direnv--finish-export-waiters root context startup-error)
         (signal (car startup-error) (cdr startup-error)))))))

(defun direnv-environment-ensure-async
    (&optional path callback)
  "Ensure PATH's direnv environment without blocking on `direnv export'.
Return `ready' when no export is needed and `pending' when CALLBACK will run
later.  CALLBACK receives two arguments: environment and error.  It is called
only for a pending request, in the requesting buffer."
  (let ((buffer (current-buffer)))
    (cond
     ((direnv--transport-busy-p)
      (run-with-idle-timer
       direnv-transport-busy-retry-delay nil
       (lambda (target target-path done)
         (when (buffer-live-p target)
           (with-current-buffer target
             (direnv-environment-ensure-async target-path done))))
       buffer path callback)
      'pending)
     (t
      (condition-case err
          (if-let* ((root (direnv--envrc-root path)))
              (let* ((context (remote-context root))
                     (fingerprint (direnv--fingerprint context))
                     (recent-error
                      (direnv--recent-export-failure root fingerprint)))
                (cond
                 ((direnv--cached-export root fingerprint)
                  (remote-environment-ensure context)
                  (setq direnv--last-error nil)
                  'ready)
                 (recent-error
                  (setq direnv--last-error recent-error)
                  (when callback
                    (run-with-idle-timer
                     0 nil
                     (lambda (target done failure)
                       (when (buffer-live-p target)
                         (with-current-buffer target
                           (funcall done nil failure))))
                     buffer callback recent-error))
                  'pending)
                 (t
                  (direnv--start-export
                   context root fingerprint buffer callback)
                  'pending)))
            (direnv-clear-environment)
            'ready)
        (error
         (setq direnv--last-error err)
         (when callback
           (run-with-idle-timer
            0 nil
            (lambda (target done failure)
              (when (buffer-live-p target)
                (with-current-buffer target
                  (funcall done nil failure))))
            buffer callback err))
         'pending))))))

(defun direnv--environment-source (environment)
  "Return direnv source metadata from ENVIRONMENT, or nil."
  (seq-find
   (lambda (source)
     (and (consp source) (eq (car source) 'direnv)))
   (remote-environment-sources environment)))

(defun direnv--selected-buffer-p ()
  "Return non-nil when the current buffer is visibly selected."
  (when-let* ((window (or (minibuffer-selected-window)
                          (selected-window))))
    (and (window-live-p window)
         (eq (current-buffer) (window-buffer window)))))

(defun direnv--report-environment-transition (&optional environment)
  "Report the selected buffer's transition to ENVIRONMENT.
Nil ENVIRONMENT means that the selected buffer uses its pre-framework base
environment.  Reapplying the same root through multiple file/window hooks is
silent."
  (when (direnv--selected-buffer-p)
    (let* ((source (and environment
                        (direnv--environment-source environment)))
           (state
            (and source
                 (list (remote-environment-target-id environment)
                       (nth 1 source)
                       (nth 2 source)
                       (nth 3 source))))
           (previous direnv--reported-selection))
      (unless (equal state previous)
        (setq direnv--reported-selection state)
        (cond
         ((and previous state)
          (message
           "direnv: switched %s -> %s on %s via %s/%s"
           (nth 1 previous)
           (nth 1 state)
           (car state)
           (or (nth 2 state) "cached")
           (or (nth 3 state) "environment")))
         (state
          (message
           "direnv: entered %s on %s via %s/%s"
           (nth 1 state)
           (car state)
           (or (nth 2 state) "cached")
           (or (nth 3 state) "environment")))
         (previous
          (message
           "direnv: left %s; active buffer restored its base environment"
           (nth 1 previous))))))))

(defun direnv--announce-environment (environment)
  "Track and report ENVIRONMENT's direnv layer for the current buffer."
  (setq direnv--active-root
        (when-let* ((source (direnv--environment-source environment)))
          (nth 1 source)))
  (direnv--report-environment-transition environment))

(defun direnv-clear-environment ()
  "Restore the current buffer's base environment and leave its envrc root.
This is deliberately buffer-local: another buffer visiting the old project
keeps its capsule, while the selected-buffer report follows the active
workspace."
  (interactive)
  (remote-environment-clear-buffer)
  (setq direnv--active-root nil
        direnv--last-error nil)
  (direnv--report-environment-transition nil))

(add-hook 'remote-environment-after-apply-hook
          #'direnv--announce-environment)

(remote-register-adapter
 "direnv"
 :capabilities '(process-sync process-async environment)
 :preferences '((default . ("tramp-rpc" "tramp" "native"))))

(remote-register-environment-provider
 "direnv"
 :priority 100
 :predicate
 (lambda (context)
   (and (direnv--envrc-root
         (remote-context-workspace-root context))
        t))
 :fingerprint #'direnv--fingerprint
 :load #'direnv--export)

(defun direnv-update-directory-environment
    (&optional directory force-summary)
  "Refresh the current buffer from DIRECTORY's direnv environment.
FORCE-SUMMARY reports the selected target and source."
  (interactive)
  (let* ((context (remote-context (or directory default-directory)))
         (environment (remote-environment-ensure context t)))
    (when (or force-summary direnv-always-show-summary)
      (message "direnv: %s via %s"
               (remote-context-target-id context)
               (or (remote-environment-sources environment) "base")))
    environment))

(defun direnv-update-environment (&optional file-name force-summary)
  "Refresh the current buffer for FILE-NAME.
With FORCE-SUMMARY, report the selected target and source."
  (interactive)
  (direnv-update-directory-environment
   (direnv--directory file-name) force-summary))

(defun direnv--cancel-refresh (&optional buffer)
  "Cancel pending refresh for BUFFER."
  (let* ((buffer (or buffer (current-buffer)))
         (timer (gethash buffer direnv--refresh-timers)))
    (when (timerp timer)
      (cancel-timer timer))
    (remhash buffer direnv--refresh-timers)))

(defun direnv--schedule-buffer-refresh (&optional buffer delay)
  "Schedule an automatic direnv refresh for BUFFER after DELAY seconds."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (direnv--cancel-refresh buffer)
      (puthash
       buffer
       (run-with-idle-timer
        (or delay 0.05) nil #'direnv--refresh-buffer buffer)
       direnv--refresh-timers))))

(defun direnv--refresh-buffer (buffer)
  "Refresh BUFFER when its transport is idle.
If a TRAMP transaction is active, defer the complete discovery operation;
even looking for `.envrc' would otherwise be a forbidden reentrant call."
  (remhash buffer direnv--refresh-timers)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (condition-case err
          (cond
           ((direnv--transport-connection-path-p
             (or buffer-file-name default-directory)))
           ((direnv--transport-busy-p)
            (direnv--schedule-buffer-refresh
             buffer direnv-transport-busy-retry-delay))
           ((direnv--envrc-root)
            (let* ((root (direnv--envrc-root))
                   (context (remote-context root))
                   (fingerprint (direnv--fingerprint context)))
              (if (direnv--cached-export root fingerprint)
                  (progn
                    (remote-environment-ensure context)
                    (setq direnv--last-error nil))
                (direnv--start-export
                 context root fingerprint buffer))))
           (t
            (direnv-clear-environment)))
        (error
         ;; Discovery (`locate-dominating-file') is itself remote I/O, so its
         ;; failures belong inside the timer boundary too.  Selection changes
         ;; or the next explicit update will naturally schedule another try.
         (setq direnv--last-error err)
         (remote-log
          'direnv-error
          :target
          (ignore-errors
            (remote-file-name-target default-directory))
          :error (error-message-string err)))))))

(defun direnv--maybe-update-environment ()
  "Schedule a coalesced direnv refresh for the current buffer."
  (unless (direnv--transport-connection-path-p
           (or buffer-file-name default-directory))
    (direnv--schedule-buffer-refresh)))

(defun direnv-allow (&optional directory)
  "Run `direnv allow' for DIRECTORY on its logical target."
  (interactive)
  (let* ((root (or (direnv--envrc-root directory)
                   (user-error "No .envrc controls this directory")))
         (context (remote-context root))
         (remote-current-adapter-id "direnv")
         (result
          (remote-exec
           "direnv" :args '("allow" ".")
           :adapter "direnv" :context context)))
    (unless (zerop (remote-exec-result-status result))
      (user-error "direnv allow failed: %s"
                  (remote-exec-result-stderr result)))
    (remote-environment-invalidate
     (remote-context-target-id context))
    (direnv-update-directory-environment root t)))

(define-derived-mode direnv-envrc-mode sh-mode "Envrc"
  "Major mode for direnv `.envrc' files.")

(add-to-list 'auto-mode-alist '("/\\.envrc\\'" . direnv-envrc-mode))

(define-minor-mode direnv-mode
  "Keep buffer-local environments synchronized with direnv."
  :global t
  :group 'direnv
  (dolist (hook direnv--hooks)
    (if direnv-mode
        (add-hook hook #'direnv--maybe-update-environment)
      (remove-hook hook #'direnv--maybe-update-environment)))
  (unless direnv-mode
    (maphash
     (lambda (buffer _timer)
       (direnv--cancel-refresh buffer))
     direnv--refresh-timers)))

(provide 'direnv)
;;; direnv.el ends here
