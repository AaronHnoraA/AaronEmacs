;;; remote-environment.el --- Target and workspace environment capsules -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; An environment capsule belongs to a logical target/workspace pair.  It
;; contains target-native PATH entries and can therefore be reused whether the
;; selected physical link is native, TRAMP, or tramp-rpc.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-process)

(cl-defstruct (remote-environment
               (:constructor remote-environment-create))
  id parent-id key target-id workspace-id workspace-root
  vars path-state provider-ids sources generated-at)

(cl-defstruct (remote-environment-provider
               (:constructor remote-environment-provider-create))
  id priority scope predicate fingerprint load)

(cl-defstruct (remote-path-layer
               (:constructor remote-path-layer-create))
  id scope operation entries source)

(cl-defstruct (remote-path-state
               (:constructor remote-path-state-create))
  id parent-id layers resolved)

(defvar remote-environment-providers nil
  "Registered workspace environment providers.")

(defvar remote-environment-cache (make-hash-table :test #'equal)
  "Environment capsules keyed by target, workspace, and provider state.")

(defvar remote-environments-by-id (make-hash-table :test #'equal)
  "Latest environment capsules keyed by stable instance ID.")

(defvar remote-environment-inhibit nil
  "Dynamically non-nil while an environment provider is executing.")

(defvar-local remote-buffer-environment nil
  "Environment capsule currently projected into this buffer.")

(defvar-local remote--buffer-base-process-environment nil)
(defvar-local remote--buffer-base-exec-path nil)

(defvar remote-environment-after-apply-hook nil
  "Hook run after an environment capsule is applied to the current buffer.
Hook functions receive the applied `remote-environment' as their argument.")

(defconst remote-environment-config-keys
  '(vars providers path pathProfiles)
  "Reserved keys in a target environment configuration object.")

(defun remote-environment-instance-id (context)
  "Return stable environment instance ID for CONTEXT."
  (format "%s@%s"
          (remote-context-target-id context)
          (or (remote-context-workspace-id context)
              (concat
               "path-"
               (substring
                (secure-hash
                 'sha1 (remote-context-workspace-root context))
                0 12)))))

(defun remote--environment-config-cell (config key)
  "Return CONFIG cell for KEY, accepting symbol and string keys."
  (or (assq key config)
      (assoc (symbol-name key) config)))

(defun remote--target-environment-config (target)
  "Return TARGET's environment configuration."
  (and target
       (listp (remote-target-environment target))
       (remote-target-environment target)))

(defun remote--target-environment-vars (target)
  "Return configured base variables for TARGET."
  (let* ((config (remote--target-environment-config target))
         (structured
          (seq-some
           (lambda (key)
             (remote--environment-config-cell config key))
           remote-environment-config-keys)))
    (if structured
        (cdr (remote--environment-config-cell config 'vars))
      config)))

(defun remote--target-environment-provider-ids (target)
  "Return explicit environment provider IDs for TARGET, or `automatic'."
  (let* ((config (remote--target-environment-config target))
         (cell (remote--environment-config-cell config 'providers)))
    (if cell
        (mapcar #'remote-normalize-id (cdr cell))
      'automatic)))

(defun remote--path-split (value)
  "Normalize PATH VALUE to a list of target-native entries."
  (cond
   ((null value) nil)
   ((stringp value) (split-string value path-separator t))
   ((listp value) (mapcar (lambda (item) (format "%s" item)) value))))

(defun remote--path-apply-layer (paths layer)
  "Apply LAYER to PATHS."
  (let* ((entries (copy-sequence (remote-path-layer-entries layer)))
         (operation (remote-path-layer-operation layer)))
    (pcase operation
      ('replace entries)
      ('prepend
       (append entries
               (seq-remove (lambda (path) (member path entries)) paths)))
      ('append
       (append
        (seq-remove (lambda (path) (member path entries)) paths)
        entries))
      ('remove
       (seq-remove (lambda (path) (member path entries)) paths))
      ('inherit paths)
      (_ (error "Unknown PATH layer operation: %S" operation)))))

(defun remote-path-state-add-layer (state layer)
  "Add LAYER to PATH STATE and return STATE."
  (setf (remote-path-state-layers state)
        (append (remote-path-state-layers state) (list layer))
        (remote-path-state-resolved state)
        (remote--path-apply-layer
         (remote-path-state-resolved state) layer))
  state)

(defun remote--path-add
    (state id scope operation entries &optional source)
  "Add one PATH layer to STATE when ENTRIES is nonempty."
  (when-let* ((entries (remote--path-split entries)))
    (remote-path-state-add-layer
     state
     (remote-path-layer-create
      :id (format "%s" id)
      :scope scope
      :operation operation
      :entries entries
      :source source))))

(defun remote--environment-with-resolved-path (vars path-state)
  "Return VARS with PATH from PATH-STATE."
  (let ((vars
         (cl-delete "PATH" (copy-sequence vars)
                    :key #'car :test #'equal))
        (paths (remote-path-state-resolved path-state)))
    (if paths
        (append vars
                (list
                 (cons "PATH"
                       (mapconcat #'identity paths path-separator))))
      vars)))

(cl-defun remote-register-environment-provider
    (id &key (priority 0) (scope 'workspace) predicate fingerprint load)
  "Register environment provider ID.
PREDICATE decides whether the provider applies to a context.  FINGERPRINT
returns state used for cache invalidation, and LOAD returns an environment
alist or `(:vars ALIST :source VALUE)'."
  (let* ((id (remote-normalize-id id))
         (provider
          (remote-environment-provider-create
           :id id
           :priority priority
           :scope scope
           :predicate predicate
           :fingerprint fingerprint
           :load load)))
    (setq remote-environment-providers
          (cons provider
                (cl-remove id remote-environment-providers
                           :key #'remote-environment-provider-id
                           :test #'equal)))
    provider))

(defalias 'remote-register-environment-maintainer
  #'remote-register-environment-provider)

(defun remote-get-environment (id)
  "Return the latest environment capsule named ID."
  (gethash (format "%s" id) remote-environments-by-id))

(defun remote--normalize-environment (value)
  "Normalize environment VALUE to an alist of string keys."
  (let (result)
    (cond
     ((null value) nil)
     ((and (listp value) (keywordp (car value)))
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (push (cons (substring (symbol-name key) 1) item) result))))
     ((listp value)
      (dolist (entry value)
        (when (consp entry)
          (push (cons (format "%s" (car entry)) (cdr entry)) result)))))
    (nreverse result)))

(defun remote--merge-environments (&rest environments)
  "Merge ENVIRONMENTS left to right."
  (let (result)
    (dolist (environment environments)
      (dolist (entry (remote--normalize-environment environment))
        (setq result
              (cl-delete (car entry) result :key #'car :test #'equal))
        (push entry result)))
    (nreverse result)))

(defun remote--applicable-environment-providers (context)
  "Return ordered providers applicable to CONTEXT."
  (let* ((target
          (remote-get-target (remote-context-target-id context)))
         (selected
          (remote--target-environment-provider-ids target)))
    (sort
     (seq-filter
      (lambda (provider)
        (and
         (or (eq selected 'automatic)
             (member (remote-environment-provider-id provider)
                     selected))
         (let ((predicate
                (remote-environment-provider-predicate provider)))
           (or (null predicate)
               (condition-case nil
                   (funcall predicate context)
                 (error nil))))))
      (copy-sequence remote-environment-providers))
     (lambda (left right)
       (let ((left-scope
              (pcase (remote-environment-provider-scope left)
                ('host 0) ('target 1) (_ 2)))
             (right-scope
              (pcase (remote-environment-provider-scope right)
                ('host 0) ('target 1) (_ 2))))
         (if (= left-scope right-scope)
             (< (remote-environment-provider-priority left)
                (remote-environment-provider-priority right))
           (< left-scope right-scope)))))))

(defun remote--environment-provider-fingerprint (provider context)
  "Return cache fingerprint for PROVIDER in CONTEXT."
  (let ((function (remote-environment-provider-fingerprint provider)))
    (cons (remote-environment-provider-id provider)
          (when function
            (condition-case err
                (funcall function context)
              (error
               (remote-log
                'environment-fingerprint-error
                :provider (remote-environment-provider-id provider)
                :error (error-message-string err))
               'error))))))

(defun remote--environment-key (context providers)
  "Return cache key for CONTEXT and PROVIDERS."
  (list
   (remote-context-target-id context)
   (remote-context-workspace-root context)
   (mapcar
    (lambda (provider)
      (remote--environment-provider-fingerprint provider context))
    providers)))

(defun remote--load-environment-provider (provider context)
  "Load PROVIDER for CONTEXT and return a normalized layer plist."
  (let ((loader (remote-environment-provider-load provider))
        (remote-environment-inhibit t))
    (when loader
      (let* ((value (funcall loader context))
             (structured
              (and (listp value) (keywordp (car value))))
             (vars
              (remote--normalize-environment
               (if structured (plist-get value :vars) value)))
             (path-cell (assoc-string "PATH" vars t))
             (path
              (or (and structured (plist-get value :path))
                  (cdr path-cell)))
             (vars
              (if path-cell
                  (cl-delete (car path-cell) vars
                             :key #'car :test #'equal)
                vars)))
        (list
         :id (remote-environment-provider-id provider)
         :scope (remote-environment-provider-scope provider)
         :vars vars
         :path path
         :path-mode
         (or (and structured (plist-get value :path-mode))
             (and path 'replace))
         :source
         (or (and structured (plist-get value :source))
             (remote-environment-provider-id provider)))))))

(defun remote-environment-apply (environment &optional buffer)
  "Project ENVIRONMENT into BUFFER and return ENVIRONMENT."
  (with-current-buffer (or buffer (current-buffer))
    (unless remote--buffer-base-process-environment
      (setq-local remote--buffer-base-process-environment
                  (copy-sequence process-environment)
                  remote--buffer-base-exec-path
                  (copy-sequence exec-path)))
    (let ((vars (remote-environment-vars environment)))
      (setq-local process-environment
                  (remote--apply-environment
                   remote--buffer-base-process-environment vars)
                  exec-path
                  (remote--exec-path-for-environment
                   vars remote--buffer-base-exec-path)
                  remote-buffer-environment environment))
    (run-hook-with-args
     'remote-environment-after-apply-hook environment)
    environment))

(defun remote-environment-clear-buffer ()
  "Restore the current buffer's pre-framework environment."
  (interactive)
  (when remote--buffer-base-process-environment
    (setq-local process-environment
                (copy-sequence remote--buffer-base-process-environment)
                exec-path
                (copy-sequence remote--buffer-base-exec-path)
                remote-buffer-environment nil)))

(defun remote--environment-config-value (object key)
  "Return KEY from alist or plist OBJECT."
  (cond
   ((and (listp object) (keywordp (car object)))
    (plist-get object (intern (concat ":" (symbol-name key)))))
   ((listp object)
    (cdr (remote--environment-config-cell object key)))))

(defun remote--environment-apply-target
    (target vars path-state)
  "Apply TARGET configuration to VARS and PATH-STATE.
Return the updated VARS."
  (let* ((target-vars
          (remote--normalize-environment
           (remote--target-environment-vars target)))
         (path-cell (assoc-string "PATH" target-vars t))
         (path-config
          (remote--environment-config-value
           (remote--target-environment-config target) 'path)))
    (when path-cell
      (remote--path-add
       path-state "target:path" 'target 'replace
       (cdr path-cell) 'target)
      (setq target-vars
            (cl-delete (car path-cell) target-vars
                       :key #'car :test #'equal)))
    (when path-config
      (let ((inherit-cell
             (and (listp path-config)
                  (remote--environment-config-cell
                   path-config 'inherit))))
        (when (and inherit-cell (null (cdr inherit-cell)))
          (remote-path-state-add-layer
           path-state
           (remote-path-layer-create
            :id "target:no-inherit" :scope 'target
            :operation 'replace :entries nil :source 'target))))
      (dolist (spec '((replace . replace)
                      (remove . remove)
                      (prepend . prepend)
                      (append . append)))
        (when-let* ((value
                     (remote--environment-config-value
                      path-config (car spec))))
          (remote--path-add
           path-state
           (format "target:%s" (car spec))
           'target (cdr spec) value 'target))))
    (remote--merge-environments vars target-vars)))

(defun remote--environment-apply-provider
    (loaded vars path-state sources provider-ids)
  "Apply provider LOADED to environment build accumulators.
Return (VARS SOURCES PROVIDER-IDS)."
  (remote--path-add
   path-state
   (plist-get loaded :id)
   (or (plist-get loaded :scope) 'workspace)
   (or (plist-get loaded :path-mode) 'inherit)
   (plist-get loaded :path)
   (plist-get loaded :source))
  (list
   (remote--merge-environments vars (plist-get loaded :vars))
   (append sources (list (plist-get loaded :source)))
   (append provider-ids (list (plist-get loaded :id)))))

(defun remote--environment-build (context force)
  "Build or retrieve an environment capsule for CONTEXT."
  (let* ((providers (remote--applicable-environment-providers context))
         (key (remote--environment-key context providers))
         (cached (and (not force)
                      (gethash key remote-environment-cache))))
    (or cached
        (let* ((target
                (remote-get-target (remote-context-target-id context)))
               (id (remote-environment-instance-id context))
               (path-state
                (remote-path-state-create
                 :id id :layers nil :resolved nil))
               vars sources provider-ids target-applied)
          (dolist (provider providers)
            (when (and (not target-applied)
                       (not (eq
                             (remote-environment-provider-scope provider)
                             'host)))
              (setq vars
                    (remote--environment-apply-target
                     target vars path-state)
                    target-applied t))
            (condition-case err
                (when-let* ((loaded
                             (remote--load-environment-provider
                              provider context)))
                  (pcase-let
                      ((`(,new-vars ,new-sources ,new-provider-ids)
                        (remote--environment-apply-provider
                         loaded vars path-state sources provider-ids)))
                    (setq vars new-vars
                          sources new-sources
                          provider-ids new-provider-ids)))
              (error
               (remote-log
                'environment-provider-error
                :target (remote-context-target-id context)
                :provider (remote-environment-provider-id provider)
                :error (error-message-string err)))))
          (unless target-applied
            (setq vars
                  (remote--environment-apply-target
                   target vars path-state)))
          (setq vars
                (remote--environment-with-resolved-path
                 vars path-state))
          (let ((environment
                 (remote-environment-create
                  :id id
                  :key key
                  :target-id (remote-context-target-id context)
                  :workspace-id
                  (remote-context-workspace-id context)
                  :workspace-root
                  (remote-context-workspace-root context)
                  :vars vars
                  :path-state path-state
                  :provider-ids provider-ids
                  :sources sources
                  :generated-at (current-time))))
            (puthash key environment remote-environment-cache)
            (puthash id environment remote-environments-by-id)
            environment)))))

(cl-defun remote-environment-derive
    (environment id
                 &key vars path-replace path-remove path-prepend path-append
                 (scope 'invocation) source)
  "Derive an isolated child of ENVIRONMENT named ID.
Variable and PATH decorations affect only the returned capsule.  PATH layers
are applied in replace, remove, prepend, append order."
  (let* ((id (remote-normalize-id id))
         (child-id (format "%s+%s"
                           (remote-environment-id environment) id))
         (parent-state (remote-environment-path-state environment))
         (path-state
          (remote-path-state-create
           :id child-id
           :parent-id (remote-path-state-id parent-state)
           :layers (copy-sequence
                    (remote-path-state-layers parent-state))
           :resolved (copy-sequence
                      (remote-path-state-resolved parent-state))))
         (vars (remote--normalize-environment vars))
         (path-cell (assoc-string "PATH" vars t)))
    (when path-cell
      (setq path-replace (or path-replace (cdr path-cell))
            vars (cl-delete (car path-cell) vars
                            :key #'car :test #'equal)))
    (dolist (spec
             `((replace ,path-replace)
               (remove ,path-remove)
               (prepend ,path-prepend)
               (append ,path-append)))
      (remote--path-add
       path-state
       (format "%s:%s" id (car spec))
       scope (car spec) (cadr spec) (or source id)))
    (let ((merged
           (remote--environment-with-resolved-path
            (remote--merge-environments
             (remote-environment-vars environment) vars)
            path-state)))
      (let ((child
             (remote-environment-create
              :id child-id
              :parent-id (remote-environment-id environment)
              :key (list :derived child-id)
              :target-id (remote-environment-target-id environment)
              :workspace-id (remote-environment-workspace-id environment)
              :workspace-root (remote-environment-workspace-root environment)
              :vars merged
              :path-state path-state
              :provider-ids
              (append (remote-environment-provider-ids environment)
                      (list id))
              :sources
              (append (remote-environment-sources environment)
                      (list (or source id)))
              :generated-at (current-time))))
        (puthash child-id child remote-environments-by-id)
        child))))

(defalias 'remote-path-decorate #'remote-environment-derive)

(defun remote-environment-resolve (&optional context force)
  "Return CONTEXT's environment capsule without modifying any buffer.
This is the process/workspace boundary.  Use `remote-environment-ensure' when
the resolved environment should also become buffer-local."
  (let ((context
         (cond
          ((remote-context-p context) context)
          ((stringp context) (remote-context context))
          (t (remote-context)))))
    (remote--environment-build context force)))

(defun remote-environment-ensure (&optional context force callback)
  "Ensure the environment capsule for CONTEXT.
Apply it buffer-locally and return it.  With CALLBACK, schedule the provider
work asynchronously and call CALLBACK with the resulting capsule; return nil
immediately."
  (let ((context
         (cond
          ((remote-context-p context) context)
          ((stringp context) (remote-context context))
          (t (remote-context))))
        (buffer (current-buffer)))
    (if callback
        (progn
          (run-at-time
           0 nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (let ((environment
                        (remote-environment-resolve context force)))
                   (remote-environment-apply environment)
                   (funcall callback environment))))))
          nil)
      (remote-environment-apply
       (remote-environment-resolve context force)))))

(defun remote-environment-invalidate (&optional target-id)
  "Invalidate cached capsules, optionally only for TARGET-ID."
  (interactive)
  (if (null target-id)
      (progn
        (clrhash remote-environment-cache)
        (clrhash remote-environments-by-id))
    (let ((target-id (remote-normalize-id target-id))
          cache-keys ids)
      (maphash
       (lambda (key _value)
         (when (equal (car key) target-id)
           (push key cache-keys)))
       remote-environment-cache)
      (maphash
       (lambda (id environment)
         (when (equal (remote-environment-target-id environment) target-id)
           (push id ids)))
       remote-environments-by-id)
      (dolist (key cache-keys)
        (remhash key remote-environment-cache))
      (dolist (id ids)
        (remhash id remote-environments-by-id)))))

(provide 'remote-environment)
;;; remote-environment.el ends here
