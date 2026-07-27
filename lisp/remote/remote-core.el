;;; remote-core.el --- Logical targets and capability routing -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The remote core is deliberately independent from TRAMP file-name syntax.
;; Targets name machines, links name usable access paths, and adapters describe
;; callers.  A route joins those three objects for one capability.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function remote-context "remote-fs" (&optional path))
(declare-function remote-backend-classify-error
                  "remote-backend-core" (route error &optional phase))

(defgroup remote nil
  "Target-oriented local and remote execution."
  :group 'files
  :group 'tools)

(defconst remote-id-regexp "\\`[a-z0-9][a-z0-9._-]*\\'"
  "Regexp accepted for externally persisted remote object identifiers.")

(defconst remote-capabilities
  '(file-read file-write directory metadata
    process-sync process-async pty watch lsp environment
    network-client network-server port-forward reverse-forward)
  "Capabilities understood by the built-in route resolver.")

(defconst remote-native-capabilities
  '(file-read file-write directory metadata
    process-sync process-async pty watch lsp environment
    network-client network-server port-forward reverse-forward)
  "Capabilities which Emacs can provide directly on the local target.")

(cl-defstruct (remote-target
               (:constructor remote-target-create))
  id label links workspaces environment preferences
  system architecture shell trusted transient source)

;; Version-1 callers used "link" for the object now exposed as a pipeline.
;; Keep those names as a compatibility vocabulary only; the stored object and
;; registry are the public pipeline types.
(defvaralias 'cl-struct-remote-link-tags
  'cl-struct-remote-pipeline-tags)

(cl-defstruct (remote-pipeline
               (:constructor remote-pipeline-create))
  id short-id target-id backend-ids enabled priority config capabilities source)

(defalias 'remote-link-p #'remote-pipeline-p)
(cl-defun remote-link-create
    (&key id short-id target-id plugin-ids backend-ids enabled priority
          config capabilities source)
  "Create a pipeline using v1 link keywords when necessary."
  (remote-pipeline-create
   :id id :short-id short-id :target-id target-id
   :backend-ids (or backend-ids plugin-ids)
   :enabled enabled :priority priority :config config
   :capabilities capabilities :source source))
(defalias 'remote-link-id #'remote-pipeline-id)
(defalias 'remote-link-short-id #'remote-pipeline-short-id)
(defalias 'remote-link-target-id #'remote-pipeline-target-id)
(defalias 'remote-link-plugin-ids #'remote-pipeline-backend-ids)
(defalias 'remote-link-enabled #'remote-pipeline-enabled)
(defalias 'remote-link-priority #'remote-pipeline-priority)
(defalias 'remote-link-config #'remote-pipeline-config)
(defalias 'remote-link-capabilities #'remote-pipeline-capabilities)
(defalias 'remote-link-source #'remote-pipeline-source)
(gv-define-setter remote-link-id (value object)
  `(setf (remote-pipeline-id ,object) ,value))
(gv-define-setter remote-link-short-id (value object)
  `(setf (remote-pipeline-short-id ,object) ,value))
(gv-define-setter remote-link-target-id (value object)
  `(setf (remote-pipeline-target-id ,object) ,value))
(gv-define-setter remote-link-plugin-ids (value object)
  `(setf (remote-pipeline-backend-ids ,object) ,value))
(gv-define-setter remote-link-enabled (value object)
  `(setf (remote-pipeline-enabled ,object) ,value))
(gv-define-setter remote-link-priority (value object)
  `(setf (remote-pipeline-priority ,object) ,value))
(gv-define-setter remote-link-config (value object)
  `(setf (remote-pipeline-config ,object) ,value))
(gv-define-setter remote-link-capabilities (value object)
  `(setf (remote-pipeline-capabilities ,object) ,value))
(gv-define-setter remote-link-source (value object)
  `(setf (remote-pipeline-source ,object) ,value))

(cl-defstruct (remote-link-plugin
               (:constructor remote-link-plugin-create))
  name capabilities available-p project-file-name
  connect connection-live-p disconnect describe)

(cl-defstruct (remote-adapter
               (:constructor remote-adapter-create))
  id capabilities preferences placement)

(cl-defstruct (remote-context
               (:constructor remote-context-create))
  target-id localname workspace-id workspace-root source)

(cl-defstruct (remote-route
               (:constructor remote-route--create))
  target-id pipeline-id backend-id capability adapter-id score reason)

(cl-defun remote-route-create
    (&key target-id pipeline-id backend-id link-id link-plugin-id
          capability adapter-id score reason)
  "Create a route using v2 names or their v1 compatibility spellings."
  (remote-route--create
   :target-id target-id
   :pipeline-id (or pipeline-id link-id)
   :backend-id (or backend-id link-plugin-id)
   :capability capability
   :adapter-id adapter-id
   :score score
   :reason reason))

(defalias 'remote-route-link-id #'remote-route-pipeline-id)
(defalias 'remote-route-link-plugin-id #'remote-route-backend-id)

(defvar remote-targets (make-hash-table :test #'equal)
  "Registry of `remote-target' objects keyed by target ID.")

(defvaralias 'remote-links 'remote-pipelines)

(defvar remote-pipelines (make-hash-table :test #'equal)
  "Registry of `remote-pipeline' objects keyed by canonical pipeline ID.")

(defvar remote-link-plugins (make-hash-table :test #'equal)
  "Registry of `remote-link-plugin' objects keyed by plugin ID.")

(defvar remote-adapters (make-hash-table :test #'equal)
  "Registry of `remote-adapter' objects keyed by adapter ID.")

(defvar remote-route-health (make-hash-table :test #'equal)
  "Runtime link health keyed by (LINK-ID CAPABILITY).")

(defvar remote-route-log nil
  "Newest-first bounded list of route decisions and failures.")

(defvar remote-transport-failure-hook nil
  "Hook run with ROUTE and ERROR after shared transport failure.")

(defcustom remote-route-log-limit 256
  "Maximum number of entries retained in `remote-route-log'."
  :type 'integer
  :group 'remote)

(defcustom remote-route-failure-cooldown 30
  "Seconds a failed link is de-prioritized."
  :type 'number
  :group 'remote)

(defvar remote-current-adapter-id nil
  "Dynamically bound adapter ID for implicit file-handler requests.")

(defvar remote-current-route nil
  "Dynamically bound `remote-route' for the active operation.")

(defun remote--kill-internal-buffer (buffer)
  "Detach processes and kill framework-owned BUFFER without user hooks.
Framework diagnostic and capture buffers are implementation details rather
than file or UI buffers.  Running global `kill-buffer-hook' integrations from
a process sentinel is both surprising and unsafe: many such integrations
legitimately assume that a user buffer has a string `buffer-file-name'."
  (when (buffer-live-p buffer)
    (when-let* ((process (get-buffer-process buffer)))
      (set-process-sentinel process #'ignore)
      (set-process-buffer process nil))
    (with-current-buffer buffer
      (let ((kill-buffer-hook nil)
            (kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

(defun remote-normalize-id (id &optional noerror)
  "Return ID as a normalized string.
Signal an error for invalid persisted identifiers unless NOERROR is non-nil."
  (let ((id (downcase (string-trim (format "%s" id)))))
    (cond
     ((string-match-p remote-id-regexp id) id)
     (noerror nil)
     (t (error "Invalid remote identifier: %S" id)))))

(defun remote-canonical-link-id (target-id link-id)
  "Return the globally unique link ID for TARGET-ID and LINK-ID."
  (format "%s/%s"
          (remote-normalize-id target-id)
          (remote-normalize-id link-id)))

(defun remote-get-target (id)
  "Return the target named ID, or nil."
  (gethash (remote-normalize-id id t) remote-targets))

(defun remote-get-link (id &optional target-id)
  "Return link ID.
When TARGET-ID is non-nil, ID may be the target-local short ID."
  (gethash (if target-id
               (remote-canonical-link-id target-id id)
             (format "%s" id))
           remote-links))

(defun remote-get-link-plugin (id)
  "Return the link plugin named ID, or nil."
  (gethash (remote-normalize-id id t) remote-link-plugins))

(defun remote-get-adapter (id)
  "Return the adapter named ID, or nil."
  (gethash (remote-normalize-id id t) remote-adapters))

(defun remote-link-plugin-id (link)
  "Return LINK's first backend plugin ID.
This compatibility accessor is suitable only for display or for links which
have exactly one backend.  Route selection must use
`remote-link-plugin-ids'."
  (car (remote-link-plugin-ids link)))

(cl-defun remote-register-target
    (id &key label workspaces environment preferences
        system architecture shell trusted transient source)
  "Register or replace target ID and return it."
  (let* ((id (remote-normalize-id id))
         (old (gethash id remote-targets))
         (target
          (remote-target-create
           :id id
           :label (or label (and old (remote-target-label old)) id)
           :links (and old (copy-sequence (remote-target-links old)))
           :workspaces workspaces
           :environment environment
           :preferences preferences
           :system system
           :architecture architecture
           :shell shell
           :trusted trusted
           :transient transient
           :source source)))
    (puthash id target remote-targets)
    target))

(cl-defun remote-register-link-plugin
    (id &key capabilities available-p project-file-name
        connect connection-live-p disconnect describe)
  "Register link implementation ID and return it."
  (let* ((id (remote-normalize-id id))
         (unknown (seq-difference capabilities remote-capabilities)))
    (when unknown
      (error "Unknown capabilities for link plugin %s: %S" id unknown))
    (let ((plugin
           (remote-link-plugin-create
            :name id
            :capabilities (copy-sequence capabilities)
            :available-p available-p
            :project-file-name project-file-name
            :connect connect
            :connection-live-p connection-live-p
            :disconnect disconnect
            :describe describe)))
      (puthash id plugin remote-link-plugins)
      plugin)))

(defun remote--link-plugin-ids (value)
  "Normalize link backend VALUE into a list of plugin IDs."
  (delete-dups
   (mapcar #'remote-normalize-id
           (if (listp value) value (list value)))))

(defun remote--link-capabilities (plugin-ids)
  "Return the capability union advertised by PLUGIN-IDS."
  (delete-dups
   (apply #'append
          (mapcar
           (lambda (plugin-id)
             (copy-sequence
              (remote-link-plugin-capabilities
               (or (remote-get-link-plugin plugin-id)
                   (error "Unknown remote link plugin: %s" plugin-id)))))
           plugin-ids))))

(defun remote--merge-plists (base overlay)
  "Return a plist containing BASE updated by OVERLAY."
  (let ((result (copy-sequence base)))
    (while overlay
      (setq result (plist-put result (pop overlay) (pop overlay))))
    result))

(defun remote--plist-conflicts (left right)
  "Return keys whose non-nil values conflict between LEFT and RIGHT.
An omitted key is an extension, not a conflict.  This lets separate config
records add execution backends to one physical pipeline without silently
changing the transport described by an earlier record."
  (let (conflicts)
    (while left
      (let* ((key (pop left))
             (value (pop left))
             (present (plist-member right key))
             (other (and present (plist-get right key))))
        (when (and present
                   (not (equal value other)))
          (push key conflicts))))
    (nreverse conflicts)))

(cl-defun remote-register-link
    (target-id id plugin-ids
               &key (enabled t) (priority 0) config capabilities source)
  "Register target-local reachability link ID using PLUGIN-IDS.
PLUGIN-IDS is one backend plugin ID or a list of IDs.  Registering the same
target/link ID again augments its backend set; it does not create another
physical reachability path."
  (let* ((target-id (remote-normalize-id target-id))
         (short-id (remote-normalize-id id))
         (plugin-ids (remote--link-plugin-ids plugin-ids))
         (target (or (remote-get-target target-id)
                     (error "Unknown remote target: %s" target-id)))
         (_plugins
          (dolist (plugin-id plugin-ids)
            (unless (remote-get-link-plugin plugin-id)
              (error "Unknown remote link plugin: %s" plugin-id))))
         (canonical-id (remote-canonical-link-id target-id short-id))
         (old (remote-get-link canonical-id))
         (conflicts
          (and old
               (remote--plist-conflicts
                (remote-link-config old) config)))
         (_compatible
          (when conflicts
            (error
             "Conflicting definitions for remote pipeline %s: %S"
             canonical-id conflicts)))
         (plugin-ids
          (delete-dups
           (append (and old (remote-link-plugin-ids old))
                   plugin-ids)))
         (link
          (remote-link-create
           :id canonical-id
           :short-id short-id
           :target-id target-id
           :plugin-ids plugin-ids
           :enabled (if old
                        (or enabled (remote-link-enabled old))
                      enabled)
           :priority (if old
                         (max priority (remote-link-priority old))
                       priority)
           :config (if old
                       (remote--merge-plists
                        (remote-link-config old) config)
                     config)
           :capabilities
           (delete-dups
            (append (and old (remote-link-capabilities old))
                    capabilities
                    (remote--link-capabilities plugin-ids)))
           :source (or source (and old (remote-link-source old))))))
    (puthash canonical-id link remote-links)
    (setf (remote-target-links target)
          (cons canonical-id
                (delete canonical-id (remote-target-links target))))
    link))

(cl-defun remote-register-adapter
    (id &key capabilities preferences (placement 'workspace))
  "Register caller adapter ID and return it."
  (let ((id (remote-normalize-id id)))
    (let ((adapter
           (remote-adapter-create
            :id id
            :capabilities (copy-sequence capabilities)
            :preferences (copy-tree preferences)
            :placement placement)))
      (puthash id adapter remote-adapters)
      adapter)))

(defun remote-links-for-target (target-id)
  "Return registered links for TARGET-ID in stable registration order."
  (when-let* ((target (remote-get-target target-id)))
    (delq nil
          (mapcar #'remote-get-link
                  (reverse (remote-target-links target))))))

(defun remote--preference-values (preferences adapter-id capability)
  "Return applicable values from PREFERENCES.
PREFERENCES is an alist keyed by capability, adapter ID, or `default'."
  (let ((adapter-id (and adapter-id (format "%s" adapter-id)))
        (preferences (and (listp preferences) preferences)))
    (append
     (copy-sequence (alist-get capability preferences))
     (copy-sequence (alist-get adapter-id preferences nil nil #'equal))
     (copy-sequence (alist-get 'default preferences)))))

(defun remote--object-value (object key)
  "Return KEY from plist or alist OBJECT."
  (cond
   ((and (listp object) (keywordp (car object)))
    (plist-get object (intern (concat ":" (symbol-name key)))))
   ((listp object)
    (or (alist-get key object)
        (alist-get (symbol-name key) object nil nil #'equal)))))

(defun remote--route-preferences
    (target adapter capability context constraints)
  "Return ordered soft preferences for a route request."
  (delete-dups
   (delq nil
         (append
          (when-let* ((link (plist-get constraints :link)))
            (list (format "%s" link)))
          (when-let* ((plugin (plist-get constraints :plugin)))
            (list (format "%s" plugin)))
          (and context
               (listp (remote-context-source context))
               (remote--preference-values
                (remote--object-value
                 (remote-context-source context) 'preferences)
                (and adapter (remote-adapter-id adapter))
                capability))
          (remote--preference-values
           (remote-target-preferences target)
           (and adapter (remote-adapter-id adapter))
           capability)
          (and adapter
               (remote--preference-values
                (remote-adapter-preferences adapter)
                (remote-adapter-id adapter)
                capability))))))

(defun remote--constraint-values (constraints modern legacy)
  "Return normalized values from CONSTRAINTS under MODERN or LEGACY key."
  (let ((value
         (or (plist-get constraints modern)
             (plist-get constraints legacy))))
    (mapcar
     (lambda (item) (format "%s" item))
     (cond
      ((null value) nil)
      ((listp value) value)
      (t (list value))))))

(defun remote--pipeline-matches-p (pipeline values)
  "Return non-nil when PIPELINE matches one of VALUES, or VALUES is nil."
  (or
   (null values)
   (seq-some
    (lambda (value)
      (member value
              (list
               (remote-pipeline-id pipeline)
               (remote-pipeline-short-id pipeline))))
    values)))

(defun remote--health-key (link capability)
  "Return health key for LINK and CAPABILITY."
  (list (remote-link-id link) capability))

(defun remote--backend-health-key (link plugin-id capability)
  "Return health key for PLUGIN-ID on LINK and CAPABILITY."
  (list (remote-link-id link) plugin-id capability))

(defun remote-link-health (link capability)
  "Return shared reachability health for LINK and CAPABILITY."
  (gethash (remote--health-key link capability) remote-route-health))

(defun remote-route-backend-health (route)
  "Return backend health for ROUTE independently of link reachability."
  (when-let* ((link (remote-route-link route)))
    (gethash
     (remote--backend-health-key
      link
      (remote-route-link-plugin-id route)
      (remote-route-capability route))
     remote-route-health)))

(defun remote--cooling-p (link capability)
  "Return non-nil when LINK reachability is cooling for CAPABILITY."
  (when-let* ((health (remote-link-health link capability))
              (failed-at (plist-get health :failed-at)))
    (< (- (float-time) failed-at) remote-route-failure-cooldown)))

(defun remote--backend-cooling-p (route)
  "Return non-nil when ROUTE's backend is unavailable or cooling."
  (when-let* ((health (remote-route-backend-health route)))
    (or (eq (plist-get health :status) 'incompatible)
        (when-let* ((failed-at (plist-get health :failed-at)))
          (< (- (float-time) failed-at)
             remote-route-failure-cooldown)))))

(defun remote-log (kind &rest properties)
  "Record route event KIND with PROPERTIES."
  (push (append (list :time (current-time) :kind kind) properties)
        remote-route-log)
  (when (> (length remote-route-log) remote-route-log-limit)
    (setcdr (nthcdr (1- remote-route-log-limit) remote-route-log) nil)))

(defun remote-report-route-success (route)
  "Mark ROUTE healthy."
  (when-let* ((link (remote-get-link (remote-route-link-id route))))
    (puthash (remote--health-key link (remote-route-capability route))
             (list :status 'healthy :succeeded-at (float-time))
             remote-route-health)
    (puthash
     (remote--backend-health-key
      link
      (remote-route-link-plugin-id route)
      (remote-route-capability route))
     (list :status 'healthy :succeeded-at (float-time))
     remote-route-health)))

(defun remote--backend-incompatible-error-p (route error)
  "Return non-nil when ERROR means ROUTE's backend cannot serve the link."
  (let ((plugin-id (remote-route-link-plugin-id route))
        (message (downcase (error-message-string error))))
    (and
     (equal plugin-id "tramp-rpc")
     (string-match-p
      (rx (or "unknown architecture"
              "tramp-rpc-server"
              "rpc response"
              "method=system.info"
              "rpc process"))
      message))))

(defun remote-report-route-failure (route error)
  "Mark ROUTE failed because of ERROR and return its failure scope.
The return value is `backend', `transport', or `operation'.  Backend
incompatibility cools only the selected implementation; transport failure
cools the shared physical link."
  (when-let* ((link (remote-get-link (remote-route-link-id route))))
    (let* ((classification
            (and
             (fboundp 'remote-backend-classify-error)
             (condition-case nil
                 (remote-backend-classify-error route error)
               (error nil))))
           (scope
            (or
             (plist-get classification :scope)
             (cond
              ((remote--backend-incompatible-error-p route error)
               'backend)
              ((and (fboundp 'remote--transport-error-p)
                    (remote--transport-error-p error))
               'transport)
              (t 'operation))))
           (health
            (list
             :status
             (or
              (plist-get classification :status)
              (if (and (eq scope 'backend)
                       (string-match-p
                        "unknown architecture"
                        (downcase (error-message-string error))))
                  'incompatible
                'failed))
             :failed-at (float-time)
             :error (error-message-string error))))
      (pcase scope
        ('backend
         (when (fboundp 'remote-connection-invalidate)
           (remote-connection-invalidate route nil error))
         (puthash
          (remote--backend-health-key
           link
           (remote-route-link-plugin-id route)
           (remote-route-capability route))
          health remote-route-health))
        ('transport
         (when (fboundp 'remote-connection-invalidate-link)
           (remote-connection-invalidate-link
            (remote-route-link-id route) nil error))
         (puthash
          (remote--health-key link (remote-route-capability route))
          health remote-route-health)))
      (remote-log
       'failure
       :scope scope
       :target (remote-route-target-id route)
       :link (remote-route-link-id route)
       :plugin (remote-route-link-plugin-id route)
       :capability (remote-route-capability route)
       :adapter (remote-route-adapter-id route)
       :phase (plist-get classification :phase)
       :retryable (plist-get classification :retryable)
       :error (error-message-string error))
      (when (eq scope 'transport)
        (run-hook-with-args
         'remote-transport-failure-hook route error))
      scope)))

(defun remote--plugin-available-p (plugin link context)
  "Return non-nil when PLUGIN can use LINK for CONTEXT."
  (let ((predicate (remote-link-plugin-available-p plugin)))
    (or (null predicate)
        (condition-case nil
            (funcall predicate link context)
          (error nil)))))

(defun remote--preference-rank (link plugin-id preferences)
  "Return preference rank for LINK and PLUGIN-ID in PREFERENCES."
  (let ((full (remote-link-id link))
        (short (remote-link-short-id link))
        (index 0)
        found)
    (while (and preferences (null found))
      (when (member (format "%s" (car preferences))
                    (list full short plugin-id))
        (setq found index))
      (setq index (1+ index)
            preferences (cdr preferences)))
    (or found 1000)))

(defun remote-routes
    (adapter-id capability &optional context constraints)
  "Return eligible routes ordered for ADAPTER-ID and CAPABILITY."
  (let* ((adapter-id (or adapter-id remote-current-adapter-id "emacs-file"))
         (adapter-id (remote-normalize-id adapter-id))
         (adapter (remote-get-adapter adapter-id))
         (context (or context
                      (and (fboundp 'remote-context)
                           (remote-context))))
         (target-id (and context (remote-context-target-id context)))
         (target (or (and target-id (remote-get-target target-id))
                     (error "No remote target for context: %S" context)))
         (_adapter-check
          (unless adapter
            (error "Unknown remote adapter: %s" adapter-id)))
         (_capability-check
          (unless (memq capability (remote-adapter-capabilities adapter))
            (error "Adapter %s does not support capability %s"
                   adapter-id capability)))
         (preferences
          (remote--route-preferences
           target adapter capability context constraints))
         (required-pipelines
          (remote--constraint-values constraints :pipeline :link))
         (required-backends
          (remote--constraint-values constraints :backend :plugin))
         (excluded-pipelines
          (append
           (remote--constraint-values
            constraints :exclude-pipelines :exclude-links)
           nil))
         routes)
    (dolist (link (remote-links-for-target target-id))
      (when (and (remote-link-enabled link)
                 (memq capability (remote-link-capabilities link))
                 (remote--pipeline-matches-p link required-pipelines)
                 (or
                  (null excluded-pipelines)
                  (not
                   (remote--pipeline-matches-p
                    link excluded-pipelines))))
        (dolist (plugin-id (remote-link-plugin-ids link))
          (when-let* (((or (null required-backends)
                           (member plugin-id required-backends)))
                      (plugin (remote-get-link-plugin plugin-id))
                      ((memq capability
                             (remote-link-plugin-capabilities plugin)))
                      ((remote--plugin-available-p plugin link context)))
            (let* ((rank
                    (remote--preference-rank
                     link plugin-id preferences))
                   (link-cooling
                    (remote--cooling-p link capability))
                   (route
                    (remote-route-create
                     :target-id target-id
                     :link-id (remote-link-id link)
                     :link-plugin-id plugin-id
                     :capability capability
                     :adapter-id adapter-id))
                   (backend-cooling
                    (remote--backend-cooling-p route))
                   (score (+ (* -1000 rank)
                             (remote-link-priority link)
                             (if (and
                                  (fboundp 'remote-connection-cached-p)
                                  (remote-connection-cached-p route))
                                 25
                               0)
                             (if link-cooling -100000 0)
                             (if backend-cooling -100000 0))))
              (setf
               (remote-route-score route) score
               (remote-route-reason route)
               (list :preference-rank rank
                     :priority (remote-link-priority link)
                     :pooled
                     (and
                      (fboundp 'remote-connection-cached-p)
                      (remote-connection-cached-p route))
                     :link-cooling link-cooling
                     :backend-cooling backend-cooling))
              (push route routes))))))
    (sort routes
          (lambda (left right)
            (> (remote-route-score left)
               (remote-route-score right))))))

(defun remote-resolve
    (adapter-id capability &optional context constraints)
  "Resolve the best route for ADAPTER-ID and CAPABILITY."
  (or (car (remote-routes adapter-id capability context constraints))
      (error "No route for adapter %s capability %s"
             adapter-id capability)))

(defun remote-route-link (route)
  "Return the registered link selected by ROUTE."
  (remote-get-link (remote-route-link-id route)))

(defun remote-route-plugin (route)
  "Return the link plugin selected by ROUTE."
  (remote-get-link-plugin (remote-route-link-plugin-id route)))

(defun remote--coerce-route (file-name route-or-link capability adapter-id)
  "Return a route for FILE-NAME from ROUTE-OR-LINK.
ROUTE-OR-LINK may be a route, link object, canonical link ID, or target-local
link ID.  CAPABILITY and ADAPTER-ID supply defaults when a route is built."
  (cond
   ((remote-route-p route-or-link) route-or-link)
   (t
    (let* ((context (remote-context file-name))
           (target-id (remote-context-target-id context))
           (link
            (cond
             ((remote-link-p route-or-link) route-or-link)
             ((stringp route-or-link)
              (or (remote-get-link route-or-link)
                  (remote-get-link route-or-link target-id)))
             ((symbolp route-or-link)
              (remote-get-link (symbol-name route-or-link) target-id)))))
      (remote-resolve
       (or adapter-id remote-current-adapter-id "emacs-file")
       (or capability 'file-read)
       context
       (and (or link route-or-link)
            (list :link
                  (if link
                      (remote-link-id link)
                    (format "%s" route-or-link)))))))))

(defun remote-project-file-name
    (file-name &optional route-or-link capability adapter-id)
  "Project logical FILE-NAME through ROUTE-OR-LINK.
ROUTE-OR-LINK may be a `remote-route', a `remote-link', a canonical link ID,
or a target-local link ID.  When nil, resolve CAPABILITY for ADAPTER-ID."
  (let* ((canonical
          (if (fboundp 'remote-canonicalize-file-name)
              (remote-canonicalize-file-name file-name)
            file-name))
         (route (remote--coerce-route
                 canonical route-or-link capability adapter-id))
         (plugin (remote-route-plugin route))
         (projector (and plugin
                         (remote-link-plugin-project-file-name plugin))))
    (unless projector
      (error "Link plugin %s cannot project file names"
             (remote-route-link-plugin-id route)))
    (funcall projector canonical (remote-route-link route) route)))

(defmacro remote-with-route
    (adapter-id capability context constraints &rest body)
  "Run BODY with a resolved route and projected execution context."
  (declare (indent 4) (debug t))
  `(let* ((remote-current-adapter-id ,adapter-id)
          (context-value (or ,context (remote-context)))
          (remote-current-route
           (remote-resolve remote-current-adapter-id
                              ,capability context-value ,constraints))
          (remote-current-connection
           (and (fboundp 'remote-connection-ensure)
                (remote-connection-ensure
                 remote-current-route context-value)))
          (default-directory
           (remote-project-file-name
            (remote-context-workspace-root context-value)
            remote-current-route)))
     ,@body))

(defun remote-reset-registries ()
  "Reset remote registries and install built-in local objects."
  (interactive)
  (clrhash remote-targets)
  (clrhash remote-links)
  (clrhash remote-link-plugins)
  (clrhash remote-adapters)
  (clrhash remote-route-health)
  (when (boundp 'remote-connection-pool)
    (clrhash remote-connection-pool))
  (setq remote-route-log nil)
  (remote-register-link-plugin
   "native"
   :capabilities remote-native-capabilities
   :available-p (lambda (_link _context) t)
   :project-file-name
   (lambda (file-name _link _route)
     (if (fboundp 'remote-file-local-name)
         (remote-file-local-name file-name)
       file-name)))
  (remote-register-target
   "local" :label "Local" :trusted t :source 'builtin)
  (remote-register-link
   "local" "native" "native"
   :priority 1000 :source 'builtin)
  (remote-register-adapter
   "emacs-file"
   :capabilities '(file-read file-write directory metadata
                   process-sync process-async watch environment)
   :preferences '((default . ("native" "tramp" "tramp-rpc")))))

(remote-reset-registries)

(provide 'remote-core)
;;; remote-core.el ends here
