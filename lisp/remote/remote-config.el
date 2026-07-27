;;; remote-config.el --- Target/pipeline configuration loader -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `etc/remote.json' persists logical targets and transport pipelines.  The
;; older `links'/`plugins' keys remain accepted as compatibility spellings.
;; SSH config imports are only a discovery source.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-pipeline)

(defvar remote-config-file
  (expand-file-name "etc/remote.json" user-emacs-directory)
  "JSON file containing logical target and link configuration.")

(defvar remote-config-settings nil
  "Top-level UI settings read from `remote-config-file'.")

(defconst remote-config-current-version 2
  "Current persisted remote configuration schema.")

(defvar remote-config-version nil
  "Schema version most recently read from `remote-config-file'.")

(defvar remote-config-generation 0)
(defvar remote-config-after-load-hook nil)

(defun remote-config--copy-target-registry ()
  "Return a detached copy of `remote-targets' for transactional loading."
  (let ((result (make-hash-table :test (hash-table-test remote-targets))))
    (maphash
     (lambda (id target)
       (let ((copy (copy-remote-target target)))
         (setf
          (remote-target-links copy)
          (copy-sequence (remote-target-links target))
          (remote-target-workspaces copy)
          (copy-tree (remote-target-workspaces target))
          (remote-target-environment copy)
          (copy-tree (remote-target-environment target))
          (remote-target-preferences copy)
          (copy-tree (remote-target-preferences target)))
         (puthash id copy result)))
     remote-targets)
    result))

(defun remote-config--copy-pipeline-registry ()
  "Return a detached copy of `remote-pipelines' for transactional loading."
  (let ((result (make-hash-table :test (hash-table-test remote-pipelines))))
    (maphash
     (lambda (id pipeline)
       (let ((copy (copy-remote-pipeline pipeline)))
         (setf
          (remote-pipeline-backend-ids copy)
          (copy-sequence (remote-pipeline-backend-ids pipeline))
          (remote-pipeline-config copy)
          (copy-tree (remote-pipeline-config pipeline))
          (remote-pipeline-capabilities copy)
          (copy-sequence (remote-pipeline-capabilities pipeline)))
         (puthash id copy result)))
     remote-pipelines)
    result))

(defun remote-config--changed-pipelines (old new)
  "Return non-local pipeline IDs changed between OLD and NEW registries."
  (let (ids changed)
    (maphash (lambda (id _pipeline) (push id ids)) old)
    (maphash (lambda (id _pipeline) (push id ids)) new)
    (dolist (id (delete-dups ids))
      (let ((before (gethash id old))
            (after (gethash id new)))
        (when (and
               (not
                (equal
                 (and before (remote-pipeline-target-id before))
                 "local"))
               (not (equal before after)))
          (push id changed))))
    changed))

(defun remote-config--invalidate-pipelines (pipeline-ids)
  "Close sessions and orphan runtimes belonging to PIPELINE-IDS."
  (dolist (pipeline-id pipeline-ids)
    (when (fboundp 'remote-connection-invalidate-link)
      (remote-connection-invalidate-link
       pipeline-id t 'config-reload))
    ;; A runtime normally belongs to a session.  Also close any runtime left
    ;; behind by an interrupted backend startup.
    (when (boundp 'remote-pipeline-runtime-pool)
      (let (runtimes)
        (maphash
         (lambda (_key runtime)
           (when
               (equal
                (remote-pipeline-runtime-pipeline-id runtime)
                pipeline-id)
             (push runtime runtimes)))
         remote-pipeline-runtime-pool)
        (dolist (runtime runtimes)
          (remote-pipeline-close runtime 'config-reload))))))

(defun remote-config--string-list (value)
  "Return VALUE as a list of strings."
  (cond
   ((null value) nil)
   ((stringp value) (list value))
   ((listp value)
    (delq nil
          (mapcar (lambda (item)
                    (and (or (stringp item) (symbolp item))
                         (format "%s" item)))
                  value)))))

(defun remote-config--strip-ssh-comment (line)
  "Strip a trailing SSH comment from LINE."
  (string-trim
   (replace-regexp-in-string "[[:space:]]+#.*\\'" "" line)))

(defun remote-config--ssh-hosts (file &optional seen)
  "Return concrete Host aliases from SSH config FILE.
SEEN prevents recursive Include cycles."
  (let* ((file (expand-file-name file))
         (seen (or seen (make-hash-table :test #'equal)))
         hosts)
    (unless (or (gethash file seen)
                (not (file-readable-p file)))
      (puthash file t seen)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line
                 (remote-config--strip-ssh-comment
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))))
            (cond
             ((string-match
               "\\`[[:space:]]*Include[[:space:]]+\\(.+\\)\\'" line)
              (dolist (pattern
                       (split-string (match-string 1 line)
                                     "[[:space:]]+" t))
                (dolist (included
                         (file-expand-wildcards
                          (expand-file-name
                           pattern (file-name-directory file))
                          t))
                  (setq hosts
                        (nconc
                         hosts
                         (remote-config--ssh-hosts included seen))))))
             ((string-match
               "\\`[[:space:]]*Host[[:space:]]+\\(.+\\)\\'" line)
              (dolist (host
                       (split-string (match-string 1 line)
                                     "[[:space:]]+" t))
                (unless (or (string-prefix-p "!" host)
                            (string-match-p "[*?]" host))
                  (push host hosts))))))
          (forward-line 1))))
    (delete-dups (nreverse hosts))))

(defun remote-config--matches-p (value patterns default)
  "Return whether VALUE matches PATTERNS, or DEFAULT when PATTERNS is empty."
  (if-let* ((patterns (remote-config--string-list patterns)))
      (seq-some
       (lambda (pattern)
         (string-match-p (wildcard-to-regexp pattern) value))
       patterns)
    default))

(defun remote-config--allowed-p (value object)
  "Return non-nil when VALUE passes OBJECT's include/exclude rules."
  (and
   (remote-config--matches-p value (alist-get 'include object) t)
   (not (remote-config--matches-p
         value (alist-get 'exclude object) nil))))

(defun remote-config--plist (object)
  "Convert flat JSON alist OBJECT to a plist."
  (let (result)
    (dolist (entry object)
      (setq result
            (plist-put
             result
             (intern (concat ":" (format "%s" (car entry))))
             (cdr entry))))
    result))

(defun remote-config--preferences (object)
  "Normalize JSON preference OBJECT to a routing alist."
  (mapcar
   (lambda (entry)
     (cons (car entry) (remote-config--string-list (cdr entry))))
   object))

(defun remote-config--clear-loaded-objects ()
  "Remove non-local configured targets and links."
  (let (target-ids link-ids)
    (maphash
     (lambda (id _target)
       (unless (equal id "local") (push id target-ids)))
     remote-targets)
    (maphash
     (lambda (id link)
       (unless (equal (remote-link-target-id link) "local")
         (push id link-ids)))
     remote-links)
    (dolist (id link-ids) (remhash id remote-links))
    (dolist (id target-ids) (remhash id remote-targets))
    (when-let* ((local (remote-get-target "local")))
      (setf (remote-target-links local)
            (seq-filter
             (lambda (id) (gethash id remote-links))
             (remote-target-links local))))))

(defun remote-config--register-pipeline-object (target-id object &optional host)
  "Register pipeline OBJECT for TARGET-ID, defaulting its host to HOST.
Both v2 backend keys and v1 plugin keys are accepted."
  (let* ((backends
          (or (remote-config--string-list
               (alist-get 'backends object))
              (remote-config--string-list
               (alist-get 'plugins object))
              (list (or (alist-get 'backend object)
                        (alist-get 'plugin object)
                        "tramp"))))
         (id (or (alist-get 'id object) "ssh"))
         (config (remote-config--plist (alist-get 'config object)))
         (config (if (or (plist-get config :host) (null host))
                     config
                   (plist-put config :host host)))
         (config (if (or (not (member "tramp" backends))
                         (plist-get config :method))
                     config
                   (plist-put config :method "ssh"))))
    (remote-register-pipeline
     target-id id backends
     :stages (alist-get 'stages object)
     :enabled (not (eq (alist-get 'enabled object t) nil))
     :priority (or (alist-get 'priority object) 0)
     :config config
     :capabilities
     (and (alist-get 'capabilities object)
          (mapcar #'intern
                  (remote-config--string-list
                   (alist-get 'capabilities object))))
     :source 'remote-config)))

(defalias 'remote-config--register-link-object
  #'remote-config--register-pipeline-object)

(defun remote-config--register-target-object (object)
  "Register explicit target OBJECT."
  (let* ((id (or (alist-get 'id object)
                 (error "Remote target has no id: %S" object)))
         (target
          (remote-register-target
           id
           :label (alist-get 'label object)
           :workspaces (alist-get 'workspaces object)
           :environment (alist-get 'environment object)
           :preferences
           (remote-config--preferences
            (alist-get 'preferences object))
           :system (alist-get 'system object)
           :architecture (alist-get 'architecture object)
           :shell (alist-get 'shell object)
           :trusted (not (eq (alist-get 'trusted object) nil))
           :source 'remote-config)))
    (dolist (pipeline
             (or (alist-get 'pipelines object)
                 (alist-get 'links object)))
      (remote-config--register-pipeline-object
       (remote-target-id target) pipeline))
    target))

(defun remote-config--import-ssh (object)
  "Import SSH config targets described by OBJECT."
  (let (hosts)
    (dolist (file (remote-config--string-list (alist-get 'files object)))
      (setq hosts
            (nconc hosts (remote-config--ssh-hosts file))))
    (dolist (host (delete-dups hosts))
      (when (remote-config--allowed-p host object)
        (let* ((id (remote-fs--slug host))
               (target
                (or (remote-get-target id)
                    (remote-register-target
                     id :label host :trusted nil
                     :source 'ssh-config))))
          (dolist (pipeline
                   (or (alist-get 'pipelines object)
                       (alist-get 'links object)))
            (when (remote-config--allowed-p host pipeline)
              (when (not (eq (alist-get 'trusted pipeline) nil))
                (setf (remote-target-trusted target) t))
              (remote-config--register-pipeline-object
               id pipeline host))))))))

(defun remote-config--schema-version (root)
  "Validate and return ROOT's configuration schema version."
  (let ((version (or (alist-get 'version root) 1)))
    (unless (and (integerp version)
                 (memq version '(1 2)))
      (error
       "Unsupported remote config version %S (supported: 1, %d)"
       version remote-config-current-version))
    version))

(defun remote-config-load (&optional file)
  "Load targets and pipelines from FILE or `remote-config-file'."
  (interactive)
  (let ((file (or file remote-config-file)))
    (unless (file-readable-p file)
      (error "Remote config is not readable: %s" file))
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (json-key-type 'symbol)
           (json-false nil)
           (root (json-read-file file))
           (version (remote-config--schema-version root))
           (settings (alist-get 'settings root))
           (old-pipelines remote-pipelines)
           (staged-targets (remote-config--copy-target-registry))
           (staged-pipelines (remote-config--copy-pipeline-registry)))
      ;; All validation and registration happens against detached registries.
      ;; A malformed import or conflicting pipeline therefore leaves the live
      ;; targets, sessions, and generation untouched.
      (let ((remote-targets staged-targets)
            (remote-pipelines staged-pipelines))
        (remote-config--clear-loaded-objects)
        (dolist (target (alist-get 'targets root))
          (remote-config--register-target-object target))
        (dolist (import (alist-get 'imports root))
          (pcase (alist-get 'type import)
            ("ssh-config" (remote-config--import-ssh import))
            (type
             (remote-log
              'config-warning
              :message (format "Unknown remote import type: %S" type))))))
      (let ((changed
             (remote-config--changed-pipelines
              old-pipelines staged-pipelines)))
        (setq remote-targets staged-targets
              remote-pipelines staged-pipelines
              remote-config-version version
              remote-config-settings settings)
        (remote-config--invalidate-pipelines changed))
      (cl-incf remote-config-generation)
      (run-hooks 'remote-config-after-load-hook)
      (remote-log
       'config
       :file file
       :version remote-config-version
       :generation remote-config-generation)
      t)))

(defalias 'remote-config-reload #'remote-config-load)

(provide 'remote-config)
;;; remote-config.el ends here
