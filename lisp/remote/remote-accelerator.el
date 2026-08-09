;;; remote-accelerator.el --- High-level remote operation providers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Optional accelerators implement the same public operation contract as the
;; ordinary Emacs fallback.  Selection is route- and capability-based; package
;; names and Emacs versions never escape this boundary.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'remote-core)
(require 'remote-compat)
(require 'remote-fs)

(declare-function remote-connection-generation-for-route
                  "remote-connection" (route))

(declare-function tramp-hlo-locate-dominating-file
                  "tramp-hlo" (file name))
(declare-function tramp-hlo-dir-locals--all-files
                  "tramp-hlo" (directory &optional base-el-only))
(declare-function tramp-hlo-dir-locals-find-file
                  "tramp-hlo" (file))

(cl-defstruct (remote-operation-provider
               (:constructor remote-operation-provider-create))
  id operations applicable-function invoke-function describe-function)

(defvar remote-operation-providers nil
  "Registered high-level operation providers in preference order.")

(defvar remote-accelerator-probe-cache (make-hash-table :test #'equal)
  "Per-route operation capability probe results.")

(defun remote-accelerator-clear-route (route)
  "Clear accelerator probes associated with ROUTE."
  (let ((prefix (list (remote-route-target-id route)
                      (remote-route-link-id route)))
        keys)
    (maphash
     (lambda (key _value)
       (when (equal (seq-take key 2) prefix)
         (push key keys)))
     remote-accelerator-probe-cache)
    (dolist (key keys)
      (remhash key remote-accelerator-probe-cache))
    (length keys)))

(defun remote-accelerator-handle-locate-dominating-file (file name)
  "Route `locate-dominating-file' for logical FILE and NAME."
  (remote-fs--call-routed 'locate-dominating-file (list file name)))

(defun remote-accelerator-handle-dir-locals--all-files
    (directory &optional base-el-only)
  "Route `dir-locals--all-files' for DIRECTORY and BASE-EL-ONLY."
  (remote-fs--call-routed
   'dir-locals--all-files (list directory base-el-only)))

(defun remote-accelerator-handle-dir-locals-find-file (file)
  "Route `dir-locals-find-file' for logical FILE."
  (remote-fs--call-routed 'dir-locals-find-file (list file)))

(cl-defun remote-register-operation-provider
    (id &key operations applicable invoke describe append)
  "Register high-level operation provider ID.
OPERATIONS is a list of public operation symbols.  APPLICABLE receives
OPERATION, ROUTE, CONTEXT, ARGS, and PHYSICAL-DEFAULT.  INVOKE receives the
same values and must return the ordinary operation result."
  (unless (and (stringp id) operations (functionp invoke))
    (error "Invalid remote operation provider: %S" id))
  (setq remote-operation-providers
        (cl-delete id remote-operation-providers
                   :key #'remote-operation-provider-id :test #'equal))
  (let ((provider
         (remote-operation-provider-create
          :id id :operations (copy-sequence operations)
          :applicable-function applicable :invoke-function invoke
          :describe-function describe)))
    (if append
        (setq remote-operation-providers
              (append remote-operation-providers (list provider)))
      (push provider remote-operation-providers))
    provider))

(defun remote-operation-provider-for
    (operation route context args physical-default)
  "Return the first provider applicable to OPERATION on ROUTE."
  (seq-find
   (lambda (provider)
     (and
      (memq operation (remote-operation-provider-operations provider))
      (if-let* ((applicable
                 (remote-operation-provider-applicable-function provider)))
          (condition-case error
              (funcall applicable operation route context args physical-default)
            (error
             (remote-log
              'accelerator-probe-error
              :provider (remote-operation-provider-id provider)
              :operation operation
              :error (error-message-string error))
             nil))
        t)))
   remote-operation-providers))

(defun remote-operation-provider-call
    (provider operation route context args physical-default)
  "Invoke PROVIDER for OPERATION on ROUTE."
  (funcall (remote-operation-provider-invoke-function provider)
           operation route context args physical-default))

(defun remote-operation-provider-list ()
  "Return stable descriptions of registered operation providers."
  (mapcar
   (lambda (provider)
     (append
      (list :id (remote-operation-provider-id provider)
            :operations
            (copy-sequence
             (remote-operation-provider-operations provider)))
      (when-let* ((describe
                   (remote-operation-provider-describe-function provider)))
        (funcall describe))))
   (reverse remote-operation-providers)))

(defun remote-accelerator--tramp-hlo-probe
    (operation route _context args physical-default)
  "Return whether tramp-hlo can serve OPERATION on ROUTE."
  (and
   (equal (remote-route-link-plugin-id route) "tramp")
   (locate-library "tramp-hlo")
   ;; Upstream explicitly does not yet preserve this public stop contract.
   (or (not (eq operation 'locate-dominating-file))
       (null locate-dominating-stop-dir-regexp))
   ;; tramp-hlo 0.0.2 does not accept the newer optional BASE-EL-ONLY
   ;; argument.  The ordinary Emacs/TRAMP path remains the correct fallback.
   (or (not (eq operation 'dir-locals--all-files))
       (null (cadr args)))
   (let* ((key (list (remote-route-target-id route)
                     (remote-route-link-id route) operation
                     (and (fboundp
                           'remote-connection-generation-for-route)
                          (remote-connection-generation-for-route route))))
          (cached (gethash key remote-accelerator-probe-cache 'missing)))
     (if (not (eq cached 'missing))
         cached
       (let* ((physical-path (car-safe args))
              (local-path
               (and (stringp physical-path)
                    (or (file-remote-p physical-path 'localname)
                        physical-path)))
              (realpath-preserves-input
               (concat
                "command -v realpath >/dev/null 2>&1 && "
                "path=${1%/}; test -n \"$path\" || path=/; "
                "test \"$(realpath \"$path\")\" = \"$path\""))
              (command
              (pcase operation
                ('dir-locals--all-files
                 realpath-preserves-input)
                ('dir-locals-find-file
                 (concat "command -v realpath >/dev/null 2>&1 && "
                         "stat -c %Y / >/dev/null 2>&1")))))
         (puthash
          key
          ;; `locate-dominating-file' needs no target utility.  Avoid opening a
          ;; second connection merely to prove that no prerequisite exists.
          (or (null command)
              (let ((default-directory physical-default))
                (zerop
                 (process-file
                  "sh" nil nil nil "-c" command
                  "remote-tramp-hlo-probe" (or local-path "/")))))
          remote-accelerator-probe-cache))))))

(defun remote-accelerator--tramp-hlo-invoke
    (operation _route _context args _physical-default)
  "Invoke tramp-hlo implementation for OPERATION with physical ARGS."
  (unless (require 'tramp-hlo nil t)
    (signal 'remote-backend-unsupported
            (list "tramp-hlo is not installed")))
  (pcase operation
    ('locate-dominating-file
     (apply #'tramp-hlo-locate-dominating-file args))
    ('dir-locals--all-files
     (apply #'tramp-hlo-dir-locals--all-files args))
    ('dir-locals-find-file
     (apply #'tramp-hlo-dir-locals-find-file args))
    (_
     (signal 'remote-backend-unsupported
             (list "Unsupported tramp-hlo operation" operation)))))

(defun remote-accelerator--project-dir-locals-result
    (result target-id _spec)
  "Project mixed `dir-locals-find-file' RESULT into TARGET-ID.
The public function returns nil, a directory string, or a cache entry whose
first element is a directory.  Keeping that union explicit avoids teaching
the generic file boundary about one package's result shape."
  (cond
   ((stringp result)
    (remote-fs--rewrap-physical result target-id))
   ((and (consp result) (stringp (car result)))
    (cons (remote-fs--rewrap-physical (car result) target-id)
          (cdr result)))
   (t result)))

(defun remote-accelerator-register-builtins ()
  "Register built-in high-level operation contracts and providers."
  (dolist (entry
           '((locate-dominating-file
              remote-accelerator-handle-locate-dominating-file
              metadata (0) path nil)
             (dir-locals--all-files
              remote-accelerator-handle-dir-locals--all-files
              metadata (0) path-list nil)
             (dir-locals-find-file
              remote-accelerator-handle-dir-locals-find-file
              metadata (0) pass
              remote-accelerator--project-dir-locals-result)))
    (pcase-let
        ((`(,operation ,handler ,capability ,arguments ,result-kind ,projector)
          entry))
      (when (assq operation remote-fs-file-name-handler-alist)
        (remote-compat-tramp-remove-external-operation operation 'remote-fs))
      (remote-register-file-operation
       operation :capability capability :path-arguments arguments
       :result-kind result-kind :result-projector projector :retry-safe t)
      ;; This makes the public high-level function participate in magic file
      ;; dispatch for `/fs:' without enabling tramp-hlo globally for `/ssh:'.
      (remote-compat-tramp-add-external-operation
       operation handler 'remote-fs 'file)))
  (remote-register-operation-provider
   "tramp-hlo"
   :operations
   '(locate-dominating-file dir-locals--all-files dir-locals-find-file)
   :applicable #'remote-accelerator--tramp-hlo-probe
   :invoke #'remote-accelerator--tramp-hlo-invoke
   :describe
   (lambda ()
     (list :available (and (locate-library "tramp-hlo") t)
           :scope 'logical-fs-tramp-sh)))
  remote-operation-providers)

(remote-accelerator-register-builtins)

(provide 'remote-accelerator)
;;; remote-accelerator.el ends here
