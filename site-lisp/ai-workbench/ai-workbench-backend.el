;;; ai-workbench-backend.el --- Backend registry for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Internal backend contribution registry.  Adapters register capabilities and
;; operation functions here; the UI and dispatch layer derive their choices
;; from this registry instead of maintaining a parallel hardcoded stack.

;;; Code:

(require 'cl-lib)

(defvar ai-workbench-backend--registry (make-hash-table :test 'eq)
  "Hash table mapping backend ids to spec plists.")

(defun ai-workbench-backend--validate (id spec)
  "Validate backend ID and SPEC."
  (unless (symbolp id)
    (error "Backend id must be a symbol: %S" id))
  (unless (stringp (plist-get spec :label))
    (error "Backend %s missing string :label" id))
  (dolist (cap (plist-get spec :capabilities))
    (unless (memq cap '(:session :send :draft :cancel :stop :headless))
      (error "Backend %s has unsupported capability %S" id cap)))
  (let ((ops (plist-get spec :operations)))
    (dolist (op '(:available-p :live-p :ensure :open))
      (unless (functionp (plist-get ops op))
        (error "Backend %s missing operation %S" id op)))
    (dolist (pair '((:send . :send) (:draft . :draft) (:stop . :stop) (:cancel . :cancel)))
      (when (memq (car pair) (plist-get spec :capabilities))
        (unless (functionp (plist-get ops (cdr pair)))
          (error "Backend %s missing operation %S" id (cdr pair))))))
  t)

;;;###autoload
(defun ai-workbench-register-backend (id &rest spec)
  "Register backend ID with SPEC and return a retractor function."
  (ai-workbench-backend--validate id spec)
  (puthash id spec ai-workbench-backend--registry)
  (let ((generation (plist-get spec :generation)))
    (lambda ()
      (when (eq generation (plist-get (gethash id ai-workbench-backend--registry)
                                      :generation))
        (remhash id ai-workbench-backend--registry)))))

(defun ai-workbench-backend-spec (id)
  "Return backend ID's spec plist, or nil."
  (gethash id ai-workbench-backend--registry))

(defun ai-workbench-backend-ids (&optional capability)
  "Return registered backend ids, optionally filtered by CAPABILITY."
  (let (ids)
    (maphash
     (lambda (id spec)
       (when (or (null capability)
                 (memq capability (plist-get spec :capabilities)))
         (push id ids)))
     ai-workbench-backend--registry)
    (sort ids (lambda (a b)
                (string< (plist-get (gethash a ai-workbench-backend--registry) :label)
                         (plist-get (gethash b ai-workbench-backend--registry) :label))))))

(defun ai-workbench-backend-label (id)
  "Return backend ID's display label."
  (or (plist-get (ai-workbench-backend-spec id) :label)
      (symbol-name id)))

(defun ai-workbench-backend-call (id operation &rest args)
  "Call OPERATION for backend ID with ARGS."
  (let* ((spec (or (ai-workbench-backend-spec id)
                   (error "Unknown backend: %s" id)))
         (fn (plist-get (plist-get spec :operations) operation)))
    (unless (functionp fn)
      (error "Backend %s does not support %S" id operation))
    (apply fn args)))

(defun ai-workbench-backend-live-p (id project-root)
  "Return non-nil when backend ID has a live session for PROJECT-ROOT."
  (ignore-errors
    (ai-workbench-backend-call id :live-p project-root)))

(provide 'ai-workbench-backend)
;;; ai-workbench-backend.el ends here
