;;; remote-compat.el --- Upstream compatibility boundary -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Keep optional and evolving Emacs/TRAMP contracts in one place.  Callers use
;; capabilities and public entry points instead of branching on Emacs versions
;; or reaching into upstream implementation variables.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tramp)
(require 'compat nil t)

(define-error 'remote-compat-error "Remote compatibility error")
(define-error 'remote-operation-contract-error
              "Remote operation contract error" 'remote-compat-error)

(defun remote-compat-tramp-vector (value)
  "Return VALUE as a parsed Tramp vector, or nil.
Tramp foreign-handler predicates have received both file-name strings and
parsed vectors across upstream revisions.  Normalize the shape at this one
boundary so predicates remain total when Tramp changes its dispatcher."
  (condition-case nil
      (cond
       ((tramp-file-name-p value) value)
       ((and (fboundp 'tramp-ensure-dissected-file-name)
             (stringp value))
        (tramp-ensure-dissected-file-name value))
       ((stringp value)
        (tramp-dissect-file-name value t)))
    (error nil)))

(defun remote-compat-function-signatures-compatible-p (operation handler)
  "Return non-nil when OPERATION and HANDLER accept the same arity."
  (cl-labels
      ((public-arity
        (function)
        ;; Tramp implements external operations with advice, whose combined
        ;; function looks variadic to `func-arity'.  The documented argument
        ;; list remains available through Emacs help metadata.
        (let ((arguments (help-function-arglist function t)))
          (if (listp arguments)
              (func-arity (list 'lambda arguments nil))
            (func-arity function)))))
    (and (functionp operation)
         (functionp handler)
         (equal (public-arity operation)
                (public-arity handler)))))

(defun remote-compat-error-has-type-p (error type)
  "Return non-nil when ERROR has condition TYPE.
Use the Emacs 31/Compat structural API when available and retain a small
portable fallback for the supported Emacs 30 floor."
  (if (fboundp 'error-has-type-p)
      (error-has-type-p error type)
    (let* ((symbol (car-safe error))
           (conditions (and (symbolp symbol)
                            (get symbol 'error-conditions))))
      (or (eq symbol type) (memq type conditions)))))

(defun remote-compat-tramp-register-foreign-handler
    (predicate handler operations)
  "Register PREDICATE and HANDLER for Tramp OPERATIONS.
Prefer Tramp's registration API.  The fallback is deliberately isolated here
and only maintains the documented handler `operations' property."
  (if (fboundp 'tramp-register-foreign-file-name-handler)
      (tramp-register-foreign-file-name-handler predicate handler)
    (add-to-list 'tramp-foreign-file-name-handler-alist
                 (cons predicate handler))
    (put #'tramp-file-name-handler
         'operations
         (seq-union (get 'tramp-file-name-handler 'operations)
                    operations))))

(defun remote-compat-tramp-external-operations-p ()
  "Return non-nil when Tramp supports public external operations."
  (and (fboundp 'tramp-add-external-operation)
       (fboundp 'tramp-remove-external-operation)))

(defun remote-compat-tramp-add-external-operation
    (operation function backend &optional placement)
  "Register FUNCTION for external OPERATION on Tramp BACKEND.
PLACEMENT has the meaning of Tramp's ARG-TYPE argument.  Return non-nil when
the operation was installed; an unavailable API simply leaves the optimization
unavailable.  Tramp documents OPERATION and FUNCTION as function symbols with
the same argument list, so reject accidental anonymous or stale adapters
before they become global advice."
  (when (remote-compat-tramp-external-operations-p)
    (unless (and (symbolp operation) (fboundp operation))
      (signal 'remote-operation-contract-error
              (list operation "External operation is not a function symbol")))
    (unless (and (symbolp function) (fboundp function))
      (signal 'remote-operation-contract-error
              (list function "External handler is not a function symbol")))
    (unless (remote-compat-function-signatures-compatible-p
             operation function)
      (signal
       'remote-operation-contract-error
       (list operation function
             (func-arity operation) (func-arity function))))
    (tramp-add-external-operation operation function backend placement)
    (or (not (fboundp 'tramp-external-operation-p))
        (tramp-external-operation-p operation backend))))

(defun remote-compat-tramp-remove-external-operation (operation backend)
  "Remove external OPERATION for Tramp BACKEND when supported."
  (when (remote-compat-tramp-external-operations-p)
    (tramp-remove-external-operation operation backend)
    t))

(defun remote-compat-report ()
  "Return a stable compatibility report for Doctor and tests."
  (list
   :emacs-version emacs-version
   :tramp-version
   (or (and (boundp 'tramp-version) tramp-version) "unknown")
   :foreign-handler-registration
   (fboundp 'tramp-register-foreign-file-name-handler)
   :foreign-handler-input-normalizer t
   :external-operations (remote-compat-tramp-external-operations-p)
   :external-operation-query (fboundp 'tramp-external-operation-p)
   :structured-errors (fboundp 'error-has-type-p)
   :public-file-notify
   (and (fboundp 'file-notify-add-watch)
        (fboundp 'file-notify-rm-watch))))

(provide 'remote-compat)
;;; remote-compat.el ends here
