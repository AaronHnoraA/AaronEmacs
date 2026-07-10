;;; config-custom.el --- Native Customize adapter for config -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module exposes the `config' registry through Emacs' native Customize
;; UI while keeping `etc/config-*.el' as the only persisted source of truth.
;; Applying a value is live-only; saving a value records it through
;; `config-set' / `config-hook-set' and removes the managed variable from
;; `custom-file' serialization.

;;; Code:

(require 'cl-lib)
(require 'config)
(require 'cus-edit)

(defvar config-custom--managed (make-hash-table :test 'eq)
  "Hash table of config-managed Custom symbols.")

(defvar config-custom--hook-proxies (make-hash-table :test 'eq)
  "Hash table mapping generated hook proxy symbols to (HOOK . FN).")

(defvar config-custom--saving nil
  "Non-nil while this adapter is routing a native Custom save.")

(defun config-custom--group-symbol (group)
  "Return the generated Custom group symbol for registry GROUP."
  (intern (format "config-group-%s" (or group 'misc))))

(defun config-custom--group-label (group)
  "Return a readable label for registry GROUP."
  (if group (symbol-name group) "misc"))

(defun config-custom--ensure-group (group)
  "Declare and return the generated Custom group for GROUP."
  (let ((symbol (config-custom--group-symbol group)))
    (unless (get symbol 'custom-group)
      (custom-declare-group
       symbol nil
       (format "Config registry group: %s." (config-custom--group-label group))
       :group 'config))
    (custom-add-to-group 'config symbol 'custom-group)
    symbol))

(defun config-custom--choice-type (choices)
  "Return a Custom type for CHOICES."
  `(choice ,@(mapcar (lambda (choice)
                       `(const :tag ,(prin1-to-string choice) ,choice))
                     choices)))

(defun config-custom--type (entry)
  "Return a native Custom type for ENTRY."
  (or (and-let* ((choices (plist-get entry :choices)))
        (config-custom--choice-type choices))
      (pcase (plist-get entry :type)
        ('boolean 'boolean)
        ('integer 'integer)
        ((or 'number 'float) 'number)
        ('string 'string)
        ('function 'function)
        ('face 'face)
        (`(choice . ,choices) (config-custom--choice-type choices))
        (_ 'sexp))))

(defun config-custom--override-value (name)
  "Return NAME's persisted override value, or nil when absent."
  (when-let* ((cell (gethash name config--override-index)))
    (cdr cell)))

(defun config-custom--saved-form (value)
  "Return VALUE encoded as a Custom saved/standard value form."
  (list (list 'quote value)))

(defun config-custom--put-saved-state (symbol value)
  "Mark SYMBOL's native Custom saved state as VALUE."
  (put symbol 'saved-value (config-custom--saved-form value))
  (put symbol 'customized-value nil))

(defun config-custom--clear-saved-state (symbol)
  "Clear native Custom saved state for SYMBOL."
  (put symbol 'saved-value nil)
  (put symbol 'customized-value nil))

(defun config-custom--variable-get (symbol)
  "Custom getter for config-managed variable SYMBOL."
  (config-get symbol))

(defun config-custom--variable-set (symbol value)
  "Custom setter for config-managed variable SYMBOL."
  (if config-custom--saving
      (config-set symbol value nil 'quiet)
    (config-set symbol value 'no-persist 'quiet))
  value)

(defun config-custom--sync-variable (entry)
  "Expose variable ENTRY as a native Custom option."
  (let* ((name (plist-get entry :name))
         (group (config-custom--ensure-group (plist-get entry :group)))
         (doc (or (plist-get entry :doc)
                  (documentation-property name 'variable-documentation t)
                  "Config registry variable.")))
    (puthash name t config-custom--managed)
    (put name 'config-custom-key name)
    (put name 'custom-type (config-custom--type entry))
    (put name 'custom-group (list group))
    (put name 'group-documentation doc)
    (put name 'variable-documentation doc)
    (put name 'standard-value
         (config-custom--saved-form (plist-get entry :initial-value)))
    (put name 'custom-get #'config-custom--variable-get)
    (put name 'custom-set #'config-custom--variable-set)
    (custom-add-to-group group name 'custom-variable)
    (if (gethash name config--override-index)
        (config-custom--put-saved-state name (config-custom--override-value name))
      (config-custom--clear-saved-state name))))

(defun config-custom--hook-proxy-symbol (hook fn)
  "Return a generated Custom proxy symbol for HOOK/FN."
  (intern (format "config-hook-%s--%s" hook fn)))

(defun config-custom--hook-proxy-get (symbol)
  "Custom getter for hook proxy SYMBOL."
  (pcase-let ((`(,hook . ,fn) (gethash symbol config-custom--hook-proxies)))
    (config-hook-member-p hook fn)))

(defun config-custom--hook-proxy-set (symbol value)
  "Custom setter for hook proxy SYMBOL."
  (pcase-let ((`(,hook . ,fn) (gethash symbol config-custom--hook-proxies)))
    (if config-custom--saving
        (config-hook-set hook fn value)
      (config-hook-set hook fn value 'no-persist)))
  value)

(defun config-custom--sync-hook (entry)
  "Expose hook membership candidates from ENTRY as native Custom booleans."
  (let* ((hook (plist-get entry :hook))
         (group (config-custom--ensure-group (plist-get entry :group))))
    (dolist (candidate (plist-get entry :candidates))
      (let* ((fn (car candidate))
             (label (or (cdr candidate) (symbol-name fn)))
             (symbol (config-custom--hook-proxy-symbol hook fn))
             (key (list :hook hook fn)))
        (puthash symbol (cons hook fn) config-custom--hook-proxies)
        (puthash symbol t config-custom--managed)
        (put symbol 'config-custom-key key)
        (put symbol 'custom-type 'boolean)
        (put symbol 'custom-group (list group))
        (put symbol 'variable-documentation
             (format "Whether `%s' is enabled on `%s'. %s" fn hook label))
        (put symbol 'standard-value
             (config-custom--saved-form
              (and (memq fn (plist-get entry :initial-value)) t)))
        (put symbol 'custom-get #'config-custom--hook-proxy-get)
        (put symbol 'custom-set #'config-custom--hook-proxy-set)
        (custom-add-to-group group symbol 'custom-variable)
        (if (gethash key config--override-index)
            (config-custom--put-saved-state symbol (cdr (gethash key config--override-index)))
          (config-custom--clear-saved-state symbol))))))

(defun config-custom--sync-file (entry)
  "Record file ENTRY's group for native Customize.
File items are still operated through `config-file-open' and the dispatch
menu; native Custom has no first-class file-management widget."
  (config-custom--ensure-group (plist-get entry :group)))

(defun config-custom-sync-entry (entry)
  "Expose config registry ENTRY to native Customize."
  (pcase (plist-get entry :kind)
    ('variable (config-custom--sync-variable entry))
    ('hook (config-custom--sync-hook entry))
    ('file (config-custom--sync-file entry))))

(defun config-custom-sync ()
  "Synchronize all registered config entries into native Customize."
  (interactive)
  (clrhash config-custom--hook-proxies)
  (clrhash config-custom--managed)
  (dolist (entry (config-list))
    (config-custom-sync-entry entry))
  (message "config-custom: synced %d items" (hash-table-count config-custom--managed)))

(defun config-custom-open ()
  "Open the native Customize group for the config registry."
  (interactive)
  (config-custom-sync)
  (customize-group 'config))

(defun config-custom--managed-p (symbol)
  "Return non-nil when SYMBOL is managed by this adapter."
  (gethash symbol config-custom--managed))

(defun config-custom--saved-value (symbol)
  "Return SYMBOL's native Custom saved value."
  (when-let* ((saved (get symbol 'saved-value)))
    (eval (car saved) t)))

(defun config-custom--persist-symbol (symbol)
  "Persist SYMBOL's native Custom saved value via the config store."
  (when (and (config-custom--managed-p symbol)
             (get symbol 'saved-value))
    (let ((value (config-custom--saved-value symbol))
          (key (get symbol 'config-custom-key)))
      (pcase key
        (`(:hook ,hook ,fn)
         (config-hook-set hook fn value))
        ((pred symbolp)
         (config-set key value))))))

(defun config-custom--around-customize-save-variable (orig symbol value &rest args)
  "Route `customize-save-variable' for config-managed SYMBOL into config."
  (if (config-custom--managed-p symbol)
      (let ((config-custom--saving t))
        (pcase (get symbol 'config-custom-key)
          (`(:hook ,hook ,fn)
           (config-hook-set hook fn value))
          ((pred symbolp)
           (config-set symbol value)))
        (config-custom--put-saved-state symbol value)
        value)
    (apply orig symbol value args)))

(defun config-custom--around-custom-save-all (orig &rest args)
  "Keep config-managed variables out of `custom-file' serialization."
  (let (managed-saved)
    (maphash
     (lambda (symbol _)
       (when (get symbol 'saved-value)
         (push (list symbol
                     (get symbol 'saved-value)
                     (get symbol 'customized-value))
               managed-saved)
         (let ((config-custom--saving t))
           (config-custom--persist-symbol symbol))
         (put symbol 'saved-value nil)
         (put symbol 'customized-value nil)))
     config-custom--managed)
    (unwind-protect
        (apply orig args)
      (dolist (state managed-saved)
        (pcase-let ((`(,symbol ,saved ,customized) state))
          (put symbol 'saved-value saved)
          (put symbol 'customized-value customized))))))

(defun config-custom--after-register (entry)
  "Synchronize newly registered ENTRY when the adapter is loaded."
  (config-custom-sync-entry entry))

(add-hook 'config-after-register-hook #'config-custom--after-register)
(advice-add 'customize-save-variable :around
            #'config-custom--around-customize-save-variable)
(advice-add 'custom-save-all :around #'config-custom--around-custom-save-all)

(config-custom-sync)

;;;###autoload
(defun config-board ()
  "Compatibility entry point; open native Customize for the config registry."
  (interactive)
  (config-custom-open))

(provide 'config-custom)
;;; config-custom.el ends here
