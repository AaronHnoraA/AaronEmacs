;;; config.el --- Unified configuration registry -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A single source of truth for configurable items in this Emacs config.
;;
;; Design principle: there are NO hardcoded "default" values in Lisp.
;; `etc/config-store.el' is committed with the repo and ships the opinionated
;; values.  It IS the config.  Any setting that should be user-tunable goes
;; into the store, not into a `setq' call.  This prevents variable pollution:
;; values are not scattered across dozens of init files.
;;
;; Workflow:
;;   1. Register a variable: `(config-register 'foo :group 'g :type 'integer)'
;;   2. Add the initial value to `etc/config-store.el' or a group-specific
;;      store via `config-set', or by editing the store directly.
;;   3. At startup `config' auto-loads the known stores before any module runs,
;;      so each `config-register' call immediately finds and applies its
;;      override.
;;   4. Live changes via `M-x config-board' (SPC h c) update both the live
;;      variable and the store atomically.
;;
;; Three kinds of items:
;;
;;   variable : backed by a Lisp symbol (`:var').  `config-set' keeps the
;;              backing variable authoritative for third-party code.
;;   hook     : membership of named functions on a hook.
;;   file     : an `etc/' config file with a loader and a post-edit script.
;;
;; Load order: `config' is required before the module graph so each module's
;; `config-register' call can apply its stored override on the spot.
;; `config-apply-store' runs again on `after-init-hook' as a final pass (e.g.
;; to fire `:on-change' hooks defined later).

;;; Code:

(require 'cl-lib)

(defgroup config nil
  "Unified configuration registry."
  :group 'convenience
  :prefix "config-")

(defvar config--registry (make-hash-table :test 'eq)
  "Hash table mapping an item NAME (symbol) to its metadata plist.")

(defvar config--overrides nil
  "Alist of persisted overrides loaded from / written to the store.

Each element is (KEY . VALUE) where KEY is either an item NAME symbol
\(a variable item) or a list (:hook HOOK FN) (a hook-membership toggle).
Ordering is significant (it determines serialization order), so this stays
an alist; `config--override-index' provides O(1) lookup over the same cells.")

(defvar config--override-index (make-hash-table :test 'equal)
  "Hash table mapping an override KEY to its cons cell in `config--overrides'.
Kept in lockstep with `config--overrides' so reads and updates are O(1)
instead of a linear `assoc' scan over a growing alist at startup.")

(defvar config--unknown-warned (make-hash-table :test 'eq)
  "Names already warned about in `config-get', to avoid repeat warnings.")

(defvar config--loaded-store-files (make-hash-table :test 'equal)
  "Store files already loaded into `config--overrides'.")

(defvar config-store-file
  (expand-file-name "etc/config-store.el" user-emacs-directory)
  "Store file that ships the opinionated config values for this machine.

Committed with the repo.  Edit via `M-x config-board' or `config-set';
both update this file atomically.  Do not scatter raw `setq' calls that
duplicate values already tracked here — that causes variable pollution.")

(defvar config-store-file-regexp "\\`config-.*\\.el\\'"
  "Regexp matching store files to preload from `config-store-file' directory.")

(defvar config-extra-store-files nil
  "Additional store files loaded before modules register configuration.
Store files matching `config-store-file-regexp' beside `config-store-file' are
already discovered and loaded automatically.  Use this for ad-hoc stores
outside that directory or naming convention.  Modules may also specify per-item
`:store-file' metadata; unknown files are loaded lazily during registration and
persisted separately.")

(defvar config--override-store-files (make-hash-table :test 'equal)
  "Hash table mapping persisted override keys to their loaded store files.")

(defvar config--configured-store-files-cache nil
  "Cached list of expanded store files discovered for startup loading.")

(defvar config--group-store-file-sets (make-hash-table :test 'eq)
  "Hash table mapping registry groups to store-file sets.
Each value is a hash table where keys are expanded store file names.")

(defvar config--current-store-file nil
  "Store file currently calling `config-store-set'.")

;;; Internal helpers ---------------------------------------------------------

(defun config--caller-source ()
  "Return a short label for the file currently being loaded."
  (cond
   (load-file-name (file-name-nondirectory load-file-name))
   ((bound-and-true-p byte-compile-current-file)
    (file-name-nondirectory byte-compile-current-file))
   (t "interactive")))

(defun config--entry (name)
  "Return the metadata plist for NAME, or nil."
  (gethash name config--registry))

(defun config--expand-store-file (file)
  "Return FILE as an absolute store path, or nil when FILE is nil."
  (when file
    (expand-file-name file user-emacs-directory)))

(defun config--entry-store-file (entry)
  "Return ENTRY's explicit expanded store file, or nil."
  (config--expand-store-file (plist-get entry :store-file)))

(defun config--discover-store-files ()
  "Return store files discovered next to `config-store-file'."
  (let ((directory (file-name-directory
                    (config--expand-store-file config-store-file))))
    (when (file-directory-p directory)
      (directory-files directory t config-store-file-regexp))))

(defun config--configured-store-files ()
  "Return expanded store files configured for startup preloading."
  (or config--configured-store-files-cache
      (setq config--configured-store-files-cache
            (delete-dups
             (delq nil
                   (mapcar #'config--expand-store-file
                           (append (list config-store-file)
                                   config-extra-store-files
                                   (config--discover-store-files))))))))

(defun config--load-configured-store-files ()
  "Load all configured store files once."
  (dolist (file (config--configured-store-files))
    (config--load-store-file file)))

(defun config--learn-group-store-file (group file)
  "Record that GROUP has persisted configuration in FILE."
  (when (and group file)
    (let ((files (or (gethash group config--group-store-file-sets)
                     (let ((table (make-hash-table :test 'equal)))
                       (puthash group table config--group-store-file-sets)
                       table))))
      (puthash file t files))))

(defun config--hash-table-keys (table)
  "Return a list of TABLE's keys."
  (let (keys)
    (maphash (lambda (key _value) (push key keys)) table)
    (nreverse keys)))

(defun config--primary-store-file ()
  "Return the expanded primary config store file."
  (config--expand-store-file config-store-file))

(defun config--group-default-store-file (group)
  "Return GROUP's inferred default store file, or nil if ambiguous.
If a group has exactly one non-primary store file, prefer it over the primary
store.  This lets broad fallback values live in `config-store-file' while a
specialized `etc/config-*.el' owns new keys for the same group."
  (when-let* ((files (gethash group config--group-store-file-sets)))
    (let* ((primary (config--primary-store-file))
           (all-files (config--hash-table-keys files))
           (specialized (cl-remove primary all-files :test #'equal)))
      (cond
       ((= (length specialized) 1) (car specialized))
       ((and (null specialized) (= (length all-files) 1)) (car all-files))
       (t nil)))))

(defun config--learn-entry-store-file (key entry)
  "Learn ENTRY's group store from persisted KEY ownership."
  (config--learn-group-store-file
   (plist-get entry :group)
   (gethash key config--override-store-files)))

(defun config--backing-var (entry name)
  "Return the backing variable symbol for ENTRY named NAME, or nil.
Defaults to NAME unless ENTRY explicitly supplies `:var' (possibly nil)."
  (if (plist-member entry :var)
      (plist-get entry :var)
    name))

(defun config--valid-type-p (type value)
  "Return non-nil when VALUE is acceptable for TYPE."
  (pcase type
    ('boolean (memq value '(t nil)))
    ('integer (integerp value))
    ('number (numberp value))
    ('float (floatp value))
    ('string (stringp value))
    ('function (or (functionp value) (null value)
                   (and (symbolp value) value)))
    (`(choice . ,choices)
     (cl-some
      (lambda (choice)
        (pcase choice
          (`(const . ,spec)
           (let ((const-value (car (last spec))))
             (equal value const-value)))
          ('integer (integerp value))
          ('number (numberp value))
          ('float (floatp value))
          ('string (stringp value))
          ('boolean (memq value '(t nil)))
          (_ t)))
      choices))
    (_ t)))

(defun config--run-on-change (entry name value)
  "Run ENTRY's `:on-change' for NAME with VALUE, reporting errors.
The function is called with no arguments when it accepts none, else with
\(NAME VALUE)."
  (when-let* ((fn (plist-get entry :on-change)))
    (when (functionp fn)
      (condition-case err
          (if (zerop (cdr (func-arity fn)))
              (funcall fn)
            (funcall fn name value))
        (error
         (display-warning
          'config
          (format "on-change for %s failed: %s" name (error-message-string err))
          :error))))))

;;; Registration -------------------------------------------------------------

(defun config-register (name &rest plist)
  "Register configuration item NAME with metadata PLIST.

Recognised keys:
  :type      one of `boolean' `integer' `number' `string' `function'
             `face' `sexp' or a `(choice ...)' form (UI hint only).
  :group     a symbol grouping related items in the board.
  :var       backing variable symbol; defaults to NAME.  Pass `:var nil'
             for a registry-only value with no backing variable.
  :doc       one-line description.
  :choices   list of allowed values (for `function'/choice editing).
  :on-change function run after a successful change; called with
             (NAME VALUE), or with no args if it takes none.
  :set       custom setter, called with (NAME VALUE) instead of setting `:var'.
  :get       custom getter, called with (NAME) instead of reading `:var'.
  :store-file persisted store file for this item; defaults to the key's
             previously loaded store, then an unambiguous inferred store for
             the entry's group, then `config-store-file'.
  :source    origin label; defaults to the loading file.

There is no `:default' key.  Initial values live in `config-store-file',
not in Lisp code.  Re-registering NAME replaces the previous entry and
immediately applies any pre-loaded store override."
  (let ((entry (copy-sequence plist)))
    (setq entry (plist-put entry :name name))
    (setq entry (plist-put entry :kind 'variable))
    (unless (plist-member plist :var)
      (setq entry (plist-put entry :var name)))
    (unless (plist-member plist :source)
      (setq entry (plist-put entry :source (config--caller-source))))
    (puthash name entry config--registry)
    (when-let* ((store-file (config--entry-store-file entry)))
      (config--load-store-file store-file))
    (config--learn-entry-store-file name entry)
    ;; Apply any pre-loaded store override immediately so modules get their
    ;; configured value as soon as they register, without waiting for
    ;; `after-init-hook'.
    (when-let* ((cell (gethash name config--override-index)))
      (condition-case nil
          (if after-init-time
              ;; Registered by a module loaded lazily after startup:
              ;; `config-apply-store' will not run again, so apply the value
              ;; AND fire `:on-change' now.
              (config-set name (cdr cell) 'no-persist 'quiet)
            ;; During startup write only the value; the `:on-change' callback
            ;; is deferred to the single `config-apply-store' pass so a callback
            ;; shared by many keys (e.g. `my/font-reset-all') runs once per
            ;; startup instead of once per registration.
            (config--set-value entry name (cdr cell)))
        (error nil)))
    name))

(defmacro config-defvar (name default &optional docstring &rest plist)
  "Define variable NAME with DEFAULT, DOCSTRING, and register it with PLIST.
DEFAULT is the Lisp-level initial value only; the canonical runtime value
comes from `config-store-file'.  DOCSTRING is also used as the registry
`:doc' value unless PLIST supplies one explicitly."
  (declare (indent 2) (doc-string 3))
  (let ((doc (and (stringp docstring) docstring))
        (metadata (if (stringp docstring)
                      plist
                    (append (list docstring) plist))))
    (when (and doc (not (plist-member metadata :doc)))
      (setq metadata (append metadata (list :doc doc))))
    `(progn
       (defvar ,name ,default ,@(and doc (list doc)))
       (config-register ',name ,@metadata))))

(defun config-register-hook (hook &rest plist)
  "Register HOOK for membership management with metadata PLIST.

Keys: :candidates (alist of (FUNCTION . LABEL)), :group, :doc, :store-file,
:source.
Pre-loaded store overrides for this hook are applied immediately."
  (let ((entry (copy-sequence plist)))
    (setq entry (plist-put entry :name hook))
    (setq entry (plist-put entry :kind 'hook))
    (setq entry (plist-put entry :hook hook))
    (unless (plist-member plist :source)
      (setq entry (plist-put entry :source (config--caller-source))))
    (puthash hook entry config--registry)
    (when-let* ((store-file (config--entry-store-file entry)))
      (config--load-store-file store-file))
    ;; Apply pre-loaded hook overrides immediately.
    (dolist (cell config--overrides)
      (when (and (consp (car cell))
                 (eq (car (car cell)) :hook)
                 (eq (nth 1 (car cell)) hook))
        (condition-case nil
            (config--hook-apply hook (nth 2 (car cell)) (cdr cell))
          (error nil))))
    hook))

(defun config-register-file (name &rest plist)
  "Register an `etc/' config file item NAME with metadata PLIST.

Keys: :path (the file), :example (template), :loader (thunk that loads
the file live), :on-change (update script run after a reload), :group,
:doc, :source."
  (let ((entry (copy-sequence plist)))
    (setq entry (plist-put entry :name name))
    (setq entry (plist-put entry :kind 'file))
    (unless (plist-member plist :source)
      (setq entry (plist-put entry :source (config--caller-source))))
    (puthash name entry config--registry)
    name))

;;; Reading ------------------------------------------------------------------

(defun config-get (name)
  "Return the current value of configuration item NAME.

For `:get' items the getter is called; for variable items the backing
variable is read; otherwise the stored value or `:default' is returned.
An unknown NAME warns once and returns nil."
  (let ((entry (config--entry name)))
    (cond
     ((null entry)
      (unless (gethash name config--unknown-warned)
        (puthash name t config--unknown-warned)
        (display-warning
         'config (format "config-get: unknown item %s" name) :warning))
      nil)
     ((plist-get entry :get) (funcall (plist-get entry :get) name))
     ((eq (plist-get entry :kind) 'hook)
      (and (boundp (plist-get entry :hook))
           (symbol-value (plist-get entry :hook))))
     (t (let ((var (config--backing-var entry name)))
          (cond
           ((and var (boundp var)) (symbol-value var))
           ((plist-member entry :value) (plist-get entry :value))
           (t nil)))))))

;;; Writing ------------------------------------------------------------------

(defun config--record-override (key value)
  "Record KEY=VALUE in `config--overrides' and persist its owning store."
  (config--put-override key value)
  (let ((file (config--store-file-for-key key)))
    (puthash key file config--override-store-files)
    (config--persist (list (expand-file-name file)))))

(defun config--drop-override (key)
  "Remove KEY from `config--overrides' and persist the store."
  (let ((file (gethash key config--override-store-files)))
    (setq config--overrides
          (assoc-delete-all key config--overrides #'equal))
    (remhash key config--override-index)
    (remhash key config--override-store-files)
    (config--persist (and file (list (expand-file-name file))))))

(defun config--set-value (entry name value)
  "Write VALUE for ENTRY named NAME without running `:on-change' or persisting.
Applies the custom `:set', the backing variable, or the registry `:value'."
  (cond
   ((plist-get entry :set) (funcall (plist-get entry :set) name value))
   (t (let ((var (config--backing-var entry name)))
        (if var
            (set var value)
          (puthash name (plist-put entry :value value) config--registry))))))

(defun config-set (name value &optional no-persist quiet)
  "Set configuration item NAME to VALUE, applying it live.

Runs the item's `:on-change'.  Unless NO-PERSIST, records the change in
the store.  When QUIET, does not message."
  (let ((entry (config--entry name)))
    (unless entry
      (user-error "config-set: unknown item %s" name))
    (when (eq (plist-get entry :kind) 'hook)
      (user-error "Use `config-hook-set' for hook item %s" name))
    (let ((type (plist-get entry :type)))
      (unless (config--valid-type-p type value)
        (user-error "config-set: %S is not a valid %s for %s" value type name)))
    (config--set-value entry name value)
    (config--run-on-change entry name value)
    (unless no-persist
      (config--record-override name value))
    (unless quiet
      (message "config: %s = %S" name value))
    value))

(defun config-reset (name)
  "Remove the stored override for NAME.
The live variable keeps its current value; on next restart it will get
whatever `etc/config-store.el' contains (which will no longer have NAME)."
  (interactive (list (config--read-item)))
  (unless (config--entry name)
    (user-error "config-reset: unknown item %s" name))
  (config--drop-override name)
  (message "config: removed stored override for %s" name))

;;; Hook membership ----------------------------------------------------------

(defun config--hook-apply (hook fn enable)
  "Add or remove FN on HOOK according to ENABLE (live only)."
  (if enable (add-hook hook fn) (remove-hook hook fn)))

(defun config-hook-member-p (hook fn)
  "Return non-nil when FN currently runs on HOOK."
  (and (boundp hook) (memq fn (symbol-value hook)) t))

(defun config-hook-set (hook fn enable &optional no-persist)
  "Set membership of FN on HOOK to ENABLE, applying it live.
Unless NO-PERSIST, records the toggle in the store."
  (config--hook-apply hook fn enable)
  (unless no-persist
    (config--record-override (list :hook hook fn) (and enable t)))
  (and enable t))

(defun config-hook-toggle (hook fn)
  "Toggle membership of FN on HOOK and persist the result."
  (config-hook-set hook fn (not (config-hook-member-p hook fn))))

;;; File items ---------------------------------------------------------------

(defun config-file-reload (name)
  "Run the loader and update script for file item NAME."
  (let ((entry (config--entry name)))
    (unless (eq (plist-get entry :kind) 'file)
      (user-error "config-file-reload: %s is not a file item" name))
    (when-let* ((loader (plist-get entry :loader)))
      (funcall loader))
    (when-let* ((fn (plist-get entry :on-change)))
      (funcall fn))
    (message "config: reloaded %s" name)))

(defun config-file-open (name)
  "Open file item NAME for editing, seeding from its example when missing."
  (let* ((entry (config--entry name))
         (path (plist-get entry :path))
         (example (plist-get entry :example)))
    (unless path (user-error "config-file-open: %s has no :path" name))
    (when (and (not (file-exists-p path)) example (file-exists-p example)
               (y-or-n-p (format "%s does not exist; seed from example? " name)))
      (copy-file example path))
    (find-file path)))

(defun config-file-reset (name)
  "Overwrite file item NAME from its example, then reload."
  (let* ((entry (config--entry name))
         (path (plist-get entry :path))
         (example (plist-get entry :example)))
    (unless (and example (file-exists-p example))
      (user-error "config-file-reset: %s has no example" name))
    (when (yes-or-no-p (format "Overwrite %s from example? " path))
      (copy-file example path t)
      (config-file-reload name))))

;;; Introspection ------------------------------------------------------------

(defun config-list (&optional group kind)
  "Return registered entries, optionally filtered by GROUP and/or KIND."
  (let (items)
    (maphash
     (lambda (_name entry)
       (when (and (or (null group) (eq group (plist-get entry :group)))
                  (or (null kind) (eq kind (plist-get entry :kind))))
         (push entry items)))
     config--registry)
    (nreverse items)))

(defun config--read-item ()
  "Read a registered item NAME with completion."
  (let (names)
    (maphash (lambda (name _entry) (push name names)) config--registry)
    (intern (completing-read "Config item: "
                             (mapcar #'symbol-name (nreverse names))
                             nil t))))

;;; Integrity ----------------------------------------------------------------

(defun config--index-drift-p ()
  "Return non-nil when `config--override-index' is out of sync with the alist."
  (or (/= (hash-table-count config--override-index) (length config--overrides))
      (catch 'drift
        (dolist (cell config--overrides)
          (unless (eq cell (gethash (car cell) config--override-index))
            (throw 'drift t)))
        nil)))

(defun config--integrity-issues ()
  "Return a list of human-readable integrity problems, empty when healthy.
Checks the override index/alist lockstep, that every override resolves to a
store file, and that every configured store file still parses."
  (let (issues)
    ;; Index <-> alist lockstep.
    (when (/= (hash-table-count config--override-index) (length config--overrides))
      (push (format "override index size %d != alist length %d"
                    (hash-table-count config--override-index)
                    (length config--overrides))
            issues))
    (dolist (cell config--overrides)
      (unless (eq cell (gethash (car cell) config--override-index))
        (push (format "index cell for %S is stale or missing" (car cell)) issues)))
    (maphash
     (lambda (key _cell)
       (unless (assoc key config--overrides #'equal)
         (push (format "index key %S has no alist entry" key) issues)))
     config--override-index)
    ;; Every override must resolve to a store file.
    (dolist (cell config--overrides)
      (unless (config--store-file-for-key (car cell))
        (push (format "override %S resolves to no store file" (car cell)) issues)))
    ;; Every configured store file must still parse.
    (dolist (file (config--configured-store-files))
      (when (file-readable-p file)
        (condition-case err
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (let ((done nil))
                (while (not done)
                  (condition-case nil
                      (read (current-buffer))
                    (end-of-file (setq done t))))))
          (error
           (push (format "store file %s fails to parse: %s"
                         (abbreviate-file-name file) (error-message-string err))
                 issues)))))
    (nreverse issues)))

(defun config-check ()
  "Verify the config registry's invariants, repairing index drift in place.
Reports remaining problems in a `*Config Check*' buffer, or confirms health."
  (interactive)
  (let ((repaired (when (config--index-drift-p)
                    (config--reindex)
                    (not (config--index-drift-p)))))
    (let ((issues (config--integrity-issues)))
      (cond
       (issues
        (with-current-buffer (get-buffer-create "*Config Check*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (when repaired
              (insert "Repaired override index drift.\n\n"))
            (insert (format "config-check found %d issue(s):\n\n" (length issues)))
            (dolist (issue issues) (insert "  - " issue "\n")))
          (goto-char (point-min))
          (special-mode)
          (display-buffer (current-buffer)))
        (message "config-check: %d issue(s)%s"
                 (length issues) (if repaired " (index repaired)" "")))
       (repaired
        (message "config-check: repaired index drift; %d items, %d overrides OK"
                 (hash-table-count config--registry) (length config--overrides)))
       (t
        (message "config-check: OK (%d items, %d overrides)"
                 (hash-table-count config--registry) (length config--overrides)))))))

;;; Persistence --------------------------------------------------------------

(defun config-store-set (overrides)
  "Merge OVERRIDES into `config--overrides' (called by store files).
Later-loaded stores win when the same key appears in more than one file."
  (config--merge-store-overrides overrides config--current-store-file))

(defun config--put-override (key value)
  "Set KEY to VALUE in `config--overrides', preserving insertion order."
  (if-let* ((cell (gethash key config--override-index)))
      (setcdr cell value)
    (let ((cell (cons key value)))
      (setq config--overrides (nconc config--overrides (list cell)))
      (puthash key cell config--override-index))))

(defun config--reindex ()
  "Rebuild `config--override-index' from `config--overrides'.
Restores the lockstep invariant if the alist and index ever drift apart."
  (clrhash config--override-index)
  (dolist (cell config--overrides)
    (puthash (car cell) cell config--override-index)))

(defun config--clear-overrides ()
  "Reset all in-memory override state in lockstep.
Clears the ordered alist, its O(1) index, and the key->store-file map together
so no caller can leave a stale index referencing detached cons cells."
  (setq config--overrides nil)
  (clrhash config--override-index)
  (clrhash config--override-store-files))

(defun config--merge-store-overrides (overrides &optional file)
  "Merge OVERRIDES into memory, recording FILE as their owner."
  (unless (listp overrides)
    (user-error "config store overrides must be a list, got %S" overrides))
  (dolist (cell overrides)
    (unless (consp cell)
      (user-error "config store entry must be a cons cell, got %S" cell))
    (config--put-override (car cell) (cdr cell))
    (when file
      (puthash (car cell)
               file
               config--override-store-files))))

(defun config--store-form-overrides (form file)
  "Return the override list encoded by FORM from FILE, or nil."
  (cond
   ((and (consp form)
         (eq (car form) 'config-store-set)
         (= (length form) 2)
         (consp (cadr form))
         (eq (car (cadr form)) 'quote))
    (cadr (cadr form)))
   (t
    (display-warning
     'config
     (format "Ignoring unsupported form in %s: %S" file form)
     :warning)
    nil)))

(defun config--read-store-file (file)
  "Read config store FILE without evaluating arbitrary Lisp."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((done nil))
      (while (not done)
        (condition-case err
            (let ((form (read (current-buffer))))
              (when-let* ((overrides (config--store-form-overrides form file)))
                (config--merge-store-overrides overrides file)))
          (end-of-file
           (setq done t))
          (error
           (display-warning
            'config
            (format "Failed to read %s: %s" file (error-message-string err))
            :error)
           (setq done t)))))))

(defun config--store-file-for-key (key)
  "Return the store file that owns persisted override KEY."
  (let ((entry
         (cond
          ((symbolp key) (config--entry key))
          ((and (consp key) (eq (car key) :hook))
           (config--entry (nth 1 key))))))
    (or (and entry (config--entry-store-file entry))
        (gethash key config--override-store-files)
        (and entry (config--group-default-store-file (plist-get entry :group)))
        (config--primary-store-file))))

(defun config--store-files ()
  "Return all known store files that should be persisted."
  (let ((files (config--configured-store-files)))
    (maphash
     (lambda (_name entry)
       (push (config--entry-store-file entry) files))
     config--registry)
    (maphash
     (lambda (file _loaded)
       (push file files))
     config--loaded-store-files)
    (maphash
     (lambda (_key file)
       (push file files))
     config--override-store-files)
    (maphash
     (lambda (_group group-files)
       (maphash (lambda (store-file _present)
                  (push store-file files))
                group-files))
     config--group-store-file-sets)
    (delete-dups (delq nil files))))

(defun config--store-file-content (file overrides)
  "Return the generated content for FILE containing OVERRIDES."
  (with-temp-buffer
    (insert ";;; "
            (file-name-nondirectory file)
            " --- generated by config.el -*- lexical-binding: t; -*-\n")
    (insert ";; Auto-generated.  Do not edit by hand; use `config-set' or the board.\n\n")
    (insert "(config-store-set\n '")
    (let ((print-level nil) (print-length nil))
      (pp overrides (current-buffer)))
    (insert ")\n")
    (buffer-string)))

(defun config--file-string (file)
  "Return FILE contents as a string, or nil when FILE is unreadable."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun config--write-string-atomically (file content)
  "Write CONTENT to FILE via a same-directory temporary file and rename."
  (let* ((directory (file-name-directory file))
         (temporary (make-temp-file
                     (expand-file-name
                      (concat "." (file-name-nondirectory file) ".")
                      directory))))
    (condition-case err
        (progn
          (with-temp-file temporary
            (insert content))
          (rename-file temporary file t))
      (error
       (when (file-exists-p temporary)
         (ignore-errors (delete-file temporary)))
       (signal (car err) (cdr err))))))

(defun config--write-store-file (file overrides)
  "Write OVERRIDES to FILE unless the generated content is unchanged."
  (make-directory (file-name-directory file) t)
  (let ((content (config--store-file-content file overrides)))
    (unless (equal content (config--file-string file))
      (config--write-string-atomically file content))))

(defun config--persist (&optional files)
  "Write `config--overrides' to their owning store files.
With FILES (a list of expanded store-file paths), only those files are
re-rendered and written; otherwise every known store file is persisted.
Restricting to the changed file keeps a single board edit from re-rendering
and re-reading all ~10 store files."
  (condition-case err
      (let ((buckets (make-hash-table :test 'equal))
            (targets (if files (delete-dups (copy-sequence files))
                       (config--store-files))))
        ;; Only bucket overrides destined for a target file.  Prefer the
        ;; authoritative key->file map (filled at load/record time) over
        ;; `config--store-file-for-key', which re-runs group inference; this
        ;; keeps a single `config-set' from doing that work for every key.
        (dolist (cell config--overrides)
          (let ((file (expand-file-name
                       (or (gethash (car cell) config--override-store-files)
                           (config--store-file-for-key (car cell))))))
            (when (or (null files) (member file targets))
              (push cell (gethash file buckets)))))
        (dolist (file targets)
          (config--write-store-file file (nreverse (gethash file buckets)))))
    (error
     (display-warning
      'config (format "Failed to persist config stores: %s"
                      (error-message-string err))
      :error))))

(defun config--load-store-file (file)
  "Load store FILE once, merging it into `config--overrides'."
  (when-let* ((file (config--expand-store-file file)))
    (unless (gethash file config--loaded-store-files)
      (puthash file t config--loaded-store-files)
      (when (file-readable-p file)
        (condition-case err
            (config--read-store-file file)
          (error
           (display-warning
            'config (format "Failed to load %s: %s" file
                            (error-message-string err))
            :error)))))))

(defun config-apply-store ()
  "Load config store files and apply every override live.
Each override is applied independently so one failure cannot abort the rest.
Value writes happen first; then each `:on-change' callback runs once: a
zero-argument callback shared by several keys (e.g. `my/font-reset-all') is
invoked a single time, while callbacks taking arguments run per entry."
  (config--load-configured-store-files)
  (let ((shared nil))
    (dolist (cell config--overrides)
      (condition-case err
          (let ((key (car cell)) (val (cdr cell)))
            (cond
             ((and (consp key) (eq (car key) :hook))
              (config--hook-apply (nth 1 key) (nth 2 key) val))
             ((symbolp key)
              (when-let* ((entry (config--entry key)))
                (config--set-value entry key val)
                (when-let* ((fn (plist-get entry :on-change)))
                  (when (functionp fn)
                    (if (zerop (cdr (func-arity fn)))
                        (cl-pushnew fn shared :test #'eq)
                      (config--run-on-change entry key val))))))))
        (error
         (display-warning
          'config (format "Failed to apply override %S: %s"
                          (car cell) (error-message-string err))
          :error))))
    ;; Run shared zero-argument callbacks exactly once, after all values set.
    (dolist (fn (nreverse shared))
      (condition-case err
          (funcall fn)
        (error
         (display-warning
          'config (format "on-change %S failed: %s" fn (error-message-string err))
          :error))))))

(defun config-refresh-store-files ()
  "Reload every store file from disk and re-apply, mirroring on-disk state.
Discards in-memory overrides first, then rediscovers and re-reads the stores,
so edits, newly added keys and removals are all reflected.  Persisted values
are safe because `config-set' always writes through to disk; nothing lives
only in memory."
  (interactive)
  (config--clear-overrides)
  (clrhash config--loaded-store-files)
  (setq config--configured-store-files-cache nil)
  (config-apply-store)
  (message "config: refreshed store files"))

;; Final pass after all modules have registered and :on-change fns are defined.
(add-hook 'after-init-hook #'config-apply-store)

;; Bootstrap: load stores NOW so every subsequent (config-register ...) call
;; finds its override in config--overrides and applies it immediately.
;; This replaces the need for scattered `setq' calls — values live in etc/.
(config--load-configured-store-files)

(provide 'config)
;;; config.el ends here
