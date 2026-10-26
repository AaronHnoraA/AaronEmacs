;;; aaron-modules.el --- Module registry, declaration and loader -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The configuration is a list of modules, each identified by a key
;; (CATEGORY . NAME), e.g. (:ui . init-ui).  `aaron!' declares them, in load
;; order, grouped by category; `aaron-modules-load' loads them; `modulep!'
;; asks whether one (and its flags) is enabled; the registry records each
;; module's state, load time and errors for the module board and doctor.
;;
;; A module is one of two kinds:
;;
;;   directory  lisp/CATEGORY/NAME/ holding init.el (early), config.el,
;;              autoload/*.el (scanned into lazy autoloads), doctor.el
;;              (self-checks), .aaronmodule (metadata: flags, depth) and
;;              README.md.  This is the target layout.
;;   feature    a legacy single file providing feature NAME (`init-foo'),
;;              loaded with `require'.  Modules move from this kind to the
;;              directory kind one by one.
;;
;; Load order follows Doom: every init.el first (core, os and remote before
;; the rest), then every config.el or feature, each phase bracketed by
;; `aaron-before/after-modules-init/config-hook'.  Within a phase modules
;; load by depth, then declaration order.  The core, os and remote
;; categories carry negative depths, so the Remote framework is always the
;; layer every other module stands on.  Anything loaded after startup (a
;; deferred module, an autoload-only module) goes through
;; `remote-require-on-client', so a remote current buffer never sends the
;; load to its target.
;;
;; Adapted from Doom Emacs (MIT, Henrik Lissner):
;;   core/lisp/doom-modules.el and core/lisp/doom-profiles.el
;;   (`doom-profile--generate-module-loader') @ 59cdaa3
;; Doom resolves `doom!' at `doom sync' time and writes a static loader;
;; this config has no sync step, so `aaron!' is evaluated at startup.

;;; Code:

(require 'aaron-lib)

(declare-function remote-require-on-client "remote-core" (feature &optional filename noerror))

;;; Variables

(defvar aaron-modules (make-hash-table :test 'equal)
  "Declared modules: (CATEGORY . NAME) to `aaron-module'.")

(defvar aaron-module-load-path
  (list (expand-file-name "lisp" user-emacs-directory))
  "Directories holding CATEGORY/NAME/ module directories.")

(defvar aaron-module-init-file "init.el"
  "File of a directory module loaded in the init phase.")

(defvar aaron-module-config-file "config.el"
  "File of a directory module loaded in the config phase.")

(defvar aaron-module-doctor-file "doctor.el"
  "File of a directory module holding its self-checks.")

(defvar aaron-module-manifest-file ".aaronmodule"
  "Metadata file of a directory module: a list of (KEY . VALUE).")

(defvar aaron-module-autoload-dir "autoload"
  "Directory of a directory module whose `;;;###autoload' cookies are scanned.")

(defvar aaron-module-autoloads-file
  (expand-file-name "var/aaron/module-autoloads.el" user-emacs-directory)
  "Generated autoloads of every directory module's autoload files.")

(defvar aaron-module-category-depths
  '((:core . -110) (:os . -105) (:remote . -100))
  "Default depth per category.
Negative depths load in the pre-init and first config positions.  The
Remote framework sits here so every other module can rely on it.")

(defcustom aaron-before-modules-init-hook nil
  "Hook run before the init phase of non-foundation modules."
  :type 'hook :group 'aaron)

(defcustom aaron-after-modules-init-hook nil
  "Hook run after every module's init file has loaded."
  :type 'hook :group 'aaron)

(defcustom aaron-before-modules-config-hook nil
  "Hook run before the config phase."
  :type 'hook :group 'aaron)

(defcustom aaron-after-modules-config-hook nil
  "Hook run after every module's config has loaded."
  :type 'hook :group 'aaron)

(defvar aaron-module-context nil
  "The `aaron-module' whose files are loading.  Never set directly.")

(defvar aaron-module-load-failures nil
  "Modules that failed to load, as (FEATURE-OR-KEY . ERROR-MESSAGE) pairs.")

(defvar aaron-module-optional-modules nil
  "Optional modules, in declaration order.")

(defvar aaron-module-deferred-triggers nil
  "Deferred modules as (FEATURE . TRIGGER-FEATURES) pairs.")

(defvar aaron-modules--starting nil
  "Non-nil while `aaron-modules-load' runs.")

(defvar aaron-modules-init-time nil
  "Seconds `aaron-modules-load' took.")

;;; The module record

(cl-defstruct (aaron-module (:constructor aaron-module--create)
                            (:copier nil))
  "A declared module."
  (index 0 :read-only t)
  group name
  path                                  ; directory of a directory module
  feature                               ; feature of a feature module
  flags
  (depth '(0 . 0))
  optional                              ; failures are recorded, not raised
  after                                 ; load after any of these features
  autoload                              ; commands to autoload instead of loading
  doctor                                ; the module's own interactive doctor
  state                                 ; nil, loaded, failed, waiting, autoload
  error
  load-time)

(defun aaron-module-key (module)
  "Return MODULE's key, (CATEGORY . NAME)."
  (cons (aaron-module-group module) (aaron-module-name module)))

(defun aaron-module-get (key)
  "Return the module declared under KEY, or nil."
  (gethash key aaron-modules))

(defun aaron-module-directory-p (module)
  "Return non-nil when MODULE is a directory module."
  (and (aaron-module-path module) t))

(defun aaron-module--has-flag-p (flags wanted)
  "Return t when FLAGS satisfy every flag in WANTED.
A `-flag' in WANTED requires `+flag' to be absent."
  (declare (pure t) (side-effect-free error-free))
  (cl-loop with flags = (ensure-list flags)
           for flag in (ensure-list wanted)
           for name = (symbol-name flag)
           if (if (eq ?- (aref name 0))
                  (memq (intern (concat "+" (substring name 1))) flags)
                (not (memq flag flags)))
           return nil
           finally return t))

(defun aaron-module--fold-flags (flags)
  "Collapse FLAGS in order: a later `-x' cancels `+x' and vice versa."
  (declare (pure t) (side-effect-free error-free))
  (let (out)
    (dolist (flag flags)
      (let* ((name (symbol-name flag))
             (opposite (intern-soft (concat (if (eq ?- (aref name 0)) "+" "-")
                                            (substring name 1)))))
        (when opposite (setq out (delq opposite out)))
        (cl-pushnew flag out :test #'eq)))
    (nreverse out)))

(defun aaron-module-active-p (group name &optional flags)
  "Return t when module GROUP NAME is declared and has all FLAGS."
  (when-let* ((module (get group name)))
    (or (null flags)
        (aaron-module--has-flag-p (aaron-module-flags module) flags))))

;;; Paths

(defun aaron-module-locate-path (key &optional file)
  "Return the directory of module KEY (or FILE in it), if it exists."
  (let ((file-name-handler-alist nil)
        (rel (file-name-concat (substring (symbol-name (car key)) 1)
                               (symbol-name (cdr key)))))
    (cl-loop for root in aaron-module-load-path
             for dir = (expand-file-name rel root)
             if (file-directory-p dir)
             return (if file
                        (let ((path (expand-file-name file dir)))
                          (and (file-exists-p path) path))
                      (file-name-as-directory dir)))))

(defun aaron-module-expand-path (module &optional file)
  "Return MODULE's directory, or FILE in it when that file exists."
  (when-let* ((dir (aaron-module-path module)))
    (if file
        (let ((path (expand-file-name file dir)))
          (and (file-exists-p path) path))
      dir)))

(defun aaron-module-source-files (module)
  "Return the Emacs Lisp source files that make up MODULE.
A directory module's files and the globs its manifest lists under
`files', or a feature module's file, plus every file under
`aaron-module-load-path' that first loaded while MODULE loaded."
  (let ((file-name-handler-alist nil))
    (delete-dups
     (append
      (if (aaron-module-directory-p module)
          (append
           (directory-files-recursively (aaron-module-path module) "\\.el\\'")
           (cl-loop for pattern in (alist-get 'files (aaron-module--read-manifest
                                                     (aaron-module-path module)))
                    append (file-expand-wildcards
                            (expand-file-name pattern (aaron-module-path module)) t)))
        (when-let* ((lib (locate-library (symbol-name (aaron-module-feature module))))
                    (src (concat (file-name-sans-extension lib) ".el")))
          (and (file-exists-p src) (list src))))
      (seq-filter #'file-exists-p
                  (aaron-module-ledger (aaron-module-key module) 'files))))))

(defvar aaron-module--path-cache (make-hash-table :test 'equal)
  "File name to module key, for `aaron-module-from-path'.")

(defun aaron-module-from-path (path)
  "Return the key of the declared module that PATH belongs to, or nil."
  (let ((file-name-handler-alist nil))
    (when path
      (setq path (expand-file-name path))
      (or (gethash path aaron-module--path-cache)
          (puthash
           path
           (or
            ;; Directory module: ROOT/CATEGORY/NAME/...
            (cl-loop for root in aaron-module-load-path
                     for root* = (file-name-as-directory (expand-file-name root))
                     if (string-prefix-p root* path)
                     return
                     (let ((parts (split-string (substring path (length root*)) "/" t)))
                       (when (>= (length parts) 3)
                         (let ((key (cons (intern (concat ":" (car parts)))
                                          (intern (cadr parts)))))
                           (and (aaron-module-get key) key)))))
            ;; Feature module: the file provides the module's feature.
            (let ((stem (file-name-sans-extension path)))
              (cl-loop for module being the hash-values of aaron-modules
                       for feature = (aaron-module-feature module)
                       for lib = (and feature (locate-library (symbol-name feature)))
                       if (and lib (equal (file-name-sans-extension lib) stem))
                       return (aaron-module-key module))))
           aaron-module--path-cache)))))

(defun aaron-module--current-key ()
  "Return the key of the module whose code is being expanded, or nil."
  (if aaron-module-context
      (aaron-module-key aaron-module-context)
    (aaron-module-from-path (or (macroexp-file-name) load-file-name
                                (buffer-file-name (buffer-base-buffer))))))

(defun aaron-module--read-manifest (dir)
  "Return the alist in DIR's `aaron-module-manifest-file', or nil."
  (let ((file (expand-file-name aaron-module-manifest-file dir)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

;;; Declaring modules

(defun aaron-module--depth (group plist manifest)
  "Return the (INIT . CONFIG) depth from PLIST, MANIFEST or GROUP's default."
  (let ((depth (or (plist-get plist :depth)
                   (alist-get 'depth manifest)
                   (alist-get group aaron-module-category-depths)
                   0)))
    (if (consp depth) depth (cons depth depth))))

(defun aaron-module--path-feature (key file)
  "Return the feature of FILE in directory module KEY: its path under lisp/.
A module file provides CATEGORY/NAME/FILE, e.g. `tools/lsp/config', so
`require' finds it through lisp/ on `load-path'."
  (intern (format "%s/%s/%s" (substring (symbol-name (car key)) 1) (cdr key) file)))

(defun aaron-module--put (key flags plist)
  "Declare module KEY with FLAGS and properties PLIST.
Declaring a key again folds the new flags into its existing ones."
  (if-let* ((module (aaron-module-get key)))
      (setf (aaron-module-flags module)
            (aaron-module--fold-flags (append (aaron-module-flags module) flags)))
    (let* ((path (aaron-module-locate-path key))
           (manifest (and path (aaron-module--read-manifest path)))
           (module
            (aaron-module--create
             :index (hash-table-count aaron-modules)
             :group (car key) :name (cdr key)
             :path path
             :feature (cond ((plist-get plist :feature))
                            ((and path (file-exists-p
                                        (expand-file-name aaron-module-config-file path)))
                             (aaron-module--path-feature key "config"))
                            (path nil)
                            (t (cdr key)))
             :flags (aaron-module--fold-flags flags)
             :depth (aaron-module--depth (car key) plist manifest)
             :optional (or (plist-get plist :optional) (plist-get plist :after))
             :after (ensure-list (plist-get plist :after))
             :autoload (ensure-list (plist-get plist :autoload))
             :doctor (plist-get plist :doctor))))
      (puthash key module aaron-modules)
      ;; `modulep!' reads this symbol-plist cache, as Doom's does.
      (put (car key) (cdr key) module)
      (clrhash aaron-module--path-cache)
      module)))

(defun aaron-module--parse-entry (entry)
  "Split ENTRY's tail into (FLAGS . PLIST)."
  (let ((rest (cdr-safe entry)) flags plist)
    (while rest
      (let ((x (pop rest)))
        (if (keywordp x)
            (setq plist (plist-put plist x (pop rest)))
          (push x flags))))
    (cons (nreverse flags) plist)))

(defun aaron-modules--declare (mplist)
  "Declare the modules in MPLIST; see `aaron!'."
  (let ((mplist (copy-sequence mplist))
        group)
    (while mplist
      (let ((m (pop mplist)))
        (cond
         ((keywordp m) (setq group m))
         ((null group) (error "aaron!: module %S has no category" m))
         ((and (consp m) (keywordp (car m)))
          (pcase (car m)
            (:cond (cl-loop for (test . mods) in (cdr m)
                            if (eval test t)
                            return (setq mplist (append mods mplist))))
            (:if (setq mplist (if (eval (cadr m) t)
                                  (cons (caddr m) mplist)
                                (append (cdddr m) mplist))))
            ((and test (or :when :unless))
             (when (xor (eval (cadr m) t) (eq test :unless))
               (setq mplist (append (cddr m) mplist))))
            (other (error "aaron!: unknown form %S" other))))
         (t
          (let* ((name (if (consp m) (car m) m))
                 (parsed (aaron-module--parse-entry m)))
            (aaron-module--put (cons group name) (car parsed) (cdr parsed)))))))))

(defmacro aaron! (&rest modules)
  "Declare the configuration's MODULES, in load order.

MODULES is a list of category keywords, each followed by its modules:

  (aaron! :core init-base
          :ui   init-ui (init-tabbar +flag)
          :tools (init-zoxide :optional t))

An entry is NAME or (NAME FLAGS... PROPERTIES...).  NAME is a directory
module under lisp/CATEGORY/NAME/, or else a feature.  FLAGS are `+x'
or `-x' symbols read with `modulep!'.  PROPERTIES:

  :optional t      record a load failure instead of aborting startup
  :after FEATURES  load when any of FEATURES loads
  :autoload CMDS   only autoload these commands from the module
  :feature SYM     the feature to require, when it differs from NAME
  :doctor COMMAND  the module's own interactive doctor
  :depth N         load position, or (INIT . CONFIG)

Forms (:if COND MODULE ELSE...), (:when COND MODULES...),
\(:unless COND MODULES...) and (:cond (COND MODULES...)...) select
modules.  A category keyword may appear more than once."
  `(aaron-modules--declare ',modules))

(defmacro modulep! (group &optional name &rest flags)
  "Return t when module GROUP NAME is declared, with all FLAGS.

  (modulep! :tools init-lsp)
  (modulep! :tools lsp +remote)

Inside a module's own files GROUP and NAME may be omitted:

  (modulep! +remote)

A `-flag' requires that flag to be off."
  (if (keywordp group)
      `(aaron-module-active-p ,group ',name ',flags)
    (let ((flags (delq nil (cons group (cons name flags))))
          (key (aaron-module--current-key)))
      (unless key
        (error "modulep!: cannot tell which module %s belongs to"
               (or (macroexp-file-name) "this code")))
      `(aaron-module-active-p ,(car key) ',(cdr key) ',flags))))

(defun aaron-module-list (&optional init-order)
  "Return declared module keys sorted for loading.
With INIT-ORDER, sort by init depth instead of config depth."
  (sort (hash-table-keys aaron-modules)
        (lambda (a b)
          (let* ((ma (aaron-module-get a))
                 (mb (aaron-module-get b))
                 (da (funcall (if init-order #'car #'cdr) (aaron-module-depth ma)))
                 (db (funcall (if init-order #'car #'cdr) (aaron-module-depth mb))))
            (if (= da db)
                (< (aaron-module-index ma) (aaron-module-index mb))
              (< da db))))))

;;; Ledger
;;
;; While modules load, the ledger records what each one installs: hook
;; functions, advice, timers and key bindings.  A key that a second module
;; binds differently is a conflict: load order alone decides the winner.
;; `with-eval-after-load' bodies registered while a module loads keep that
;; module as their owner when they run during startup.  Instrumentation is
;; installed only for the duration of `aaron-modules-load' and of each
;; deferred module load; code that runs later is not recorded.

(defvar aaron-module--loading nil
  "The `aaron-module' whose code is running under the ledger.")

(defvar aaron-module-ledger (make-hash-table :test 'equal)
  "Module key to an alist of (KIND . ENTRIES) installed while it loaded.
KIND is `hooks' ((HOOK . FN)), `advice' ((SYMBOL HOW FN)), `timers'
\(TIMER), `keys' ((KEYMAP KEY DEF)) or `files' (sources under
`aaron-module-load-path' first loaded by this module).")

(defvar aaron-module-key-conflicts nil
  "Key bindings set differently by two modules, newest first.
Each is a plist (:keymap :key :old :old-module :new :new-module).")

(defvar aaron-module--key-owners (make-hash-table :test 'eq :weakness 'key)
  "KEYMAP to a table of KEY-DESCRIPTION to the key of the module that bound it.
Keyed by identity: a keymap's `equal' hash changes as it is modified.")

(defun aaron-module--ledger-add (kind entry)
  "Record ENTRY of KIND for the module that is loading."
  (when-let* ((module aaron-module--loading))
    (let* ((key (aaron-module-key module))
           (alist (gethash key aaron-module-ledger)))
      (push entry (alist-get kind alist))
      (puthash key alist aaron-module-ledger))))

(defun aaron-module-ledger (key kind)
  "Return the KIND entries recorded for module KEY, oldest first."
  (reverse (alist-get kind (gethash key aaron-module-ledger))))

(defvar aaron-module--file-owners (make-hash-table :test 'equal)
  "Source file to the key of the module that loaded it first.
Only files under `aaron-module-load-path' are recorded.")

(defun aaron-module--ledger-load-h (file)
  "Record FILE, just loaded, as part of the loading module."
  (when-let* ((module aaron-module--loading)
              (src (concat (file-name-sans-extension
                            (file-name-sans-extension file))
                           ".el"))
              ((cl-some (lambda (root)
                          (string-prefix-p (file-name-as-directory (expand-file-name root))
                                           src))
                        aaron-module-load-path))
              ((not (gethash src aaron-module--file-owners))))
    (puthash src (aaron-module-key module) aaron-module--file-owners)
    (aaron-module--ledger-add 'files src)))

(defun aaron-module--ledger-hook-a (hook function &optional _depth local)
  "Record FUNCTION added to HOOK, unless buffer-local."
  (unless local
    (aaron-module--ledger-add 'hooks (cons hook function))))

(defun aaron-module--ledger-advice-a (symbol how function &optional _props)
  "Record FUNCTION added as HOW advice to SYMBOL."
  (aaron-module--ledger-add 'advice (list symbol how function)))

(defun aaron-module--ledger-timer-a (timer)
  "Record TIMER, returned by a timer constructor."
  (aaron-module--ledger-add 'timers timer)
  timer)

(defvar aaron-module--in-after-load nil
  "Non-nil while a module's `eval-after-load' body runs under the ledger.")

(defun aaron-module--user-code-p ()
  "Return non-nil when the running code is the configuration's, not a package's.
That is a file under `aaron-module-load-path' being loaded, or a module's
`eval-after-load' body."
  (or aaron-module--in-after-load
      (and load-file-name
           (cl-some (lambda (root)
                      (string-prefix-p (file-name-as-directory (expand-file-name root))
                                       load-file-name))
                    aaron-module-load-path))))

(defun aaron-module--ledger-key-a (fn keymap key def &rest args)
  "Call FN to bind KEY to DEF in KEYMAP, recording owner and conflicts.
Only the configuration's own bindings count: a package's default binding
is owned by `:package', and replacing it is not a conflict."
  (if-let* ((module aaron-module--loading)
            ((keymapp keymap))
            (desc (ignore-errors (key-description key))))
      (let* ((owners (or (gethash keymap aaron-module--key-owners)
                         (puthash keymap (make-hash-table :test 'equal)
                                  aaron-module--key-owners)))
             (owner (gethash desc owners))
             (user (aaron-module--user-code-p))
             (mkey (if user (aaron-module-key module) :package))
             (old (and user owner (not (eq owner :package))
                       (not (equal owner mkey))
                       (let ((b (ignore-errors (lookup-key keymap key))))
                         (and b (not (numberp b)) b)))))
        (when (and old def (not (keymapp old)) (not (keymapp def))
                   (not (equal old def)))
          (push (list :keymap keymap :key desc :old old :old-module owner
                      :new def :new-module mkey)
                aaron-module-key-conflicts))
        (puthash desc mkey owners)
        (when user
          (aaron-module--ledger-add 'keys (list keymap desc def)))
        (apply fn keymap key def args))
    (apply fn keymap key def args)))

(defun aaron-module--ledger-after-load-a (args)
  "Make the `eval-after-load' body in ARGS run as the loading module."
  (let ((module aaron-module--loading)
        (form (nth 1 args)))
    (if (and module (functionp form))
        (list (car args)
              (lambda ()
                (let ((aaron-module--loading module)
                      (aaron-module--in-after-load t))
                  (funcall form))))
      args)))

(defvar aaron-module--instrument-depth 0
  "Nesting count of `aaron-module--with-ledger'.")

(defun aaron-module--instrument (on)
  "Install the ledger advice when ON, remove it otherwise."
  (if on
      (progn
        (add-hook 'after-load-functions #'aaron-module--ledger-load-h)
        (advice-add 'add-hook :after #'aaron-module--ledger-hook-a)
        (advice-add 'advice-add :after #'aaron-module--ledger-advice-a)
        (advice-add 'run-at-time :filter-return #'aaron-module--ledger-timer-a)
        (advice-add 'run-with-idle-timer :filter-return #'aaron-module--ledger-timer-a)
        (advice-add 'define-key :around #'aaron-module--ledger-key-a)
        (advice-add 'eval-after-load :filter-args #'aaron-module--ledger-after-load-a))
    (remove-hook 'after-load-functions #'aaron-module--ledger-load-h)
    (advice-remove 'add-hook #'aaron-module--ledger-hook-a)
    (advice-remove 'advice-add #'aaron-module--ledger-advice-a)
    (advice-remove 'run-at-time #'aaron-module--ledger-timer-a)
    (advice-remove 'run-with-idle-timer #'aaron-module--ledger-timer-a)
    (advice-remove 'define-key #'aaron-module--ledger-key-a)
    (advice-remove 'eval-after-load #'aaron-module--ledger-after-load-a)))

(defvar aaron-module-ledger-enabled t
  "Non-nil to record what modules install while they load.")

(defmacro aaron-module--with-ledger (&rest body)
  "Run BODY with the ledger instrumentation installed."
  (declare (indent 0))
  `(if (not aaron-module-ledger-enabled)
       (progn ,@body)
     (when (zerop aaron-module--instrument-depth)
       (aaron-module--instrument t))
     (cl-incf aaron-module--instrument-depth)
     (unwind-protect (progn ,@body)
       (cl-decf aaron-module--instrument-depth)
       (when (zerop aaron-module--instrument-depth)
         (aaron-module--instrument nil)))))

;;; Loading

(defun aaron-module--register-optional (feature)
  "Record optional FEATURE once, keeping declaration order."
  (unless (memq feature aaron-module-optional-modules)
    (setq aaron-module-optional-modules
          (append aaron-module-optional-modules (list feature)))))

(defun aaron-module--require (feature)
  "Require FEATURE as a module.
Once `aaron-modules-load' has finished it loads through
`remote-require-on-client', so a remote current buffer cannot send the
load to its target.  During `aaron-modules-load' it is plain `require':
the current buffer is local then, and the client
binding of `process-environment' and `exec-path' would discard what a
module sets at load time."
  (if aaron-modules--starting
      (require feature)
    (remote-require-on-client feature)))

(defun aaron-module--record (module state &optional err time)
  "Record MODULE's STATE, ERR and load TIME."
  (setf (aaron-module-state module) state
        (aaron-module-error module) err)
  (when time (setf (aaron-module-load-time module) time))
  (let ((id (or (aaron-module-feature module) (aaron-module-key module))))
    (if err
        (setf (alist-get id aaron-module-load-failures nil nil #'equal) err)
      (setq aaron-module-load-failures
            (cl-remove id aaron-module-load-failures :key #'car :test #'equal)))))

(defmacro aaron-module--timed (&rest body)
  "Run BODY and return (VALUE . SECONDS)."
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       (cons (progn ,@body) (- (float-time) ,start)))))

(defun aaron-module--load-file (module file)
  "Load FILE of directory MODULE with its context bound.
After `aaron-modules-load' the load runs from a client directory, as
`remote-require-on-client' does for features."
  (let ((aaron-module-context module)
        (default-directory (if aaron-modules--starting
                               default-directory
                             temporary-file-directory)))
    (load (file-name-sans-extension file) nil 'nomessage)))

(defun aaron-module--load (module thunk)
  "Run THUNK to load MODULE, recording state, time and failures.
An optional module's error is recorded and warned about; any other
module's error is raised after recording."
  (condition-case-unless-debug err
      (let ((result (let ((aaron-module--loading module))
                      (aaron-module--timed (funcall thunk)))))
        (aaron-module--record module 'loaded nil
                              (+ (or (aaron-module-load-time module) 0) (cdr result)))
        (car result))
    (error
     (let ((msg (error-message-string err)))
       (aaron-module--record module 'failed msg)
       (if (not (aaron-module-optional module))
           (signal (car err) (cdr err))
         (display-warning 'init-modules
                          (format "Failed to load %s: %s"
                                  (or (aaron-module-feature module)
                                      (aaron-module-key module))
                                  msg)
                          :error)
         nil)))))

(defun aaron-module--config-thunk (module)
  "Return a function that loads MODULE's config.
A directory module's config.el and a feature module are both loaded with
`require', so a module another module already required is not loaded
again."
  (let ((feature (aaron-module-feature module)))
    (lambda ()
      (when feature
        (let ((aaron-module-context module))
          (aaron-module--require feature))))))

(defun aaron-module--load-deferred (module)
  "Load deferred MODULE, unless it already has."
  (unless (eq (aaron-module-state module) 'loaded)
    (aaron-module--with-ledger
      (aaron-module--load module (aaron-module--config-thunk module)))))

(defun aaron-module--load-config (module)
  "Load MODULE in the config phase, honouring :autoload and :after."
  (let ((feature (aaron-module-feature module)))
    (when (aaron-module-optional module)
      (aaron-module--register-optional feature))
    (dolist (command (aaron-module-autoload module))
      (autoload command (symbol-name (or feature (aaron-module-name module))) nil t))
    (cond
     ((aaron-module-after module)
      (let ((triggers (aaron-module-after module)))
        (setf (alist-get feature aaron-module-deferred-triggers) triggers)
        (if (cl-some #'featurep triggers)
            (aaron-module--load-deferred module)
          (setf (aaron-module-state module) 'waiting)
          (dolist (trigger triggers)
            (with-eval-after-load trigger
              (aaron-module--load-deferred module))))))
     ((aaron-module-autoload module)
      (setf (aaron-module-state module) 'autoload))
     (t (aaron-module--load module (aaron-module--config-thunk module))))))

(defun aaron-modules--load-autoloads ()
  "Load the generated autoloads of directory modules, regenerating if stale."
  (let* ((dirs (cl-loop for key in (aaron-module-list)
                        for module = (aaron-module-get key)
                        for dir = (aaron-module-expand-path module aaron-module-autoload-dir)
                        if (and dir (file-directory-p dir)) collect dir))
         (out aaron-module-autoloads-file))
    (when dirs
      (when (or (not (file-exists-p out))
                (not (equal dirs (aaron-modules--autoloads-sources out)))
                (cl-some (lambda (dir)
                           (cl-some (lambda (f) (file-newer-than-file-p f out))
                                    (directory-files dir t "\\.el\\'")))
                         dirs))
        (aaron-modules-generate-autoloads dirs))
      (load out nil 'nomessage))))

(defun aaron-modules--autoloads-sources (file)
  "Return the source directories recorded in autoloads FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (when (re-search-forward "^;; aaron-module-sources: \\(.*\\)$" nil t)
      (car (read-from-string (match-string 1))))))

(defun aaron-modules-generate-autoloads (dirs)
  "Write `aaron-module-autoloads-file' from the autoload files in DIRS."
  (require 'loaddefs-gen)
  (let ((out aaron-module-autoloads-file)
        (file-name-handler-alist nil))
    (make-directory (file-name-directory out) t)
    (when (file-exists-p out) (delete-file out))
    (loaddefs-generate dirs out nil
                       (format ";; aaron-module-sources: %S\n" dirs))
    ;; `loaddefs-generate' names files relative to OUT; make them relative to
    ;; their module root, which is on `load-path', so `autoload' finds them.
    (with-temp-file out
      (insert-file-contents out)
      (dolist (root aaron-module-load-path)
        (let ((rel (file-relative-name (file-name-as-directory root)
                                       (file-name-directory out))))
          (goto-char (point-min))
          (while (search-forward (concat "\"" rel) nil t)
            (replace-match "\"" t t)))))))

(defun aaron-modules-load ()
  "Load every declared module: init phase, then config phase."
  (let ((aaron-modules--starting t)
        (start (float-time))
        (init-keys (aaron-module-list t))
        (config-keys (aaron-module-list)))
    (aaron-modules--load-autoloads)
    (aaron-module--with-ledger
     (cl-flet ((init (keys)
                (dolist (key keys)
                  (let ((module (aaron-module-get key)))
                    (when-let* ((file (aaron-module-expand-path
                                       module aaron-module-init-file)))
                      (aaron-module--load module
                                          (lambda () (aaron-module--load-file module file)))))))
              (config (keys)
                (dolist (key keys)
                  (aaron-module--load-config (aaron-module-get key)))))
      (let ((foundation (seq-filter (lambda (k) (< (car (aaron-module-depth (aaron-module-get k))) 0))
                                    init-keys)))
        (init foundation)
        (aaron-run-hooks 'aaron-before-modules-init-hook)
        (init (seq-difference init-keys foundation))
        (aaron-run-hooks 'aaron-after-modules-init-hook))
      (let ((post (seq-filter (lambda (k) (>= (cdr (aaron-module-depth (aaron-module-get k))) 100))
                              config-keys)))
        (aaron-run-hooks 'aaron-before-modules-config-hook)
        (config (seq-difference config-keys post))
        (aaron-run-hooks 'aaron-after-modules-config-hook)
        (config post))))
    (setq aaron-modules-init-time (- (float-time) start))))

;;; Commands

(defun aaron-module-read (prompt &optional predicate)
  "Read a declared module key with PROMPT, limited by PREDICATE."
  (let* ((cands (cl-loop for key in (aaron-module-list)
                         for module = (aaron-module-get key)
                         if (or (null predicate) (funcall predicate module))
                         collect (cons (format "%s %s" (car key) (cdr key)) key)))
         (choice (completing-read prompt cands nil t)))
    (cdr (assoc choice cands))))

(defun aaron/load-module (key)
  "Load module KEY now: retry a failed module or start a waiting one."
  (interactive
   (list (aaron-module-read "Load module: "
                            (lambda (m) (not (eq (aaron-module-state m) 'loaded))))))
  (let ((module (or (aaron-module-get key) (user-error "No module %S" key))))
    (aaron-module--with-ledger
      (aaron-module--load module (aaron-module--config-thunk module)))
    (message "%s %s: %s" (car key) (cdr key) (aaron-module-state module))))

(provide 'aaron-modules)
;;; aaron-modules.el ends here
