;;; aaron-lib.el --- Core macro and hook library -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The configuration's standard library: named hooks and advice, compound
;; `after!' forms, closure factories, and hook runners that report which
;; function failed.
;;
;; Naming follows Doom: macros keep Doom's `!' names so code can be compared
;; with Doom's source directly; core functions and variables are `aaron-'
;; (`aaron--' when private, `aaron/' for commands); hook functions end in
;; `-h' and advice in `-a'.

;; Adapted from Doom Emacs (MIT, Henrik Lissner):
;;   core/lisp/doom-lib.el @ 59cdaa3
;; Dropped: Doom's module/profile context, `load!'/`add-load-path!' (load
;; order lives in `init-modules.el'), `versionp!' and the pcase extensions.
;;
;; This file must stay dependency-free: it is loaded first and required at
;; compile time by any module that uses its macros.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Logging

(defgroup aaron nil
  "The configuration's core: library, lifecycle and modules."
  :group 'initialization)

(defvar aaron-log-level (if init-file-debug 2 0)
  "How verbosely `aaron-log' reports.
0 logs nothing, 1 warnings, 2 notices, 3 debug detail.  Logged lines go
to *Messages* without touching the echo area.")

(defun aaron--log (level text &rest args)
  "Log TEXT formatted with ARGS at LEVEL."
  (let ((inhibit-message (> level 1)))
    (apply #'message
           (concat "* %.06f: " text)
           (float-time (time-subtract (current-time) before-init-time))
           args)))

(defmacro aaron-log (message &rest args)
  "Log MESSAGE formatted with ARGS when `aaron-log-level' allows it.
An optional leading integer is the level (default 2).  A macro, so ARGS
are not evaluated while logging is off."
  (declare (debug t))
  (let ((level (if (integerp message)
                   (prog1 message (setq message (pop args)))
                 2)))
    `(when (<= ,level aaron-log-level)
       (aaron--log ,level ,message ,@args))))

;;; Helpers

(defun aaron-unquote (exp)
  "Return EXP with any number of `quote' and `function' wrappers removed."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun aaron-rpartial (fn &rest args)
  "Return FN with its last arguments fixed to ARGS."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(defun aaron--resolve-hook-forms (hooks)
  "Turn HOOKS, a mode or list of modes, into a list of hook symbols.
A quoted element is kept as is; a quoted list is returned as is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (aaron-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun aaron--setq-hook-fns (hooks rest &optional singles)
  "Return (VAR VAL HOOK FN) entries for `setq-hook!' over HOOKS and REST.
With SINGLES, REST holds variables only."
  (unless (or singles (zerop (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'cl-evenp (length rest))))
  (cl-loop with vars = (let ((args rest) vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (aaron--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "aaron--setq-%s-for-%s-h"
                                          var mode))))))

;;; Running hooks

(define-error 'aaron-hook-error "Error in a hook")

(defvar aaron--hook nil
  "The hook variable `aaron-run-hooks' is currently running.")

(defun aaron-run-hook (hook)
  "Call HOOK, a hook function, signalling `aaron-hook-error' if it fails.
Meant for `run-hook-wrapped'; returns nil so the run continues."
  (aaron-log 3 "hook:%s: run %S in %S" (or aaron--hook '*) hook (current-buffer))
  (condition-case-unless-debug e
      (funcall hook)
    (error (signal 'aaron-hook-error (list hook e))))
  nil)

(defun aaron-run-hooks (&rest hooks)
  "Run HOOKS, hook variable symbols, naming the function that fails.
A failing function is reported with a warning that names it, and the
error is re-signalled as `aaron-hook-error'."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (let ((aaron--hook hook))
          (run-hook-wrapped hook #'aaron-run-hook))
      (aaron-hook-error
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (if (symbolp (cadr e)) (symbol-name (cadr e)) (cadr e))
                (caddr e)))
       (signal 'aaron-hook-error (cons hook (cdr e)))))))

(defun aaron-run-hook-on (hook-var trigger-hooks &optional predicate)
  "Run HOOK-VAR once, the first time any of TRIGGER-HOOKS runs after startup.
TRIGGER-HOOKS are hook symbols; `find-file-hook' is matched by advising
`after-find-file', which runs before modes are set up.  PREDICATE, when
given, must return non-nil for the trigger to count.  HOOK-VAR is set to
nil after it runs, and every trigger removes itself.  Returns the
trigger functions."
  (let* ((state (list nil))             ; (RUNNING . TRIGGERS)
         (remove-all
          (lambda ()
            (pcase-dolist (`(,hook . ,fn) (cdr state))
              (remove-hook 'server-after-make-frame-hook fn)
              (if (eq hook 'find-file-hook)
                  (advice-remove 'after-find-file fn)
                (remove-hook hook fn))))))
    (dolist (hook trigger-hooks)
      (let ((fn (make-symbol (format "chain-%s-to-%s-h" hook-var hook))))
        (fset fn
              (lambda (&rest _)
                (when (and (not (car state))
                           after-init-time
                           (or (daemonp)
                               ;; A hook let-bound to nil means the caller is
                               ;; processing buffers internally.
                               (and (boundp hook) (symbol-value hook))))
                  ;; The predicate or hooks may change the current buffer,
                  ;; which would break `after-find-file'.
                  (save-current-buffer
                    (when (or (null predicate) (funcall predicate))
                      (setcar state t)
                      (funcall remove-all)
                      (unwind-protect (aaron-run-hooks hook-var)
                        (set hook-var nil)))))))
        (when (daemonp)
          (add-hook 'server-after-make-frame-hook fn 'append))
        (if (eq hook 'find-file-hook)
            (advice-add 'after-find-file :before fn '((depth . -101)))
          (add-hook hook fn -101))
        (setcdr state (cons (cons hook fn) (cdr state)))))
    (mapcar #'cdr (reverse (cdr state)))))

;;; Sugars

(defmacro file! ()
  "Return the file in which this macro is expanded."
  (or (macroexp-file-name)
      (buffer-file-name (buffer-base-buffer))
      (error "file!: cannot deduce the current file path")))

(defmacro dir! (&rest segments)
  "Return the directory of the file this macro is expanded in.
SEGMENTS are appended to it."
  (let* ((file-name-handler-alist nil)
         (dir (file-name-directory (macroexpand '(file!)))))
    (if segments
        `(file-name-concat ,dir ,@segments)
      dir)))

(defmacro letf! (bindings &rest body)
  "Temporarily bind functions, macros and advice around BODY.

BINDINGS is a list of (PLACE VALUE) pairs as for `cl-letf*' (a
sharp-quoted PLACE means its `symbol-function'), or one or a list of:

  (defun NAME ARGS BODY...)     lexical function (`cl-flet')
  (defun* NAME ARGS BODY...)    recursive lexical function (`cl-labels')
  (defun! NAME ARGS BODY...)    global override for BODY's duration
  (defmacro NAME ARGS BODY...)  lexical macro (`cl-macrolet')
  (defadvice FN HOW ADVICE)     `advice-add' for BODY's duration
  (defadvice FN (HOW ARGS) BODY...)  temporary `define-advice'"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defun! defmacro defadvice))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (setq
     body (pcase binding
            (`(defmacro . ,rest) `(cl-macrolet (,rest) ,body))
            (`(defun    . ,rest) `(cl-flet     (,rest) ,body))
            (`(defun*   . ,rest) `(cl-labels   (,rest) ,body))
            (`(defun! ,name . ,rest)
             `(cl-letf (((symbol-function #',name)
                         (cl-function (lambda ,@rest))))
                ,body))
            (`(defadvice ,target ,first . ,rest)
             (if (keywordp first)
                 (let ((sym (gensym "fn")))
                   `(when-let* ((,sym ,@rest))
                      (advice-add ,target ,first ,sym)
                      (unwind-protect ,body (advice-remove ,target ,sym))))
               (when (< (length first) 3)
                 (setq first (list (nth 0 first)
                                   (nth 1 first)
                                   (gensym "aaron-letf-"))))
               (let ((sym (intern (format "%s@%s" target (nth 2 first)))))
                 `(progn
                    (define-advice ,target ,first ,@rest)
                    (unwind-protect ,body
                      (advice-remove #',target #',sym)
                      (fmakunbound ',sym))))))
            (`((function ,fn) ,value)
             `(cl-letf (((symbol-function #',fn) ,value)) ,body))
            (_ `(let (,binding) ,body))))))

(defmacro quiet!! (&rest forms)
  "Run FORMS without any output, including to *Messages*."
  (declare (indent 0))
  `(if init-file-debug
       (progn ,@forms)
     (letf! ((standard-output (lambda (&rest _)))
             (defadvice message (:override (msg &rest _)) msg)
             (defadvice load (:around (fn file &optional noerror _nomessage nosuffix must-suffix))
               (funcall fn file noerror t nosuffix must-suffix))
             (defadvice write-region (:around (fn start end filename &optional append visit lockname mustbenew))
               (unless visit (setq visit 'no-message))
               (funcall fn start end filename append visit lockname mustbenew)))
       ,@forms)))

(defmacro quiet! (&rest forms)
  "Run FORMS without echo-area output.
Interactively *Messages* still records it; in batch nothing is printed."
  (declare (indent 0))
  `(if init-file-debug
       (progn ,@forms)
     ,(if noninteractive
          `(quiet!! ,@forms)
        `(let ((inhibit-message t)
               (save-silently t))
           (prog1 (progn ,@forms) (message ""))))))

(defmacro with-delayed-gc! (&rest body)
  "Evaluate BODY with garbage collection deferred."
  (declare (indent defun))
  (if (featurep 'igc)
      (macroexp-progn body)
    `(let ((gc-cons-threshold most-positive-fixnum)
           (gc-cons-percentage 1.0))
       ,@body)))

;;; Closure factories

(defun aaron--lambda-allow-other-keys (args)
  "Return ARGS with `&allow-other-keys' added after any `&key'."
  (let ((add (and (memq '&key args)
                  (not (memq '&allow-other-keys args))))
        out)
    (dolist (arg args)
      (when (and add (eq arg '&aux))
        (push '&allow-other-keys out))
      (push (if (proper-list-p arg)
                (aaron--lambda-allow-other-keys arg)
              arg)
            out))
    (setq out (nreverse out))
    (if (and add (not (memq '&aux args)))
        (nconc out (list '&allow-other-keys))
      out)))

(defmacro lambda! (arglist &rest body)
  "Return (cl-function (lambda ARGLIST BODY...)).
`&key' implies `&allow-other-keys'."
  (declare (indent defun) (doc-string 1) (side-effect-free t))
  `(cl-function (lambda ,(aaron--lambda-allow-other-keys arglist) ,@body)))

(defun aaron--fn-arglist (args)
  "Return the positional argument list implied by `%' symbols in ARGS."
  (let ((argv (make-vector 10 nil))
        (stack (list args)))
    (while stack
      (let ((data (pop stack)))
        (cond ((symbolp data)
               (when-let* ((pos (cond ((eq data '%*) 0)
                                      ((eq data '%) 1)
                                      ((memq data '(%1 %2 %3 %4 %5 %6 %7 %8 %9))
                                       (- (aref (symbol-name data) 1) ?0)))))
                 (when (and (= pos 1) (aref argv 1) (not (eq data (aref argv 1))))
                   (error "%% and %%1 are mutually exclusive"))
                 (aset argv pos data)))
              ((and (not (eq (car-safe data) 'fn!))
                    (or (consp data) (vectorp data)))
               (setq stack (append data stack))))))
    (nconc
     (cl-loop with seen for i downfrom 9 to 1
              for sym = (aref argv i)
              if (or seen sym) do (setq seen t)
              and collect (or sym (intern (format "_%%%d" i))) into arglist
              finally return (nreverse arglist))
     (and (aref argv 0) '(&rest %*)))))

(defmacro fn! (&rest args)
  "Return a lambda whose arguments are the `%1'..`%9' symbols in ARGS.
`%' is short for `%1'; `%*' collects the rest."
  (declare (doc-string 1) (side-effect-free t))
  `(lambda ,(aaron--fn-arglist args) ,@args))

(defmacro cmd! (&rest body)
  "Return an interactive command that runs BODY."
  (declare (doc-string 1) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro cmd!! (command &optional arg &rest args)
  "Return a command calling COMMAND interactively with prefix ARG and ARGS."
  (declare (doc-string 1) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,arg arg)))
       (,(if args #'funcall-interactively #'call-interactively)
        (let ((command ,command))
          (or (command-remapping command) command))
        ,@args))))

(defmacro cmds! (&rest branches)
  "Return a key definition that dispatches on BRANCHES.
BRANCHES is a flat list of CONDITION COMMAND pairs, evaluated each time
the key is pressed; an odd last element is the fallback.  Without a
match and without a fallback, the key falls through to the next keymap."
  (declare (doc-string 1))
  (let ((docstring (if (stringp (car branches)) (pop branches) ""))
        fallback)
    (when (cl-oddp (length branches))
      (setq fallback (car (last branches))
            branches (butlast branches)))
    (let ((defs (cl-loop for (key value) on branches by #'cddr
                         unless (keywordp key)
                         collect (list key value))))
      `'(menu-item
         ,docstring nil
         :filter (lambda (&optional _)
                   (let (it)
                     (cond ,@(mapcar (lambda (pred-def)
                                       `((setq it ,(car pred-def))
                                         ,(cadr pred-def)))
                                     defs)
                           (t ,fallback))))))))

;;; Loading

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE loads.
PACKAGE is a feature symbol, or a list combining features with
:or/:any and :and/:all (a bare list means :and):

  (after! magit ...)
  (after! (magit diff-hl) ...)
  (after! (:or evil-mode evil-collection) ...)"
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (list (if (or (not (bound-and-true-p byte-compile-current-file))
                    (require package nil 'noerror))
                #'progn
              #'with-no-warnings)
            `(with-eval-after-load ',package ,@body))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

(defmacro defer-until! (condition &rest body)
  "Run BODY now if CONDITION holds, else after the first load that makes it hold."
  (declare (indent defun) (debug t))
  `(if ,condition
       (progn ,@body)
     ,(let ((fn (intern (format "aaron--delay-form-%s-h"
                                (sxhash (cons condition body))))))
        `(progn
           (fset ',fn (lambda (&rest args)
                        (when ,(or condition t)
                          (remove-hook 'after-load-functions #',fn)
                          (unintern ',fn nil)
                          (ignore args)
                          ,@body)))
           (put ',fn 'permanent-local-hook t)
           (add-hook 'after-load-functions #',fn)))))

(defmacro defer-feature! (feature &rest fns)
  "Pretend FEATURE is not loaded until one of FNS (default FEATURE) runs.
For features loaded at startup (`elisp-mode'), so their `after!' blocks
run when the feature is first used."
  (let ((advice-fn (intern (format "aaron--defer-feature-%s-a" feature)))
        (fns (or fns (list feature))))
    `(progn
       (cl-callf2 delq ',feature features)
       (defadvice! ,advice-fn (&rest _)
         ,(format "Provide `%s' when one of %S is first called.\nCreated by `defer-feature!'."
                  feature fns)
         :before ',fns
         ;; Well-behaved packages that run a mode internally use
         ;; `delay-mode-hooks'; that must not count as first use.
         (unless delay-mode-hooks
           (unwind-protect
               (unless (featurep ',feature)
                 (save-current-buffer (provide ',feature)))
             (dolist (fn ',fns)
               (advice-remove fn #',advice-fn))))))))

;;; Hooks

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Run FORMS once, the first time HOOK-OR-FUNCTION runs.
HOOK-OR-FUNCTION is a quoted hook or a sharp-quoted function (advised
:before, or :after when FORMS start with :after)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        (fn (gensym "aaron-transient-hook")))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (aaron-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

(defmacro add-hook! (hooks &rest rest)
  "Add functions to one or more hooks.

HOOKS is an unquoted mode or list of modes (`-hook' is appended), or a
quoted hook or list of hooks.  Then optional :append, :local, :depth N
and :remove.  Then the functions: sharp-quoted or quoted symbols, a
quoted list of them, `defun' forms (defined and added), or arbitrary
forms, which are wrapped in one lambda.

\(fn HOOKS [:append :local :depth N] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (aaron--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil lambda lambda!))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook ',(nreverse hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "Remove functions from hooks; takes the same arguments as `add-hook!'."
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Set buffer-local variables when HOOKS run.
Each pair gets a named hook function, removable with `unsetq-hook!'.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (aaron--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _) (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Undo `setq-hook!' for VARS on HOOKS."
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (aaron--setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))

;;; Definers

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define SYMBOL as a function and add it as advice to its targets.
After ARGLIST and DOCSTRING, BODY starts with one or more HOW TARGETS
pairs: HOW is an `advice-add' keyword, TARGETS a function or list.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [HOW TARGETS...] BODY)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Remove advice SYMBOL; takes the same arguments as `defadvice!'."
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))

(provide 'aaron-lib)
;;; aaron-lib.el ends here
