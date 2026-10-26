;;; tools/project-local/config.el --- Project-local workflow overrides -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-funcs)
(require 'aaron-open)
(require 'pp)
(require 'seq)
(require 'subr-x)

(declare-function +project-current-root "tools/project/config")

(defgroup +project-local nil
  "Project-local overrides for workflows and language-server behavior."
  :group 'convenience)

(config-defvar +project-local-overrides nil
  "Project-local workflow overrides keyed by project matcher.
Each entry is of the form (MATCHER . PLIST).

Supported matcher forms:
- directory string: matches the directory and projects inside it
- (:regexp . REGEXP): matches project roots by regexp
- function: called with the normalized project root

Supported plist keys:
- `:env' / `:test-env' / `:task-env' / `:run-env' / `:debug-env' / `:lsp-env'
- `:test' as an alist like ((nearest . \"...\") (project . \"...\"))
- `:task', `:run', and `:debug' as alists of (LABEL . SPEC)
- `:debug-default' as a debug SPEC or profile label
- `:language-server' as one of `lsp-mode' or `disabled'
- `:lsp-workspace' as extra workspace configuration merged before startup
- `:toolchain' as a language-family to profile-id alist
- `:toolchain-profiles' as named language-server toolchain profiles
- `:noema-jupyter' as project Jupyter cell defaults."
  :type '(repeat (cons :tag "Project override" sexp sexp))
  :group '+project-local)

(defvar-local +project-local-settings nil
  "Buffer-local project override plist, typically set via `.dir-locals.el'.")

(defun +project-local--simple-toolchain-selection-p (value)
  "Return non-nil when VALUE is a declarative toolchain selection."
  (or (and value (symbolp value))
      (stringp value)
      (and (listp value)
           (seq-every-p
            (lambda (entry)
              (and (consp entry)
                   (symbolp (car entry))
                   (or (symbolp (cdr entry)) (stringp (cdr entry)))))
            value))))

(defun +project-local--simple-jupyter-defaults-p (value)
  "Return non-nil when VALUE contains safe Noema Jupyter defaults."
  (and (listp value)
       (let ((rest value)
             (allowed '(:language :kernel :session))
             (valid t))
         (while (and valid rest)
           (let ((key (pop rest)))
             (if (null rest)
                 (setq valid nil)
               (let ((item (pop rest)))
                 (setq valid
                       (and (memq key allowed)
                            item
                            (or (symbolp item) (stringp item))))))))
         (and valid (null rest)))))

(defun +project-local-settings-safe-p (value)
  "Return non-nil for the non-executable project settings in VALUE.

Command, environment, custom server and custom profile entries intentionally
remain subject to Emacs' normal local-variable confirmation."
  (and (listp value)
       (let ((rest value)
             (valid t))
         (while (and valid rest)
           (let ((key (pop rest)))
             (if (null rest)
                 (setq valid nil)
               (let ((item (pop rest)))
                 (setq valid
                       (pcase key
                         (:toolchain
                          (+project-local--simple-toolchain-selection-p item))
                         (:noema-jupyter
                          (+project-local--simple-jupyter-defaults-p item))
                         (_ nil)))))))
         (and valid (null rest)))))

(put '+project-local-settings 'safe-local-variable
     #'+project-local-settings-safe-p)

(defconst +project-local--env-keys
  '(:env :test-env :task-env :run-env :debug-env :lsp-env)
  "Plist keys that contain environment variable alists.")

(defconst +project-local--candidate-keys
  '(:task :run :debug)
  "Plist keys that contain named candidate alists.")

(defconst +project-local--named-profile-keys
  '(:toolchain-profiles)
  "Plist keys that contain named profile alists.")

(defvar +project-local--scoped-root nil
  "Dynamically pinned (BUFFER . ROOT) for one synchronous operation.")

(defun +project-local-root ()
  "Return the best root for project-local workflow overrides."
  (or (and (consp +project-local--scoped-root)
           (eq (car +project-local--scoped-root) (current-buffer))
           (cdr +project-local--scoped-root))
      (file-name-as-directory
       (expand-file-name
        (or (and (fboundp '+project-current-root)
                 (+project-current-root))
            (when-let* ((project (project-current nil default-directory)))
              (project-root project))
            default-directory)))))

(defun +project-local--pair-list-p (value)
  "Return non-nil when VALUE is a list of cons cells."
  (and (listp value)
       (or (null value)
           (consp (car value)))))

(defun +project-local--delete-pair (key pairs)
  "Return PAIRS without entries whose car matches KEY."
  (seq-remove (lambda (entry)
                (equal (car-safe entry) key))
              pairs))

(defun +project-local--merge-pairs (left right)
  "Merge RIGHT pairs into LEFT, preferring RIGHT on duplicate keys."
  (let ((result (copy-tree (or left nil))))
    (dolist (entry right)
      (when (consp entry)
        (setq result (+project-local--delete-pair (car entry) result))
        (setq result (append result (list entry)))))
    result))

(defun +project-local--merge-value (key old new)
  "Merge OLD and NEW values for plist KEY."
  (pcase key
    ((pred (lambda (item) (memq item +project-local--env-keys)))
     (+project-local--merge-pairs old new))
    ((pred (lambda (item) (memq item +project-local--candidate-keys)))
     (append old new))
    ((pred (lambda (item) (memq item +project-local--named-profile-keys)))
     (+project-local--merge-pairs old new))
    (:test
     (if (and (+project-local--pair-list-p old)
              (+project-local--pair-list-p new))
         (+project-local--merge-pairs old new)
       new))
    (_ new)))

(defun +project-local--merge-plists (base override)
  "Merge OVERRIDE plist into BASE plist."
  (let ((result (copy-tree (or base nil)))
        (plist (copy-tree (or override nil))))
    (while plist
      (let* ((key (pop plist))
             (value (pop plist))
             (current (plist-get result key)))
        (setq result
              (plist-put result key
                         (+project-local--merge-value key current value)))))
    result))

(defun +project-local--matcher-match-p (matcher root)
  "Return non-nil when MATCHER applies to ROOT."
  (pcase matcher
    (`(:regexp . ,regexp)
     (and (stringp regexp)
          (string-match-p regexp root)))
    ((pred functionp)
     (funcall matcher root))
    ((pred stringp)
     (let ((path (file-name-as-directory (expand-file-name matcher))))
       (or (string= path root)
           (file-in-directory-p root path))))
    (_ nil)))

(defun +project-local--matching-overrides (&optional root)
  "Return all override plists that match ROOT."
  (let ((root (or root (+project-local-root)))
        matches)
    (dolist (entry +project-local-overrides (nreverse matches))
      (when (and (consp entry)
                 (+project-local--matcher-match-p (car entry) root))
        (push (cdr entry) matches)))))

(defun +project-local-entry (&optional root)
  "Return the merged override plist for ROOT."
  (let ((root (or root (+project-local-root)))
        (merged nil))
    (dolist (plist (+project-local--matching-overrides root))
      (setq merged (+project-local--merge-plists merged plist)))
    (setq merged (+project-local--merge-plists merged +project-local-settings))
    merged))

(defun +project-local-value (key &optional root)
  "Return project-local KEY value for ROOT."
  (plist-get (+project-local-entry root) key))

(defun +project-local-resolve (value &optional root)
  "Resolve VALUE for ROOT.
When VALUE is a function, call it with ROOT when possible."
  (let ((root (or root (+project-local-root))))
    (cond
     ((functionp value)
      (condition-case nil
          (funcall value root)
        (wrong-number-of-arguments
         (funcall value))))
     (t value))))

(defun +project-local-test-command (scope &optional root)
  "Return the project-local test command for SCOPE in ROOT."
  (let* ((root (or root (+project-local-root)))
         (value (+project-local-value :test root))
         (entry (cond
                 ((+project-local--pair-list-p value)
                  (or (assoc scope value)
                      (assoc 'default value)
                      (assoc t value)))
                 (t value)))
         (command (+project-local-resolve (if (consp entry) (cdr entry) entry) root)))
    (when (and (stringp command)
               (not (string-empty-p command)))
      command)))

(defun +project-local--candidate-key (kind)
  "Return the plist key for candidate KIND."
  (pcase kind
    ('task :task)
    ('run :run)
    ('debug :debug)
    (_ (intern (format ":%s" kind)))))

(defun +project-local-merge-candidates (kind base &optional root)
  "Merge project-local KIND candidates into BASE for ROOT."
  (let* ((root (or root (+project-local-root)))
         (extra (+project-local-value (+project-local--candidate-key kind) root))
         (result (copy-tree (or base nil))))
    (dolist (entry extra result)
      (when (consp entry)
        (let* ((label (format "%s" (car entry)))
               (value (+project-local-resolve (cdr entry) root)))
          (when value
            (setq result
                  (append
                   (seq-remove (lambda (item)
                                 (equal (car-safe item) label))
                               result)
                   (list (cons label value))))))))))

(defun +project-local-env (kind &optional root)
  "Return merged environment alist for KIND in ROOT."
  (let* ((root (or root (+project-local-root)))
         (entry (+project-local-entry root))
         (env (plist-get entry :env))
         (kind-env (plist-get entry (intern (format ":%s-env" kind)))))
    (+project-local--merge-pairs env kind-env)))

(defun +project-local-apply-env (env &optional base)
  "Apply ENV alist on top of BASE process environment and return the result."
  (let ((result (copy-sequence (or base process-environment))))
    (dolist (entry env result)
      (when (consp entry)
        (let* ((name (format "%s" (car entry)))
               (value (cdr entry))
               (prefix (concat name "=")))
          (setq result
                (seq-remove (lambda (item)
                              (string-prefix-p prefix item))
                            result))
          (when value
            (push (concat prefix (format "%s" value)) result)))))))

(defun +project-local/describe ()
  "Describe active project-local overrides for the current project."
  (interactive)
  (let ((root (+project-local-root))
        (entry (+project-local-entry)))
    (with-help-window (help-buffer)
      (princ (format "Project-local overrides for %s\n\n"
                     (abbreviate-file-name root)))
      (if entry
          (pp entry)
        (princ "No project-local overrides are active.\n")))))

(defun +project-local/open-dir-locals ()
  "Open the current project's `.dir-locals.el'."
  (interactive)
  (aaron-open-visit-file
   (aaron-open-resolve-file-name
    (expand-file-name ".dir-locals.el" (+project-local-root)))))

(define-leader-key!
  "p l" '(:def +project-local/describe :which-key "project local overrides")
  "p L" '(:def +project-local/open-dir-locals :which-key "project dir-locals"))

(provide 'tools/project-local/config)
;;; tools/project-local/config.el ends here
