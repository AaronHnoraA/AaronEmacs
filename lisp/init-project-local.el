;;; init-project-local.el --- Project-local workflow overrides -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'pp)
(require 'seq)
(require 'subr-x)

(declare-function my/project-current-root "init-project")

(defgroup my/project-local nil
  "Project-local overrides for workflows and language-server behavior."
  :group 'convenience)

(defcustom my/project-local-overrides nil
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
- `:language-server' as one of `eglot', `lsp-mode', or `disabled'
- `:eglot-workspace' as extra workspace configuration merged before startup."
  :type '(repeat
          (cons :tag "Project override"
                sexp
                sexp))
  :group 'my/project-local)

(defvar-local my/project-local-settings nil
  "Buffer-local project override plist, typically set via `.dir-locals.el'.")

(defconst my/project-local--env-keys
  '(:env :test-env :task-env :run-env :debug-env :lsp-env)
  "Plist keys that contain environment variable alists.")

(defconst my/project-local--candidate-keys
  '(:task :run :debug)
  "Plist keys that contain named candidate alists.")

(defun my/project-local-root ()
  "Return the best root for project-local workflow overrides."
  (file-name-as-directory
   (expand-file-name
    (or (and (fboundp 'my/project-current-root)
             (my/project-current-root))
        (when-let* ((project (project-current nil default-directory)))
          (project-root project))
        default-directory))))

(defun my/project-local--pair-list-p (value)
  "Return non-nil when VALUE is a list of cons cells."
  (and (listp value)
       (or (null value)
           (consp (car value)))))

(defun my/project-local--delete-pair (key pairs)
  "Return PAIRS without entries whose car matches KEY."
  (seq-remove (lambda (entry)
                (equal (car-safe entry) key))
              pairs))

(defun my/project-local--merge-pairs (left right)
  "Merge RIGHT pairs into LEFT, preferring RIGHT on duplicate keys."
  (let ((result (copy-tree (or left nil))))
    (dolist (entry right)
      (when (consp entry)
        (setq result (my/project-local--delete-pair (car entry) result))
        (setq result (append result (list entry)))))
    result))

(defun my/project-local--merge-value (key old new)
  "Merge OLD and NEW values for plist KEY."
  (pcase key
    ((pred (lambda (item) (memq item my/project-local--env-keys)))
     (my/project-local--merge-pairs old new))
    ((pred (lambda (item) (memq item my/project-local--candidate-keys)))
     (append old new))
    (:test
     (if (and (my/project-local--pair-list-p old)
              (my/project-local--pair-list-p new))
         (my/project-local--merge-pairs old new)
       new))
    (_ new)))

(defun my/project-local--merge-plists (base override)
  "Merge OVERRIDE plist into BASE plist."
  (let ((result (copy-tree (or base nil)))
        (plist (copy-tree (or override nil))))
    (while plist
      (let* ((key (pop plist))
             (value (pop plist))
             (current (plist-get result key)))
        (setq result
              (plist-put result key
                         (my/project-local--merge-value key current value)))))
    result))

(defun my/project-local--matcher-match-p (matcher root)
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

(defun my/project-local--matching-overrides (&optional root)
  "Return all override plists that match ROOT."
  (let ((root (or root (my/project-local-root)))
        matches)
    (dolist (entry my/project-local-overrides (nreverse matches))
      (when (and (consp entry)
                 (my/project-local--matcher-match-p (car entry) root))
        (push (cdr entry) matches)))))

(defun my/project-local-entry (&optional root)
  "Return the merged override plist for ROOT."
  (let ((root (or root (my/project-local-root)))
        (merged nil))
    (dolist (plist (my/project-local--matching-overrides root))
      (setq merged (my/project-local--merge-plists merged plist)))
    (setq merged (my/project-local--merge-plists merged my/project-local-settings))
    merged))

(defun my/project-local-value (key &optional root)
  "Return project-local KEY value for ROOT."
  (plist-get (my/project-local-entry root) key))

(defun my/project-local-resolve (value &optional root)
  "Resolve VALUE for ROOT.
When VALUE is a function, call it with ROOT when possible."
  (let ((root (or root (my/project-local-root))))
    (cond
     ((functionp value)
      (condition-case nil
          (funcall value root)
        (wrong-number-of-arguments
         (funcall value))))
     (t value))))

(defun my/project-local-test-command (scope &optional root)
  "Return the project-local test command for SCOPE in ROOT."
  (let* ((root (or root (my/project-local-root)))
         (value (my/project-local-value :test root))
         (entry (cond
                 ((my/project-local--pair-list-p value)
                  (or (assoc scope value)
                      (assoc 'default value)
                      (assoc t value)))
                 (t value)))
         (command (my/project-local-resolve (if (consp entry) (cdr entry) entry) root)))
    (when (and (stringp command)
               (not (string-empty-p command)))
      command)))

(defun my/project-local--candidate-key (kind)
  "Return the plist key for candidate KIND."
  (pcase kind
    ('task :task)
    ('run :run)
    ('debug :debug)
    (_ (intern (format ":%s" kind)))))

(defun my/project-local-merge-candidates (kind base &optional root)
  "Merge project-local KIND candidates into BASE for ROOT."
  (let* ((root (or root (my/project-local-root)))
         (extra (my/project-local-value (my/project-local--candidate-key kind) root))
         (result (copy-tree (or base nil))))
    (dolist (entry extra result)
      (when (consp entry)
        (let* ((label (format "%s" (car entry)))
               (value (my/project-local-resolve (cdr entry) root)))
          (when value
            (setq result
                  (append
                   (seq-remove (lambda (item)
                                 (equal (car-safe item) label))
                               result)
                   (list (cons label value))))))))))

(defun my/project-local-env (kind &optional root)
  "Return merged environment alist for KIND in ROOT."
  (let* ((root (or root (my/project-local-root)))
         (entry (my/project-local-entry root))
         (env (plist-get entry :env))
         (kind-env (plist-get entry (intern (format ":%s-env" kind)))))
    (my/project-local--merge-pairs env kind-env)))

(defun my/project-local-apply-env (env &optional base)
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

(defun my/project-local-describe ()
  "Describe active project-local overrides for the current project."
  (interactive)
  (let ((root (my/project-local-root))
        (entry (my/project-local-entry)))
    (with-help-window (help-buffer)
      (princ (format "Project-local overrides for %s\n\n"
                     (abbreviate-file-name root)))
      (if entry
          (pp entry)
        (princ "No project-local overrides are active.\n")))))

(defun my/project-local-open-dir-locals ()
  "Open the current project's `.dir-locals.el'."
  (interactive)
  (find-file (expand-file-name ".dir-locals.el" (my/project-local-root))))

(my/leader!
  "p"   '(:ignore t :which-key "project")
  "p l" '(:def my/project-local-describe :which-key "project local overrides")
  "p L" '(:def my/project-local-open-dir-locals :which-key "project dir-locals"))

(provide 'init-project-local)
;;; init-project-local.el ends here
