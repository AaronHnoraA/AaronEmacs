;;; aaron-doctor.el --- Module doctor and policy checks -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; One diagnosis per module, built from four sources:
;;
;;   load     the registry: failed, waiting, load time
;;   ledger   key bindings this module set over another module's
;;   policy   the configuration's rules (AGENT.md) checked against the
;;            module's source: `aaron-module-policies'
;;   doctor   a directory module's doctor.el, written with Doom's
;;            `assert!', `warn!', `error!' and `explain!'
;;
;; A module's own deeper doctor (LSP, Remote, LaTeX...) is named by its
;; `:doctor' property and offered from the module board; it is interactive
;; and not run here.
;;
;; A policy hit on a line that also says `aaron-allow: ID' (in a comment)
;; is an accepted exception, e.g. `file-remote-p' at a TRAMP compatibility
;; boundary.
;;
;; Adapted from Doom Emacs (MIT, Henrik Lissner): core/bin/doom-doctor @ 59cdaa3

;;; Code:

(require 'aaron-modules)

;;; Doctor DSL

(defvar aaron-doctor--errors nil)
(defvar aaron-doctor--warnings nil)
(defvar aaron-doctor--notes nil)

(defmacro error! (&rest args)
  "Record a doctor error formatted from ARGS."
  `(push (format ,@args) aaron-doctor--errors))

(defmacro warn! (&rest args)
  "Record a doctor warning formatted from ARGS."
  `(push (format ,@args) aaron-doctor--warnings))

(defmacro explain! (&rest args)
  "Record an explanation for the previous finding, from ARGS."
  `(push (concat ,@args) aaron-doctor--notes))

(defmacro assert! (condition message &rest args)
  "Record an error formatted from MESSAGE and ARGS unless CONDITION holds."
  `(unless ,condition (error! ,message ,@args)))

;;; Policies

(defvar aaron-module-policies
  '((:id file-remote-p
     :regexp "(file-remote-p\\_>"
     :message "selects behaviour with `file-remote-p'; ask the backend's declared costs"
     :exempt-groups (:remote))
    (:id local-target
     :regexp "(\\(?:equal\\|string=\\|eq\\)[^()\n]*\"local\")"
     :message "compares a target with \"local\"; local and remote take one code path"
     :exempt-groups (:remote))
    (:id client-processes
     :regexp "(\\(?:list-system-processes\\|process-attributes\\)\\_>"
     :message "asks about processes through `default-directory'; use `remote-client-list-system-processes'"
     :exempt-groups (:remote))
    (:id global-set-key
     :regexp "(global-set-key\\_>"
     :message "binds with `global-set-key'; use `general-define-key'")
    (:id package-vc-install
     :regexp "(package-vc-install\\_>"
     :message "installs a VC package directly; use `aaron-package-ensure-vc' or `aaron-package-register-vc'"
     :exempt-files ("core/aaron-packages.el" "bootstrap.el" "package-lock.el"))
    (:id legacy-name
     :regexp "\\_<my[-/]\\(?:\\sw\\|\\s_\\)"
     :message "uses a legacy `my/' or `my-' name; modules use `+DOMAIN-' and `+DOMAIN/COMMAND'")
    (:id config-setq
     :regexp aaron-doctor--config-setq-regexp
     :when aaron-doctor--load-time-p
     :message "sets a `config' setting in code; its value lives in etc/ (`config-set', the store)"))
  "Rules checked against module sources.
Each is a plist: :id, :regexp, :message, and optionally :exempt-groups
\(categories), :exempt-files (file names) and :kinds (`directory' or
`feature'; default both).  :regexp may be a function returning the
regexp, or nil to skip the rule.  :when, a function, is called at each
match with the match data set and keeps only matches it returns non-nil
for.")

(defvar aaron-doctor-runtime-forms
  '(defun defun* defsubst defmacro cl-defun cl-defmacro cl-defmethod lambda
    define-minor-mode define-globalized-minor-mode define-derived-mode
    defadvice! fn! cmd! cmd!! add-hook! setq-hook! add-transient-hook!
    transient-define-suffix transient-define-infix)
  "Forms whose bodies run later, not while their file loads.")

(defun aaron-doctor--load-time-p ()
  "Return non-nil when the match runs while its file loads.
False when any enclosing form is one of `aaron-doctor-runtime-forms'."
  (save-excursion
    (not (cl-some (lambda (open)
                    (goto-char (1+ open))
                    (memq (ignore-errors (read (current-buffer)))
                          aaron-doctor-runtime-forms))
                  (nth 9 (syntax-ppss (match-beginning 0)))))))

(defvar config--registry)

(defvar aaron-doctor--config-setq-cache nil
  "(COUNT . REGEXP) for `aaron-doctor--config-setq-regexp'.")

(defun aaron-doctor--config-setq-regexp ()
  "Match a `setq', `setq-default' or `setopt' of a `config' setting.
Only the first variable of the form is checked.  `setq-local' is a
buffer's own value and is not matched."
  (when (and (boundp 'config--registry) (hash-table-p config--registry))
    (let ((count (hash-table-count config--registry)))
      (unless (eql (car aaron-doctor--config-setq-cache) count)
        (let (vars)
          (maphash (lambda (_ entry)
                     (when-let* (((eq (plist-get entry :kind) 'variable))
                                 (var (plist-get entry :var)))
                       (push (symbol-name var) vars)))
                   config--registry)
          (setq aaron-doctor--config-setq-cache
                (cons count
                      (and vars
                           (concat "(\\(?:setq\\(?:-default\\)?\\|setopt\\)[ \t\n]+"
                                   (regexp-opt vars 'symbols)))))))
      (cdr aaron-doctor--config-setq-cache))))

(defun aaron-doctor--policy-applies-p (policy module file)
  "Return non-nil when POLICY applies to FILE of MODULE."
  (and (not (memq (aaron-module-group module) (plist-get policy :exempt-groups)))
       (not (member (file-name-nondirectory file) (plist-get policy :exempt-files)))
       (let ((kinds (plist-get policy :kinds)))
         (or (null kinds)
             (memq (if (aaron-module-directory-p module) 'directory 'feature) kinds)))))

(defun aaron-doctor-scan-file (file policies)
  "Return policy hits in FILE as (POLICY-ID FILE LINE TEXT).
Matches inside comments and strings, and lines marked `aaron-allow:
ID', are skipped."
  (let (hits)
    (with-temp-buffer
      (insert-file-contents file)
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (dolist (policy policies)
        (goto-char (point-min))
        (while (when-let* ((re (plist-get policy :regexp))
                           (re (if (functionp re) (funcall re) re)))
                 (re-search-forward re nil t))
          (let ((ppss (save-excursion (syntax-ppss (match-beginning 0))))
                (line (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))))
            (unless (or (nth 3 ppss) (nth 4 ppss)
                        (let ((pred (plist-get policy :when)))
                          (and pred (not (save-match-data
                                           (save-excursion (funcall pred))))))
                        (string-match-p
                         (format "aaron-allow:.*\\_<%s\\_>" (plist-get policy :id))
                         line))
              (push (list (plist-get policy :id) file (line-number-at-pos)
                          (string-trim line))
                    hits))))))
    (nreverse hits)))

(defun aaron-doctor-policy-violations (module)
  "Return MODULE's policy hits, as from `aaron-doctor-scan-file'."
  (cl-loop for file in (aaron-module-source-files module)
           for policies = (seq-filter
                           (lambda (p) (aaron-doctor--policy-applies-p p module file))
                           aaron-module-policies)
           when policies
           append (aaron-doctor-scan-file file policies)))

;;; Running the doctor

(defvar aaron-doctor-results (make-hash-table :test 'equal)
  "Module key to its last diagnosis plist.")

(defun aaron-doctor-module (key)
  "Diagnose module KEY and return a plist.
Keys: :errors :warnings :notes :violations :conflicts."
  (let* ((module (aaron-module-get key))
         (aaron-doctor--errors nil)
         (aaron-doctor--warnings nil)
         (aaron-doctor--notes nil))
    (pcase (aaron-module-state module)
      ('failed (error! "failed to load: %s" (aaron-module-error module)))
      ('waiting (warn! "waiting for %s"
                       (mapconcat #'symbol-name (aaron-module-after module) ", "))))
    (when-let* ((file (aaron-module-expand-path module aaron-module-doctor-file)))
      (condition-case err
          (let ((aaron-module-context module)
                (inhibit-message t))
            (load file nil 'nomessage))
        (error (error! "doctor.el failed: %s" (error-message-string err)))))
    (let ((result
           (list :errors (nreverse aaron-doctor--errors)
                 :warnings (nreverse aaron-doctor--warnings)
                 :notes (nreverse aaron-doctor--notes)
                 :violations (aaron-doctor-policy-violations module)
                 :conflicts (seq-filter
                             (lambda (c) (equal (plist-get c :new-module) key))
                             aaron-module-key-conflicts))))
      (puthash key result aaron-doctor-results)
      result)))

(defun aaron-doctor-run ()
  "Diagnose every declared module; return the number of modules with findings."
  (cl-loop for key in (aaron-module-list)
           for r = (aaron-doctor-module key)
           count (or (plist-get r :errors) (plist-get r :warnings)
                     (plist-get r :violations) (plist-get r :conflicts))))

(defun aaron-doctor-summary ()
  "Return totals of the last doctor run as a plist."
  (let ((errors 0) (warnings 0) (violations 0) (conflicts 0) (modules 0))
    (maphash (lambda (_key r)
               (cl-incf modules)
               (cl-incf errors (length (plist-get r :errors)))
               (cl-incf warnings (length (plist-get r :warnings)))
               (cl-incf violations (length (plist-get r :violations)))
               (cl-incf conflicts (length (plist-get r :conflicts))))
             aaron-doctor-results)
    (list :modules modules :errors errors :warnings warnings
          :violations violations :conflicts conflicts)))

(defun aaron-doctor-report-string ()
  "Run the doctor and return a plain-text report."
  (aaron-doctor-run)
  (with-temp-buffer
    (dolist (key (aaron-module-list))
      (let ((r (gethash key aaron-doctor-results)))
        (when (or (plist-get r :errors) (plist-get r :warnings)
                  (plist-get r :violations) (plist-get r :conflicts))
          (insert (format "%s %s\n" (car key) (cdr key)))
          (dolist (e (plist-get r :errors)) (insert "  error: " e "\n"))
          (dolist (w (plist-get r :warnings)) (insert "  warning: " w "\n"))
          (pcase-dolist (`(,id ,file ,line ,_text) (plist-get r :violations))
            (insert (format "  policy %s: %s:%d\n" id (abbreviate-file-name file) line)))
          (dolist (c (plist-get r :conflicts))
            (insert (format "  key %s: %S (%s %s) replaced by %S\n"
                            (plist-get c :key) (plist-get c :old)
                            (car (plist-get c :old-module)) (cdr (plist-get c :old-module))
                            (plist-get c :new)))))))
    (let ((s (aaron-doctor-summary)))
      (insert (format "\n%d modules: %d errors, %d warnings, %d policy hits, %d key conflicts\n"
                      (plist-get s :modules) (plist-get s :errors) (plist-get s :warnings)
                      (plist-get s :violations) (plist-get s :conflicts))))
    (buffer-string)))

(provide 'aaron-doctor)
;;; aaron-doctor.el ends here
