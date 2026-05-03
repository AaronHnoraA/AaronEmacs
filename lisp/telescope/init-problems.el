;;; init-problems.el --- Diagnostics pickers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'seq)

(declare-function consult--jump-state "consult" ())
(declare-function consult--lookup-candidate "consult" (selected candidates &optional no-error))
(declare-function consult--read "consult" (table &rest options))
(declare-function consult--type-group "consult" (types))
(declare-function consult--type-narrow "consult" (types))
(declare-function consult-flymake--candidates "consult-flymake" (diags))
(declare-function flymake--lookup-type-property "flymake" (type property &optional default))
(declare-function flymake--project-diagnostics "flymake" (&optional project))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))

(defconst my/problems--narrow
  '((?e . "Error")
    (?w . "Warning")
    (?n . "Note"))
  "Narrow keys used by the diagnostics picker.")

(defun my/problems--severity-label (severity)
  "Return a display label for SEVERITY."
  (pcase severity
    (:error "Errors")
    (:warning "Warnings")
    (:note "Notes")
    (_ "Diagnostics")))

(defun my/problems--severity-category (diag)
  "Return DIAG severity as `error', `warning', or `note'."
  (pcase (flymake--lookup-type-property
          (flymake-diagnostic-type diag)
          'flymake-category)
    ('flymake-error 'error)
    ('flymake-warning 'warning)
    (_ 'note)))

(defun my/problems--filter-diags (diags severity)
  "Return DIAGS filtered by SEVERITY when non-nil."
  (let ((category (pcase severity
                    (:error 'error)
                    (:warning 'warning)
                    (:note 'note)
                    (_ nil))))
    (if category
        (seq-filter (lambda (diag)
                      (eq (my/problems--severity-category diag) category))
                    diags)
      diags)))

(defun my/problems--collect (scope)
  "Collect diagnostics for SCOPE."
  (pcase scope
    ('project
     (when-let* ((project (project-current nil default-directory)))
       (if (fboundp 'flymake--project-diagnostics)
           (flymake--project-diagnostics project)
         (flymake-diagnostics))))
    (_
     (unless (bound-and-true-p flymake-mode)
       (user-error "Flymake is not active in the current buffer"))
     (flymake-diagnostics))))

(defun my/problems--read (scope &optional severity)
  "Read a diagnostic from SCOPE, optionally filtered by SEVERITY."
  (require 'consult)
  (require 'consult-flymake)
  (consult--read
   (consult-flymake--candidates
    (my/problems--filter-diags
     (my/problems--collect scope)
     severity))
   :prompt (format "%s %s: "
                   (pcase scope
                     ('project "Project")
                     (_ "Buffer"))
                   (downcase (my/problems--severity-label severity)))
   :category 'consult-flymake-error
   :history t
   :require-match t
   :sort nil
   :group (consult--type-group my/problems--narrow)
   :narrow (consult--type-narrow my/problems--narrow)
   :lookup #'consult--lookup-candidate
   :state (consult--jump-state)))

(defun my/problems-buffer ()
  "Show diagnostics for the current buffer with preview."
  (interactive)
  (my/problems--read 'buffer))

(defun my/problems-project ()
  "Show diagnostics for the current project with preview."
  (interactive)
  (my/problems--read 'project))

(defun my/problems-buffer-errors ()
  "Show current-buffer errors with preview."
  (interactive)
  (my/problems--read 'buffer :error))

(defun my/problems-buffer-warnings ()
  "Show current-buffer warnings with preview."
  (interactive)
  (my/problems--read 'buffer :warning))

(defun my/problems-buffer-notes ()
  "Show current-buffer notes with preview."
  (interactive)
  (my/problems--read 'buffer :note))

(defun my/problems-project-errors ()
  "Show project errors with preview."
  (interactive)
  (my/problems--read 'project :error))

(defun my/problems-project-warnings ()
  "Show project warnings with preview."
  (interactive)
  (my/problems--read 'project :warning))

(defun my/problems-project-notes ()
  "Show project notes with preview."
  (interactive)
  (my/problems--read 'project :note))

(my/leader!
  "c !" '(:def my/problems-buffer :which-key "buffer problems"))

(provide 'init-problems)
;;; init-problems.el ends here
