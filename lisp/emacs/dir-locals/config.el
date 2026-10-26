;;; emacs/dir-locals/config.el --- Dir-locals management and templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Tools for creating, editing, merging, reloading, and silencing .dir-locals.el.
;; Templates live under templates/emacs/.
;;
;; Keybindings (under leader "p d"):
;;   e  edit .dir-locals.el
;;   c  create from template (replaces existing)
;;   m  merge a template into existing
;;   r  reload dir-locals for current buffer
;;   s  silence / approve unsafe local variables
;;   d  describe which entries apply to current buffer

;;; Code:

(require 'config)
(require 'aaron-open)

(require 'seq)
(require 'pp)
(require 'subr-x)

(declare-function +project-local-root "tools/project-local/config")

(defgroup +dir-locals nil
  "Dir-locals management tools."
  :group 'convenience)

(config-defvar +dir-locals-template-root nil
  "Directory containing .dir-locals.el templates."
  :type 'directory
  :group '+dir-locals)

;;; Internal helpers

(defun +dir-locals--root ()
  "Return the project root to use for dir-locals operations."
  (file-name-as-directory
   (aaron-open-resolve-file-name
    (if (fboundp '+project-local-root)
        (+project-local-root)
      (or (when-let* ((proj (project-current nil default-directory)))
            (project-root proj))
          default-directory)))))

(defun +dir-locals--file (&optional root)
  "Return the .dir-locals.el path under ROOT."
  (expand-file-name ".dir-locals.el" (or root (+dir-locals--root))))

(defun +dir-locals--read-file (file)
  "Read the dir-locals alist from FILE.  Return nil on empty or unreadable."
  (when (and (stringp file) (file-readable-p file))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (forward-comment (buffer-size))
          (unless (eobp) (read (current-buffer))))
      (error nil))))

(defun +dir-locals--templates ()
  "Return sorted list of template names (without .el) in `+dir-locals-template-root'."
  (when (file-directory-p +dir-locals-template-root)
    (sort (mapcar #'file-name-sans-extension
                  (seq-filter (lambda (f) (string-suffix-p ".el" f))
                              (directory-files +dir-locals-template-root nil
                                               directory-files-no-dot-files-regexp)))
          #'string<)))

(defun +dir-locals--template-path (name)
  "Return the full path for template NAME."
  (expand-file-name (concat name ".el") +dir-locals-template-root))

(defun +dir-locals--read-template-name ()
  "Prompt for a template name, completing from available templates."
  (let ((names (+dir-locals--templates)))
    (unless names
      (user-error "No templates found in %s" +dir-locals-template-root))
    (completing-read "Dir-locals template: " names nil t)))

;;; Merge helpers

(defun +dir-locals--merge-vars (base overlay)
  "Merge OVERLAY var alist into BASE; OVERLAY wins on duplicate keys."
  (let ((result (copy-tree base)))
    (dolist (entry overlay)
      (setq result (assq-delete-all (car entry) result))
      (setq result (append result (list entry))))
    result))

(defun +dir-locals--merge (base template)
  "Merge TEMPLATE dir-locals alist into BASE."
  (let ((result (copy-tree (or base nil))))
    (dolist (entry template)
      (let* ((mode (car entry))
             (vars (cdr entry))
             (existing (assq mode result)))
        (if existing
            (setcdr existing (+dir-locals--merge-vars (cdr existing) vars))
          (setq result (append result (list entry))))))
    result))

;;; Write

(defun +dir-locals--write (alist file)
  "Write ALIST as a pretty-printed dir-locals form to FILE."
  (with-temp-file file
    (insert ";; -*- mode: emacs-lisp; coding: utf-8 -*-\n")
    (insert ";; Directory Local Variables — see (info \"(emacs) Directory Variables\")\n\n")
    (let ((print-level nil)
          (print-length nil))
      (pp alist (current-buffer)))))

;;; Commands

;;;###autoload
(defun +dir-locals/edit ()
  "Open .dir-locals.el for the current project root."
  (interactive)
  (aaron-open-visit-file (aaron-open-resolve-file-name
                       (+dir-locals--file))))

;;;###autoload
(defun +dir-locals/create (&optional template-name)
  "Create .dir-locals.el from TEMPLATE-NAME, replacing any existing file."
  (interactive)
  (let* ((name (or template-name (+dir-locals--read-template-name)))
         (tmpl-file (+dir-locals--template-path name))
         (target (+dir-locals--file)))
    (unless (file-readable-p tmpl-file)
      (user-error "Template not found: %s" tmpl-file))
    (when (and (file-exists-p target)
               (not (y-or-n-p
                     (format ".dir-locals.el exists in %s. Replace? "
                             (abbreviate-file-name (+dir-locals--root))))))
      (user-error "Aborted"))
    (let ((tmpl-alist (+dir-locals--read-file tmpl-file)))
      (unless tmpl-alist
        (user-error "Template '%s' is empty or could not be read" name))
      (+dir-locals--write tmpl-alist target))
    (aaron-open-visit-file (aaron-open-resolve-file-name target))
    (message "Created .dir-locals.el from template '%s'" name)))

;;;###autoload
(defun +dir-locals/merge-template (&optional template-name)
  "Merge TEMPLATE-NAME into the current project's .dir-locals.el."
  (interactive)
  (let* ((name (or template-name (+dir-locals--read-template-name)))
         (tmpl-file (+dir-locals--template-path name))
         (target (+dir-locals--file)))
    (unless (file-readable-p tmpl-file)
      (user-error "Template not found: %s" tmpl-file))
    (let* ((tmpl-alist (+dir-locals--read-file tmpl-file))
           (base-alist (+dir-locals--read-file target))
           (merged (+dir-locals--merge base-alist tmpl-alist)))
      (unless tmpl-alist
        (user-error "Template '%s' is empty or could not be read" name))
      (+dir-locals--write merged target))
    (aaron-open-visit-file (aaron-open-resolve-file-name target))
    (message "Merged template '%s' into .dir-locals.el" name)))

;;;###autoload
(defun +dir-locals/reload ()
  "Re-apply dir-local variables and refresh project environment (direnv).

Runs `hack-dir-local-variables' + `hack-local-variables-apply' to push fresh
.dir-locals.el values into this buffer, then calls direnv so PATH and other
shell-level variables reflect the current .envrc."
  (interactive)
  (hack-dir-local-variables)
  (hack-local-variables-apply)
  (when (fboundp '+direnv-update-environment-maybe)
    (+direnv-update-environment-maybe))
  (message "Dir-locals reloaded and environment refreshed for %s" (buffer-name)))

;;;###autoload
(defun +dir-locals/silence ()
  "Add all dir-local variables for this project to `safe-local-variable-values'.

Skips `eval' entries.  Saves to custom file."
  (interactive)
  (let* ((file (+dir-locals--file))
         (alist (+dir-locals--read-file file))
         (added 0))
    (unless alist
      (user-error "No .dir-locals.el found at %s"
                  (abbreviate-file-name (+dir-locals--root))))
    (dolist (mode-entry alist)
      (dolist (var-entry (cdr mode-entry))
        (let ((var (car var-entry))
              (val (cdr var-entry)))
          (when (and (not (eq var 'eval))
                     (not (safe-local-variable-p var val))
                     (not (member (cons var val) safe-local-variable-values)))
            (add-to-list 'safe-local-variable-values (cons var val))
            (setq added (1+ added))))))
    (if (zerop added)
        (message "All dir-local variables are already marked safe.")
      (customize-save-variable 'safe-local-variable-values safe-local-variable-values)
      (message "Marked %d variable(s) as safe (eval forms skipped)." added))))

;;;###autoload
(defun +dir-locals/describe ()
  "Show which dir-local entries apply to the current buffer."
  (interactive)
  (let* ((root (+dir-locals--root))
         (file (+dir-locals--file root))
         (alist (+dir-locals--read-file file)))
    (if (null alist)
        (message "No .dir-locals.el in %s" (abbreviate-file-name root))
      (let ((applicable
             (seq-filter
              (lambda (entry)
                (let ((mode (car entry)))
                  (or (null mode)
                      (and (symbolp mode)
                           (or (eq mode major-mode)
                               (derived-mode-p mode))))))
              alist)))
        (with-help-window "*Dir-locals*"
          (princ (format "Project: %s\n" (abbreviate-file-name root)))
          (princ (format "Mode:    %s\n\n" major-mode))
          (if (null applicable)
              (princ "No dir-local entries apply to this buffer.\n")
            (dolist (entry applicable)
              (princ (format "── %s\n" (or (car entry) "nil (all modes)")))
              (dolist (var-entry (cdr entry))
                (princ (format "   %-40s %S\n" (car var-entry) (cdr var-entry))))
              (princ "\n")))
          (princ (format "File: %s\n" (abbreviate-file-name file))))))))

;;; Keybindings  ("p e" = project environment / dir-locals)

(define-leader-key!
  "p e"   '(:ignore t :which-key "dir-locals / env")
  "p e e" '(:def +dir-locals/edit           :which-key "edit")
  "p e c" '(:def +dir-locals/create         :which-key "create from template")
  "p e m" '(:def +dir-locals/merge-template :which-key "merge template")
  "p e r" '(:def +dir-locals/reload         :which-key "reload + direnv")
  "p e s" '(:def +dir-locals/silence        :which-key "silence/approve")
  "p e d" '(:def +dir-locals/describe       :which-key "describe"))

(provide 'emacs/dir-locals/config)
;;; emacs/dir-locals/config.el ends here
