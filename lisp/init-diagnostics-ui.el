;;; init-diagnostics-ui.el --- Persistent diagnostics buffers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-buffer "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake--project-diagnostics "flymake" (project))

(defvar-local my/diagnostics-buffer-scope 'buffer
  "Scope used by the current diagnostics UI buffer.")

(define-derived-mode my/diagnostics-mode special-mode "Diagnostics"
  "Major mode for persistent diagnostics lists.")

(defun my/diagnostics--severity-name (diag)
  "Return a display name for DIAG severity."
  (let ((type (flymake-diagnostic-type diag)))
    (cond
     ((eq type :error) "Error")
     ((eq type :warning) "Warning")
     ((eq type :note) "Note")
     (t (format "%s" type)))))

(defun my/diagnostics--line-number (diag)
  "Return the source line for DIAG."
  (with-current-buffer (flymake-diagnostic-buffer diag)
    (line-number-at-pos (flymake-diagnostic-beg diag) t)))

(defun my/diagnostics--source-label (diag)
  "Return a readable source label for DIAG."
  (with-current-buffer (flymake-diagnostic-buffer diag)
    (or buffer-file-name
        (buffer-name))))

(defun my/diagnostics--collect-buffer ()
  "Collect diagnostics for the current buffer."
  (flymake-diagnostics))

(defun my/diagnostics--collect-project ()
  "Collect diagnostics for the current project."
  (when-let* ((project (project-current nil default-directory)))
    (if (fboundp 'flymake--project-diagnostics)
        (flymake--project-diagnostics project)
      (my/diagnostics--collect-buffer))))

(defun my/diagnostics-refresh ()
  "Refresh the diagnostics list."
  (interactive)
  (let ((inhibit-read-only t)
        (diags (pcase my/diagnostics-buffer-scope
                 ('project (my/diagnostics--collect-project))
                 (_ (my/diagnostics--collect-buffer)))))
    (erase-buffer)
    (insert (format "%s diagnostics\n\n"
                    (capitalize (symbol-name my/diagnostics-buffer-scope))))
    (if diags
        (dolist (diag diags)
          (let* ((buffer (flymake-diagnostic-buffer diag))
                 (beg (flymake-diagnostic-beg diag))
                 (severity (my/diagnostics--severity-name diag))
                 (source (abbreviate-file-name (my/diagnostics--source-label diag)))
                 (line (my/diagnostics--line-number diag))
                 (text (flymake-diagnostic-text diag))
                 (label (format "%-8s %s:%d  %s\n" severity source line text)))
            (insert-text-button
             label
             'action (lambda (_)
                       (pop-to-buffer buffer)
                       (goto-char beg)
                       (recenter))
             'follow-link t)))
      (insert "No diagnostics.\n"))
    (goto-char (point-min))))

(defun my/diagnostics-open (scope)
  "Open a persistent diagnostics buffer for SCOPE."
  (let ((buffer (get-buffer-create
                 (format "*Diagnostics: %s*" (capitalize (symbol-name scope))))))
    (with-current-buffer buffer
      (my/diagnostics-mode)
      (setq-local my/diagnostics-buffer-scope scope)
      (use-local-map (copy-keymap special-mode-map))
      (local-set-key (kbd "g") #'my/diagnostics-refresh)
      (my/diagnostics-refresh))
    (pop-to-buffer buffer)))

(defun my/diagnostics-buffer-ui ()
  "Open a persistent diagnostics buffer for the current buffer."
  (interactive)
  (my/diagnostics-open 'buffer))

(defun my/diagnostics-project-ui ()
  "Open a persistent diagnostics buffer for the current project."
  (interactive)
  (my/diagnostics-open 'project))

(my/evil-global-leader-set "c D" #'my/diagnostics-buffer-ui "diagnostics ui")
(my/evil-global-leader-set "c P" #'my/diagnostics-project-ui "project diagnostics ui")

(provide 'init-diagnostics-ui)
;;; init-diagnostics-ui.el ends here
