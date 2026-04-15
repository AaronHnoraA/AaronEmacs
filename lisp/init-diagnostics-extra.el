;;; init-diagnostics-extra.el --- Extra diagnostics workflows -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'transient)

(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function my/problems-buffer "init-problems" ())
(declare-function my/problems-project "init-problems" ())
(declare-function my/diagnostics-buffer-ui "init-diagnostics-ui")
(declare-function my/diagnostics-mode "init-diagnostics-ui")
(declare-function my/diagnostics-open "init-diagnostics-ui" (scope &optional filter title))
(declare-function my/diagnostics-project-ui "init-diagnostics-ui")
(declare-function my/diagnostics-refresh "init-diagnostics-ui")

(defgroup my/diagnostics-extra nil
  "Extra diagnostics helpers."
  :group 'tools)

(defvar my/diagnostics--modeline-cookie '(:eval (my/diagnostics-modeline-string))
  "Mode line cookie used by `my/diagnostics-modeline-mode'.")

(defvar my/diagnostics--refresh-timer nil
  "Idle timer used to coalesce diagnostics UI refreshes.")

(defun my/diagnostics--counts (&optional diags)
  "Return diagnostic counts for DIAGS or the current buffer."
  (let ((errors 0)
        (warnings 0)
        (notes 0))
    (dolist (diag (or diags (flymake-diagnostics)))
      (pcase (flymake-diagnostic-type diag)
        (:error (setq errors (1+ errors)))
        (:warning (setq warnings (1+ warnings)))
        (_ (setq notes (1+ notes)))))
    (list :error errors :warning warnings :note notes)))

(defun my/diagnostics-modeline-string ()
  "Return a compact Flymake status string for the mode line."
  (when (and my/diagnostics-modeline-mode
             (bound-and-true-p flymake-mode))
    (pcase-let* ((counts (my/diagnostics--counts))
                 (errors (plist-get counts :error))
                 (warnings (plist-get counts :warning))
                 (notes (plist-get counts :note)))
      (when (> (+ errors warnings notes) 0)
        (concat
         " Fly:"
         (when (> errors 0)
           (propertize (format " E%s" errors) 'face 'error))
         (when (> warnings 0)
           (propertize (format " W%s" warnings) 'face 'warning))
         (when (> notes 0)
           (propertize (format " N%s" notes) 'face 'success)))))))

(define-minor-mode my/diagnostics-modeline-mode
  "Show current Flymake counts in the mode line."
  :init-value t
  :global t
  :lighter nil
  (if my/diagnostics-modeline-mode
      (unless (member my/diagnostics--modeline-cookie mode-line-misc-info)
        (setq mode-line-misc-info
              (append mode-line-misc-info
                      (list my/diagnostics--modeline-cookie))))
    (setq mode-line-misc-info
          (delete my/diagnostics--modeline-cookie mode-line-misc-info)))
  (force-mode-line-update t))

(defun my/diagnostics--refresh-open-buffers (&rest _)
  "Refresh open diagnostics buffers and mode lines."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'my/diagnostics-mode)
        (ignore-errors
          (my/diagnostics-refresh)))))
  (force-mode-line-update t))

(defun my/diagnostics--schedule-refresh-open-buffers (&rest _)
  "Coalesce diagnostics buffer refreshes after Flymake saves."
  (when (timerp my/diagnostics--refresh-timer)
    (cancel-timer my/diagnostics--refresh-timer))
  (setq my/diagnostics--refresh-timer
        (run-with-idle-timer
         0.12 nil
         (lambda ()
           (setq my/diagnostics--refresh-timer nil)
           (my/diagnostics--refresh-open-buffers)))))

(define-minor-mode my/diagnostics-auto-refresh-mode
  "Auto-refresh diagnostics buffers after Flymake saves."
  :init-value t
  :global t
  :lighter nil
  (if my/diagnostics-auto-refresh-mode
      (add-hook 'flymake-after-save-hook #'my/diagnostics--schedule-refresh-open-buffers)
    (remove-hook 'flymake-after-save-hook #'my/diagnostics--schedule-refresh-open-buffers)
    (when (timerp my/diagnostics--refresh-timer)
      (cancel-timer my/diagnostics--refresh-timer)
      (setq my/diagnostics--refresh-timer nil))))

(defun my/diagnostics-buffer-errors ()
  "Open a diagnostics buffer filtered to current-buffer errors."
  (interactive)
  (my/diagnostics-open 'buffer :error "Buffer Errors"))

(defun my/diagnostics-buffer-warnings ()
  "Open a diagnostics buffer filtered to current-buffer warnings."
  (interactive)
  (my/diagnostics-open 'buffer :warning "Buffer Warnings"))

(defun my/diagnostics-project-errors ()
  "Open a diagnostics buffer filtered to project errors."
  (interactive)
  (my/diagnostics-open 'project :error "Project Errors"))

(defun my/diagnostics-project-warnings ()
  "Open a diagnostics buffer filtered to project warnings."
  (interactive)
  (my/diagnostics-open 'project :warning "Project Warnings"))

(transient-define-prefix my/diagnostics-dispatch ()
  "Diagnostics workflow."
  [["Jump"
    ("!" "buffer picker" my/problems-buffer)
    ("?" "project picker" my/problems-project)]
   ["Panel"
    ("b" "buffer panel" my/diagnostics-buffer-ui)
    ("e" "buffer errors" my/diagnostics-buffer-errors)
    ("w" "buffer warnings" my/diagnostics-buffer-warnings)]
   ["Project"
    ("p" "project panel" my/diagnostics-project-ui)
    ("E" "project errors" my/diagnostics-project-errors)
    ("W" "project warnings" my/diagnostics-project-warnings)]
   ["Display"
    ("m" "toggle modeline" my/diagnostics-modeline-mode)
    ("a" "toggle auto refresh" my/diagnostics-auto-refresh-mode)]])

(my/evil-global-leader-set "c m" #'my/diagnostics-dispatch "diagnostics menu")
(my/evil-global-leader-set "c ?" #'my/diagnostics-dispatch "diagnostics")

(my/diagnostics-modeline-mode 1)
(my/diagnostics-auto-refresh-mode 1)

(provide 'init-diagnostics-extra)
;;; init-diagnostics-extra.el ends here
