;;; init-diagnostics-extra.el --- Extra diagnostics workflows -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'seq)
(require 'transient)

(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function my/problems-buffer "init-problems" ())
(declare-function my/problems-project "init-problems" ())
(declare-function my/problems-project-full "init-problems" ())
(declare-function my/problems-project-errors "init-problems" ())
(declare-function my/problems-project-warnings "init-problems" ())
(declare-function my/problems-project-notes "init-problems" ())
(declare-function my/diagnostics-buffer-ui "init-diagnostics-ui")
(declare-function my/diagnostics-mode "init-diagnostics-ui")
(declare-function my/diagnostics-open "init-diagnostics-ui" (&optional filter origin-buffer))
(declare-function my/diagnostics-project-ui "init-diagnostics-ui")
(declare-function my/diagnostics-refresh "init-diagnostics-ui")

(defgroup my/diagnostics-extra nil
  "Extra diagnostics helpers."
  :group 'tools)

(defvar my/diagnostics--modeline-cookie '(:eval (my/diagnostics-modeline-string))
  "Mode line cookie used by `my/diagnostics-modeline-mode'.")

(defvar my/diagnostics--refresh-timer nil
  "Idle timer used to coalesce diagnostics UI refreshes.")

(defvar my/diagnostics--pending-refresh-sources nil
  "Source buffers awaiting a visible diagnostics panel refresh.")

(defvar my/diagnostics-modeline-mode)

(defvar-local my/diagnostics--modeline-cache nil
  "Cached diagnostics mode line string for the current buffer.")

(defvar-local my/diagnostics--modeline-cache-tick nil
  "Buffer modification tick used by `my/diagnostics--modeline-cache'.")

(defvar-local my/diagnostics--modeline-cache-size nil
  "Buffer size used by `my/diagnostics--modeline-cache'.")

(defvar my/diagnostics-modeline-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'my/diagnostics-buffer-ui)
    (define-key map [mode-line C-mouse-1] #'my/diagnostics-project-ui)
    (define-key map [mode-line mouse-3] #'diagnostics-menu)
    map)
  "Mouse keymap used by the diagnostics mode-line segment.")

(defun my/diagnostics--counts (&optional diags)
  "Return diagnostic counts for DIAGS or the current buffer."
  (let ((errors 0)
        (warnings 0)
        (notes 0))
    (dolist (diag (or diags (flymake-diagnostics)))
      (pcase (flymake--lookup-type-property
              (flymake-diagnostic-type diag)
              'flymake-category)
        ('flymake-error (setq errors (1+ errors)))
        ('flymake-warning (setq warnings (1+ warnings)))
        (_ (setq notes (1+ notes)))))
    (list :error errors :warning warnings :note notes)))

(defun my/diagnostics--format-modeline-counts (counts)
  "Return a compact mode line string for diagnostic COUNTS."
  (pcase-let* ((errors (plist-get counts :error))
               (warnings (plist-get counts :warning))
               (notes (plist-get counts :note)))
    (when (> (+ errors warnings notes) 0)
      (propertize
       (concat
        " Fly:"
        (when (> errors 0)
          (propertize (format " E%s" errors) 'face 'error))
        (when (> warnings 0)
          (propertize (format " W%s" warnings) 'face 'warning))
        (when (> notes 0)
          (propertize (format " N%s" notes) 'face 'success)))
       'mouse-face 'mode-line-highlight
       'local-map my/diagnostics-modeline-map
       'keymap my/diagnostics-modeline-map
       'help-echo "Diagnostics\nmouse-1: buffer diagnostics\nC-mouse-1: project diagnostics\nmouse-3: diagnostics menu"))))

(defun my/diagnostics--clear-modeline-cache (&rest _)
  "Clear cached diagnostics modeline state in the current buffer."
  (setq-local my/diagnostics--modeline-cache nil
              my/diagnostics--modeline-cache-tick nil
              my/diagnostics--modeline-cache-size nil))

(defun my/diagnostics--clear-modeline-cache-a (&rest _)
  "Clear diagnostics modeline cache after Flymake publishes diagnostics."
  (my/diagnostics--clear-modeline-cache)
  (force-mode-line-update))

(defun my/diagnostics-modeline-string ()
  "Return a compact Flymake status string for the mode line."
  (when (and my/diagnostics-modeline-mode
             (bound-and-true-p flymake-mode))
    (let ((tick (buffer-chars-modified-tick))
          (size (buffer-size)))
      (unless (and (eql tick my/diagnostics--modeline-cache-tick)
                   (eql size my/diagnostics--modeline-cache-size))
        (setq-local my/diagnostics--modeline-cache-tick tick
                    my/diagnostics--modeline-cache-size size)
        (setq-local my/diagnostics--modeline-cache
                    (my/diagnostics--format-modeline-counts
                     (my/diagnostics--counts))))
      my/diagnostics--modeline-cache)))

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

(with-eval-after-load 'flymake
  (when (fboundp 'flymake--publish-diagnostics)
    (advice-remove 'flymake--publish-diagnostics
                   #'my/diagnostics--clear-modeline-cache-a)
    (advice-add 'flymake--publish-diagnostics
                :after #'my/diagnostics--clear-modeline-cache-a)))

(defun my/diagnostics--refresh-open-buffers (&optional sources)
  "Refresh visible diagnostics buffers for SOURCES and their mode lines.
When SOURCES is nil, refresh every visible diagnostics buffer."
  (dolist (buffer (buffer-list))
    (when (get-buffer-window buffer t)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'my/diagnostics-mode)
                   (or (null sources)
                       (memq my/diagnostics-origin-buffer sources)))
          (let* ((windows (get-buffer-window-list buffer nil t))
                 (positions
                  (mapcar (lambda (window)
                            (list window (window-point window)
                                  (window-start window)))
                          windows)))
            (ignore-errors
              (my/diagnostics-refresh))
            (dolist (position positions)
              (pcase-let ((`(,window ,point ,start) position))
                (when (and (window-live-p window)
                           (eq (window-buffer window) buffer))
                  (set-window-point window (min point (point-max)))
                  (set-window-start window (min start (point-max)) t)))))))))
  (force-mode-line-update t))

(defun my/diagnostics--open-buffer-p (&optional source)
  "Return non-nil when a diagnostics UI buffer for SOURCE is visible."
  (seq-some
   (lambda (buffer)
     (and (get-buffer-window buffer t)
          (with-current-buffer buffer
            (and (derived-mode-p 'my/diagnostics-mode)
                 (or (null source)
                     (eq my/diagnostics-origin-buffer source))))))
   (buffer-list)))

(defun my/diagnostics--schedule-refresh-open-buffers (&rest _)
  "Coalesce diagnostics buffer refreshes after Flymake saves."
  (let ((source (current-buffer)))
    (when (my/diagnostics--open-buffer-p source)
      (add-to-list 'my/diagnostics--pending-refresh-sources source)
      (unless (timerp my/diagnostics--refresh-timer)
        (setq my/diagnostics--refresh-timer
              (run-with-idle-timer
               0.12 nil
               (lambda ()
                 (let ((sources my/diagnostics--pending-refresh-sources))
                   (setq my/diagnostics--refresh-timer nil
                         my/diagnostics--pending-refresh-sources nil)
                   (my/diagnostics--refresh-open-buffers sources)))))))))

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
      (setq my/diagnostics--refresh-timer nil))
    (setq my/diagnostics--pending-refresh-sources nil)))

(defun my/diagnostics-buffer-errors ()
  "Open a diagnostics buffer filtered to current-buffer errors."
  (interactive)
  (my/diagnostics-open :error))

(defun my/diagnostics-buffer-warnings ()
  "Open a diagnostics buffer filtered to current-buffer warnings."
  (interactive)
  (my/diagnostics-open :warning))

(defun my/diagnostics-buffer-notes ()
  "Open a diagnostics buffer filtered to current-buffer notes."
  (interactive)
  (my/diagnostics-open :note))

(defun my/diagnostics-project-errors ()
  "Pick an error from open files in the current project."
  (interactive)
  (my/problems-project-errors))

(defun my/diagnostics-project-warnings ()
  "Pick a warning from open files in the current project."
  (interactive)
  (my/problems-project-warnings))

(defun my/diagnostics-project-notes ()
  "Pick a note from open files in the current project."
  (interactive)
  (my/problems-project-notes))

(transient-define-prefix diagnostics-menu ()
  "Open diagnostic pickers, panels, and Flymake project results."
  [["Jump"
    ("!" "buffer picker" my/problems-buffer)
    ("?" "open-file project picker" my/problems-project)]
   ["Panel"
    ("b" "buffer panel" my/diagnostics-buffer-ui)
    ("e" "buffer errors" my/diagnostics-buffer-errors)
    ("w" "buffer warnings" my/diagnostics-buffer-warnings)
    ("n" "buffer notes" my/diagnostics-buffer-notes)]
   ["Project"
    ("p" "Flymake project list" my/diagnostics-project-ui)
    ("E" "open-file errors" my/diagnostics-project-errors)
    ("W" "open-file warnings" my/diagnostics-project-warnings)
    ("N" "open-file notes" my/diagnostics-project-notes)]
   ["Display"
    ("m" "toggle modeline" my/diagnostics-modeline-mode)
    ("a" "toggle auto refresh" my/diagnostics-auto-refresh-mode)]])

(my/leader!
  "c m" '(:def diagnostics-menu :which-key "diagnostics menu")
  "c ?" '(:def diagnostics-menu :which-key "diagnostics"))

(my/diagnostics-modeline-mode 1)
(my/diagnostics-auto-refresh-mode 1)

(provide 'init-diagnostics-extra)
;;; init-diagnostics-extra.el ends here
