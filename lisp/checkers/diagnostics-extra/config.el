;;; checkers/diagnostics-extra/config.el --- Extra diagnostics workflows -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-funcs)
(require 'seq)
(require 'transient)

(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function +problems/buffer "checkers/problems/config" ())
(declare-function +problems/project "checkers/problems/config" ())
(declare-function +problems/project-full "checkers/problems/config" ())
(declare-function +problems/project-errors "checkers/problems/config" ())
(declare-function +problems/project-warnings "checkers/problems/config" ())
(declare-function +problems/project-notes "checkers/problems/config" ())
(declare-function +diagnostics/buffer-ui "checkers/diagnostics/config")
(declare-function +diagnostics-mode "checkers/diagnostics/config")
(declare-function +diagnostics-open "checkers/diagnostics/config" (&optional filter origin-buffer))
(declare-function +diagnostics/project-ui "checkers/diagnostics/config")
(declare-function +diagnostics/refresh "checkers/diagnostics/config")

(defgroup +diagnostics-extra nil
  "Extra diagnostics helpers."
  :group 'tools)

(defvar +diagnostics--modeline-cookie '(:eval (+diagnostics-modeline-string))
  "Mode line cookie used by `+diagnostics-modeline-mode'.")

(defvar +diagnostics--refresh-timer nil
  "Idle timer used to coalesce diagnostics UI refreshes.")

(defvar +diagnostics--pending-refresh-sources nil
  "Source buffers awaiting a visible diagnostics panel refresh.")

(defvar +diagnostics-modeline-mode)

(defvar-local +diagnostics--modeline-cache nil
  "Cached diagnostics mode line string for the current buffer.")

(defvar-local +diagnostics--modeline-cache-tick nil
  "Buffer modification tick used by `+diagnostics--modeline-cache'.")

(defvar-local +diagnostics--modeline-cache-size nil
  "Buffer size used by `+diagnostics--modeline-cache'.")

(defvar +diagnostics-modeline-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'+diagnostics/buffer-ui)
    (define-key map [mode-line C-mouse-1] #'+diagnostics/project-ui)
    (define-key map [mode-line mouse-3] #'diagnostics-menu)
    map)
  "Mouse keymap used by the diagnostics mode-line segment.")

(defun +diagnostics--counts (&optional diags)
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

(defun +diagnostics--format-modeline-counts (counts)
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
       'local-map +diagnostics-modeline-map
       'keymap +diagnostics-modeline-map
       'help-echo "Diagnostics\nmouse-1: buffer diagnostics\nC-mouse-1: project diagnostics\nmouse-3: diagnostics menu"))))

(defun +diagnostics--clear-modeline-cache (&rest _)
  "Clear cached diagnostics modeline state in the current buffer."
  (setq-local +diagnostics--modeline-cache nil
              +diagnostics--modeline-cache-tick nil
              +diagnostics--modeline-cache-size nil))

(defun +diagnostics--clear-modeline-cache-a (&rest _)
  "Clear diagnostics modeline cache after Flymake publishes diagnostics."
  (+diagnostics--clear-modeline-cache)
  (force-mode-line-update))

(defun +diagnostics-modeline-string ()
  "Return a compact Flymake status string for the mode line."
  (when (and +diagnostics-modeline-mode
             (bound-and-true-p flymake-mode))
    (let ((tick (buffer-chars-modified-tick))
          (size (buffer-size)))
      (unless (and (eql tick +diagnostics--modeline-cache-tick)
                   (eql size +diagnostics--modeline-cache-size))
        (setq-local +diagnostics--modeline-cache-tick tick
                    +diagnostics--modeline-cache-size size)
        (setq-local +diagnostics--modeline-cache
                    (+diagnostics--format-modeline-counts
                     (+diagnostics--counts))))
      +diagnostics--modeline-cache)))

(define-minor-mode +diagnostics-modeline-mode
  "Show current Flymake counts in the mode line."
  :init-value t
  :global t
  :lighter nil
  (if +diagnostics-modeline-mode
      (unless (member +diagnostics--modeline-cookie mode-line-misc-info)
        (setq mode-line-misc-info
              (append mode-line-misc-info
                      (list +diagnostics--modeline-cookie))))
    (setq mode-line-misc-info
          (delete +diagnostics--modeline-cookie mode-line-misc-info)))
  (force-mode-line-update t))

(with-eval-after-load 'flymake
  (when (fboundp 'flymake--publish-diagnostics)
    (advice-remove 'flymake--publish-diagnostics
                   #'+diagnostics--clear-modeline-cache-a)
    (advice-add 'flymake--publish-diagnostics
                :after #'+diagnostics--clear-modeline-cache-a)))

(defun +diagnostics--refresh-open-buffers (&optional sources)
  "Refresh visible diagnostics buffers for SOURCES and their mode lines.
When SOURCES is nil, refresh every visible diagnostics buffer."
  (dolist (buffer (buffer-list))
    (when (get-buffer-window buffer t)
      (with-current-buffer buffer
        (when (and (derived-mode-p '+diagnostics-mode)
                   (or (null sources)
                       (memq +diagnostics-origin-buffer sources)))
          (let* ((windows (get-buffer-window-list buffer nil t))
                 (positions
                  (mapcar (lambda (window)
                            (list window (window-point window)
                                  (window-start window)))
                          windows)))
            (ignore-errors
              (+diagnostics/refresh))
            (dolist (position positions)
              (pcase-let ((`(,window ,point ,start) position))
                (when (and (window-live-p window)
                           (eq (window-buffer window) buffer))
                  (set-window-point window (min point (point-max)))
                  (set-window-start window (min start (point-max)) t)))))))))
  (force-mode-line-update t))

(defun +diagnostics--open-buffer-p (&optional source)
  "Return non-nil when a diagnostics UI buffer for SOURCE is visible."
  (seq-some
   (lambda (buffer)
     (and (get-buffer-window buffer t)
          (with-current-buffer buffer
            (and (derived-mode-p '+diagnostics-mode)
                 (or (null source)
                     (eq +diagnostics-origin-buffer source))))))
   (buffer-list)))

(defun +diagnostics--schedule-refresh-open-buffers (&rest _)
  "Coalesce diagnostics buffer refreshes after Flymake saves."
  (let ((source (current-buffer)))
    (when (+diagnostics--open-buffer-p source)
      (add-to-list '+diagnostics--pending-refresh-sources source)
      (unless (timerp +diagnostics--refresh-timer)
        (setq +diagnostics--refresh-timer
              (run-with-idle-timer
               0.12 nil
               (lambda ()
                 (let ((sources +diagnostics--pending-refresh-sources))
                   (setq +diagnostics--refresh-timer nil
                         +diagnostics--pending-refresh-sources nil)
                   (+diagnostics--refresh-open-buffers sources)))))))))

(define-minor-mode +diagnostics-auto-refresh-mode
  "Auto-refresh diagnostics buffers after Flymake saves."
  :init-value t
  :global t
  :lighter nil
  (if +diagnostics-auto-refresh-mode
      (add-hook 'flymake-after-save-hook #'+diagnostics--schedule-refresh-open-buffers)
    (remove-hook 'flymake-after-save-hook #'+diagnostics--schedule-refresh-open-buffers)
    (when (timerp +diagnostics--refresh-timer)
      (cancel-timer +diagnostics--refresh-timer)
      (setq +diagnostics--refresh-timer nil))
    (setq +diagnostics--pending-refresh-sources nil)))

(defun +diagnostics/buffer-errors ()
  "Open a diagnostics buffer filtered to current-buffer errors."
  (interactive)
  (+diagnostics-open :error))

(defun +diagnostics/buffer-warnings ()
  "Open a diagnostics buffer filtered to current-buffer warnings."
  (interactive)
  (+diagnostics-open :warning))

(defun +diagnostics/buffer-notes ()
  "Open a diagnostics buffer filtered to current-buffer notes."
  (interactive)
  (+diagnostics-open :note))

(defun +diagnostics/project-errors ()
  "Pick an error from open files in the current project."
  (interactive)
  (+problems/project-errors))

(defun +diagnostics/project-warnings ()
  "Pick a warning from open files in the current project."
  (interactive)
  (+problems/project-warnings))

(defun +diagnostics/project-notes ()
  "Pick a note from open files in the current project."
  (interactive)
  (+problems/project-notes))

(transient-define-prefix diagnostics-menu ()
  "Open diagnostic pickers, panels, and Flymake project results."
  [["Jump"
    ("!" "buffer picker" +problems/buffer)
    ("?" "open-file project picker" +problems/project)]
   ["Panel"
    ("b" "buffer panel" +diagnostics/buffer-ui)
    ("e" "buffer errors" +diagnostics/buffer-errors)
    ("w" "buffer warnings" +diagnostics/buffer-warnings)
    ("n" "buffer notes" +diagnostics/buffer-notes)]
   ["Project"
    ("p" "Flymake project list" +diagnostics/project-ui)
    ("E" "open-file errors" +diagnostics/project-errors)
    ("W" "open-file warnings" +diagnostics/project-warnings)
    ("N" "open-file notes" +diagnostics/project-notes)]
   ["Display"
    ("m" "toggle modeline" +diagnostics-modeline-mode)
    ("a" "toggle auto refresh" +diagnostics-auto-refresh-mode)]])

(define-leader-key!
  "c m" '(:def diagnostics-menu :which-key "diagnostics menu")
  "c ?" '(:def diagnostics-menu :which-key "diagnostics"))

(+diagnostics-modeline-mode 1)
(+diagnostics-auto-refresh-mode 1)

(provide 'checkers/diagnostics-extra/config)
;;; checkers/diagnostics-extra/config.el ends here
