;;; checkers/diagnostics/config.el --- Persistent diagnostics buffers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-ui-board)
(require 'flymake)
(require 'aaron-funcs)
(require 'aaron-open)
(require 'project)
(require 'seq)

(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-buffer "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-start "flymake" ())
(declare-function flymake--lookup-type-property "flymake" (type property &optional default))
(declare-function flymake--project-diagnostics-buffer "flymake" (root))
(declare-function diagnostics-menu "checkers/diagnostics-extra/config" ())
(declare-function +problems/project-full "checkers/problems/config" ())
(declare-function +frame-short-id "ui/tabbar/config" (frame))

(defvar +diagnostics-ui--theme-signature nil
  "Last theme signature applied by `+diagnostics-apply-ui'.")

(defvar-local +diagnostics-buffer-filter nil
  "Optional severity filter for the current diagnostics buffer.")

(defvar-local +diagnostics-origin-buffer nil
  "Source buffer whose diagnostics this frame's panel shows.")

(defvar-local +diagnostics-owner-frame nil
  "Frame that owns this diagnostics panel.")

(defvar +diagnostics-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'+diagnostics/refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "d") #'+diagnostics/clear-severity-filter)
    (define-key map (kbd "e") #'+diagnostics/show-errors)
    (define-key map (kbd "w") #'+diagnostics/show-warnings)
    (define-key map (kbd "n") #'+diagnostics/show-notes)
    (define-key map (kbd "x") #'+diagnostics/rerun)
    (define-key map (kbd "?") #'diagnostics-menu)
    (define-key map (kbd "C-c ?") #'diagnostics-menu)
    map)
  "Keymap for `+diagnostics-mode'.")

(define-derived-mode +diagnostics-mode aaron-ui-board-mode "Diagnostics"
  "Major mode for persistent diagnostics lists.")

(defun +diagnostics-apply-ui ()
  "Apply local diagnostic faces."
  (when (display-graphic-p)
    (let ((signature (list custom-enabled-themes
                           (face-attribute 'default :background nil t)
                           (face-attribute 'default :foreground nil t))))
      (unless (equal signature +diagnostics-ui--theme-signature)
        (setq +diagnostics-ui--theme-signature signature)
        (when (facep 'flymake-error)
          (aaron-ui-set-face 'flymake-error
                             :underline '(:style line :color accent-red-soft)
                             :foreground 'unspecified))
        (when (facep 'flymake-warning)
          (aaron-ui-set-face 'flymake-warning
                             :underline '(:style line :color accent-orange)
                             :foreground 'unspecified))
        (when (facep 'flymake-note)
          (aaron-ui-set-face 'flymake-note
                             :underline '(:style line :color accent-cyan)
                             :foreground 'unspecified))
        (when (facep 'error)
          (aaron-ui-set-face 'error :foreground 'accent-red))
        (when (facep 'warning)
          (aaron-ui-set-face 'warning :foreground 'accent-yellow-soft))
        (when (facep 'success)
          (aaron-ui-set-face 'success :foreground 'accent-green-soft))))))

(add-hook 'after-init-hook #'+diagnostics-apply-ui)
(add-hook 'server-after-make-frame-hook #'+diagnostics-apply-ui)
(add-hook 'after-load-theme-hook #'+diagnostics-apply-ui)

(defun +diagnostics--severity-category (diag)
  "Return DIAG severity as `error', `warning', or `note'."
  (pcase (flymake--lookup-type-property
          (flymake-diagnostic-type diag)
          'flymake-category)
    ('flymake-error 'error)
    ('flymake-warning 'warning)
    (_ 'note)))

(defun +diagnostics--severity-rank (diag)
  "Return a sort rank for DIAG severity."
  (pcase (+diagnostics--severity-category diag)
    ('error 0)
    ('warning 1)
    ('note 2)
    (_ 3)))

(defun +diagnostics--severity-name (diag)
  "Return a display name for DIAG severity."
  (pcase (+diagnostics--severity-category diag)
    ('error "Error")
    ('warning "Warning")
    ('note "Note")
    (_ (format "%s" (flymake-diagnostic-type diag)))))

(defun +diagnostics--line-number (diag)
  "Return the source line for DIAG."
  (with-current-buffer (flymake-diagnostic-buffer diag)
    (line-number-at-pos (flymake-diagnostic-beg diag) t)))

(defun +diagnostics--source-label (diag)
  "Return a readable source label for DIAG."
  (with-current-buffer (flymake-diagnostic-buffer diag)
    (or buffer-file-name
        (buffer-name))))

(defun +diagnostics--origin-buffer-name ()
  "Return a readable name for the origin buffer."
  (if (buffer-live-p +diagnostics-origin-buffer)
      (with-current-buffer +diagnostics-origin-buffer
        (abbreviate-file-name
         (or buffer-file-name
             (buffer-name))))
    "unknown"))

(defun +diagnostics--with-origin-buffer (fn)
  "Call FN in `+diagnostics-origin-buffer'."
  (unless (buffer-live-p +diagnostics-origin-buffer)
    (user-error "The source buffer for this diagnostics panel is no longer available"))
  (with-current-buffer +diagnostics-origin-buffer
    (funcall fn)))

(defun +diagnostics--collect-buffer ()
  "Collect diagnostics for the origin buffer."
  (+diagnostics--with-origin-buffer
   (lambda ()
     (flymake-diagnostics))))

(defun +diagnostics--apply-filter (diags)
  "Apply the active severity filter to DIAGS."
  (if +diagnostics-buffer-filter
      (seq-filter (lambda (diag)
                    (eq (+diagnostics--severity-category diag)
                        (pcase +diagnostics-buffer-filter
                          (:error 'error)
                          (:warning 'warning)
                          (:note 'note)
                          (_ +diagnostics-buffer-filter))))
                  diags)
    diags))

(defun +diagnostics--sort (diags)
  "Return DIAGS sorted by severity, file, and position."
  (sort (copy-sequence diags)
        (lambda (a b)
          (let ((ra (+diagnostics--severity-rank a))
                (rb (+diagnostics--severity-rank b)))
            (if (/= ra rb)
                (< ra rb)
              (let ((sa (+diagnostics--source-label a))
                    (sb (+diagnostics--source-label b)))
                (if (not (string= sa sb))
                    (string-lessp sa sb)
                  (let ((la (+diagnostics--line-number a))
                        (lb (+diagnostics--line-number b)))
                    (if (/= la lb)
                        (< la lb)
                      (< (flymake-diagnostic-beg a)
                         (flymake-diagnostic-beg b)))))))))))

(defun +diagnostics--counts-string (diags)
  "Return a compact counts string for DIAGS."
  (let ((errors 0)
        (warnings 0)
        (notes 0))
    (dolist (diag diags)
      (pcase (+diagnostics--severity-category diag)
        ('error (setq errors (1+ errors)))
        ('warning (setq warnings (1+ warnings)))
        (_ (setq notes (1+ notes)))))
    (format "E%d W%d N%d"
            errors warnings notes)))

(defun +diagnostics--severity-filter-label ()
  "Return the current severity filter label."
  (pcase +diagnostics-buffer-filter
    (:error "errors")
    (:warning "warnings")
    (:note "notes")
    (_ "all severities")))

(defun +diagnostics--goto-location (file buffer-name position)
  "Visit FILE or BUFFER-NAME and move to POSITION."
  (let ((buffer (and buffer-name (get-buffer buffer-name)))
        (resolved (and file (aaron-open-resolve-file-name file))))
    (cond
     ((and buffer
           (buffer-live-p buffer)
           (or (null resolved)
               (with-current-buffer buffer
                 (and buffer-file-name
                      (equal (aaron-open-resolve-file-name buffer-file-name)
                             resolved)))))
      (pop-to-buffer buffer))
     (resolved
      (aaron-open-visit-file resolved))
     (t
      (user-error "Diagnostic source buffer is no longer available"))))
  (goto-char position)
  (recenter))

(defun +diagnostics--insert-toolbar ()
  "Insert the diagnostics toolbar."
  (aaron-ui-board-insert-page-header
   "Buffer diagnostics"
   :icon 'diagnostics
   :subtitle (format "Current: %s  View: %s"
                     (+diagnostics--origin-buffer-name)
                     (+diagnostics--severity-filter-label)))
  (insert "   ")
  (aaron-ui-board-insert-action "All Sev"   #'+diagnostics/clear-severity-filter "All severities")
  (insert " ")
  (aaron-ui-board-insert-action "Errors"    #'+diagnostics/show-errors    "Errors only")
  (insert " ")
  (aaron-ui-board-insert-action "Warnings"  #'+diagnostics/show-warnings  "Warnings only")
  (insert " ")
  (aaron-ui-board-insert-action "Notes"     #'+diagnostics/show-notes     "Notes only")
  (insert " ")
  (aaron-ui-board-insert-action "Recheck"   #'+diagnostics/rerun          "Run Flymake again")
  (insert "\n\n"))

(defun +diagnostics--insert-diag-button (diag)
  "Insert a button for DIAG."
  (let* ((buffer (flymake-diagnostic-buffer diag))
         (file (with-current-buffer buffer
                 (and buffer-file-name
                      (expand-file-name buffer-file-name))))
         (buffer-name (buffer-name buffer))
         (beg (flymake-diagnostic-beg diag))
         (severity (+diagnostics--severity-name diag))
         (source (abbreviate-file-name (+diagnostics--source-label diag)))
         (line (+diagnostics--line-number diag))
         (text (flymake-diagnostic-text diag))
         (label (format "%-8s %s:%d  %s\n" severity source line text)))
    (insert-text-button
     label
     'follow-link t
     'action (lambda (_button)
               (+diagnostics--goto-location file buffer-name beg)))))

(defun +diagnostics--insert-section (title diags empty-message)
  "Insert TITLE for DIAGS or EMPTY-MESSAGE when DIAGS is empty."
  (aaron-ui-board-insert-section
   (format "%s (%s)" title (+diagnostics--counts-string diags))
   (length diags)
   (cond
    ((seq-find (lambda (d) (eq 'error (+diagnostics--severity-category d))) diags) 'danger)
    ((seq-find (lambda (d) (eq 'warning (+diagnostics--severity-category d))) diags) 'warning)
    (diags 'info)
    (t 'muted)))
  (if diags
      (dolist (diag diags)
        (+diagnostics--insert-diag-button diag))
    (aaron-ui-board-insert-empty empty-message))
  (insert "\n"))

(defun +diagnostics/refresh ()
  "Refresh the diagnostics list."
  (interactive)
  (let ((inhibit-read-only t)
        (diags (+diagnostics--sort
                (+diagnostics--apply-filter
                 (+diagnostics--collect-buffer)))))
    (erase-buffer)
    (+diagnostics--insert-toolbar)
    (+diagnostics--insert-section "Current Buffer" diags
                                    "No diagnostics.")
    (goto-char (point-min))))

(defun +diagnostics--clear-origin-buffer-references ()
  "Close diagnostics panels owned by the buffer being killed."
  (let ((origin (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p '+diagnostics-mode)
                     (eq +diagnostics-origin-buffer origin))
            (setq-local +diagnostics-origin-buffer nil)
            (quit-windows-on buffer t)))))))

(defun +diagnostics--frame-deleted (frame)
  "Release diagnostics panels owned by FRAME as it closes."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (and (derived-mode-p '+diagnostics-mode)
                      (eq +diagnostics-owner-frame frame))))
      (kill-buffer buffer))))

(add-hook 'delete-frame-functions #'+diagnostics--frame-deleted)

(defun +diagnostics-open (&optional filter origin-buffer)
  "Open this frame's buffer diagnostics panel, optionally showing FILTER.
ORIGIN-BUFFER overrides the current editing buffer.  Reopening the panel for
the same source retains its severity filter unless FILTER is specified."
  (let* ((invoking (window-buffer (selected-window)))
         (origin
          (let ((candidate
                 (or origin-buffer
                     (with-current-buffer invoking
                       (if (derived-mode-p '+diagnostics-mode)
                           +diagnostics-origin-buffer
                         invoking)))))
            (unless (buffer-live-p candidate)
              (user-error "The source buffer for this diagnostics panel is gone"))
            candidate))
         (buffer (get-buffer-create
                  (format "*Diagnostics F%d*"
                          (+frame-short-id (selected-frame)))))
         (same-origin
          (with-current-buffer buffer
            (and (derived-mode-p '+diagnostics-mode)
                 (eq +diagnostics-origin-buffer origin)))))
    (with-current-buffer origin
      (add-hook 'kill-buffer-hook
                #'+diagnostics--clear-origin-buffer-references nil t))
    (with-current-buffer buffer
      (unless same-origin
        (+diagnostics-mode))
      (aaron-ui-board-set-header "Buffer diagnostics" 'diagnostics)
      (setq-local aaron-ui-board-refresh-function #'+diagnostics/refresh)
      (when (or filter (not same-origin))
        (setq-local +diagnostics-buffer-filter filter))
      (setq-local +diagnostics-origin-buffer origin)
      (setq-local +diagnostics-owner-frame (selected-frame))
      (setq-local default-directory
                  (with-current-buffer origin
                    default-directory))
      (+diagnostics/refresh))
    (pop-to-buffer buffer)))

(defun +diagnostics--close-visible-buffer (buffer)
  "Close windows in the current frame showing BUFFER.

Return non-nil when BUFFER was visible."
  (let ((windows (get-buffer-window-list buffer nil nil)))
    (when windows
      (dolist (window windows)
        (when (window-live-p window)
          (quit-window nil window)))
      t)))

(defun +diagnostics--set-view (filter)
  "Set the severity FILTER for the current diagnostics buffer."
  (unless (derived-mode-p '+diagnostics-mode)
    (user-error "Not in a diagnostics buffer"))
  (setq-local +diagnostics-buffer-filter filter)
  (+diagnostics/refresh))

(defun +diagnostics/clear-severity-filter ()
  "Show all severities."
  (interactive)
  (+diagnostics--set-view nil))

(defun +diagnostics/show-errors ()
  "Show only errors."
  (interactive)
  (+diagnostics--set-view :error))

(defun +diagnostics/show-warnings ()
  "Show only warnings."
  (interactive)
  (+diagnostics--set-view :warning))

(defun +diagnostics/show-notes ()
  "Show only notes."
  (interactive)
  (+diagnostics--set-view :note))

(defun +diagnostics/rerun ()
  "Run Flymake again in the origin buffer and refresh."
  (interactive)
  (unless (buffer-live-p +diagnostics-origin-buffer)
    (user-error "The source buffer for this diagnostics panel is no longer available"))
  (with-current-buffer +diagnostics-origin-buffer
    (unless (bound-and-true-p flymake-mode)
      (user-error "Flymake is not active in %s" (buffer-name)))
    (flymake-start))
  (+diagnostics/refresh))

(defun +diagnostics/buffer-ui ()
  "Open a persistent diagnostics buffer for the current buffer."
  (interactive)
  (+diagnostics-open))

(defun +diagnostics/project-ui ()
  "Toggle Emacs Flymake's diagnostics list for the current project."
  (interactive)
  (let* ((project (project-current nil default-directory)))
    (unless project
      (user-error "No project for project diagnostics"))
    (let ((buffer (flymake--project-diagnostics-buffer
                   (project-root project))))
      (if (+diagnostics--close-visible-buffer buffer)
          (message "Closed project diagnostics")
        (+problems/project-full)))))

(define-leader-key!
  "c D" '(:def +diagnostics/buffer-ui :which-key "diagnostics ui")
  "c P" '(:def +diagnostics/project-ui :which-key "Flymake project list"))

;; Popup rule for this module's buffers (see ui/popup).
(defvar +display-rules-bottom-size)
(set-popup-rule! "\\*Diagnostics:.*\\*" :select t :align 'below :size +display-rules-bottom-size :regexp t)

;; Boards start in Emacs state.
(set-evil-initial-state! '+diagnostics-mode 'emacs)

(provide 'checkers/diagnostics/config)
;;; checkers/diagnostics/config.el ends here
