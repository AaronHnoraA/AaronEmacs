;;; init-diagnostics-ui.el --- Persistent diagnostics buffers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-ui-board)
(require 'flymake)
(require 'init-funcs)
(require 'init-open)
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
(declare-function diagnostics-menu "init-diagnostics-extra" ())
(declare-function my/problems-project-full "init-problems" ())
(declare-function my/frame-short-id "init-tabbar" (frame))

(defvar my/diagnostics-ui--theme-signature nil
  "Last theme signature applied by `my/diagnostics-apply-ui'.")

(defvar-local my/diagnostics-buffer-filter nil
  "Optional severity filter for the current diagnostics buffer.")

(defvar-local my/diagnostics-origin-buffer nil
  "Source buffer whose diagnostics this frame's panel shows.")

(defvar-local my/diagnostics-owner-frame nil
  "Frame that owns this diagnostics panel.")

(defvar my/diagnostics-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'my/diagnostics-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "d") #'my/diagnostics-clear-severity-filter)
    (define-key map (kbd "e") #'my/diagnostics-show-errors)
    (define-key map (kbd "w") #'my/diagnostics-show-warnings)
    (define-key map (kbd "n") #'my/diagnostics-show-notes)
    (define-key map (kbd "x") #'my/diagnostics-rerun)
    (define-key map (kbd "?") #'diagnostics-menu)
    (define-key map (kbd "C-c ?") #'diagnostics-menu)
    map)
  "Keymap for `my/diagnostics-mode'.")

(define-derived-mode my/diagnostics-mode aaron-ui-board-mode "Diagnostics"
  "Major mode for persistent diagnostics lists.")

(defun my/diagnostics-apply-ui ()
  "Apply local diagnostic faces."
  (when (display-graphic-p)
    (let ((signature (list custom-enabled-themes
                           (face-attribute 'default :background nil t)
                           (face-attribute 'default :foreground nil t))))
      (unless (equal signature my/diagnostics-ui--theme-signature)
        (setq my/diagnostics-ui--theme-signature signature)
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

(add-hook 'after-init-hook #'my/diagnostics-apply-ui)
(add-hook 'server-after-make-frame-hook #'my/diagnostics-apply-ui)
(add-hook 'after-load-theme-hook #'my/diagnostics-apply-ui)

(defun my/diagnostics--severity-category (diag)
  "Return DIAG severity as `error', `warning', or `note'."
  (pcase (flymake--lookup-type-property
          (flymake-diagnostic-type diag)
          'flymake-category)
    ('flymake-error 'error)
    ('flymake-warning 'warning)
    (_ 'note)))

(defun my/diagnostics--severity-rank (diag)
  "Return a sort rank for DIAG severity."
  (pcase (my/diagnostics--severity-category diag)
    ('error 0)
    ('warning 1)
    ('note 2)
    (_ 3)))

(defun my/diagnostics--severity-name (diag)
  "Return a display name for DIAG severity."
  (pcase (my/diagnostics--severity-category diag)
    ('error "Error")
    ('warning "Warning")
    ('note "Note")
    (_ (format "%s" (flymake-diagnostic-type diag)))))

(defun my/diagnostics--line-number (diag)
  "Return the source line for DIAG."
  (with-current-buffer (flymake-diagnostic-buffer diag)
    (line-number-at-pos (flymake-diagnostic-beg diag) t)))

(defun my/diagnostics--source-label (diag)
  "Return a readable source label for DIAG."
  (with-current-buffer (flymake-diagnostic-buffer diag)
    (or buffer-file-name
        (buffer-name))))

(defun my/diagnostics--origin-buffer-name ()
  "Return a readable name for the origin buffer."
  (if (buffer-live-p my/diagnostics-origin-buffer)
      (with-current-buffer my/diagnostics-origin-buffer
        (abbreviate-file-name
         (or buffer-file-name
             (buffer-name))))
    "unknown"))

(defun my/diagnostics--with-origin-buffer (fn)
  "Call FN in `my/diagnostics-origin-buffer'."
  (unless (buffer-live-p my/diagnostics-origin-buffer)
    (user-error "The source buffer for this diagnostics panel is no longer available"))
  (with-current-buffer my/diagnostics-origin-buffer
    (funcall fn)))

(defun my/diagnostics--collect-buffer ()
  "Collect diagnostics for the origin buffer."
  (my/diagnostics--with-origin-buffer
   (lambda ()
     (flymake-diagnostics))))

(defun my/diagnostics--apply-filter (diags)
  "Apply the active severity filter to DIAGS."
  (if my/diagnostics-buffer-filter
      (seq-filter (lambda (diag)
                    (eq (my/diagnostics--severity-category diag)
                        (pcase my/diagnostics-buffer-filter
                          (:error 'error)
                          (:warning 'warning)
                          (:note 'note)
                          (_ my/diagnostics-buffer-filter))))
                  diags)
    diags))

(defun my/diagnostics--sort (diags)
  "Return DIAGS sorted by severity, file, and position."
  (sort (copy-sequence diags)
        (lambda (a b)
          (let ((ra (my/diagnostics--severity-rank a))
                (rb (my/diagnostics--severity-rank b)))
            (if (/= ra rb)
                (< ra rb)
              (let ((sa (my/diagnostics--source-label a))
                    (sb (my/diagnostics--source-label b)))
                (if (not (string= sa sb))
                    (string-lessp sa sb)
                  (let ((la (my/diagnostics--line-number a))
                        (lb (my/diagnostics--line-number b)))
                    (if (/= la lb)
                        (< la lb)
                      (< (flymake-diagnostic-beg a)
                         (flymake-diagnostic-beg b)))))))))))

(defun my/diagnostics--counts-string (diags)
  "Return a compact counts string for DIAGS."
  (let ((errors 0)
        (warnings 0)
        (notes 0))
    (dolist (diag diags)
      (pcase (my/diagnostics--severity-category diag)
        ('error (setq errors (1+ errors)))
        ('warning (setq warnings (1+ warnings)))
        (_ (setq notes (1+ notes)))))
    (format "E%d W%d N%d"
            errors warnings notes)))

(defun my/diagnostics--severity-filter-label ()
  "Return the current severity filter label."
  (pcase my/diagnostics-buffer-filter
    (:error "errors")
    (:warning "warnings")
    (:note "notes")
    (_ "all severities")))

(defun my/diagnostics--goto-location (file buffer-name position)
  "Visit FILE or BUFFER-NAME and move to POSITION."
  (let ((buffer (and buffer-name (get-buffer buffer-name)))
        (resolved (and file (my/open-resolve-file-name file))))
    (cond
     ((and buffer
           (buffer-live-p buffer)
           (or (null resolved)
               (with-current-buffer buffer
                 (and buffer-file-name
                      (equal (my/open-resolve-file-name buffer-file-name)
                             resolved)))))
      (pop-to-buffer buffer))
     (resolved
      (my/open-visit-file resolved))
     (t
      (user-error "Diagnostic source buffer is no longer available"))))
  (goto-char position)
  (recenter))

(defun my/diagnostics--insert-toolbar ()
  "Insert the diagnostics toolbar."
  (aaron-ui-board-insert-page-header
   "Buffer diagnostics"
   :icon 'diagnostics
   :subtitle (format "Current: %s  View: %s"
                     (my/diagnostics--origin-buffer-name)
                     (my/diagnostics--severity-filter-label)))
  (insert "   ")
  (aaron-ui-board-insert-action "All Sev"   #'my/diagnostics-clear-severity-filter "All severities")
  (insert " ")
  (aaron-ui-board-insert-action "Errors"    #'my/diagnostics-show-errors    "Errors only")
  (insert " ")
  (aaron-ui-board-insert-action "Warnings"  #'my/diagnostics-show-warnings  "Warnings only")
  (insert " ")
  (aaron-ui-board-insert-action "Notes"     #'my/diagnostics-show-notes     "Notes only")
  (insert " ")
  (aaron-ui-board-insert-action "Recheck"   #'my/diagnostics-rerun          "Run Flymake again")
  (insert "\n\n"))

(defun my/diagnostics--insert-diag-button (diag)
  "Insert a button for DIAG."
  (let* ((buffer (flymake-diagnostic-buffer diag))
         (file (with-current-buffer buffer
                 (and buffer-file-name
                      (expand-file-name buffer-file-name))))
         (buffer-name (buffer-name buffer))
         (beg (flymake-diagnostic-beg diag))
         (severity (my/diagnostics--severity-name diag))
         (source (abbreviate-file-name (my/diagnostics--source-label diag)))
         (line (my/diagnostics--line-number diag))
         (text (flymake-diagnostic-text diag))
         (label (format "%-8s %s:%d  %s\n" severity source line text)))
    (insert-text-button
     label
     'follow-link t
     'action (lambda (_button)
               (my/diagnostics--goto-location file buffer-name beg)))))

(defun my/diagnostics--insert-section (title diags empty-message)
  "Insert TITLE for DIAGS or EMPTY-MESSAGE when DIAGS is empty."
  (aaron-ui-board-insert-section
   (format "%s (%s)" title (my/diagnostics--counts-string diags))
   (length diags)
   (cond
    ((seq-find (lambda (d) (eq 'error (my/diagnostics--severity-category d))) diags) 'danger)
    ((seq-find (lambda (d) (eq 'warning (my/diagnostics--severity-category d))) diags) 'warning)
    (diags 'info)
    (t 'muted)))
  (if diags
      (dolist (diag diags)
        (my/diagnostics--insert-diag-button diag))
    (aaron-ui-board-insert-empty empty-message))
  (insert "\n"))

(defun my/diagnostics-refresh ()
  "Refresh the diagnostics list."
  (interactive)
  (let ((inhibit-read-only t)
        (diags (my/diagnostics--sort
                (my/diagnostics--apply-filter
                 (my/diagnostics--collect-buffer)))))
    (erase-buffer)
    (my/diagnostics--insert-toolbar)
    (my/diagnostics--insert-section "Current Buffer" diags
                                    "No diagnostics.")
    (goto-char (point-min))))

(defun my/diagnostics--clear-origin-buffer-references ()
  "Close diagnostics panels owned by the buffer being killed."
  (let ((origin (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'my/diagnostics-mode)
                     (eq my/diagnostics-origin-buffer origin))
            (setq-local my/diagnostics-origin-buffer nil)
            (quit-windows-on buffer t)))))))

(defun my/diagnostics--frame-deleted (frame)
  "Release diagnostics panels owned by FRAME as it closes."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (and (derived-mode-p 'my/diagnostics-mode)
                      (eq my/diagnostics-owner-frame frame))))
      (kill-buffer buffer))))

(add-hook 'delete-frame-functions #'my/diagnostics--frame-deleted)

(defun my/diagnostics-open (&optional filter origin-buffer)
  "Open this frame's buffer diagnostics panel, optionally showing FILTER.
ORIGIN-BUFFER overrides the current editing buffer.  Reopening the panel for
the same source retains its severity filter unless FILTER is specified."
  (let* ((invoking (window-buffer (selected-window)))
         (origin
          (let ((candidate
                 (or origin-buffer
                     (with-current-buffer invoking
                       (if (derived-mode-p 'my/diagnostics-mode)
                           my/diagnostics-origin-buffer
                         invoking)))))
            (unless (buffer-live-p candidate)
              (user-error "The source buffer for this diagnostics panel is gone"))
            candidate))
         (buffer (get-buffer-create
                  (format "*Diagnostics F%d*"
                          (my/frame-short-id (selected-frame)))))
         (same-origin
          (with-current-buffer buffer
            (and (derived-mode-p 'my/diagnostics-mode)
                 (eq my/diagnostics-origin-buffer origin)))))
    (with-current-buffer origin
      (add-hook 'kill-buffer-hook
                #'my/diagnostics--clear-origin-buffer-references nil t))
    (with-current-buffer buffer
      (unless same-origin
        (my/diagnostics-mode))
      (aaron-ui-board-set-header "Buffer diagnostics" 'diagnostics)
      (setq-local aaron-ui-board-refresh-function #'my/diagnostics-refresh)
      (when (or filter (not same-origin))
        (setq-local my/diagnostics-buffer-filter filter))
      (setq-local my/diagnostics-origin-buffer origin)
      (setq-local my/diagnostics-owner-frame (selected-frame))
      (setq-local default-directory
                  (with-current-buffer origin
                    default-directory))
      (my/diagnostics-refresh))
    (pop-to-buffer buffer)))

(defun my/diagnostics--close-visible-buffer (buffer)
  "Close windows in the current frame showing BUFFER.

Return non-nil when BUFFER was visible."
  (let ((windows (get-buffer-window-list buffer nil nil)))
    (when windows
      (dolist (window windows)
        (when (window-live-p window)
          (quit-window nil window)))
      t)))

(defun my/diagnostics--set-view (filter)
  "Set the severity FILTER for the current diagnostics buffer."
  (unless (derived-mode-p 'my/diagnostics-mode)
    (user-error "Not in a diagnostics buffer"))
  (setq-local my/diagnostics-buffer-filter filter)
  (my/diagnostics-refresh))

(defun my/diagnostics-clear-severity-filter ()
  "Show all severities."
  (interactive)
  (my/diagnostics--set-view nil))

(defun my/diagnostics-show-errors ()
  "Show only errors."
  (interactive)
  (my/diagnostics--set-view :error))

(defun my/diagnostics-show-warnings ()
  "Show only warnings."
  (interactive)
  (my/diagnostics--set-view :warning))

(defun my/diagnostics-show-notes ()
  "Show only notes."
  (interactive)
  (my/diagnostics--set-view :note))

(defun my/diagnostics-rerun ()
  "Run Flymake again in the origin buffer and refresh."
  (interactive)
  (unless (buffer-live-p my/diagnostics-origin-buffer)
    (user-error "The source buffer for this diagnostics panel is no longer available"))
  (with-current-buffer my/diagnostics-origin-buffer
    (unless (bound-and-true-p flymake-mode)
      (user-error "Flymake is not active in %s" (buffer-name)))
    (flymake-start))
  (my/diagnostics-refresh))

(defun my/diagnostics-buffer-ui ()
  "Open a persistent diagnostics buffer for the current buffer."
  (interactive)
  (my/diagnostics-open))

(defun my/diagnostics-project-ui ()
  "Toggle Emacs Flymake's diagnostics list for the current project."
  (interactive)
  (let* ((project (project-current nil default-directory)))
    (unless project
      (user-error "No project for project diagnostics"))
    (let ((buffer (flymake--project-diagnostics-buffer
                   (project-root project))))
      (if (my/diagnostics--close-visible-buffer buffer)
          (message "Closed project diagnostics")
        (my/problems-project-full)))))

(my/leader!
  "c D" '(:def my/diagnostics-buffer-ui :which-key "diagnostics ui")
  "c P" '(:def my/diagnostics-project-ui :which-key "Flymake project list"))

(provide 'init-diagnostics-ui)
;;; init-diagnostics-ui.el ends here
