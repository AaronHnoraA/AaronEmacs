;;; init-diagnostics-ui.el --- Persistent diagnostics buffers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'project)
(require 'seq)

(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-buffer "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-start "flymake" ())
(declare-function flymake--project-diagnostics "flymake" (project))
(declare-function my/diagnostics-dispatch "init-diagnostics-extra" ())

(defconst my/diagnostics-project-buffer-name "*Diagnostics: Project*"
  "Buffer name used by the toggleable project diagnostics panel.")

(defvar my/diagnostics-ui--theme-signature nil
  "Last theme signature applied by `my/diagnostics-apply-ui'.")

(defvar-local my/diagnostics-buffer-scope 'buffer
  "Scope used by the current diagnostics UI buffer.")

(defvar-local my/diagnostics-buffer-filter nil
  "Optional severity filter for the current diagnostics buffer.")

(defvar-local my/diagnostics-buffer-file-scope nil
  "Optional file scope for the current diagnostics buffer.

When nil, show all files.  When `current', show only the origin file.
When `other', show only other files.")

(defvar-local my/diagnostics-buffer-title nil
  "Readable title for the current diagnostics buffer.")

(defvar-local my/diagnostics-origin-buffer nil
  "Source buffer used as the current-file reference for this diagnostics UI.")

(defvar my/diagnostics-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'my/diagnostics-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "a") #'my/diagnostics-show-all-files)
    (define-key map (kbd "c") #'my/diagnostics-show-current-file)
    (define-key map (kbd "o") #'my/diagnostics-show-other-files)
    (define-key map (kbd "d") #'my/diagnostics-clear-severity-filter)
    (define-key map (kbd "e") #'my/diagnostics-show-errors)
    (define-key map (kbd "w") #'my/diagnostics-show-warnings)
    (define-key map (kbd "n") #'my/diagnostics-show-notes)
    (define-key map (kbd "x") #'my/diagnostics-rerun)
    (define-key map (kbd "?") #'my/diagnostics-dispatch)
    (define-key map (kbd "C-c ?") #'my/diagnostics-dispatch)
    map)
  "Keymap for `my/diagnostics-mode'.")

(define-derived-mode my/diagnostics-mode special-mode "Diagnostics"
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
          (set-face-attribute 'flymake-error nil
                              :underline '(:style line :color "#bf7f7f")
                              :foreground 'unspecified))
        (when (facep 'flymake-warning)
          (set-face-attribute 'flymake-warning nil
                              :underline '(:style line :color "#d0a86e")
                              :foreground 'unspecified))
        (when (facep 'flymake-note)
          (set-face-attribute 'flymake-note nil
                              :underline '(:style line :color "#8aa6c1")
                              :foreground 'unspecified))
        (when (facep 'error)
          (set-face-attribute 'error nil :foreground "#d79a9a"))
        (when (facep 'warning)
          (set-face-attribute 'warning nil :foreground "#d8b27f"))
        (when (facep 'success)
          (set-face-attribute 'success nil :foreground "#8fbf8f"))))))

(add-hook 'after-init-hook #'my/diagnostics-apply-ui)
(add-hook 'server-after-make-frame-hook #'my/diagnostics-apply-ui)
(add-hook 'after-load-theme-hook #'my/diagnostics-apply-ui)

(defun my/diagnostics--severity-rank (diag)
  "Return a sort rank for DIAG severity."
  (pcase (flymake-diagnostic-type diag)
    (:error 0)
    (:warning 1)
    (:note 2)
    (_ 3)))

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

(defun my/diagnostics--buffer-id (buffer)
  "Return a stable identity string for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (and buffer-file-name
               (expand-file-name buffer-file-name))
          (format "buffer:%s" (buffer-name))))))

(defun my/diagnostics--origin-buffer-name ()
  "Return a readable name for the origin buffer."
  (if (buffer-live-p my/diagnostics-origin-buffer)
      (with-current-buffer my/diagnostics-origin-buffer
        (abbreviate-file-name
         (or buffer-file-name
             (buffer-name))))
    "unknown"))

(defun my/diagnostics--origin-project-root ()
  "Return the project root used by the diagnostics buffer."
  (when (buffer-live-p my/diagnostics-origin-buffer)
    (with-current-buffer my/diagnostics-origin-buffer
      (when-let* ((project (project-current nil default-directory)))
        (abbreviate-file-name (project-root project))))))

(defun my/diagnostics--diag-in-origin-buffer-p (diag)
  "Return non-nil when DIAG belongs to the origin buffer."
  (let ((origin-id (my/diagnostics--buffer-id my/diagnostics-origin-buffer)))
    (and origin-id
         (equal origin-id
                (my/diagnostics--buffer-id
                 (flymake-diagnostic-buffer diag))))))

(defun my/diagnostics--collect-buffer ()
  "Collect diagnostics for the current buffer."
  (flymake-diagnostics))

(defun my/diagnostics--collect-project ()
  "Collect diagnostics for the current project."
  (when-let* ((project (project-current nil default-directory)))
    (if (fboundp 'flymake--project-diagnostics)
        (flymake--project-diagnostics project)
      (my/diagnostics--collect-buffer))))

(defun my/diagnostics--apply-file-scope (diags)
  "Apply the active file scope to DIAGS."
  (pcase my/diagnostics-buffer-file-scope
    ('current (seq-filter #'my/diagnostics--diag-in-origin-buffer-p diags))
    ('other (seq-remove #'my/diagnostics--diag-in-origin-buffer-p diags))
    (_ diags)))

(defun my/diagnostics--apply-filter (diags)
  "Apply the active severity and file filters to DIAGS."
  (let ((scoped (my/diagnostics--apply-file-scope diags)))
    (if my/diagnostics-buffer-filter
        (seq-filter (lambda (diag)
                      (eq (flymake-diagnostic-type diag)
                          my/diagnostics-buffer-filter))
                    scoped)
      scoped)))

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
      (pcase (flymake-diagnostic-type diag)
        (:error (setq errors (1+ errors)))
        (:warning (setq warnings (1+ warnings)))
        (_ (setq notes (1+ notes)))))
    (format "E%d W%d N%d"
            errors warnings notes)))

(defun my/diagnostics--file-scope-label ()
  "Return the current file-scope label."
  (pcase my/diagnostics-buffer-file-scope
    ('current "current file")
    ('other "other files")
    (_ "all files")))

(defun my/diagnostics--severity-filter-label ()
  "Return the current severity filter label."
  (pcase my/diagnostics-buffer-filter
    (:error "errors")
    (:warning "warnings")
    (:note "notes")
    (_ "all severities")))

(defun my/diagnostics--insert-action-button (label command help)
  "Insert an action button with LABEL, COMMAND, and HELP."
  (insert-text-button
   label
   'help-echo help
   'follow-link t
   'action (lambda (_button)
             (call-interactively command))))

(defun my/diagnostics--insert-toolbar ()
  "Insert the diagnostics toolbar."
  (insert (propertize (format "%s\n" my/diagnostics-buffer-title)
                      'face 'bold))
  (insert (format "Current: %s\n" (my/diagnostics--origin-buffer-name)))
  (when-let* ((root (and (eq my/diagnostics-buffer-scope 'project)
                         (my/diagnostics--origin-project-root))))
    (insert (format "Project: %s\n" root)))
  (insert (format "View: %s, %s\n"
                  (my/diagnostics--file-scope-label)
                  (my/diagnostics--severity-filter-label)))
  (insert "Actions: ")
  (if (eq my/diagnostics-buffer-scope 'project)
      (progn
        (my/diagnostics--insert-action-button "[a] all" #'my/diagnostics-show-all-files
                                              "Show diagnostics from all files")
        (insert " ")
        (my/diagnostics--insert-action-button "[c] current" #'my/diagnostics-show-current-file
                                              "Show diagnostics from the current file")
        (insert " ")
        (my/diagnostics--insert-action-button "[o] other" #'my/diagnostics-show-other-files
                                              "Show diagnostics from other files")
        (insert " "))
    (my/diagnostics--insert-action-button "[a] all" #'my/diagnostics-show-all-files
                                          "Show all diagnostics in this buffer")
    (insert " "))
  (my/diagnostics--insert-action-button "[d] all severities"
                                        #'my/diagnostics-clear-severity-filter
                                        "Clear the severity filter")
  (insert " ")
  (my/diagnostics--insert-action-button "[e] errors" #'my/diagnostics-show-errors
                                        "Show only errors")
  (insert " ")
  (my/diagnostics--insert-action-button "[w] warnings" #'my/diagnostics-show-warnings
                                        "Show only warnings")
  (insert " ")
  (my/diagnostics--insert-action-button "[n] notes" #'my/diagnostics-show-notes
                                        "Show only notes")
  (insert " ")
  (my/diagnostics--insert-action-button "[g] refresh" #'my/diagnostics-refresh
                                        "Refresh diagnostics")
  (insert " ")
  (my/diagnostics--insert-action-button "[x] recheck" #'my/diagnostics-rerun
                                        "Run Flymake again in the source buffer")
  (insert " ")
  (my/diagnostics--insert-action-button "[q] close" #'quit-window
                                        "Close the diagnostics window")
  (insert "\n\n"))

(defun my/diagnostics--insert-diag-button (diag)
  "Insert a button for DIAG."
  (let* ((buffer (flymake-diagnostic-buffer diag))
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
               (pop-to-buffer buffer)
               (goto-char beg)
               (recenter)))))

(defun my/diagnostics--insert-section (title diags empty-message)
  "Insert TITLE for DIAGS or EMPTY-MESSAGE when DIAGS is empty."
  (insert (propertize
           (format "%s (%s, %d)\n" title
                   (my/diagnostics--counts-string diags)
                   (length diags))
           'face 'bold))
  (if diags
      (dolist (diag diags)
        (my/diagnostics--insert-diag-button diag))
    (insert (format "%s\n" empty-message)))
  (insert "\n"))

(defun my/diagnostics-refresh ()
  "Refresh the diagnostics list."
  (interactive)
  (let ((inhibit-read-only t)
        (diags (my/diagnostics--sort
                (my/diagnostics--apply-filter
                 (pcase my/diagnostics-buffer-scope
                   ('project (my/diagnostics--collect-project))
                   (_ (my/diagnostics--collect-buffer)))))))
    (erase-buffer)
    (my/diagnostics--insert-toolbar)
    (pcase my/diagnostics-buffer-scope
      ('project
       (let* ((current (seq-filter #'my/diagnostics--diag-in-origin-buffer-p diags))
              (others (seq-remove #'my/diagnostics--diag-in-origin-buffer-p diags)))
         (pcase my/diagnostics-buffer-file-scope
           ('current
            (my/diagnostics--insert-section "Current File" current
                                            "No diagnostics in the current file."))
           ('other
            (my/diagnostics--insert-section "Other Files" others
                                            "No diagnostics in other files."))
           (_
            (my/diagnostics--insert-section "Current File" current
                                            "No diagnostics in the current file.")
            (my/diagnostics--insert-section "Other Files" others
                                            "No diagnostics in other files.")))))
      (_
       (my/diagnostics--insert-section "Current Buffer" diags
                                       "No diagnostics.")))
    (goto-char (point-min))))

(defun my/diagnostics-open (scope &optional filter title origin-buffer file-scope buffer-name)
  "Open a persistent diagnostics buffer for SCOPE, FILTER, and TITLE.

ORIGIN-BUFFER is used as the reference for current-file grouping.
FILE-SCOPE limits display to `current', `other', or nil.
BUFFER-NAME overrides the generated buffer name."
  (let* ((title (or title
                    (format "%s diagnostics"
                            (capitalize (symbol-name scope)))))
         (origin (or origin-buffer
                     (window-buffer (selected-window))))
         (buffer (get-buffer-create
                  (or buffer-name
                      (format "*Diagnostics: %s*" title)))))
    (with-current-buffer buffer
      (my/diagnostics-mode)
      (setq-local my/diagnostics-buffer-scope scope)
      (setq-local my/diagnostics-buffer-filter filter)
      (setq-local my/diagnostics-buffer-file-scope file-scope)
      (setq-local my/diagnostics-buffer-title title)
      (setq-local my/diagnostics-origin-buffer origin)
      (setq-local default-directory
                  (with-current-buffer origin
                    default-directory))
      (my/diagnostics-refresh))
    (pop-to-buffer buffer)))

(defun my/diagnostics--close-visible-buffer (buffer)
  "Close all visible windows showing BUFFER.

Return non-nil when BUFFER was visible."
  (let ((windows (get-buffer-window-list buffer nil t)))
    (when windows
      (dolist (window windows)
        (when (window-live-p window)
          (quit-window nil window)))
      t)))

(defun my/diagnostics--set-view (file-scope filter)
  "Set FILE-SCOPE and FILTER for the current diagnostics buffer."
  (unless (derived-mode-p 'my/diagnostics-mode)
    (user-error "Not in a diagnostics buffer"))
  (setq-local my/diagnostics-buffer-file-scope file-scope)
  (setq-local my/diagnostics-buffer-filter filter)
  (my/diagnostics-refresh))

(defun my/diagnostics-show-all-files ()
  "Show diagnostics from all files."
  (interactive)
  (my/diagnostics--set-view nil my/diagnostics-buffer-filter))

(defun my/diagnostics-show-current-file ()
  "Show diagnostics from the current file."
  (interactive)
  (if (eq my/diagnostics-buffer-scope 'project)
      (my/diagnostics--set-view 'current my/diagnostics-buffer-filter)
    (user-error "Current-file filtering is only available in project diagnostics")))

(defun my/diagnostics-show-other-files ()
  "Show diagnostics from files other than the current file."
  (interactive)
  (if (eq my/diagnostics-buffer-scope 'project)
      (my/diagnostics--set-view 'other my/diagnostics-buffer-filter)
    (user-error "Other-file filtering is only available in project diagnostics")))

(defun my/diagnostics-clear-severity-filter ()
  "Show all severities."
  (interactive)
  (my/diagnostics--set-view my/diagnostics-buffer-file-scope nil))

(defun my/diagnostics-show-errors ()
  "Show only errors."
  (interactive)
  (my/diagnostics--set-view my/diagnostics-buffer-file-scope :error))

(defun my/diagnostics-show-warnings ()
  "Show only warnings."
  (interactive)
  (my/diagnostics--set-view my/diagnostics-buffer-file-scope :warning))

(defun my/diagnostics-show-notes ()
  "Show only notes."
  (interactive)
  (my/diagnostics--set-view my/diagnostics-buffer-file-scope :note))

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
  (my/diagnostics-open 'buffer))

(defun my/diagnostics-project-ui ()
  "Toggle the persistent diagnostics buffer for the current project."
  (interactive)
  (let ((buffer (get-buffer my/diagnostics-project-buffer-name))
        (origin (current-buffer)))
    (if (and buffer
             (my/diagnostics--close-visible-buffer buffer))
        (message "Closed project diagnostics")
      (my/diagnostics-open 'project
                           nil
                           "Project Diagnostics"
                           origin
                           nil
                           my/diagnostics-project-buffer-name))))

(my/evil-global-leader-set "c D" #'my/diagnostics-buffer-ui "diagnostics ui")
(my/evil-global-leader-set "c P" #'my/diagnostics-project-ui "project diagnostics ui")

(provide 'init-diagnostics-ui)
;;; init-diagnostics-ui.el ends here
