;;; init-md.el --- Open Markdown files in Noema -*- lexical-binding: t -*-

;;; Commentary:
;; Markdown editing is owned by Noema.  Emacs redirects opened Markdown
;; files into the local Noema Web bridge for interactive opens, and
;; falls back to a raw text buffer for programmatic callers (magit, grep, xref).

;;; Code:

(require 'config)

(require 'cl-lib)
(require 'init-open)
(require 'subr-x)
(require 'init-noema)

(declare-function evil-emacs-state "evil" ())

(my/package-ensure-vc
 'markdown-mode
 "https://github.com/jrblevin/markdown-mode.git"
 "f441e8bc9951e73b12c61e9198658488dd8e86e1")

(defgroup my/markdown nil
  "Markdown file handoff to Noema."
  :group 'my/noema)

(config-defvar my/noema-close-emacs-markdown-buffer nil
  "When non-nil, close the temporary Emacs buffer after opening Markdown in Noema."
  :type 'boolean
  :group 'my/markdown)

(config-defvar my/noema-markdown-default-surface nil
  "Which surface an interactive Markdown open lands in.

`emacs' opens the file in `gfm-mode', where the roam keymap on `C-c r',
the `SPC r' leader, the `noema-roam' xref backend and ordinary
`markdown-mode' fontification all work.  `app' hands the file to the Noema
web editor and leaves a read-only stub buffer behind.

Either way `my/noema-open-markdown-raw' forces Emacs and
`my/noema-open-in-app' forces Noema, so this only sets the default."
  :type '(choice (const :tag "Edit in Emacs" emacs)
                 (const :tag "Open in Noema" app))
  :group 'my/markdown)

(defconst my/noema-markdown-auto-mode-patterns
  '("README\\(?:\\.md\\)?\\'" "\\.markdown\\'" "\\.md\\'")
  "File patterns redirected from Emacs buffers into Noema.")

(defvar-local my/noema--markdown-redirected nil
  "Non-nil when this buffer has already been handed to Noema.")

(defvar-local my/noema--markdown-placeholder-state nil
  "Current opening state of a redirected Markdown placeholder.")

(defvar-local my/noema--markdown-open-generation 0
  "Identity of the latest opening request from this Markdown buffer.")

(defvar-local my/noema--markdown-open-timer nil
  "Timer starting Noema after `find-file' has shown this buffer.")

(defvar my/noema-markdown-placeholder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'my/noema-markdown-placeholder-retry)
    (define-key map (kbd "e") #'markdown-edit-raw)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keys available while a Markdown file waits for Noema.")

(define-derived-mode my/noema-markdown-placeholder-mode fundamental-mode
  "Noema-Wait"
  "Read-only Markdown placeholder while the Noema page is opening.")

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/noema-markdown-placeholder-mode 'emacs))

(defun my/noema--markdown-placeholder-set-state (state)
  "Set the current Markdown placeholder to STATE and update its help line."
  (setq-local my/noema--markdown-placeholder-state state
              header-line-format
              (pcase state
                ('starting " Noema is starting…  g retry · e edit in Emacs · q close")
                ('failed " Noema did not open  ·  g retry · e edit in Emacs · q close")
                ('waiting " Noema open paused  ·  g retry · e edit in Emacs · q close")
                (_ " Noema page opened  ·  g reopen · e edit in Emacs · q close"))))

(defun my/noema--markdown-placeholder-cancel-timer ()
  "Cancel this Markdown placeholder's pending start timer."
  (when (timerp my/noema--markdown-open-timer)
    (cancel-timer my/noema--markdown-open-timer))
  (setq-local my/noema--markdown-open-timer nil))

(defun my/noema--markdown-placeholder-open (buffer generation)
  "Open BUFFER in Noema if GENERATION is still current and visible."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local my/noema--markdown-open-timer nil)
      (when (and my/noema--markdown-redirected
                 (= generation my/noema--markdown-open-generation))
        (if-let* ((window (get-buffer-window buffer 'visible)))
            (condition-case err
                (my/noema-open-file buffer-file-name window)
              ((error quit)
               (my/noema--markdown-placeholder-set-state 'failed)
               (message "Noema could not open %s: %s"
                        (abbreviate-file-name buffer-file-name)
                        (error-message-string err))))
          (my/noema--markdown-placeholder-set-state 'waiting))))))

(defun my/noema--markdown-placeholder-schedule-open ()
  "Open this Markdown placeholder after its visit finishes."
  (my/noema--markdown-placeholder-cancel-timer)
  (cl-incf my/noema--markdown-open-generation)
  (my/noema--markdown-placeholder-set-state 'starting)
  (setq-local my/noema--markdown-open-timer
              (run-at-time 0 nil #'my/noema--markdown-placeholder-open
                           (current-buffer)
                           my/noema--markdown-open-generation)))

(defun my/noema-markdown-placeholder-retry ()
  "Retry opening this Markdown placeholder in Noema."
  (interactive)
  (unless (and my/noema--markdown-redirected buffer-file-name)
    (user-error "This is not a Noema Markdown placeholder"))
  (my/noema--markdown-placeholder-schedule-open))

(defvar my/noema--inhibit-redirect nil
  "Non-nil inhibits the markdown-to-Noema redirect.
Bind this to t around programmatic find-file calls (e.g. in magit hooks,
test helpers) where you want a raw Emacs buffer instead of the web editor.")

(defconst my/noema--redirect-commands
  '(find-file find-file-other-window find-file-other-frame
    find-file-literally find-alternate-file
    dired-find-file dired-find-file-other-window dired-view-file)
  "Interactive commands from which Markdown opens should redirect to Noema.")

(defun my/noema--should-redirect-p ()
  "Return non-nil when the current open warrants an Noema redirect."
  (and (not noninteractive)
       (not my/noema--inhibit-redirect)
       ;; A tool reading the file (Treemacs indexing headings) is not the
       ;; person opening it; redirecting would blank its buffer's outline.
       (not (bound-and-true-p my/programmatic-file-visit))
       (eq my/noema-markdown-default-surface 'app)
       ;; Block known programmatic callers that are not user-facing file opens.
       (not (memq this-command
                  '(magit-find-file magit-find-file-other-window
                    magit-diff-visit-file magit-diff-visit-worktree-file
                    xref-goto-xref
                    next-error previous-error
                    org-open-at-point)))))

(defun my/noema--markdown-auto-mode-entry-p (entry)
  "Return non-nil when ENTRY is a Markdown auto-mode entry."
  (and (consp entry)
       (let ((pattern (car entry)))
         (and (stringp pattern)
              (or (member pattern my/noema-markdown-auto-mode-patterns)
                  (string-match-p
                   "\\(?:markdown\\|md\\)"
                   pattern))))))

(defun my/noema--pin-markdown-redirect-mode ()
  "Keep Markdown file patterns routed to Noema ahead of other modes."
  (setq auto-mode-alist
        (append
         (mapcar (lambda (pattern)
                   (cons pattern #'my/noema-markdown-redirect-mode))
                 my/noema-markdown-auto-mode-patterns)
         (cl-remove-if #'my/noema--markdown-auto-mode-entry-p
                       auto-mode-alist))))

(defun my/noema--ensure-markdown-file (file)
  "Ensure Markdown FILE exists before Noema opens it."
  (let ((file (expand-file-name file)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (write-region "" nil file nil 'silent))
    file))

(defun my/noema--kill-redirected-markdown-buffer (buffer file)
  "Kill redirected Markdown BUFFER when it is still visiting FILE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (not (buffer-modified-p))
                 (file-equal-p buffer-file-name file))
        (kill-buffer buffer)))))

(defun my/noema--redirect-current-markdown-buffer ()
  "Open the current Markdown buffer in Noema and optionally close it."
  (let ((file buffer-file-name))
    (unless (and file (my/noema--markdown-file-p file))
      (user-error "Current buffer is not visiting a Markdown file"))
    (my/noema-markdown-placeholder-mode)
    ;; Changing major mode clears ordinary buffer-local variables.  Keep the
    ;; marker on the placeholder so a cold host can find its window later.
    (setq-local my/noema--markdown-redirected t)
    (setq-local buffer-read-only t)
    (setq-local quit-window-kill-buffer t)
    (add-hook 'kill-buffer-hook
              #'my/noema--markdown-placeholder-cancel-timer nil t)
    (my/noema--ensure-markdown-file file)
    ;; auto-mode runs in `find-file-noselect', before `find-file' displays the
    ;; buffer.  Even a ready host must wait until that native visit finishes.
    (my/noema--markdown-placeholder-schedule-open)))

(defun my/noema-redirect-markdown-file-h ()
  "Fallback Markdown handoff for packages that override `auto-mode-alist'."
  (cond
   ((derived-mode-p 'my/noema-markdown-placeholder-mode)
    ;; The global set-auto-mode advice restores the prior Evil state after
    ;; major-mode setup.  Find-file-hook runs later, when that restoration is
    ;; complete, so the placeholder's q/g/e keys win in actual key lookup.
    (when (bound-and-true-p evil-local-mode)
      (evil-emacs-state)))
   ((and buffer-file-name
         (my/noema--markdown-file-p buffer-file-name)
         (not my/noema--markdown-redirected)
         (my/noema--should-redirect-p))
    (my/noema--redirect-current-markdown-buffer)
    (when (bound-and-true-p evil-local-mode)
      (evil-emacs-state)))))

;;;###autoload
(defun my/noema-markdown-redirect-mode ()
  "Major-mode replacement that opens the current Markdown file in Noema.
For programmatic opens (magit, grep, org-link), falls back to a raw text mode."
  (interactive)
  (if (my/noema--should-redirect-p)
      (my/noema--redirect-current-markdown-buffer)
    ;; Programmatic open: use a raw markdown view without redirecting.
    (if (require 'markdown-mode nil t)
        (gfm-mode)
      (text-mode))))

;;;###autoload
(defun my/noema-open-markdown-raw (&optional file)
  "Open FILE (or the current note) as a raw Markdown buffer in Emacs.
Bypasses the Noema redirect.  Useful for diffing, fixing broken notes,
or editing when the web editor is unavailable."
  (interactive
   (list (read-file-name "Raw Markdown: " nil
                         (or (my/noema-buffer-file) buffer-file-name)
                         t)))
  (let ((my/noema--inhibit-redirect t)
        (target (my/open-resolve-file-name
                 (or file
                     (my/noema-buffer-file)
                     (and buffer-file-name
                          (my/noema--markdown-file-p buffer-file-name)
                          buffer-file-name)
                     (user-error "No Markdown file to open")))))
    (unless (my/noema--markdown-file-p target)
      (user-error "Not a Markdown file: %s" target))
    ;; Visiting an existing placeholder would otherwise return its inert,
    ;; read-only fundamental-mode buffer.  Close it and visit afresh as raw
    ;; Markdown; stale Noema callbacks are invalidated with that buffer.
    (when-let* ((placeholder (my/open-call-with-file-context
                             target #'find-buffer-visiting)))
      (when (buffer-local-value 'my/noema--markdown-redirected placeholder)
        (with-current-buffer placeholder
          (my/noema--markdown-placeholder-cancel-timer)
          (cl-incf my/noema--markdown-open-generation))
        (kill-buffer placeholder)))
    (my/open-visit-file target)
    ;; A copy an AI bridge opened for reading becomes the buffer asked for.
    (when (fboundp 'noema-md-bridge-claim)
      (noema-md-bridge-claim))))

(defalias 'markdown-edit-raw #'my/noema-open-markdown-raw
  "Open the current Markdown file for native Emacs editing.")


;;;###autoload
(defun my/noema-toggle-markdown-surface ()
  "Toggle where an interactive Markdown open lands, and report the new value."
  (interactive)
  (let ((next (if (eq my/noema-markdown-default-surface 'app) 'emacs 'app)))
    (config-set 'my/noema-markdown-default-surface next)
    (message "Markdown opens in %s"
             (if (eq next 'app) "Noema" "Emacs"))))

(my/noema--pin-markdown-redirect-mode)

;; `treesit-auto' adds its own Markdown entries during configuration.  Re-pin
;; the Noema handoff after that package loads so no grammar prompt runs.
(with-eval-after-load 'treesit-auto
  (my/noema--pin-markdown-redirect-mode))

(with-eval-after-load 'init-treesit
  (my/noema--pin-markdown-redirect-mode))

(add-hook 'after-init-hook #'my/noema--pin-markdown-redirect-mode 90)
(add-hook 'find-file-hook #'my/noema-redirect-markdown-file-h)

(provide 'init-md)

;;; init-md.el ends here
