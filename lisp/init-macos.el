;;; init-macos.el --- Tweaks for MacOS -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'browse-url)
(require 'subr-x)

(declare-function consult-buffer "consult" ())
(declare-function dape "dape" (config &optional skip-compile))
(declare-function comment-or-uncomment "init-base" ())
(declare-function duplicate-line-or-region-above "duplicate-line" (&optional reverse))
(declare-function duplicate-line-or-region-below "duplicate-line" ())
(declare-function dired-get-file-for-visit "dired" ())
(declare-function dired-jump "dired-x" (&optional other-window file-name))
(declare-function dired-jump-with-zoxide "init-zoxide" (&optional other-window))
(declare-function mc/edit-lines "multiple-cursors" ())
(declare-function mc/mark-all-like-this-dwim "multiple-cursors" ())
(declare-function mc/mark-next-like-this "multiple-cursors" ())
(declare-function mc/mark-previous-like-this "multiple-cursors" ())
(declare-function move-text-down "move-text" (arg))
(declare-function move-text-up "move-text" (arg))
(declare-function my/bookmark-jump-dwim "init-windows" ())
(declare-function my/code-actions-dispatch "init-code-actions" ())
(declare-function my/compile-dispatch "init-compile" ())
(declare-function my/contract-region "init-expand-region" ())
(declare-function my/debug-profile-dispatch "init-debug-profile" ())
(declare-function my/diagnostics-dispatch "init-diagnostics-extra" ())
(declare-function my/expand-region "init-expand-region" ())
(declare-function claude-code-ide-menu "claude-code-ide" ())
(declare-function my/health-dispatch "init-health" ())
(declare-function my/hyper-dispatch "init-hyper" ())
(declare-function my/hyper-execute-command "init-hyper" ())
(declare-function my/browser-current-url "init-browser" ())
(declare-function my/kill-buffer-dwim "init-windows" ())
(declare-function my/language-server-dispatch "init-lsp-tools" ())
(declare-function my/output-dispatch "init-output" ())
(declare-function magit-status "magit-status" (&optional directory))
(declare-function open-newline-above "open-newline" (arg))
(declare-function open-newline-below "open-newline" (arg))
(declare-function org-agenda "org" (&optional arg keys restriction))
(declare-function my/org-download-clipboard "init-org-utility" ())
(declare-function my/org-insert-id-link "init-org-utility" ())
(declare-function my/org-insert-target-link "init-org-utility" ())
(declare-function my/org-latex-preview-visible-now "init-org-latex" ())
(declare-function my/org-reference-create-target-dwim "init-org-utility" ())
(declare-function my/org-reference-ensure-target-at-point "init-org-utility" ())
(declare-function my/org-reference-insert-link "init-org-utility" ())
(declare-function my/org-toc-insert-or-update "init-org-core" (&optional depth))
(declare-function my/org-zotero-fill-metadata "init-org-zotero" ())
(declare-function popper-toggle "popper" ())
(declare-function my/project-dispatch "init-project" ())
(declare-function my/project-run-dispatch "init-project-run" ())
(declare-function my/test-dispatch "init-test" ())
(declare-function my/workspace-dispatch "init-workspaces" ())
(declare-function my/search-line-forward "init-search" ())
(declare-function my/search-open-recent-file "init-search" ())
(declare-function telescope "init-telescope" ())
(declare-function my/telescope-ripgrep "init-telescope" ())
(declare-function my/test-nearest "init-test" ())
(declare-function show-imenu "init-project" ())
(declare-function org-element-context "org-element" ())
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))

(autoload 'my/org-download-clipboard "init-org-utility" nil t)
(autoload 'my/org-insert-id-link "init-org-utility" nil t)
(autoload 'my/org-insert-target-link "init-org-utility" nil t)
(autoload 'my/org-latex-preview-visible-now "init-org-latex" nil t)
(autoload 'my/org-reference-create-target-dwim "init-org-utility" nil t)
(autoload 'my/org-reference-ensure-target-at-point "init-org-utility" nil t)
(autoload 'my/org-reference-insert-link "init-org-utility" nil t)
(autoload 'my/org-toc-insert-or-update "init-org-core" nil t)
(autoload 'my/org-zotero-fill-metadata "init-org-zotero" nil t)

(defvar my/macos-idle-gc-timer nil
  "Timer used to run a delayed GC after the UI goes idle on macOS.")

(defgroup my/macos nil
  "macOS-specific local integration."
  :group 'environment)

(defcustom my/macos-scroll-profile 'line
  "Default macOS scrolling profile.
`line' keeps `pixel-scroll-precision-mode' off and lets regular wheel
scrolling handle trackpad input, which costs much less during long Org
reading sessions.  `pixel' restores precision pixel scrolling for users who
prefer the smoother feel over lower power draw."
  :type '(choice (const :tag "Lower-power line scrolling" line)
                 (const :tag "Precision pixel scrolling" pixel))
  :group 'my/macos)

(defcustom my/macos-idle-gc-delay 8.0
  "Seconds to wait before the explicit macOS idle GC runs.
`gcmh' already manages ordinary idle GC.  This macOS hook is only a fallback
for focus-loss cleanup, so a longer delay avoids waking Emacs immediately
after short minibuffer or window-management commands."
  :type 'number
  :group 'my/macos)

(defcustom my/macos-gc-after-minibuffer nil
  "Whether exiting the minibuffer should schedule an explicit macOS idle GC.
This is disabled by default because `gcmh' already handles normal idle GC and
minibuffer exits are common during command-heavy work."
  :type 'boolean
  :group 'my/macos)

(defvar my/macos-use-transparent-titlebar nil
  "Whether macOS GUI frames should use a transparent titlebar.")

(defvar my/macos-startup-window-state 'fullscreen
  "Startup window state for macOS GUI frames.
Use nil for a regular window, `maximized' for a maximized window, or
`fullscreen' for a fullscreen window.")

(defvar pixel-scroll-precision-mode)

(defun my/macos-open--normalize-target (target)
  "Return TARGET in a form suitable for the macOS open command."
  (let* ((target (string-trim (substring-no-properties target)))
         (expanded (and (not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target))
                        (expand-file-name target))))
    (cond
     ((string-empty-p target) nil)
     ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target) target)
     ((and expanded (file-exists-p expanded)) expanded)
     ((and expanded (file-name-absolute-p target)) expanded)
     (t target))))

(defun my/macos-open-target (target)
  "Open TARGET with macOS open."
  (interactive "sOpen with macOS open: ")
  (unless (eq system-type 'darwin)
    (user-error "macOS open is only available on Darwin"))
  (unless (executable-find "open")
    (user-error "Cannot find macOS open command"))
  (if-let* ((target (my/macos-open--normalize-target target)))
      (let ((process-connection-type nil))
        (start-process "macos-open" nil "open" target)
        (message "open: %s" target))
    (user-error "No target to open")))

(defun my/macos-open-url (url)
  "Open URL with macOS open."
  (interactive (browse-url-interactive-arg "URL: "))
  (my/macos-open-target
   (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url)
       url
     (concat "https://" url))))

(defun my/macos-open-file (file)
  "Open FILE with macOS open."
  (interactive "fOpen file with macOS open: ")
  (my/macos-open-target file))

(defun my/macos-open--region-target ()
  "Return the active region as an open target, or nil."
  (when (use-region-p)
    (string-trim
     (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun my/macos-open--org-link-target ()
  "Return the Org link at point as an open target, or nil."
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-element-context))
    (let ((context (ignore-errors (org-element-context))))
      (when (eq (ignore-errors (org-element-type context)) 'link)
        (let ((link-type (org-element-property :type context))
              (path (org-element-property :path context)))
          (cond
           ((member link-type '("http" "https"))
            (concat link-type ":" path))
           ((equal link-type "file")
            (expand-file-name path))
           ((and link-type path
                 (not (member link-type '("custom-id" "fuzzy" "id"))))
            (concat link-type ":" path))))))))

(defun my/macos-open--target-at-point ()
  "Return a file or link target near point."
  (or (my/macos-open--region-target)
      (my/macos-open--org-link-target)
      (thing-at-point 'url t)
      (and (derived-mode-p 'dired-mode)
           (ignore-errors (dired-get-file-for-visit)))
      (let ((file (thing-at-point 'filename t)))
        (and file (file-exists-p (expand-file-name file)) file))
      (and (fboundp 'my/browser-current-url)
           (ignore-errors (my/browser-current-url)))
      buffer-file-name))

(defun my/macos-open-at-point ()
  "Open the file or link at point with macOS open.
When point has no obvious target, open the current browser URL or current
buffer file."
  (interactive)
  (if-let* ((target (my/macos-open--target-at-point)))
      (my/macos-open-target target)
    (call-interactively #'my/macos-open-target)))

(defun my/macos-schedule-idle-gc-after-focus-change ()
  "Queue idle GC when the selected frame loses focus."
  (unless (frame-focus-state)
    (my/macos-schedule-idle-gc)))

(defun my/macos-cancel-idle-gc-timer ()
  "Cancel the pending macOS idle GC timer."
  (when (timerp my/macos-idle-gc-timer)
    (cancel-timer my/macos-idle-gc-timer)
    (setq my/macos-idle-gc-timer nil)))

(defun my/macos-schedule-idle-gc ()
  "Run GC shortly after focus leaves Emacs or the minibuffer closes."
  (my/macos-cancel-idle-gc-timer)
  (setq my/macos-idle-gc-timer
        (run-with-idle-timer my/macos-idle-gc-delay nil
                             (lambda ()
                               (setq my/macos-idle-gc-timer nil)
                               (garbage-collect)))))

(defun my/macos-apply-scroll-profile (&optional profile)
  "Apply macOS scroll PROFILE.
When PROFILE is nil, use `my/macos-scroll-profile'."
  (let ((profile (or profile my/macos-scroll-profile)))
    (setq fast-but-imprecise-scrolling t
          redisplay-skip-fontification-on-input t)
    (when (boundp 'pixel-scroll-precision-interpolate-page)
      (setq pixel-scroll-precision-interpolate-page nil))
    (when (boundp 'pixel-scroll-precision-use-momentum)
      (setq pixel-scroll-precision-use-momentum nil))
    (when (fboundp 'pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode
       (if (eq profile 'pixel) 1 -1)))))

(defun my/macos-toggle-pixel-scroll-precision (&optional arg)
  "Toggle macOS precision pixel scrolling.
With positive ARG, enable it.  With zero or negative ARG, disable it."
  (interactive "P")
  (setq my/macos-scroll-profile
        (if (if arg
                (> (prefix-numeric-value arg) 0)
              (not (bound-and-true-p pixel-scroll-precision-mode)))
            'pixel
          'line))
  (my/macos-apply-scroll-profile my/macos-scroll-profile)
  (message "macOS scroll profile: %s" my/macos-scroll-profile))

(defun my/macos-apply-performance-tweaks ()
  "Apply macOS tuning that approximates the best parts of `emacs-plus'."
  ;; Keep NS process IO in larger chunks. This improves throughput for
  ;; LSP/Lean, terminals and other chatty subprocesses on macOS.
  (setq-default process-adaptive-read-buffering nil
                read-process-output-max (max read-process-output-max
                                             (* 4 1024 1024)))
  (setq process-adaptive-read-buffering (default-value 'process-adaptive-read-buffering)
        read-process-output-max (default-value 'read-process-output-max))

  ;; These are already good defaults elsewhere in the config, but keeping the
  ;; macOS-specific rendering knobs here makes the platform tuning explicit.
  (setq auto-window-vscroll nil
        ffap-machine-p-known 'reject
        fast-but-imprecise-scrolling t
        garbage-collection-messages nil
        redisplay-skip-fontification-on-input t)
  (setq use-dialog-box nil
        use-file-dialog nil)

  (when (boundp 'ns-use-proxy-icon)
    (setq ns-use-proxy-icon nil))
  (when (boundp 'ns-use-srgb-colorspace)
    (setq ns-use-srgb-colorspace t))
  (my/macos-apply-scroll-profile))

(my/macos-apply-performance-tweaks)
(if (boundp 'after-focus-change-function)
    (progn
      (remove-function after-focus-change-function
                       #'my/macos-schedule-idle-gc-after-focus-change)
      (add-function :after after-focus-change-function
                    #'my/macos-schedule-idle-gc-after-focus-change))
  (with-suppressed-warnings ((obsolete focus-out-hook))
    (remove-hook 'focus-out-hook #'my/macos-schedule-idle-gc)
    (add-hook 'focus-out-hook #'my/macos-schedule-idle-gc)))
(remove-hook 'minibuffer-exit-hook #'my/macos-schedule-idle-gc)
(when my/macos-gc-after-minibuffer
  (add-hook 'minibuffer-exit-hook #'my/macos-schedule-idle-gc))
(add-hook 'kill-emacs-hook #'my/macos-cancel-idle-gc-timer)

(use-package emacs
  :ensure nil
  :config
  ;; `Option' is mapped to Hyper on macOS. Keep the common clipboard actions
  ;; and use the remaining Option keys for high-frequency global entry points.
  (define-prefix-command 'my/org-hyper-map)
  (dolist (binding '(("H-," . my/hyper-dispatch)
                     ("H-x" . my/hyper-execute-command)
                     ("H-X" . clipboard-kill-region)
                     ("H-c" . clipboard-kill-ring-save)
                     ("H-v" . clipboard-yank)
                     ("H-q" . my/kill-buffer-dwim)
                     ("H-Q" . save-buffers-kill-terminal)
                     ("H-w" . my/delete-frame-dwim)
                     ("H-f" . find-file)
                     ("H-F" . find-file-other-window)
                     ("H-b" . consult-buffer)
                     ("H-B" . switch-to-buffer-other-window)
                     ("H-r" . my/search-open-recent-file)
                     ("H-R" . my/project-run-dispatch)
                     ("H-s" . my/search-line-forward)
                     ("H-g" . my/telescope-ripgrep)
                     ("H-p" . my/org-reference-insert-link)
                     ("H-P" . my/workspace-dispatch)
                     ("H-t" . telescope)
                     ("H-T" . my/test-dispatch)
                     ("H-m" . magit-status)
                     ("H-a" . org-agenda)
                     ("H-l" . claude-code-ide-menu)
                     ("H-h" . help-command)
                     ("H-H" . my/health-dispatch)
                     ("H-e" . my/code-actions-dispatch)
                     ("H-E" . my/compile-dispatch)
                     ("H-d" . my/diagnostics-dispatch)
                     ("H-D" . my/debug-profile-dispatch)
                     ("H-i" . show-imenu)
                     ("H-u" . my/language-server-dispatch)
                     ("H-j" . dape)
                     ("H-n" . my/test-nearest)
                     ("H-N" . my/output-dispatch)
                     ("H-y" . my/org-download-clipboard)
                     ("H-z" . dired-jump-with-zoxide)
                     ("H-Z" . dired-jump)
                     ("H-0" . delete-window)
                     ("H-1" . toggle-one-window)
                     ("H-2" . split-window-below)
                     ("H-3" . split-window-right)
                     ("H-o" . my/org-hyper-map)
                     ("H-O" . open-newline-above)
                     ("H-k" . duplicate-line-or-region-below)
                     ("H-K" . duplicate-line-or-region-above)
                     ("H-;" . comment-or-uncomment)
                     ("H-'" . mc/edit-lines)
                     ("H-[" . mc/mark-previous-like-this)
                     ("H-]" . mc/mark-next-like-this)
                     ("H-/" . mc/mark-all-like-this-dwim)
                     ("H--" . my/contract-region)
                     ("H-=" . my/expand-region)
                     ("H-<up>" . move-text-up)
                     ("H-<down>" . move-text-down)
                     ("H-`" . popper-toggle)))
    (global-set-key (kbd (car binding)) (cdr binding)))
  (dolist (binding '(("a" . org-agenda)
                     ("c" . org-capture)
                     ("f" . org-roam-node-find)
                     ("i" . org-roam-node-insert)
                     ("l" . org-roam-buffer-toggle)
                     ("o" . my/org-reference-create-target-dwim)
                     ("H-o" . my/org-reference-ensure-target-at-point)
                     ("I" . my/org-insert-id-link)
                     ("L" . my/org-insert-target-link)
                     ("T" . my/org-toc-insert-or-update)
                     ("v" . my/org-latex-preview-visible-now)
                     ("t" . org-todo)
                     ("s" . org-schedule)
                     ("d" . org-deadline)
                     ("r" . org-refile)
                     ("q" . org-set-tags-command)
                     ("p" . org-set-property)
                     ("e" . org-set-effort)
                     ("A" . org-archive-subtree)
                     ("z" . my/org-zotero-fill-metadata)))
    (define-key my/org-hyper-map (kbd (car binding)) (cdr binding)))

  ;; Make titlebar match the dark theme, but keep transparency opt-in.
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (when my/macos-use-transparent-titlebar
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

  ;; Useful when use an external keyboard
  (defun +mac-swap-option-and-command ()
    "Swap `mac-option-modifier' and `mac-command-modifier'."
    (interactive)
    (cl-rotatef mac-option-modifier mac-command-modifier)
    (message "mac-option-modifier: %s, mac-command-modifier: %s" mac-option-modifier mac-command-modifier))

  ;; Emoji support
  (let ((fonts '("Apple Color Emoji")))
    (cl-loop with script = 'emoji
             for font in fonts
             when (member font (font-family-list))
             return (set-fontset-font t script (font-spec :family font) nil 'prepend)))

  ;; Better variable-pitch font
  (let ((fonts '("Merriweather" "Bookerly" "Overpass" "Verdana" "Lucida Grande")))
    (cl-loop for font in fonts
             when (member font (font-family-list))
             return (custom-set-faces `(variable-pitch ((t (:family ,font)))))))
  :custom
  (mac-option-modifier 'hyper)
  (mac-command-modifier 'meta)
  (delete-by-moving-to-trash t)
  ;; Curse Lion and its sudden but inevitable fullscreen mode!
  ;; NOTE Meaningless to railwaycat's emacs-mac build
  (ns-use-native-fullscreen t)
  (ns-use-proxy-icon nil)
  ;(ns-use-native-fullscreen t)
  (use-dialog-box nil)
  (use-file-dialog nil)
  ;; Visit files opened outside of Emacs in existing frame, not a new one
  (ns-pop-up-frames nil))


(defun my/macos-apply-startup-window-state ()
  "Apply the configured startup window state on macOS GUI builds."
  (when (display-graphic-p)
    (pcase my/macos-startup-window-state
      ('maximized
       (toggle-frame-maximized))
      ('fullscreen
       (toggle-frame-fullscreen))
      (_ nil))))

(add-hook 'window-setup-hook #'my/macos-apply-startup-window-state)


(provide 'init-macos)
;;; init-macos.el ends here
