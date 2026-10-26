;;; os/macos/config.el --- Tweaks for MacOS -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'config)
(require 'general)
(require 'aaron-keybinds)

(require 'cl-lib)
(require 'browse-url)
(require 'aaron-open)
(require 'subr-x)

(declare-function remote-client-file-name "remote-fs" (file-name &optional adapter))
(declare-function consult-buffer "consult" ())
(declare-function consult-buffer-other-window "consult" ())
(declare-function comment-or-uncomment "aaron-base" ())
(declare-function duplicate-line-or-region-above "duplicate-line" (&optional reverse))
(declare-function duplicate-line-or-region-below "duplicate-line" ())
(declare-function dired-get-file-for-visit "dired" ())
(declare-function dired-jump "dired-x" (&optional other-window file-name))
(declare-function dired-jump-with-zoxide "tools/zoxide/config" (&optional other-window))
(declare-function mc/edit-lines "multiple-cursors" ())
(declare-function mc/mark-all-like-this-dwim "multiple-cursors" ())
(declare-function mc/mark-next-like-this "multiple-cursors" ())
(declare-function mc/mark-previous-like-this "multiple-cursors" ())
(declare-function move-text-down "move-text" (arg))
(declare-function move-text-up "move-text" (arg))
(declare-function code-actions-menu "tools/code-actions/config" ())
(declare-function config-compile-menu "config/compile/config" ())
(declare-function +expand-region/contract-region "editor/expand-region/config" ())
(declare-function debug-menu "tools/debugger/config" ())
(declare-function debug-profile-menu "tools/debug-profile/config" ())
(declare-function diagnostics-menu "checkers/diagnostics-extra/config" ())
(declare-function +expand-region/expand-region "editor/expand-region/config" ())
(declare-function config-health-menu "config/health/config" ())
(declare-function command-hub "config/hyper/config" ())
(declare-function +browser-current-url "app/browser/config" ())
(declare-function +windows/kill-buffer-dwim "ui/windows/config" ())
(declare-function language-server-menu "tools/lsp-hub/config" ())
(declare-function noema-agenda "noema/bridge/config" (&optional query))
(declare-function output-menu "tools/output/config" ())
(declare-function magit-status "magit-status" (&optional directory))
(declare-function open-newline-above "open-newline" (arg))
(declare-function open-newline-below "open-newline" (arg))
(declare-function popper-toggle "popper" ())
(declare-function project-menu "tools/project/config" ())
(declare-function project-run-menu "tools/run/config" ())
(declare-function test-menu "tools/test/config" ())
(declare-function workspace-menu "ui/workspaces/config" ())
(declare-function +search/line-forward "completion/search/config" ())
(declare-function +search/open-recent-file "completion/search/config" ())
(declare-function telescope "completion/telescope/config" ())
(declare-function +telescope/ripgrep "completion/telescope/config" ())
(declare-function +test/nearest "tools/test/config" ())
(declare-function show-imenu "tools/project/config" ())
(declare-function org-element-context "org-element" ())
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function +neomacs/apply-modifier-remap "os/neomacs/config" ())
(defvar +neomacs-modifier-remap)
(defvar +neomacs-default-modifier-remap)

(defun +parameter-adjust--number-bounds ()
  "Return bounds for the number at point or just before point."
  (let* ((line-beg (line-beginning-position))
         (line-end (line-end-position))
         (positions (list (point)
                          (max line-beg (1- (point))))))
    (catch 'bounds
      (dolist (pos positions)
        (save-excursion
          (goto-char line-beg)
          (while (re-search-forward "[-+]?[0-9]+\\(?:\\.[0-9]+\\)?" line-end t)
            (when (and (<= (match-beginning 0) pos)
                       (< pos (match-end 0)))
              (throw 'bounds (cons (match-beginning 0)
                                   (match-end 0))))))))))

(defun +parameter-adjust--format-number (text value)
  "Format numeric VALUE using TEXT's sign, width, and precision style."
  (if (string-match-p "\\." text)
      (let* ((precision (if (string-match "\\.\\([0-9]+\\)" text)
                            (length (match-string 1 text))
                          0))
             (formatted (format (format "%%.%df" precision) value)))
        (if (and (string-prefix-p "+" text)
                 (not (string-prefix-p "-" formatted)))
            (concat "+" formatted)
          formatted))
    (let* ((rounded (truncate value))
           (sign (cond
                  ((< rounded 0) "-")
                  ((string-prefix-p "+" text) "+")
                  (t "")))
           (digits (replace-regexp-in-string "\\`[-+]" "" text))
           (width (length digits)))
      (concat sign
              (format (format "%%0%dd" width)
                      (abs rounded))))))

(defun +parameter-adjust--number (delta)
  "Adjust the number at point by DELTA."
  (when-let* ((bounds (+parameter-adjust--number-bounds)))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (old (buffer-substring-no-properties beg end))
           (offset (- (point) beg))
           (new (+parameter-adjust--format-number
                 old
                 (+ (string-to-number old) delta))))
      (delete-region beg end)
      (insert new)
      (goto-char (+ beg (min (max offset 0) (length new))))
      t)))

(defun +parameter-adjust--letter-pos ()
  "Return the position of an ASCII letter at or just before point."
  (cond
   ((and (char-after)
         (or (<= ?a (char-after) ?z)
             (<= ?A (char-after) ?Z)))
    (point))
   ((and (> (point) (point-min))
         (let ((char (char-before)))
           (or (<= ?a char ?z)
               (<= ?A char ?Z))))
    (1- (point)))))

(defun +parameter-adjust--letter (delta)
  "Adjust the ASCII letter at point by DELTA."
  (when-let* ((pos (+parameter-adjust--letter-pos)))
    (let* ((char (char-after pos))
           (base (if (<= ?a char ?z) ?a ?A))
           (next (+ base (mod (+ (- char base) delta) 26))))
      (save-excursion
        (goto-char pos)
        (delete-char 1)
        (insert-char next))
      t)))

(defun +macos/parameter-adjust (delta)
  "Adjust a number or ASCII letter at point by DELTA."
  (interactive "p")
  (atomic-change-group
    (unless (or (+parameter-adjust--number delta)
                (+parameter-adjust--letter delta))
      (user-error "No number or letter at point"))))

(defun +macos/parameter-increment (&optional arg)
  "Increment the number or letter at point by ARG."
  (interactive "p")
  (+macos/parameter-adjust (or arg 1)))

(defun +macos/parameter-decrement (&optional arg)
  "Decrement the number or letter at point by ARG."
  (interactive "p")
  (+macos/parameter-adjust (- (or arg 1))))

(defvar +macos-idle-gc-timer nil
  "Timer used to run a delayed GC after the UI goes idle on macOS.")

(defgroup +macos nil
  "macOS-specific local integration."
  :group 'environment)

(config-defvar +macos-scroll-profile nil
  "Default macOS scrolling profile.
`line' keeps `pixel-scroll-precision-mode' off and lets regular wheel
scrolling handle trackpad input, which costs much less during long Org
reading sessions.  `pixel' restores precision pixel scrolling for users who
prefer the smoother feel over lower power draw."
  :type '(choice (const :tag "Lower-power line scrolling" line) (const :tag "Precision pixel scrolling" pixel))
  :group '+macos)

(config-defvar +macos-idle-gc-delay nil
  "Seconds to wait before the explicit macOS idle GC runs.
`gcmh' already manages ordinary idle GC.  This macOS hook is only a fallback
for focus-loss cleanup, so a longer delay avoids waking Emacs immediately
after short minibuffer or window-management commands."
  :type 'number
  :group '+macos)

(config-defvar +macos-gc-after-minibuffer nil
  "Whether exiting the minibuffer should schedule an explicit macOS idle GC.
This is disabled by default because `gcmh' already handles normal idle GC and
minibuffer exits are common during command-heavy work."
  :type 'boolean
  :group '+macos)

(defvar +macos-use-transparent-titlebar nil
  "Whether macOS GUI frames should use a transparent titlebar.")

(defvar +macos-startup-window-state nil
  "Startup window state for macOS GUI frames.
Use nil for a regular window, `maximized' for a maximized window, or
`fullscreen' for a fullscreen window.")

(defvar pixel-scroll-precision-mode)

(defun +macos-open--normalize-target (target)
  "Return TARGET in a form suitable for the macOS open command."
  (let* ((target (string-trim (substring-no-properties target)))
         (target
          (if (and (string-prefix-p "fs://" target)
                   (fboundp 'remote-uri-to-file-name))
              (remote-uri-to-file-name target)
            target))
         (expanded (and (not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target))
                        (aaron-open-resolve-file-name target)))
         (expanded
          (if (and expanded
                   (fboundp 'remote-fs-file-name-p)
                   (remote-fs-file-name-p expanded))
              (or (remote-client-file-name expanded)
                  (user-error
                   "macOS open cannot directly access target %s"
                   (remote-file-name-target expanded)))
            expanded)))
    (cond
     ((string-empty-p target) nil)
     ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target) target)
     ((and expanded (file-exists-p expanded)) expanded)
     ((and expanded (file-name-absolute-p target)) expanded)
     (t target))))

(defun +macos/open-target (target)
  "Open TARGET with macOS open."
  (interactive "sOpen with macOS open: ")
  (unless (eq system-type 'darwin)
    (user-error "macOS open is only available on Darwin"))
  (unless (executable-find "open")
    (user-error "Cannot find macOS open command"))
  (if-let* ((target (+macos-open--normalize-target target)))
      (let ((process-connection-type nil))
        (start-process "macos-open" nil "open" target)
        (message "open: %s" target))
    (user-error "No target to open")))

(defun +macos/open-url (url)
  "Open URL with macOS open."
  (interactive (browse-url-interactive-arg "URL: "))
  (+macos/open-target
   (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url)
       url
     (concat "https://" url))))

(defun +macos/open-file (file)
  "Open FILE with macOS open."
  (interactive "fOpen file with macOS open: ")
  (+macos/open-target file))

(defun +macos-open--region-target ()
  "Return the active region as an open target, or nil."
  (when (use-region-p)
    (string-trim
     (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun +macos-open--org-link-target ()
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
            (aaron-open-resolve-file-name path))
           ((and link-type path
                 (not (member link-type '("custom-id" "fuzzy" "id"))))
            (concat link-type ":" path))))))))

(defun +macos-open--target-at-point ()
  "Return a file or link target near point."
  (or (+macos-open--region-target)
      (+macos-open--org-link-target)
      (thing-at-point 'url t)
      (and (derived-mode-p 'dired-mode)
           (ignore-errors (dired-get-file-for-visit)))
      (let ((file (thing-at-point 'filename t)))
        (and file
             (file-exists-p (aaron-open-resolve-file-name file))
             file))
      (and (fboundp '+browser-current-url)
           (ignore-errors (+browser-current-url)))
      buffer-file-name))

(defun macos-open-at-point ()
  "Open the file or link at point with macOS open.
When point has no obvious target, open the current browser URL or current
buffer file."
  (interactive)
  (if-let* ((target (+macos-open--target-at-point)))
      (+macos/open-target target)
    (call-interactively #'+macos/open-target)))

(defun +macos-schedule-idle-gc-after-focus-change ()
  "Queue idle GC when the selected frame loses focus."
  (unless (frame-focus-state)
    (+macos-schedule-idle-gc)))

(defun +macos-cancel-idle-gc-timer ()
  "Cancel the pending macOS idle GC timer."
  (when (timerp +macos-idle-gc-timer)
    (cancel-timer +macos-idle-gc-timer)
    (setq +macos-idle-gc-timer nil)))

(defun +macos-schedule-idle-gc ()
  "Run GC shortly after focus leaves Emacs or the minibuffer closes."
  (+macos-cancel-idle-gc-timer)
  (setq +macos-idle-gc-timer
        (run-with-idle-timer +macos-idle-gc-delay nil
                             (lambda ()
                               (setq +macos-idle-gc-timer nil)
                               (garbage-collect)))))

(defun +macos-apply-scroll-profile (&optional profile)
  "Apply macOS scroll PROFILE.
When PROFILE is nil, use `+macos-scroll-profile'."
  (let ((profile (or profile +macos-scroll-profile)))
    (setq fast-but-imprecise-scrolling t
          redisplay-skip-fontification-on-input t)
    (when (boundp 'pixel-scroll-precision-interpolate-page)
      (setq pixel-scroll-precision-interpolate-page nil))
    (when (boundp 'pixel-scroll-precision-use-momentum)
      (setq pixel-scroll-precision-use-momentum nil))
    (when (fboundp 'pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode
       (if (eq profile 'pixel) 1 -1)))))

(defun +macos/toggle-pixel-scroll-precision (&optional arg)
  "Toggle macOS precision pixel scrolling.
With positive ARG, enable it.  With zero or negative ARG, disable it."
  (interactive "P")
  (setq +macos-scroll-profile
        (if (if arg
                (> (prefix-numeric-value arg) 0)
              (not (bound-and-true-p pixel-scroll-precision-mode)))
            'pixel
          'line))
  (+macos-apply-scroll-profile +macos-scroll-profile)
  (message "macOS scroll profile: %s" +macos-scroll-profile))

(defun +macos-apply-performance-tweaks ()
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

  (when (boundp 'ns-use-proxy-icon)
    (setq ns-use-proxy-icon nil))
  (when (boundp 'ns-use-srgb-colorspace)
    (setq ns-use-srgb-colorspace t))
  (+macos-apply-scroll-profile))

(+macos-apply-performance-tweaks)
(if (boundp 'after-focus-change-function)
    (progn
      (remove-function after-focus-change-function
                       #'+macos-schedule-idle-gc-after-focus-change)
      (add-function :after after-focus-change-function
                    #'+macos-schedule-idle-gc-after-focus-change))
  (with-suppressed-warnings ((obsolete focus-out-hook))
    (remove-hook 'focus-out-hook #'+macos-schedule-idle-gc)
    (add-hook 'focus-out-hook #'+macos-schedule-idle-gc)))
(remove-hook 'minibuffer-exit-hook #'+macos-schedule-idle-gc)
(when +macos-gc-after-minibuffer
  (add-hook 'minibuffer-exit-hook #'+macos-schedule-idle-gc))
(add-hook 'kill-emacs-hook #'+macos-cancel-idle-gc-timer)

(use-package emacs
  :ensure nil
  :config
  ;; `Option' is mapped to Hyper on macOS. Keep the common clipboard actions
  ;; and use the remaining Option keys for high-frequency global entry points.
  ;; `Command' remains Meta, so bind `M-c' / `M-v' to clipboard copy/paste.
  (dolist (binding '(("H-," . command-hub)
                     ("M-c" . clipboard-kill-ring-save)
                     ("M-v" . clipboard-yank)
                     ("H-x" . telescope)
                     ("H-X" . clipboard-kill-region)
                     ("H-c" . clipboard-kill-ring-save)
                     ("H-v" . clipboard-yank)
                     ("H-q" . +windows/kill-buffer-dwim)
                     ("H-Q" . save-buffers-kill-terminal)
                     ("H-w" . aaron/delete-frame-dwim)
                     ("H-f" . file-open)
                     ("H-F" . find-file-other-window)
                     ("H-b" . consult-buffer)
                     ("H-B" . consult-buffer-other-window)
                     ("H-r" . +search/open-recent-file)
                     ("H-R" . project-run-menu)
                     ("H-s" . +search/line-forward)
                     ("H-g" . +telescope/ripgrep)
                     ("H-p" . project-menu)
                     ("H-P" . workspace-menu)
                     ("H-t" . telescope)
                     ("H-T" . test-menu)
                     ("H-m" . magit-status)
                     ("H-a" . noema-agenda)
                     ("H-h" . help-command)
                     ("H-H" . config-health-menu)
                     ("H-e" . code-actions-menu)
                     ("H-E" . config-compile-menu)
                     ("H-d" . diagnostics-menu)
                     ("H-D" . debug-profile-menu)
                     ("H-i" . show-imenu)
                     ("H-u" . language-server-menu)
                     ("H-j" . debug-menu)
                     ("H-n" . +test/nearest)
                     ("H-N" . output-menu)
                     ("H-z" . dired-jump-with-zoxide)
                     ("H-Z" . dired-jump)
                     ("H-0" . delete-window)
                     ("H-1" . toggle-one-window)
                     ("H-2" . split-window-below)
                     ("H-3" . split-window-right)
                     ("H-O" . open-newline-above)
                     ("H-k" . duplicate-line-or-region-below)
                     ("H-K" . duplicate-line-or-region-above)
                     ("H-;" . comment-or-uncomment)
                     ("H-'" . mc/edit-lines)
                     ("H-[" . mc/mark-previous-like-this)
                     ("H-]" . mc/mark-next-like-this)
                     ("H-/" . mc/mark-all-like-this-dwim)
                     ("H--" . +expand-region/contract-region)
                     ("H-=" . +expand-region/expand-region)
                     ("H-<left>" . +macos/parameter-decrement)
                     ("H-<right>" . +macos/parameter-increment)
                     ("H-<up>" . move-text-up)
                     ("H-<down>" . move-text-down)
                     ("H-`" . popper-toggle)))
    (general-define-key (car binding) (cdr binding)))

  ;; Make titlebar match the dark theme, but keep transparency opt-in.
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (when +macos-use-transparent-titlebar
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

  ;; Useful when use an external keyboard
  (defun +mac-swap-option-and-command ()
    "Swap the roles of the physical Option and Command keys."
    (interactive)
    (require 'os/neomacs/config nil t)
    (cond
     ;; Neomacs has no `mac-*-modifier' variables; its input bridge hard-codes
     ;; Option to Meta and Command to Super, and `os/neomacs/config.el' renames them
     ;; back.  Swapping there means swapping that rename table.
     ((bound-and-true-p +neomacs-p)
      (setq +neomacs-modifier-remap
            (if (equal +neomacs-modifier-remap '((meta . meta) (super . hyper)))
                +neomacs-default-modifier-remap
              '((meta . meta) (super . hyper))))
      (+neomacs/apply-modifier-remap)
      (message "Option/Command layout: %S" +neomacs-modifier-remap))
     (t
      (cl-rotatef mac-option-modifier mac-command-modifier)
      (message "mac-option-modifier: %s, mac-command-modifier: %s"
               mac-option-modifier mac-command-modifier))))

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
  ;; Visit files opened outside of Emacs in existing frame, not a new one
  (ns-pop-up-frames nil))


(defun +macos-apply-startup-window-state ()
  "Apply the configured startup window state on macOS GUI builds."
  (when (display-graphic-p)
    (pcase +macos-startup-window-state
      ('maximized
       (toggle-frame-maximized))
      ('fullscreen
       (toggle-frame-fullscreen))
      (_ nil))))

(add-hook 'window-setup-hook #'+macos-apply-startup-window-state)


(defun +macos/copy-file-to-clipboard ()
  "Put the current file itself on the macOS clipboard.
Pasting into another application (a chat, Mail, Finder) attaches the file
rather than inserting its path.  Only a file this machine can read
directly qualifies; a file on another target is refused."
  (interactive)
  (let* ((file (or buffer-file-name (user-error "Current buffer is not visiting a file")))
         (path (or (remote-client-file-name file)
                   (user-error "Not a file on this machine: %s" file)))
         ;; osascript is a process of this machine, whatever the buffer's target.
         (default-directory temporary-file-directory))
    (unless (file-exists-p path)
      (user-error "File does not exist: %s" path))
    (unless (eql 0 (call-process "osascript" nil nil nil "-e"
                                 (format "set the clipboard to (POSIX file %s)"
                                         (prin1-to-string path))))
      (user-error "Failed to copy file to clipboard: %s" path))
    (message "Copied file to clipboard: %s" path)))

(define-leader-key! "fg" '(:def +macos/copy-file-to-clipboard
                           :which-key "copy file to clipboard"))

(provide 'os/macos/config)
;;; os/macos/config.el ends here
