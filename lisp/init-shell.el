;;; init-shell.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-ui)

(defvar eshell-last-dir-ring)
(defvar eshell-buffer-name)

(declare-function my/terminal-cd-command "init-funcs" (directory))
(declare-function my/terminal-home-directory "init-funcs" (&optional directory))
(declare-function my/terminal-normalize-directory "init-funcs" (directory))
(declare-function comint-simple-send "comint" (proc string))
(declare-function ring-elements "ring")
(declare-function eshell/cd "esh-mode")
(declare-function eshell-reset "esh-mode")
(declare-function eshell-grep "em-unix")
(declare-function vterm "vterm" (&optional buffer-name))
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-yank "vterm")
(declare-function turn-off-evil-mode "evil")
(declare-function evil-set-initial-state "evil-core")
(declare-function vterm-copy-mode "vterm" (&optional arg))
(declare-function vterm-copy-mode-done "vterm" (&optional arg))
(defvar vterm-mode-map)
(defvar vterm-copy-mode-map)

(defcustom my/vterm-startup-send-delay 0.05
  "Delay between retries when sending startup commands to a fresh VTerm."
  :type 'number
  :group 'term)

(defcustom my/vterm-startup-send-retries 20
  "Maximum retry count for startup commands sent to a fresh VTerm."
  :type 'integer
  :group 'term)

(defvar my/terminal-startup-cd-inhibited nil
  "When non-nil, start terminal buffers directly in the target directory.")

(defvar-local my/vterm-target-directory nil
  "Target directory a VTerm buffer should represent after startup sync.")

(defvar-local my/vterm-copy-mode-visual-line-anchor nil
  "Marker for the line where VTerm visual-line selection started.")

(defun my/terminal--startup-directory (&optional directory)
  "Return a safe startup directory for DIRECTORY, or nil to use DIRECTORY directly."
  (unless my/terminal-startup-cd-inhibited
    (my/terminal-home-directory (or directory default-directory))))

(defun my/terminal--resolve-launch-directories (&optional directory)
  "Return (STARTUP-DIRECTORY . TARGET-DIRECTORY) for terminal launch."
  (when-let* ((target-directory (my/terminal-normalize-directory
                                 (or directory default-directory))))
    (cons (or (my/terminal--startup-directory target-directory)
              target-directory)
          target-directory)))

(defun my/vterm-send-command (buffer command &optional retries)
  "Send COMMAND to VTerm BUFFER once its subprocess is ready.
Append a trailing return automatically.  RETRIES defaults to
`my/vterm-startup-send-retries'."
  (when (and (buffer-live-p buffer)
             (stringp command)
             (not (string-empty-p command)))
    (let ((remaining (or retries my/vterm-startup-send-retries)))
      (if-let* ((process (get-buffer-process buffer))
                ((process-live-p process)))
          (with-current-buffer buffer
            (vterm-send-string command)
            (vterm-send-return))
        (when (> remaining 0)
          ;; Fresh vterm shells can still be starting up here; retry so the
          ;; command is not injected mid-startup and lose its first character.
          (run-at-time my/vterm-startup-send-delay nil
                       #'my/vterm-send-command
                       buffer
                       command
                       (1- remaining)))))))

(defun shell-mode-common-init ()
  "The common initialization procedure for term/shell."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil))

(defun my/terminal-apply-ui ()
  "Apply a restrained terminal UI in shell-like buffers."
  (when (display-graphic-p)
    (setq-local line-spacing 0)
    (when (derived-mode-p 'eshell-mode)
      (setq-local mode-line-format nil)
      (setq-local header-line-format
                  '(" "
                    (:propertize "%b" face mode-line-buffer-id)
                    "  "
                    (:propertize "eshell" face shadow))))
    (when (facep 'eshell-prompt)
      (aaron-ui-set-face 'eshell-prompt
                         :foreground 'accent-cyan
                         :weight 'medium))
    (when (facep 'eshell-ls-directory)
      (aaron-ui-set-face 'eshell-ls-directory
                         :foreground 'fg-dim
                         :weight 'medium))
    (when (facep 'eshell-ls-executable)
      (aaron-ui-set-face 'eshell-ls-executable
                         :foreground 'accent-green-soft))
    (when (facep 'vterm-color-default)
      (aaron-ui-set-face 'vterm-color-default
                         :background 'bg-elevated
                         :foreground 'fg-soft))))

(defun my/terminal-context-key (&optional directory)
  "Return a stable local-or-remote context key for DIRECTORY."
  (when-let* ((directory (my/terminal-normalize-directory
                          (or directory default-directory))))
    (if-let* ((remote-prefix (file-remote-p directory)))
        (replace-regexp-in-string
         "[^[:alnum:]@._#-]+"
         ":"
         (replace-regexp-in-string "\\`/+\\|:+\\'" "" remote-prefix))
      "local")))

(defun my/terminal-context-buffer-name (base-name &optional directory)
  "Return BASE-NAME specialized for DIRECTORY's local-or-remote context."
  (let ((context-key (my/terminal-context-key directory)))
    (if (or (null context-key)
            (string= context-key "local"))
        base-name
      (format "%s:%s*"
              (if (string-suffix-p "*" base-name)
                  (substring base-name 0 -1)
                base-name)
              context-key))))

(defun my/shell--sync-directory (buffer directory)
  "Change shell BUFFER to DIRECTORY."
  (when-let* ((buffer (and (buffer-live-p buffer) buffer))
              (directory (my/terminal-normalize-directory directory))
              (process (get-buffer-process buffer))
              ((process-live-p process))
              (command (my/terminal-cd-command directory)))
    (with-current-buffer buffer
      (unless (equal (my/terminal-normalize-directory default-directory)
                     directory)
        (setq default-directory directory)
        (comint-simple-send process command)))))

(defun my/shell--ensure-directory (buffer directory)
  "Ensure shell BUFFER is associated with DIRECTORY."
  (when-let* ((buffer (and (buffer-live-p buffer) buffer))
              (directory (my/terminal-normalize-directory directory)))
    (with-current-buffer buffer
      (setq default-directory directory))
    (my/shell--sync-directory buffer directory)))

(defun my/eshell--sync-directory (buffer directory)
  "Change Eshell BUFFER to DIRECTORY and redraw its prompt."
  (when-let* ((buffer (and (buffer-live-p buffer) buffer))
              (directory (my/terminal-normalize-directory directory)))
    (with-current-buffer buffer
      (unless (equal (my/terminal-normalize-directory default-directory)
                     directory)
        (eshell/cd directory)
        (setq default-directory directory)
        (eshell-reset)
        (goto-char (point-max))))))

(defun my/eshell--ensure-directory (buffer directory)
  "Ensure Eshell BUFFER is associated with DIRECTORY."
  (when-let* ((buffer (and (buffer-live-p buffer) buffer))
              (directory (my/terminal-normalize-directory directory)))
    (with-current-buffer buffer
      (setq default-directory directory))
    (my/eshell--sync-directory buffer directory)))

(defun my/shell-context-buffer-name (&optional directory)
  "Return the standard shell buffer name for DIRECTORY."
  (my/terminal-context-buffer-name "*shell*" directory))

(defun my/shell-popup-buffer-name (&optional directory)
  "Return the popup shell buffer name for DIRECTORY."
  (my/terminal-context-buffer-name "*shell-popup*" directory))

(defun my/eshell-context-buffer-name (&optional directory)
  "Return the standard Eshell buffer name for DIRECTORY."
  (my/terminal-context-buffer-name "*eshell*" directory))

(defun my/shell-reuse-by-context-a (orig-fn &optional buffer file-name)
  "Reuse `shell' buffers per local-or-remote context."
  (if buffer
      (funcall orig-fn buffer file-name)
    (let* ((directories (my/terminal--resolve-launch-directories default-directory))
           (startup-directory (car directories))
           (target-directory (cdr directories))
           (buffer-name (my/shell-context-buffer-name target-directory))
           (buffer (let ((default-directory startup-directory))
                     (funcall orig-fn buffer-name file-name))))
      (my/shell--ensure-directory buffer target-directory)
      buffer)))

(defun my/eshell-reuse-by-context-a (orig-fn &optional arg)
  "Reuse `eshell' buffers per local-or-remote context.
Keep the stock prefix-argument behaviour for explicitly creating or selecting
numbered sessions."
  (if arg
      (funcall orig-fn arg)
    (let* ((directories (my/terminal--resolve-launch-directories default-directory))
           (startup-directory (car directories))
           (target-directory (cdr directories))
           (buffer-name (my/eshell-context-buffer-name target-directory))
           (eshell-buffer-name buffer-name)
           (buffer (let ((default-directory startup-directory))
                     (funcall orig-fn nil))))
      (my/eshell--ensure-directory buffer target-directory)
      buffer)))

(defun my/eshell-emacs-state-setup ()
  "Keep eshell out of Evil stateful editing."
  (when (fboundp 'evil-emacs-state)
    (evil-emacs-state))
  (run-at-time
   0 nil
   (lambda (buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (bound-and-true-p evil-local-mode)
           (turn-off-evil-mode)))))
   (current-buffer)))

(defun shell-self-destroy-sentinel (proc _exit-msg)
  "Make PROC self destroyable."
  (when (memq (process-status proc) '(exit signal stop))
    (when-let* ((buffer (process-buffer proc))
                ((buffer-live-p buffer)))
      (kill-buffer buffer))
    (ignore-errors (delete-window))))

(defun shell-delete-window (&optional win)
  "Delete WIN wrapper."
  (ignore-errors (delete-window win)))

;; General term mode
;;
;; If you use bash, directory track is supported natively.
;; See https://www.emacswiki.org/emacs/AnsiTermHints for more information.
(use-package term
  :ensure nil
  :hook ((term-mode . shell-mode-common-init)
         (term-mode . term-mode-prompt-regexp-setup)
         (term-exec . term-mode-set-sentinel))
  :config
  (defun term-mode-prompt-regexp-setup ()
    "Setup `term-prompt-regexp' for term-mode."
    (setq-local term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

  (defun term-mode-set-sentinel ()
    "Close buffer after exit."
    (when-let* ((proc (ignore-errors (get-buffer-process (current-buffer)))))
      (set-process-sentinel proc #'shell-self-destroy-sentinel))))

;; The Emacs shell & friends
(use-package eshell
  :ensure nil
  :hook ((eshell-mode . shell-mode-common-init)
         (eshell-mode . completion-preview-mode)
         (eshell-mode . my/eshell-emacs-state-setup)
         (eshell-mode . my/terminal-apply-ui))
  :config
  (advice-remove 'eshell #'my/eshell-reuse-by-context-a)
  (advice-add 'eshell :around #'my/eshell-reuse-by-context-a)
  ;; Prevent accident typing
  (defalias 'eshell/vi 'find-file)
  (defalias 'eshell/vim 'find-file)
  (defalias 'eshell/nvim 'find-file)
  (defun eshell/bat (file)
    "cat FILE with syntax highlight."
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file))
        (delay-mode-hooks
          (set-auto-mode)
          (font-lock-ensure)))
      (buffer-string)))

  (defun eshell/f (filename &optional dir)
    "Search for files matching FILENAME in either DIR or the
current directory."
    (find-dired (or dir ".")
                (concat " -not -path '*/.git*'"
                        " -and -not -path 'build'" ;; the cmake build directory
                        " -and"
                        " -type f"
                        " -and"
                        " -iname " (format "'*%s*'" filename))))

  (defun eshell/z ()
    "cd to directory with completions."
    (let ((dir (completing-read "Directory: " (delete-dups (ring-elements eshell-last-dir-ring)) nil t)))
      (eshell/cd dir)))

  (defun eshell/rg (&rest args)
    "ripgrep with eshell integration."
    (eshell-grep "rg" (append '("--no-heading") args) t))
  :custom
  (eshell-directory-name
   (or (and (boundp 'my/eshell-state-dir) my/eshell-state-dir)
       (expand-file-name "var/eshell/" user-emacs-directory)))
  ;; The following cmds will run on term.
  (eshell-visual-commands '("top" "htop" "less" "more" "telnet"))
  (eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show")))
  (eshell-visual-options '(("git" "--help" "--paginate")))
  ;; Completion like bash
  (eshell-cmpl-ignore-case t)
  (eshell-cmpl-cycle-completions nil))

(use-package em-hist
  :ensure nil
  :hook (eshell-hist-load . eshell-hist-initialize)
  :bind (:map eshell-hist-mode-map
         ("M-r" . consult-history))
  :custom
  (eshell-history-size 10000))

(use-package em-rebind
  :ensure nil
  :commands eshell-delchar-or-maybe-eof)

(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
         ([remap kill-region] . backward-kill-word)
         ([remap delete-char] . eshell-delchar-or-maybe-eof))
  :config
  ;; Delete the last "word"
  (dolist (ch '(?_ ?- ?.))
    (modify-syntax-entry ch "w" eshell-mode-syntax-table)))

;; The interactive shell.
;;
;; It can be used as a `sh-mode' REPL.
;;
;; `shell' is recommended to use over `tramp'.
(use-package shell
  :ensure nil
  :hook ((shell-mode . shell-mode-common-init)
         (shell-mode . revert-tab-width-to-default))
  :config
  (advice-remove 'shell #'my/shell-reuse-by-context-a)
  (advice-add 'shell :around #'my/shell-reuse-by-context-a)
  (defun shell-toggle ()
    "Toggle a persistent shell popup window.
If popup is visible but unselected, select it.
If popup is focused, kill it."
    (interactive)
    (let* ((directory (my/terminal-normalize-directory default-directory))
           (buffer-name (my/shell-popup-buffer-name directory)))
      (if-let* ((win (get-buffer-window buffer-name)))
          (if (eq (selected-window) win)
              ;; If users attempt to delete the sole ordinary window, silence it.
              (shell-delete-window win)
            (progn
              (my/shell--sync-directory (window-buffer win) directory)
              (select-window win)))
        (let ((display-buffer-alist '(((category . comint)
                                       (display-buffer-at-bottom))))
              (existing (get-buffer buffer-name)))
          (let ((default-directory directory))
            (when-let* ((buffer (shell buffer-name)))
              (when existing
                (my/shell--sync-directory buffer directory))
              (when-let* ((proc (ignore-errors (get-buffer-process buffer))))
                (set-process-sentinel proc #'shell-self-destroy-sentinel))))))))

  ;; Correct indentation for `ls'
  (defun revert-tab-width-to-default ()
    "Revert `tab-width' to default value."
    (setq-local tab-width 8))
  :custom
  (shell-kill-buffer-on-exit t)
  (shell-get-old-input-include-continuation-lines t))

(defvar my/ssh-host-history nil)

(defun my/ssh-config-hosts ()
  "Return concrete host entries from `~/.ssh/config'."
  (let ((config (expand-file-name "~/.ssh/config")))
    (when (file-readable-p config)
      (with-temp-buffer
        (insert-file-contents config)
        (let (hosts)
          (while (re-search-forward "^[[:space:]]*Host[[:space:]]+\\(.+\\)$" nil t)
            (dolist (host (split-string (match-string 1) "[[:space:]]+" t))
              (unless (string-match-p "[*?]" host)
                (push host hosts))))
          (delete-dups (nreverse hosts)))))))

(defun my/read-ssh-host ()
  "Read an SSH host, preferring entries from `~/.ssh/config'."
  (let ((hosts (my/ssh-config-hosts)))
    (if hosts
        (completing-read "SSH host: " hosts nil nil nil 'my/ssh-host-history)
      (read-string "SSH host: " nil 'my/ssh-host-history))))

(require 'init-vterm-popup)

(defun my/vterm--set-target-directory (buffer directory)
  "Record DIRECTORY as BUFFER's target working directory."
  (when-let* ((buffer (and (buffer-live-p buffer) buffer))
              (directory (my/terminal-normalize-directory directory)))
    (with-current-buffer buffer
      (setq-local my/vterm-target-directory directory
                  default-directory directory))
    directory))

(defun my/vterm--sync-target-directory-h ()
  "Re-apply the recorded target directory for the current VTerm buffer."
  (when-let* ((directory (my/terminal-normalize-directory my/vterm-target-directory)))
    (setq-local default-directory directory)))

(defun my/vterm--send-cd (buffer directory)
  "Send `cd DIRECTORY' to VTerm BUFFER."
  (when-let* ((buffer (and (buffer-live-p buffer) buffer))
              (directory (my/terminal-normalize-directory directory))
              (command (my/terminal-cd-command directory)))
    (my/vterm--set-target-directory buffer directory)
    (my/vterm-send-command buffer command)))

(defun my/vterm--start-in-home-and-cd (orig-fn &rest args)
  "Create VTerm buffers from home, then `cd' into the original directory."
  (if-let* ((directories (my/terminal--resolve-launch-directories default-directory))
            (startup-directory (car directories))
            (target-directory (cdr directories)))
      (let ((default-directory startup-directory))
        (let ((buffer (apply orig-fn args)))
          (my/vterm--send-cd buffer target-directory)
          buffer))
    (apply orig-fn args)))

(defun my/vterm--ssh-target-directory (host)
  "Return the preferred TRAMP directory for SSH HOST."
  (let* ((directory (my/terminal-normalize-directory default-directory))
         (method (and directory (file-remote-p directory 'method)))
         (remote-host (and directory (file-remote-p directory 'host))))
    (if (and method
             remote-host
             (string= host remote-host)
             (or (string-prefix-p "ssh" method)
                 (string= method "scp")))
        directory
      (format "/ssh:%s:~/" host))))

(defun my/vterm--open-in-directory (buffer-name directory)
  "Open or switch to BUFFER-NAME, creating it in DIRECTORY when needed."
  (if-let* ((buffer (get-buffer buffer-name)))
      (progn
        (my/vterm--send-cd buffer directory)
        buffer)
    (let ((default-directory directory))
      (vterm buffer-name))))

(defun my/vterm-named (name)
  "Create or switch to a named vterm buffer."
  (interactive "sVTerm name: ")
  (let ((buf (format "*vterm:%s*" name)))
    (if (get-buffer buf)
        (switch-to-buffer buf)
      (vterm buf))))

(defun my/vterm-ssh (host)
  "Open a dedicated vterm buffer and start an SSH session to HOST."
  (interactive (list (my/read-ssh-host)))
  (let ((buffer-name (format "*vterm:ssh:%s*" host))
        (target-directory (my/vterm--ssh-target-directory host)))
    (pop-to-buffer
     (my/vterm--open-in-directory buffer-name target-directory))))

(defun my/vterm-copy-mode-enter ()
  "Enter `vterm-copy-mode' without enabling region selection."
  (interactive)
  (my/vterm-copy-mode-clear-visual-line-anchor)
  (vterm-copy-mode 1))

(defun my/vterm-copy-mode-clear-visual-line-anchor ()
  "Release the VTerm visual-line copy marker in the current buffer."
  (when (markerp my/vterm-copy-mode-visual-line-anchor)
    (set-marker my/vterm-copy-mode-visual-line-anchor nil))
  (setq-local my/vterm-copy-mode-visual-line-anchor nil))

(defun my/vterm-copy-mode--anchor-position ()
  "Return the current visual-line anchor position, if any."
  (when (markerp my/vterm-copy-mode-visual-line-anchor)
    (marker-position my/vterm-copy-mode-visual-line-anchor)))

(defun my/vterm-copy-mode--set-visual-line-region ()
  "Update the region to whole lines between anchor and point."
  (when-let* ((anchor (my/vterm-copy-mode--anchor-position)))
    (let* ((anchor-beg anchor)
           (anchor-end (save-excursion
                         (goto-char anchor)
                         (line-end-position)))
           (current-beg (line-beginning-position))
           (current-end (line-end-position)))
      (if (<= anchor current-beg)
          (progn
            (goto-char anchor-beg)
            (push-mark (point) t t)
            (goto-char current-end))
        (goto-char anchor-end)
        (push-mark (point) t t)
        (goto-char current-beg))
      (setq deactivate-mark nil))))

(defun my/vterm-copy-mode-visual-line ()
  "Enter `vterm-copy-mode' and select the current line."
  (interactive)
  (my/vterm-copy-mode-clear-visual-line-anchor)
  (let ((anchor (line-beginning-position)))
    (setq-local my/vterm-copy-mode-visual-line-anchor
                (copy-marker anchor)))
  (vterm-copy-mode 1)
  (goto-char (marker-position my/vterm-copy-mode-visual-line-anchor))
  (my/vterm-copy-mode--set-visual-line-region))

(defun my/vterm-copy-mode-next-line ()
  "Move down in VTerm copy mode, preserving visual-line selection."
  (interactive)
  (forward-line 1)
  (if (my/vterm-copy-mode--anchor-position)
      (my/vterm-copy-mode--set-visual-line-region)
    (setq goal-column nil)))

(defun my/vterm-copy-mode-previous-line ()
  "Move up in VTerm copy mode, preserving visual-line selection."
  (interactive)
  (forward-line -1)
  (if (my/vterm-copy-mode--anchor-position)
      (my/vterm-copy-mode--set-visual-line-region)
    (setq goal-column nil)))

(defun my/vterm-copy-mode-quit ()
  "Leave `vterm-copy-mode' without copying."
  (interactive)
  (my/vterm-copy-mode-clear-visual-line-anchor)
  (vterm-copy-mode -1))

(defun my/vterm-copy-mode-done ()
  "Copy VTerm selection and release visual-line copy state."
  (interactive)
  (unwind-protect
      (vterm-copy-mode-done)
    (my/vterm-copy-mode-clear-visual-line-anchor)))

(defun my/vterm-disable-evil-h ()
  "Keep `vterm' fully detached from `evil'."
  (when (bound-and-true-p evil-local-mode)
    (turn-off-evil-mode)))

(defun my/vterm-disable-word-count-h ()
  "Disable mode-line size and region word counting in `vterm'."
  (setq-local size-indication-mode nil))

(defun my/vterm-low-latency-h ()
  "Prefer lower latency over output coalescing in the current VTerm buffer."
  (setq-local process-adaptive-read-buffering nil))

(defun my/vterm-copy-mode-setup-keys ()
  "Install Vim-like copy-mode keys scoped to VTerm."
  (keymap-set vterm-mode-map "M-v" #'my/vterm-copy-mode-enter)
  (keymap-set vterm-mode-map "M-c" #'my/vterm-copy-mode-visual-line)
  (keymap-set vterm-mode-map "M-V" #'vterm-yank)
  (keymap-set vterm-copy-mode-map "h" #'backward-char)
  (keymap-set vterm-copy-mode-map "j" #'my/vterm-copy-mode-next-line)
  (keymap-set vterm-copy-mode-map "k" #'my/vterm-copy-mode-previous-line)
  (keymap-set vterm-copy-mode-map "l" #'forward-char)
  (keymap-set vterm-copy-mode-map "y" #'my/vterm-copy-mode-done)
  (keymap-set vterm-copy-mode-map "q" #'my/vterm-copy-mode-quit)
  (keymap-set vterm-copy-mode-map "<escape>" #'my/vterm-copy-mode-quit))

(use-package vterm
  :ensure t
  :commands (vterm)
  :init
  ;; Emacs 31 snapshots have shown stale/delayed VTerm redraws on this setup.
  ;; Prefer immediate terminal redraws over timer coalescing; large command
  ;; bursts may cost a little more CPU, but the terminal should not appear stuck.
  (setq vterm-timer-delay nil)
  :hook (vterm-mode . (lambda ()
                        (shell-mode-common-init)
                        (my/terminal-apply-ui)
                        (my/vterm-disable-evil-h)
                        (my/vterm-disable-word-count-h)
                        (my/vterm-low-latency-h)
                        (my/vterm--sync-target-directory-h)))
  :custom
  (vterm-shell "zsh")
  (vterm-always-compile-module nil)
  :config
  (my/vterm-copy-mode-setup-keys)
  (advice-add 'vterm :around #'my/vterm--start-in-home-and-cd))

(with-eval-after-load 'evil
  (evil-set-initial-state 'eshell-mode 'emacs))

(provide 'init-shell)
;;; init-shell.el ends here
