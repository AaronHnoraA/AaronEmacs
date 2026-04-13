;;; init-shell.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

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
(declare-function turn-off-evil-mode "evil")
(declare-function evil-set-initial-state "evil-core")

(defcustom my/vterm-startup-send-delay 0.05
  "Delay between retries when sending startup commands to a fresh VTerm."
  :type 'number
  :group 'term)

(defcustom my/vterm-startup-send-retries 20
  "Maximum retry count for startup commands sent to a fresh VTerm."
  :type 'integer
  :group 'term)

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
    (let* ((directory (my/terminal-normalize-directory default-directory))
           (buffer-name (my/shell-context-buffer-name directory))
           (existing (get-buffer buffer-name))
           (buffer (funcall orig-fn buffer-name file-name)))
      (when existing
        (my/shell--sync-directory buffer directory))
      buffer)))

(defun my/eshell-reuse-by-context-a (orig-fn &optional arg)
  "Reuse `eshell' buffers per local-or-remote context.
Keep the stock prefix-argument behaviour for explicitly creating or selecting
numbered sessions."
  (if arg
      (funcall orig-fn arg)
    (let* ((directory (my/terminal-normalize-directory default-directory))
           (buffer-name (my/eshell-context-buffer-name directory))
           (existing (get-buffer buffer-name))
           (eshell-buffer-name buffer-name)
           (buffer (funcall orig-fn nil)))
      (when existing
        (my/eshell--sync-directory buffer directory))
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
    (kill-buffer (process-buffer proc))
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
         (eshell-mode . my/eshell-emacs-state-setup))
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

(defvar my/vterm-startup-cd-inhibited nil
  "When non-nil, do not inject an initial `cd' into newly created VTerm buffers.")

(defun my/vterm--startup-directory (&optional directory)
  "Return the same-connection home directory for DIRECTORY, or nil."
  (unless my/vterm-startup-cd-inhibited
    (my/terminal-home-directory (or directory default-directory))))

(defun my/vterm--send-cd (buffer directory)
  "Send `cd DIRECTORY' to VTerm BUFFER."
  (when-let* ((buffer (and (buffer-live-p buffer) buffer))
              (command (my/terminal-cd-command directory)))
    (my/vterm-send-command buffer command)))

(defun my/vterm--start-in-home-and-cd (orig-fn &rest args)
  "Create VTerm buffers from home, then `cd' into the original directory."
  (let* ((target-directory (my/terminal-normalize-directory default-directory))
         (startup-directory (and target-directory
                                 (my/vterm--startup-directory target-directory))))
    (if startup-directory
        (let ((default-directory startup-directory))
          (let ((buffer (apply orig-fn args)))
            (my/vterm--send-cd buffer target-directory)
            buffer))
      (apply orig-fn args))))

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

(use-package vterm
  :ensure t
  :commands (vterm)
  :hook (vterm-mode . (lambda ()
                        (when (fboundp 'evil-emacs-state)
                          (evil-emacs-state))
                        (run-at-time
                         0 nil
                         (lambda (buffer)
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (when (bound-and-true-p evil-local-mode)
                                 (turn-off-evil-mode)))))
                         (current-buffer))))
  :custom
  (vterm-shell "zsh")
  (vterm-always-compile-module t)
  :config
  (advice-add 'vterm :around #'my/vterm--start-in-home-and-cd))

(with-eval-after-load 'evil
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs))

(provide 'init-shell)
;;; init-shell.el ends here
