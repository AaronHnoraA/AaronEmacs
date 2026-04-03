;;; init-shell.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar eshell-last-dir-ring)

(declare-function my/terminal-cd-command "init-funcs" (directory))
(declare-function my/terminal-home-directory "init-funcs" (&optional directory))
(declare-function my/terminal-normalize-directory "init-funcs" (directory))
(declare-function ring-elements "ring")
(declare-function eshell/cd "esh-mode")
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
  (defun shell-toggle ()
    "Toggle a persistent shell popup window.
If popup is visible but unselected, select it.
If popup is focused, kill it."
    (interactive)
    (if-let* ((win (get-buffer-window "*shell-popup*")))
        (if (eq (selected-window) win)
            ;; If users attempt to delete the sole ordinary window, silence it.
            (shell-delete-window)
          (select-window win))
      (let ((display-buffer-alist '(((category . comint)
                                     (display-buffer-at-bottom)))))
        (when-let* ((proc (ignore-errors (get-buffer-process (shell "*shell-popup*")))))
          (set-process-sentinel proc #'shell-self-destroy-sentinel)))))

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
  :config
  (advice-add 'vterm :around #'my/vterm--start-in-home-and-cd))

(with-eval-after-load 'evil
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs))

(provide 'init-shell)
;;; init-shell.el ends here
