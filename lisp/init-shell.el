;;; init-shell.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-ui)
(require 'remote-core)

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
(declare-function evil-set-initial-state "evil-core")
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function remote-canonicalize-file-name "remote-fs"
                  (file-name &optional directory))
(declare-function remote-environment-apply "remote-environment"
                  (environment &optional buffer))
(declare-function remote-context "remote-fs" (&optional path))
(declare-function remote-context-workspace-id "remote-core" (context))
(declare-function remote-context-target-id "remote-core" (context))
(declare-function remote-get-target "remote-core" (target-id))
(declare-function remote-target-shell "remote-core" (target))
(declare-function remote-terminal-adopt "remote-terminal"
                  (workspace buffer &rest keys))
(declare-function remote-terminal-command "remote-terminal"
                  (&optional workspace profile probe))
(declare-function remote-terminal-metadata "remote-terminal" (terminal))
(declare-function remote-terminal-name "remote-terminal" (terminal))
(declare-function remote-terminal-workspace-id "remote-terminal" (terminal))
(declare-function remote-workspace-context-id "remote-workspace"
                  (&optional context))
(declare-function remote-workspace-environment "remote-workspace" (workspace))
(declare-function remote-workspace-open "remote-workspace"
                  (&optional context &rest keys))
(defvar remote-terminal-instance)
(defvar remote-environment-inhibit)
(defvar my/terminal-startup-cd-inhibited nil
  "When non-nil, retain a caller-supplied terminal startup directory.")

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
                         :foreground 'accent-green-soft))))

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

(require 'init-ghostel)
(require 'init-ghostel-popup)

(with-eval-after-load 'evil
  (evil-set-initial-state 'eshell-mode 'emacs))

(provide 'init-shell)
;;; init-shell.el ends here
