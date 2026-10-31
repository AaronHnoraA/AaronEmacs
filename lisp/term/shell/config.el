;;; term/shell/config.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'config)
(require 'subr-x)

(require 'aaron-ui)
(require 'remote-core)

(defvar eshell-last-dir-ring)
(defvar eshell-buffer-name)

(declare-function aaron-terminal-cd-command "aaron-funcs" (directory))
(declare-function aaron-terminal-home-directory "aaron-funcs" (&optional directory))
(declare-function aaron-terminal-normalize-directory "aaron-funcs" (directory))
(declare-function comint-simple-send "comint" (proc string))
(declare-function ring-elements "ring")
(declare-function eshell/cd "esh-mode")
(declare-function eshell-mode "esh-mode" ())
(declare-function eshell-reset "esh-mode")
(declare-function eshell-grep "em-unix")
(declare-function eshell-input-filter-default "em-hist" (input))
(declare-function eshell-input-filter-initial-space "em-hist" (input))
(declare-function +eshell-busy-p "term/shell/eshell" (&optional buffer))
(declare-function +eshell--display "term/shell/eshell" (buffer))
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
(defvar +terminal-startup-cd-inhibited nil
  "When non-nil, retain a caller-supplied terminal startup directory.")

(defun +terminal--startup-directory (&optional directory)
  "Return a safe startup directory for DIRECTORY, or nil to use DIRECTORY directly."
  (unless +terminal-startup-cd-inhibited
    (aaron-terminal-home-directory (or directory default-directory))))

(defun +terminal--resolve-launch-directories (&optional directory)
  "Return (STARTUP-DIRECTORY . TARGET-DIRECTORY) for terminal launch."
  (when-let* ((target-directory (aaron-terminal-normalize-directory
                                 (or directory default-directory))))
    (cons (or (+terminal--startup-directory target-directory)
              target-directory)
          target-directory)))


(defun shell-mode-common-init ()
  "The common initialization procedure for term/shell."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil))

(defun +terminal-apply-ui ()
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

(defun +terminal-context-key (&optional directory)
  "Return a stable local-or-remote context key for DIRECTORY."
  (when-let* ((directory (aaron-terminal-normalize-directory
                          (or directory default-directory))))
    (if-let* ((remote-prefix (file-remote-p directory))
              (_ (not (and (remote-fs-file-name-p directory)
                           (equal (remote-file-name-target directory) "local")))))
        (replace-regexp-in-string
         "[^[:alnum:]@._#-]+"
         ":"
         (replace-regexp-in-string "\\`/+\\|:+\\'" "" remote-prefix))
      "local")))

(defun +terminal-context-buffer-name (base-name &optional directory)
  "Return BASE-NAME specialized for DIRECTORY's local-or-remote context."
  (let ((context-key (+terminal-context-key directory)))
    (if (or (null context-key)
            (string= context-key "local"))
        base-name
      (format "%s:%s*"
              (if (string-suffix-p "*" base-name)
                  (substring base-name 0 -1)
                base-name)
              context-key))))

(defun +shell--sync-directory (buffer directory)
  "Change shell BUFFER to DIRECTORY."
  (when-let* ((buffer (and (buffer-live-p buffer) buffer))
              (directory (aaron-terminal-normalize-directory directory))
              (process (get-buffer-process buffer))
              ((process-live-p process))
              (command (aaron-terminal-cd-command directory)))
    (with-current-buffer buffer
      (unless (equal (aaron-terminal-normalize-directory default-directory)
                     directory)
        (setq default-directory directory)
        (comint-simple-send process command)))))

(defun +shell--ensure-directory (buffer directory)
  "Ensure shell BUFFER is associated with DIRECTORY."
  (when-let* ((buffer (and (buffer-live-p buffer) buffer))
              (directory (aaron-terminal-normalize-directory directory)))
    (with-current-buffer buffer
      (setq default-directory directory))
    (+shell--sync-directory buffer directory)))

(defun +eshell--sync-directory (buffer directory)
  "Change Eshell BUFFER to DIRECTORY and redraw its prompt."
  (when-let* ((buffer (and (buffer-live-p buffer) buffer))
              (directory (aaron-terminal-normalize-directory directory)))
    (with-current-buffer buffer
      (unless (equal (aaron-terminal-normalize-directory default-directory)
                     directory)
        (eshell/cd directory)
        (setq default-directory directory)
        (eshell-reset)
        (goto-char (point-max))))))

(defun +eshell--ensure-directory (buffer directory)
  "Ensure Eshell BUFFER is associated with DIRECTORY."
  (when-let* ((buffer (and (buffer-live-p buffer) buffer))
              (directory (aaron-terminal-normalize-directory directory)))
    (unless (+eshell-busy-p buffer)
      (+eshell--sync-directory buffer directory))))

(defun +shell-context-buffer-name (&optional directory)
  "Return the standard shell buffer name for DIRECTORY."
  (+terminal-context-buffer-name "*shell*" directory))

(defun +shell-popup-buffer-name (&optional directory)
  "Return the popup shell buffer name for DIRECTORY."
  (+terminal-context-buffer-name "*shell-popup*" directory))

(defun +eshell-context-buffer-name (&optional directory task-name)
  "Return the Eshell buffer name for DIRECTORY and optional TASK-NAME.
Unnamed sessions keep the ordinary per-target name.  A named automation task
has a separate queue and a readable target/task label in buffer lists."
  (if (not task-name)
      (+terminal-context-buffer-name "*eshell*" directory)
    (unless (and (stringp task-name)
                 (not (string-empty-p (string-trim task-name))))
      (user-error "Eshell task name must be a nonempty string"))
    (let ((label (replace-regexp-in-string
                  "[^[:alnum:]_.-]+" "-" (string-trim task-name))))
      (setq label (substring label 0 (min 48 (length label))))
      (format "*eshell:%s/%s*"
              (or (+terminal-context-key directory) "local") label))))

(defun +shell-reuse-by-context-a (orig-fn &optional buffer file-name)
  "Reuse `shell' buffers per local-or-remote context."
  (if buffer
      (funcall orig-fn buffer file-name)
    (let* ((directories (+terminal--resolve-launch-directories default-directory))
           (startup-directory (car directories))
           (target-directory (cdr directories))
           (buffer-name (+shell-context-buffer-name target-directory))
           (buffer (let ((default-directory startup-directory))
                     (funcall orig-fn buffer-name file-name))))
      (+shell--ensure-directory buffer target-directory)
      buffer)))

(defun +eshell--make-buffer (name directory &optional force-new)
  "Initialize an Eshell buffer named NAME in DIRECTORY without displaying it.
FORCE-NEW uses a fresh numbered buffer even when NAME already exists."
  (let ((buffer (if force-new
                    (generate-new-buffer name)
                  (get-buffer-create name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'eshell-mode)
        (setq default-directory directory)
        (eshell-mode)))
    buffer))

(defun +eshell-reuse-by-context-a (_orig-fn &optional arg)
  "Reuse `eshell' buffers per local-or-remote context.
Keep the stock prefix-argument behaviour for explicitly creating or selecting
numbered sessions."
  (let* ((directory (aaron-terminal-normalize-directory default-directory))
         (name (if arg
                   (if (numberp arg)
                       (format "%s<%d>" eshell-buffer-name arg)
                     eshell-buffer-name)
                 (+eshell-context-buffer-name directory)))
         (buffer (+eshell--make-buffer name directory
                                       (and arg (not (numberp arg))))))
    (unless arg
      (+eshell--ensure-directory buffer directory))
    (+eshell--display buffer)
    buffer))

(defun +eshell-emacs-state-setup ()
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

(defun +shell--send-dropped-paths (files)
  "Send the dropped FILES to this terminal as typed text."
  (term-send-raw-string (+macos-drop-text files)))

(set-file-drop-handler! 'shell-mode #'+macos-drop-insert-paths)
(set-file-drop-handler! 'term-mode #'+shell--send-dropped-paths)

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
         (eshell-mode . +eshell-emacs-state-setup)
         (eshell-mode . +terminal-apply-ui))
  :config
  (advice-remove 'eshell #'+eshell-reuse-by-context-a)
  (advice-add 'eshell :around #'+eshell-reuse-by-context-a)
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

  ;; From Doom: `emacs FILE' opens FILE here instead of starting a nested
  ;; Emacs inside the shell.
  (defun eshell/emacs (&rest files)
    "Visit FILES in this Emacs."
    (mapc #'find-file (mapcar #'expand-file-name (flatten-tree files))))
  (defalias 'eshell/e #'eshell/emacs)

  (defun eshell/rg (&rest args)
    "ripgrep with eshell integration."
    (eshell-grep "rg" (append '("--no-heading") args) t))
  :custom
  (eshell-directory-name
   (or (and (boundp 'aaron-eshell-state-dir) aaron-eshell-state-dir)
       (expand-file-name "var/eshell/" user-emacs-directory)))
  ;; The following cmds will run on term.
  (eshell-visual-commands '("top" "htop" "less" "more" "telnet"))
  (eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show")))
  (eshell-visual-options '(("git" "--help" "--paginate")))
  ;; Completion like bash
  (eshell-cmpl-ignore-case t)
  (eshell-cmpl-cycle-completions nil))

(defun +eshell-history-input-p (input)
  "Return non-nil when INPUT belongs in the history.
Blank input never does, nor, as with a shell's ignorespace, input that starts
with a space: a command carrying a secret can be kept out (from Doom)."
  (and (eshell-input-filter-default input)
       (eshell-input-filter-initial-space input)))

(use-package em-hist
  :ensure nil
  :hook (eshell-hist-load . eshell-hist-initialize)
  :bind (:map eshell-hist-mode-map
         ("M-r" . consult-history))
  :custom
  (eshell-history-size 10000)
  ;; From Doom: a command repeated back to back is recorded once.
  (eshell-hist-ignoredups t)
  (eshell-input-filter #'+eshell-history-input-p))

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
  (advice-remove 'shell #'+shell-reuse-by-context-a)
  (advice-add 'shell :around #'+shell-reuse-by-context-a)
  (defun shell-toggle ()
    "Toggle a persistent shell popup window.
If popup is visible but unselected, select it.
If popup is focused, kill it."
    (interactive)
    (let* ((directory (aaron-terminal-normalize-directory default-directory))
           (buffer-name (+shell-popup-buffer-name directory)))
      (if-let* ((win (get-buffer-window buffer-name)))
          (if (eq (selected-window) win)
              ;; If users attempt to delete the sole ordinary window, silence it.
              (shell-delete-window win)
            (progn
              (+shell--sync-directory (window-buffer win) directory)
              (select-window win)))
        (let ((display-buffer-alist '(((category . comint)
                                       (display-buffer-at-bottom))))
              (existing (get-buffer buffer-name)))
          (let ((default-directory directory))
            (when-let* ((buffer (shell buffer-name)))
              (when existing
                (+shell--sync-directory buffer directory))
              (when-let* ((proc (ignore-errors (get-buffer-process buffer))))
                (set-process-sentinel proc #'shell-self-destroy-sentinel))))))))

  ;; Correct indentation for `ls'
  (defun revert-tab-width-to-default ()
    "Revert `tab-width' to default value."
    (setq-local tab-width 8))
  :custom
  (shell-kill-buffer-on-exit t)
  (shell-get-old-input-include-continuation-lines t))

(defvar +ssh-host-history nil)

(defun +ssh-config-hosts ()
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

(defun +read-ssh-host ()
  "Read an SSH host, preferring entries from `~/.ssh/config'."
  (let ((hosts (+ssh-config-hosts)))
    (if hosts
        (completing-read "SSH host: " hosts nil nil nil '+ssh-host-history)
      (read-string "SSH host: " nil '+ssh-host-history))))

(require 'term/shell/eshell)
(require 'term/portal/config)
(require 'term/portal/popup)

(set-evil-initial-state! 'eshell-mode 'emacs)

(provide 'term/shell/config)
;;; term/shell/config.el ends here
