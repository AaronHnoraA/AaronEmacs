;;; init-shell.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar eshell-last-dir-ring)

(declare-function ring-elements "ring")
(declare-function eshell/cd "esh-mode")
(declare-function eshell-grep "em-unix")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function turn-off-evil-mode "evil")
(declare-function evil-set-initial-state "evil-core")

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

(global-set-key (kbd "M-`") #'vterm-toggle)
(global-set-key (kbd "C-c e") #'vterm-toggle)
(global-set-key (kbd "C-c E") #'my/vterm-toggle-fixed)

(defcustom my/vterm-popup-window-height 0.2
  "Height of the shared vterm popup window."
  :type 'number
  :group 'term)

(defvar my/vterm-popup-fixed nil
  "Whether the shared vterm popup should stay visible after focus changes.")

(defun my/vterm--window ()
  "Return the visible shared vterm window, if any."
  (get-buffer-window "*vterm-popup*" (selected-frame)))

(defun my/vterm--delete-own-window ()
  "Delete the selected vterm popup window if it is visible."
  (setq my/vterm-popup-fixed nil)
  (when-let* ((win (get-buffer-window (current-buffer) (selected-frame))))
    (ignore-errors (delete-window win))))

(defun my/vterm-hide-popup ()
  "Hide the shared vterm popup window."
  (interactive)
  (setq my/vterm-popup-fixed nil)
  (when-let* ((win (my/vterm--window)))
    (ignore-errors (delete-window win))))

(defun my/vterm--ensure-buffer ()
  "Return the shared vterm popup buffer, creating it when needed."
  (or (get-buffer "*vterm-popup*")
      (progn
        (require 'vterm)
        (with-current-buffer (save-window-excursion (vterm "*vterm-popup*"))
          (add-hook 'kill-buffer-hook #'my/vterm--delete-own-window nil t)
          (current-buffer)))))

(defun my/vterm-show-popup ()
  "Show the shared vterm buffer in a top popup window."
  (let ((window (display-buffer-in-side-window
                 (my/vterm--ensure-buffer)
                 `((side . top)
                   (slot . 1)
                   (window-height . ,my/vterm-popup-window-height)))))
    (set-window-parameter window 'my-vterm-popup t)
    (set-window-parameter window 'my-vterm-fixed my/vterm-popup-fixed)
    (set-window-parameter window 'no-delete-other-windows t)
    (window-preserve-size window nil t)
    window))

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
  (let ((buffer-name (format "*vterm:ssh:%s*" host)))
    (pop-to-buffer
     (or (get-buffer buffer-name)
         (with-current-buffer (vterm buffer-name)
           (vterm-send-string (format "ssh %s" host))
           (vterm-send-return)
           (current-buffer))))))

(defun my/vterm-toggle-fixed ()
  "Toggle whether the shared vterm popup stays visible when it loses focus."
  (interactive)
  (setq my/vterm-popup-fixed (not my/vterm-popup-fixed))
  (let ((win (or (my/vterm--window)
                 (my/vterm-show-popup))))
    (set-window-parameter win 'my-vterm-fixed my/vterm-popup-fixed)
    (select-window win))
  (message "VTerm popup %s"
           (if my/vterm-popup-fixed "fixed" "temporary")))

(defun my/vterm--auto-hide-popup (&rest _)
  "Hide the shared vterm popup when it loses focus in temporary mode."
  (if-let* ((win (my/vterm--window)))
      (unless (or my/vterm-popup-fixed
                  (active-minibuffer-window)
                  (eq (selected-window) win))
        (my/vterm-hide-popup))
    (setq my/vterm-popup-fixed nil)))

(defun vterm-toggle ()
  "Toggle the shared temporary vterm popup window."
  (interactive)
  (if-let* ((win (my/vterm--window)))
      (if (eq (selected-window) win)
          (my/vterm-hide-popup)
        (select-window win))
    (setq my/vterm-popup-fixed nil)
    (select-window (my/vterm-show-popup))))

(use-package vterm
  :ensure t
  :commands (vterm vterm-toggle my/vterm-toggle-fixed my/vterm-named my/vterm-ssh)
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
  (vterm-shell "zsh"))

(add-hook 'window-selection-change-functions #'my/vterm--auto-hide-popup)

(with-eval-after-load 'evil
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs))

(provide 'init-shell)
;;; init-shell.el ends here
