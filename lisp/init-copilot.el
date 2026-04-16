;;; init-copilot.el --- AI-assisted IDE integrations -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; ── GitHub Copilot ────────────────────────────────────────────────────────
(declare-function copilot-server-executable "copilot" ())
(declare-function copilot--overlay-visible "copilot" ())
(declare-function copilot-accept-completion "copilot" (&optional transform-fn))
(declare-function copilot-accept-completion-by-word "copilot" (&optional n))
(declare-function copilot-accept-completion-to-char "copilot" (char &optional count))
(declare-function my/snippet-active-p "init-funcs" ())
(declare-function my/snippet-next-field-dwim "init-funcs" ())
(declare-function my/forward-delimiter-dwim "init-funcs" ())
(declare-function my/backward-delimiter-or-snippet-dwim "init-funcs" ())

(defun my/copilot-available-p ()
  "Return non-nil when Copilot can start in the current environment."
  (and (not buffer-read-only)
       (not (minibufferp))
       (ignore-errors
         (when-let* ((server (copilot-server-executable)))
           (file-exists-p server)))))

(defun my/copilot-auto-enable-h ()
  "Auto-enable `copilot-mode' in supported editing buffers."
  (when (my/copilot-available-p)
    (copilot-mode 1)))

(defun my/copilot-completion-visible-p ()
  "Return non-nil when Copilot currently shows a completion overlay."
  (and (bound-and-true-p copilot-mode)
       (fboundp 'copilot--overlay-visible)
       (copilot--overlay-visible)))

(defun my/forward-delimiter-or-copilot-dwim ()
  "Prefer active snippet/Copilot actions, then jump forward by delimiter."
  (interactive)
  (cond
   ((my/snippet-active-p)
    (my/snippet-next-field-dwim))
   ((and (fboundp 'copilot-accept-completion)
         (my/copilot-completion-visible-p))
    (copilot-accept-completion))
   (t
    (my/forward-delimiter-dwim))))

(defun my/forward-delimiter-or-copilot-by-word-dwim ()
  "Prefer snippet field advance, then Copilot accept-by-word, then jump."
  (interactive)
  (cond
   ((my/snippet-active-p)
    (my/snippet-next-field-dwim))
   ((and (fboundp 'copilot-accept-completion-by-word)
         (my/copilot-completion-visible-p))
    (copilot-accept-completion-by-word))
   ((and (fboundp 'copilot-accept-completion)
         (my/copilot-completion-visible-p))
    (copilot-accept-completion))
   (t
    (my/forward-delimiter-dwim))))

(defun my/forward-delimiter-or-copilot-to-char-dwim ()
  "Prefer snippet field advance, then Copilot accept-to-char, then jump."
  (interactive)
  (cond
   ((my/snippet-active-p)
    (my/snippet-next-field-dwim))
   ((and (fboundp 'copilot-accept-completion-to-char)
         (my/copilot-completion-visible-p))
    (call-interactively #'copilot-accept-completion-to-char))
   ((and (fboundp 'copilot-accept-completion)
         (my/copilot-completion-visible-p))
    (copilot-accept-completion))
   (t
    (my/forward-delimiter-dwim))))

(defun my/copilot-setup-dwim-keys (keymap)
  "Install shared DWIM navigation/accept keys into KEYMAP."
  (define-key keymap (kbd "M-]") #'my/forward-delimiter-or-copilot-dwim)
  (define-key keymap (kbd "M-[") #'my/backward-delimiter-or-snippet-dwim)
  (define-key keymap (kbd "M-\\") #'my/forward-delimiter-or-copilot-by-word-dwim)
  (define-key keymap (kbd "M-}") #'my/forward-delimiter-or-copilot-to-char-dwim)
  (define-key keymap (kbd "M-(") nil)
  (define-key keymap (kbd "M-)") nil))

(global-set-key (kbd "M-]") #'my/forward-delimiter-or-copilot-dwim)
(global-set-key (kbd "M-[") #'my/backward-delimiter-or-snippet-dwim)
(global-set-key (kbd "M-\\") #'my/forward-delimiter-or-copilot-by-word-dwim)
(global-set-key (kbd "M-}") #'my/forward-delimiter-or-copilot-to-char-dwim)

(use-package copilot
  :ensure t
  :hook ((prog-mode . my/copilot-auto-enable-h)
         (org-mode . my/copilot-auto-enable-h))
  :custom
  (copilot-install-dir (expand-file-name "var/copilot" user-emacs-directory))
  (copilot-idle-delay 0.30)
  (copilot-indent-offset-warning-disable t)
  (copilot-lsp-settings '(:github (:copilot ())))
  :config
  (my/copilot-setup-dwim-keys copilot-mode-map)
  (my/copilot-setup-dwim-keys copilot-completion-map)
  (defun my/copilot-check-status ()
    "Report current `copilot.el' authentication status.

Compatibility wrapper for old `lsp-copilot-check-status' workflows."
    (interactive)
    (let* ((response (copilot--request 'checkStatus nil))
           (status (plist-get response :status))
           (user (plist-get response :user)))
      (message "%s"
               (cond
                ((and (stringp user) (not (string-empty-p user)))
                 (format "Copilot is signed in as %s%s"
                         user
                         (if (and (stringp status) (not (string-empty-p status)))
                             (format " [%s]" status)
                           "")))
                ((and (stringp status) (not (string-empty-p status)))
                 (format "Copilot status: %s" status))
                (t
                 (format "Copilot status response: %S" response))))))
  (defun my/copilot-check-quota ()
    "Report quota or entitlement information from the Copilot server."
    (interactive)
    (message "Copilot quota: %S" (copilot--request 'checkQuota nil)))
  (when (and (fboundp 'my/copilot--suppress-cancelled-errors)
             (advice-member-p #'my/copilot--suppress-cancelled-errors 'copilot--log))
    (advice-remove 'copilot--log #'my/copilot--suppress-cancelled-errors))
  (defalias 'lsp-copilot-check-status #'my/copilot-check-status)
  (defalias 'lsp-copilot-login #'copilot-login)
  (defalias 'lsp-copilot-logout #'copilot-logout))





(provide 'init-copilot)
;;; init-copilot.el ends here
