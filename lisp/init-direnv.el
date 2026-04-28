;;; init-direnv.el --- Project environment integration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function direnv-update-environment "direnv" (&optional file-name force-summary))
(declare-function direnv-update-directory-environment "direnv" (&optional directory force-summary))
(declare-function direnv--maybe-update-environment "direnv" ())

(defvar direnv-always-show-summary)
(defvar direnv--hooks)

(defcustom my/enable-direnv t
  "Whether to enable `direnv-mode' automatically."
  :type 'boolean
  :group 'environment)

(defvar my/direnv-subprocess-sync-inhibited nil
  "When non-nil, skip automatic direnv sync advice for subprocess commands.")

(defvar my/direnv--selection-sync-timer nil
  "Idle timer used to coalesce direnv sync after buffer/window changes.")

(defun my/direnv--resolve-directory (&optional path)
  "Return the local directory that should be used for direnv refresh."
  (let* ((target (or path buffer-file-name default-directory))
         (expanded (and target (expand-file-name target)))
         (directory (cond
                     ((null expanded) nil)
                     ((file-directory-p expanded)
                      (file-name-as-directory expanded))
                     (t
                       (file-name-directory expanded)))))
    (when (and directory
               (not (file-remote-p directory))
               (file-directory-p directory))
      directory)))

(defun my/direnv--envrc-directory-p (directory)
  "Return non-nil when DIRECTORY is controlled by a `.envrc'."
  (and directory
       (locate-dominating-file directory ".envrc")))

(defun my/direnv-update-environment-maybe (&optional path)
  "Refresh the buffer environment from direnv for PATH or the current buffer."
  (when-let* ((directory (my/direnv--resolve-directory path)))
    (when (and my/enable-direnv
               (my/direnv--envrc-directory-p directory)
               (or (fboundp 'direnv-update-directory-environment)
                   (require 'direnv nil t)))
      (let ((direnv-always-show-summary nil))
        (ignore-errors
          (direnv-update-directory-environment directory nil))))))

(defun my/direnv--sync-selected-buffer ()
  "Refresh direnv for the selected window's buffer after UI selection settles."
  (setq my/direnv--selection-sync-timer nil)
  (when-let* ((window (selected-window))
              ((window-live-p window))
              (buffer (window-buffer window))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (my/direnv-update-environment-maybe default-directory))))

(defun my/direnv-schedule-selected-buffer-sync (&rest _)
  "Coalesce direnv refresh after buffer or window selection changes."
  (when (and my/enable-direnv
             (not (timerp my/direnv--selection-sync-timer)))
    (setq my/direnv--selection-sync-timer
          (run-with-idle-timer
           0.1 nil #'my/direnv--sync-selected-buffer))))

(defun my/direnv--maybe-update-environment-a (orig-fn &rest args)
  "Skip package-level direnv checks in directories without `.envrc'."
  (when (and my/enable-direnv
             (my/direnv--envrc-directory-p
              (my/direnv--resolve-directory default-directory)))
    (apply orig-fn args)))

(defun my/direnv--sync-before-subprocess (orig-fn &rest args)
  "Refresh direnv for `default-directory' before calling ORIG-FN with ARGS."
  (unless my/direnv-subprocess-sync-inhibited
    (my/direnv-update-environment-maybe default-directory))
  (apply orig-fn args))

(defun my/direnv--settle-env-after-dir-locals ()
  "Sync `exec-path' from `process-environment' after dir-locals finish loading.

Runs from `hack-local-variables-hook'.  `dir-local-variables-alist' being
non-nil is Emacs's own confirmation that a .dir-locals.el was found and its
variables applied — including any `eval' forms that may have modified
`process-environment' (e.g. direnv, nix-shell, conda activate).
Zero cost for buffers with no dir-locals: the alist check short-circuits
before any I/O."
  (when (and my/enable-direnv
             buffer-file-name
             (not (file-remote-p default-directory))
             (bound-and-true-p dir-local-variables-alist))
    (when-let* ((path (getenv "PATH")))
      (setq exec-path
            (append (split-string path path-separator t)
                    (list exec-directory))))))

(use-package direnv
  :ensure t
  :if my/enable-direnv
  :demand t
  :hook (find-file . my/direnv-update-environment-maybe)
  :init
  ;; The package default includes `post-command-hook'.  Buffer/window changes
  ;; are enough for this config and avoid checking direnv after every command.
  (setq direnv--hooks '(before-hack-local-variables-hook))
  :custom
  (direnv-always-show-summary nil)
  :config
  (advice-add 'direnv--maybe-update-environment
              :around #'my/direnv--maybe-update-environment-a)
  (advice-add 'compile :around #'my/direnv--sync-before-subprocess)
  (add-hook 'buffer-list-update-hook #'my/direnv-schedule-selected-buffer-sync)
  (add-hook 'window-selection-change-functions
            #'my/direnv-schedule-selected-buffer-sync)
  (add-hook 'hack-local-variables-hook #'my/direnv--settle-env-after-dir-locals)
  (direnv-mode 1))

(provide 'init-direnv)

;;; init-direnv.el ends here
