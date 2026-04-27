;;; init-direnv.el --- Project environment integration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function direnv-update-environment "direnv" (&optional file-name force-summary))
(declare-function direnv-update-directory-environment "direnv" (&optional directory force-summary))

(defvar direnv-always-show-summary)

(defcustom my/enable-direnv t
  "Whether to enable `direnv-mode' automatically."
  :type 'boolean
  :group 'environment)

(defvar my/direnv-subprocess-sync-inhibited nil
  "When non-nil, skip automatic direnv sync advice for subprocess commands.")

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

(defun my/direnv-update-environment-maybe (&optional path)
  "Refresh the buffer environment from direnv for PATH or the current buffer."
  (when (and my/enable-direnv
             (or (fboundp 'direnv-update-environment)
                 (require 'direnv nil t)))
    (when-let* ((directory (my/direnv--resolve-directory path)))
      (let ((direnv-always-show-summary nil))
        (ignore-errors
          (direnv-update-directory-environment directory nil))))))

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
  :custom
  (direnv-always-show-summary nil)
  :config
  (advice-add 'compile :around #'my/direnv--sync-before-subprocess)
  (add-hook 'hack-local-variables-hook #'my/direnv--settle-env-after-dir-locals)
  (direnv-mode 1))

(provide 'init-direnv)

;;; init-direnv.el ends here
