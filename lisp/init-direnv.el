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

(defun my/direnv--sync-after-compile (buffer _status)
  "Refresh direnv using BUFFER's working directory after compilation finishes.

Runs after every compile/task/test so environment changes made by the task
(e.g. nix build, nix develop, conda env create) are immediately visible."
  (unless my/direnv-subprocess-sync-inhibited
    (with-current-buffer buffer
      (my/direnv-update-environment-maybe default-directory))))

(use-package direnv
  :ensure t
  :if my/enable-direnv
  :demand t
  :hook (find-file . my/direnv-update-environment-maybe)
  :custom
  (direnv-always-show-summary nil)
  :config
  (advice-add 'compile :around #'my/direnv--sync-before-subprocess)
  (add-hook 'compilation-finish-functions #'my/direnv--sync-after-compile)
  (direnv-mode 1))

(provide 'init-direnv)

;;; init-direnv.el ends here
