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

(defvar my/direnv--env-settle-roots nil
  "Project roots where post-compile exec-path settling is active.
A root is added once, when Emacs loads dir-locals for a buffer whose project
also has an .envrc.  Compilation-finish checks are pure string ops against
this list — no I/O on every compile.")

(defun my/direnv--maybe-register-settle-root ()
  "Register current buffer's .envrc root for post-compile env settling.

Runs from `hack-local-variables-hook', so it only fires when Emacs actually
processes dir-locals for a buffer.  Short-circuits immediately via the
`dir-local-variables-alist' check when no dir-locals were loaded — zero I/O
for files in ordinary projects.  One `locate-dominating-file' call happens
at most once per buffer-open for qualified projects."
  (when (and my/enable-direnv
             buffer-file-name
             (not (file-remote-p default-directory))
             (bound-and-true-p dir-local-variables-alist))
    (when-let* ((root (locate-dominating-file default-directory ".envrc")))
      (cl-pushnew (expand-file-name root)
                  my/direnv--env-settle-roots
                  :test #'equal))))

(defun my/direnv--apply-path-to-exec-path ()
  "Sync `exec-path' from the current process-environment PATH.

Direnv already updated process-environment before the task ran.  This call
settles exec-path so Emacs's command lookup matches the environment that was
actually active during the task — without shelling out to direnv again."
  (when-let* ((path (getenv "PATH")))
    (setq exec-path
          (append (split-string path path-separator t)
                  (list exec-directory)))))

(defun my/direnv--settle-env-after-compile (buffer _status)
  "Settle exec-path after compilation if BUFFER is under a registered root.

Guard is a pure string check against `my/direnv--env-settle-roots' — no I/O.
Roots are registered lazily by `my/direnv--maybe-register-settle-root' only
when dir-locals are actually loaded for a qualifying project."
  (when (and my/enable-direnv
             (not my/direnv-subprocess-sync-inhibited)
             my/direnv--env-settle-roots)
    (with-current-buffer buffer
      (when (and (not (file-remote-p default-directory))
                 (seq-some (lambda (root)
                             (file-in-directory-p default-directory root))
                           my/direnv--env-settle-roots))
        (my/direnv--apply-path-to-exec-path)))))

(use-package direnv
  :ensure t
  :if my/enable-direnv
  :demand t
  :hook (find-file . my/direnv-update-environment-maybe)
  :custom
  (direnv-always-show-summary nil)
  :config
  (advice-add 'compile :around #'my/direnv--sync-before-subprocess)
  (add-hook 'hack-local-variables-hook #'my/direnv--maybe-register-settle-root)
  (add-hook 'compilation-finish-functions #'my/direnv--settle-env-after-compile)
  (direnv-mode 1))

(provide 'init-direnv)

;;; init-direnv.el ends here
