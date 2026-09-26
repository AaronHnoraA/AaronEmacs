;;; init-jupyter-command.el --- Target-owned Jupyter commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Discover the catalog tool on its owning target.  A catalog executable is
;; not the selected kernel interpreter; kernels still use their spec argv.
;; File reads and executable probes use existing Remote/Emacs handlers.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'remote-fs)
(require 'remote-process)

(defun my/jupyter-target-command (context &optional configured)
  "Return a Jupyter command argv owned by CONTEXT.
Prefer CONFIGURED, project environments, then PATH and registered Conda
environments.  Never search the Emacs host for a remote target executable."
  (let* ((root (or (remote-context-workspace-root context)
                   (remote-make-file-name (remote-context-target-id context) "/")))
         (default-directory root)
         (target (remote-context-target-id context))
         (native-root (file-local-name root))
         (environments (mapcar (lambda (name) (expand-file-name name native-root))
                              '(".venv" ".conda" "venv")))
         tried)
    (cl-labels
        ((executable (program)
           (remote-executable-find program context))
         (env-command (environment)
           (let ((command (executable (expand-file-name "bin/jupyter" environment))))
             (when command (list command)))))
      (or (when configured
            (or (when-let* ((command (executable configured))) (list command))
                (user-error "Configured Jupyter executable is unavailable on %s: %s"
                            target configured)))
          (seq-some #'env-command environments)
          (when-let* ((command (executable "jupyter"))) (list command))
          ;; Conda owns this registry.  Read it via the owning file handler;
          ;; no SSH shell startup, home-directory guess or filesystem scan.
          (let* ((registry (remote-expand-file-name "~/.conda/environments.txt" nil context))
                 (registered
                  (when (file-readable-p registry)
                    (with-temp-buffer
                      (insert-file-contents registry)
                      (seq-filter (lambda (path) (string-prefix-p "/" path))
                                  (split-string (buffer-string) "[\r\n]+" t "[ \t]+"))))))
            (setq environments (delete-dups (append environments registered)))
            (seq-some #'env-command registered))
          ;; Python installations can have Jupyter modules without a console
          ;; entry point on PATH.  Test on the target before choosing one.
          (cl-loop for python in
                   (append (mapcar (lambda (environment)
                                     (expand-file-name "bin/python" environment))
                                   environments)
                           '("python3" "python"))
                   for command = (executable python)
                   when (and command (not (member command tried)))
                   do (push command tried)
                   and when
                   (with-temp-buffer
                     (zerop (process-file command nil t nil "-c"
                                          "import jupyter_core.command,jupyter_client.kernelspec")))
                   return (list command "-m" "jupyter"))
          (user-error
           "No Jupyter installation found on %s for %s; select a project environment with jupyter_client installed"
           target native-root)))))

(provide 'init-jupyter-command)
;;; init-jupyter-command.el ends here
