;;; init-sage.el --- SageMath configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Sage editing, REPL and LSP support.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar eglot-server-programs)

(declare-function eglot-current-server "eglot")
(declare-function eglot-ensure "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function eglot-shutdown "eglot" (server))
(declare-function my/eglot-ensure "init-lsp")

(defgroup my/sage nil
  "SageMath editing and execution support."
  :group 'languages)

(defcustom my/sage-local-executable-path
  (or (executable-find "sage") "/usr/local/bin/sage")
  "Local Sage executable used on macOS."
  :type 'string
  :group 'my/sage)

(defcustom my/sage-remote-executable-path
  "/home/hc/anaconda3/envs/sage/bin/sage"
  "Remote Sage executable used on Aaron-WSL2."
  :type 'string
  :group 'my/sage)

(defvar my/sage--local-pythonpath-cache nil
  "Cached Python path exported by the local Sage runtime.")

(defcustom my/sage-eglot-sync-connect 1
  "Seconds to block while connecting Eglot in Sage buffers.

Sage files benefit from a small synchronous wait so the first LSP
session is actually ready when the buffer finishes opening."
  :type '(choice (const :tag "Never block" nil)
                 (integer :tag "Seconds to block"))
  :group 'my/sage)

(defvar my/sage--allow-eglot-ensure nil
  "Dynamically non-nil when Sage buffers may call `eglot-ensure' now.")

(defun my/sage-local-executable ()
  "Return the local Sage executable."
  my/sage-local-executable-path)

(defun my/sage-remote-executable ()
  "Return the remote Sage executable."
  my/sage-remote-executable-path)

(defun my/sage-local-python-command ()
  "Return the local Sage-backed Python command."
  (format "%s --python" (my/sage-local-executable)))

(defun my/sage-remote-python-command ()
  "Return the remote Sage-backed Python command."
  (format "%s --python" (my/sage-remote-executable)))

(defun my/sage--call-process-string (program &rest args)
  "Run PROGRAM with ARGS and return trimmed stdout on success."
  (when (and (stringp program)
             (file-executable-p program))
    (with-temp-buffer
      (when (zerop (apply #'process-file program nil t nil args))
        (string-trim (buffer-string))))))

(defun my/sage-local-pythonpath ()
  "Return a Python path string compatible with the local Sage install."
  (or my/sage--local-pythonpath-cache
      (setq my/sage--local-pythonpath-cache
            (my/sage--call-process-string
             (my/sage-local-executable)
             "--python"
             "-c"
             "import sys; print(':'.join(p for p in sys.path if p))"))))

(defun my/sage-local-pythonpath-entries ()
  "Return Sage's Python search path as a list of directories."
  (split-string (or (my/sage-local-pythonpath) "") path-separator t))

(defun my/sage-eglot-contact (_interactive _project)
  "Return the Eglot contact for Sage buffers."
  '("python3" "-m" "pylsp"))

(defun my/sage-eglot-workspace-configuration ()
  "Return workspace settings for `pylsp' in Sage buffers."
  `(:pylsp
    (:plugins
     (:jedi
      (:extra_paths ,(vconcat (my/sage-local-pythonpath-entries))
       :prioritize_extra_paths t)))))

(defun my/sage--skip-early-eglot-ensure (orig-fn &rest args)
  "Defer automatic `eglot-ensure' calls in Sage buffers.

This only suppresses the automatic startup that happens while the mode
hooks are still running. Manual `M-x eglot-ensure' remains available."
  (if (and (derived-mode-p 'sage-shell:sage-mode)
           (not my/sage--allow-eglot-ensure)
           (not (memq this-command
                      '(eglot eglot-ensure my/eglot-ensure my/language-server-ensure))))
      nil
    (apply orig-fn args)))

(defun my/sage-eglot-ensure ()
  "Start Eglot for the current Sage buffer with Sage-aware settings."
  (when (and buffer-file-name
             (fboundp 'my/eglot-ensure))
    (setq-local eglot-workspace-configuration
                (my/sage-eglot-workspace-configuration))
    (setq-local eglot-sync-connect my/sage-eglot-sync-connect)
    (when (and (fboundp 'eglot-managed-p)
               (eglot-managed-p))
      (ignore-errors
        (eglot-shutdown (eglot-current-server))))
    (let ((my/sage--allow-eglot-ensure t))
      (my/eglot-ensure))))

(defun my/sage-mode-setup ()
  "Configure Python and LSP integration for `sage-shell:sage-mode'."
  (setq-local python-shell-interpreter (my/sage-local-executable))
  (setq-local python-shell-interpreter-args "--python")
  (setq-local python-shell-extra-pythonpaths (my/sage-local-pythonpath-entries))
  (my/sage-eglot-ensure))

(my/package-ensure-vc 'sage-shell "https://github.com/sagemath/sage-shell-mode.git")

(use-package sage-shell-mode
  :commands (sage-shell:define-alias
             sage-shell:run-new-sage
             sage-shell:run-sage
             sage-shell:sage-mode)
  :mode ("\\.sage\\'" . sage-shell:sage-mode)
  :init
  (setq sage-shell:sage-executable (my/sage-local-executable)
        sage-shell:use-prompt-toolkit nil
        sage-shell:use-simple-prompt t
        sage-shell:set-ipython-version-on-startup nil
        sage-shell:check-ipython-version-on-startup nil)
  :hook ((sage-shell-mode . eldoc-mode)
         (sage-shell:sage-mode . eldoc-mode)
         (sage-shell:sage-mode . my/sage-mode-setup))
  :config
  (sage-shell:define-alias)
  (with-eval-after-load 'eglot
    (unless (advice-member-p #'my/sage--skip-early-eglot-ensure 'eglot-ensure)
      (advice-add 'eglot-ensure :around #'my/sage--skip-early-eglot-ensure))
    (add-to-list 'eglot-server-programs
                 '((sage-shell:sage-mode :language-id "python")
                   . my/sage-eglot-contact))))

(provide 'init-sage)
;;; init-sage.el ends here
