;;; init-jupyter.el --- Jupyter and Jupytext integration -*- lexical-binding: t -*-

;;; Commentary:
;; Jupyter REPL support plus a script-first Jupytext workflow:
;;
;;   1. Keep running the notebook in JupyterLab.
;;   2. Save the notebook when you want to preserve outputs.
;;   3. Edit the paired script in Emacs.
;;   4. Save the script and let `jupytext --sync` update the notebook.
;;   5. In JupyterLab, run "Reload from Disk" and continue executing cells.
;;
;; See docs/jupyter-workflow.org for the full workflow and maintenance notes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup my/jupyter nil
  "Jupyter REPL and connection-file helpers."
  :group 'tools
  :prefix "my/jupyter-")

(defcustom my/jupyter-known-languages
  '("python" "sage" "sagemath" "maple")
  "Languages managed through Jupyter connection files."
  :type '(repeat string)
  :group 'my/jupyter)

(defvar my/jupyter-connection-file-history nil
  "History of Jupyter kernel connection files.")

(defvar my/jupyter-language-connection-files nil
  "Alist mapping language names to active Jupyter connection files.")

(declare-function jupyter-connect-repl "jupyter-repl"
                  (file &optional repl-name associate-buffer client-class display))
(declare-function jupyter-run-repl "jupyter-repl"
                  (kernel-name &optional repl-name associate-buffer client-class display))
(declare-function jupyter-runtime-directory "jupyter-env")
(declare-function my/org-babel-jupyter-set-language-session "init-org-babel"
                  (lang session &optional async))
(declare-function org-babel-get-src-block-info "ob-core" (&optional light))
(declare-function org-babel-where-is-src-block-head "ob-core")
(declare-function org-in-src-block-p "org-src" (&optional inside))

(defun my/jupyter--normalize-language (language)
  "Normalize LANGUAGE to a stable Jupyter-facing identifier."
  (pcase (downcase (string-trim (format "%s" language)))
    ((or "python" "python3" "jupyter-python") "python")
    ((or "sage" "sagemath" "jupyter-sage" "jupyter-sagemath") "sage")
    ("maple" "maple")
    (lang lang)))

(defun my/jupyter--language-family (language)
  "Return all aliases that should share LANGUAGE's connection file."
  (pcase (my/jupyter--normalize-language language)
    ("sage" '("sage" "sagemath"))
    (lang (list lang))))

(defun my/jupyter-managed-language-p (language)
  "Return non-nil when LANGUAGE is managed through Jupyter here."
  (member (my/jupyter--normalize-language language)
          (mapcar #'my/jupyter--normalize-language my/jupyter-known-languages)))

(defun my/jupyter-current-language ()
  "Return the current Jupyter language, or nil when it cannot be inferred."
  (let ((language
         (or
          (when (and (derived-mode-p 'org-mode)
                     (fboundp 'org-in-src-block-p)
                     (ignore-errors (org-in-src-block-p t)))
            (my/jupyter--normalize-language
             (car (org-babel-get-src-block-info 'light))))
          (pcase major-mode
            ((or 'python-mode 'python-ts-mode) "python")
            ((or 'sage-mode 'sage-shell:sage-mode) "sage")
            ('maple-mode "maple")
            (_ nil)))))
    (when (my/jupyter-managed-language-p language)
      language)))

(defun my/jupyter-read-language (&optional prompt)
  "Prompt for a managed Jupyter language and return it."
  (let* ((default (my/jupyter-current-language))
         (choices (delete-dups
                   (append (mapcar #'my/jupyter--normalize-language
                                   my/jupyter-known-languages)
                           (mapcar #'car my/jupyter-language-connection-files))))
         (input (completing-read
                 (if default
                     (format "%s (default %s): " (or prompt "Language") default)
                   (format "%s: " (or prompt "Language")))
                 choices nil t nil nil default)))
    (my/jupyter--normalize-language
     (if (string-empty-p input) default input))))

(defun my/jupyter-language-connection-file (language)
  "Return the active connection file for LANGUAGE, if one is registered."
  (cl-loop for alias in (my/jupyter--language-family language)
           for file = (cdr (assoc alias my/jupyter-language-connection-files))
           thereis (and (stringp file) (expand-file-name file))))

(defun my/jupyter-default-session-for-language (language fallback)
  "Return the preferred Org session for LANGUAGE, or FALLBACK."
  (let ((file (my/jupyter-language-connection-file language)))
    (if (and (stringp file) (file-exists-p file))
        file
      fallback)))

(defun my/jupyter--runtime-directory ()
  "Return the local Jupyter runtime directory."
  (require 'jupyter-env)
  (file-name-as-directory (expand-file-name (jupyter-runtime-directory))))

(defun my/jupyter--connection-files ()
  "Return known local kernel connection files, newest first."
  (let ((files (directory-files (my/jupyter--runtime-directory)
                                t
                                "\\`kernel-.*\\.json\\'")))
    (sort files
          (lambda (left right)
            (time-less-p (file-attribute-modification-time
                          (file-attributes right))
                         (file-attribute-modification-time
                          (file-attributes left)))))))

(defun my/jupyter-read-connection-file (&optional prompt language)
  "Prompt for a Jupyter connection file.
PROMPT overrides the default minibuffer prompt.  LANGUAGE is used
to seed the default with the language's currently registered file."
  (let* ((dir (my/jupyter--runtime-directory))
         (default (or (my/jupyter-language-connection-file language)
                      (car (my/jupyter--connection-files))))
         (file (read-file-name
                (or prompt "Connection file: ")
                dir
                default
                t
                nil
                (lambda (candidate)
                  (or (file-directory-p candidate)
                      (string-suffix-p ".json" candidate t))))))
    (expand-file-name file)))

(defun my/jupyter--remember-connection-file (language file)
  "Remember FILE as the active Jupyter connection for LANGUAGE."
  (setq file (expand-file-name file))
  (setq my/jupyter-connection-file-history
        (cons file (delete file my/jupyter-connection-file-history)))
  (dolist (alias (my/jupyter--language-family language))
    (setf (alist-get alias my/jupyter-language-connection-files nil 'remove #'equal)
          file)
    (when (featurep 'init-org-babel)
      (my/org-babel-jupyter-set-language-session alias file "yes")))
  file)

(defun my/jupyter-register-language-connection-file (language file &optional quiet)
  "Register FILE as LANGUAGE's active Jupyter connection file.
When QUIET is non-nil, skip the confirmation message."
  (interactive
   (let* ((language (my/jupyter-read-language))
          (file (my/jupyter-read-connection-file
                 (format "%s connection file: " language)
                 language)))
     (list language file)))
  (setq language (my/jupyter--normalize-language language))
  (unless (my/jupyter-managed-language-p language)
    (user-error "Unsupported Jupyter language: %s" language))
  (my/jupyter--remember-connection-file language file)
  (unless quiet
    (message "Jupyter %s now uses %s"
             language
             (abbreviate-file-name file))))

(defun my/jupyter--org-header-arg-value (value)
  "Return VALUE formatted for insertion into an Org source header."
  (if (string-match-p "[[:space:]]" value)
      (prin1-to-string value)
    value))

(defun my/jupyter--org-set-current-header-arg (name value)
  "Set the current Org source block header arg NAME to VALUE."
  (unless (and (derived-mode-p 'org-mode)
               (fboundp 'org-in-src-block-p)
               (org-in-src-block-p t))
    (user-error "Point is not in an Org source block"))
  (let ((head (org-babel-where-is-src-block-head))
        (pattern (format "[ \t]+:%s\\(?:[ \t]+\\(?:\"[^\"]*\"\\|[^ \t\n]+\\)\\)?"
                         (regexp-quote name))))
    (save-excursion
      (goto-char head)
      (let* ((bol (line-beginning-position))
             (eol (line-end-position))
             (line (buffer-substring-no-properties bol eol)))
        (setq line (replace-regexp-in-string pattern "" line t t))
        (setq line (replace-regexp-in-string "[ \t]\\{2,\\}" " " (string-trim-right line)))
        (delete-region bol eol)
        (insert line
                " :"
                name
                " "
                (my/jupyter--org-header-arg-value value))))))

(defun my/jupyter-use-connection-file-for-org-block (file &optional language)
  "Bind the current Org source block to an existing kernel connection FILE."
  (interactive
   (let ((language (my/jupyter-current-language)))
     (unless language
       (user-error "Point is not in a Jupyter-backed Org source block"))
     (list (my/jupyter-read-connection-file
            (format "%s connection file for block: " language)
            language)
           language)))
  (setq language (or language (my/jupyter-current-language)))
  (unless language
    (user-error "Point is not in a Jupyter-backed Org source block"))
  (my/jupyter-register-language-connection-file language file t)
  (my/jupyter--org-set-current-header-arg "session" file)
  (my/jupyter--org-set-current-header-arg "async" "yes")
  (message "Current Org block now uses %s" (abbreviate-file-name file)))

(defun my/jupyter-connect-existing-repl (file &optional repl-name language)
  "Connect a REPL to an existing kernel using connection FILE."
  (interactive
   (let* ((language (my/jupyter-current-language))
          (file (my/jupyter-read-connection-file
                 "Existing kernel connection file: "
                 language))
          (repl-name (when current-prefix-arg
                       (read-string "REPL name: "))))
     (list file repl-name language)))
  (setq file (expand-file-name file))
  (when language
    (my/jupyter-register-language-connection-file language file t))
  (jupyter-connect-repl file repl-name t nil t))

(defun my/jupyter-connect-repl-dwim (&optional arg)
  "Prefer connecting to an existing kernel; with ARG, start a new one."
  (interactive "P")
  (if arg
      (call-interactively #'jupyter-run-repl)
    (let* ((language (my/jupyter-current-language))
           (file (and language
                      (my/jupyter-language-connection-file language))))
      (if (and (stringp file) (file-exists-p file))
          (my/jupyter-connect-existing-repl file nil language)
        (call-interactively #'my/jupyter-connect-existing-repl)))))

(defun my/jupyter-apply-emacs-jupyter-state-fixes ()
  "Patch broken `emacs-jupyter' monad helpers in this environment.

Recent builds can native-compile a handful of state-threading helpers
incorrectly, producing `void-variable state' whenever REPL setup or Org
execution touches them.  Rebinding them here keeps the package usable
without forking the dependency."
  (with-eval-after-load 'jupyter-monads
    (defun jupyter-get-client ()
      "Return a monadic value that extracts the current client."
      (lambda (state)
        (let ((client (if (listp state) (car state) state)))
          (cl-check-type client jupyter-kernel-client)
          (cons client state))))
    (defun jupyter-push (s)
      "Push S onto the monadic state stack."
      (lambda (state)
        (cons nil (cons s (if (listp state) state (list state))))))
    (defun jupyter-pop ()
      "Pop and return the head of the monadic state stack."
      (lambda (state)
        (if (listp state)
            (cons (car state) (cdr state))
          (cons state nil))))
    (defun jupyter-set-client (client)
      "Return a monadic value that replaces the current client with CLIENT."
      (cl-check-type client jupyter-kernel-client)
      (lambda (state)
        (cons nil
              (if (listp state)
                  (cons client (cdr state))
                client))))
    (defun jupyter-at-point (action)
      "Return a monadic value evaluating ACTION at the captured point."
      (let ((marker (point-marker)))
        (lambda (state)
          (let (value)
            (when (and (marker-buffer marker) (marker-position marker))
              (unwind-protect
                  (with-current-buffer (marker-buffer marker)
                    (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (marker-position marker))
                        (setq value (jupyter-run-with-state state action)))))
                (move-marker marker nil)))
            (cons value state)))))))

(my/jupyter-apply-emacs-jupyter-state-fixes)

(defgroup my/jupytext nil
  "Jupytext workflow for script buffers paired with notebooks."
  :group 'tools
  :prefix "my/jupytext-")

(defcustom my/jupytext-command "jupytext"
  "Command used to run Jupytext."
  :type 'string
  :group 'my/jupytext)

(defcustom my/jupytext-default-notebook-extension ".ipynb"
  "Notebook extension used when inferring the paired notebook path."
  :type 'string
  :group 'my/jupytext)

(defcustom my/jupytext-format-alist
  '(("py" . "py:percent")
    ("r" . "R:percent")
    ("sage" . "sage:percent"))
  "Overrides from file extension to Jupytext format string.

Extensions should not include the leading dot. Any extension not
listed here falls back to EXT`:percent`, which keeps the workflow
usable for other kernels and script types."
  :type '(alist :key-type string :value-type string)
  :group 'my/jupytext)

(defcustom my/jupytext-log-buffer-name "*jupytext*"
  "Buffer used to capture Jupytext stdout and stderr."
  :type 'string
  :group 'my/jupytext)

(defcustom my/jupytext-sanitized-environment-variables
  '("PYTHONPATH" "PYTHONHOME" "__PYVENV_LAUNCHER__")
  "Environment variables removed before invoking Jupytext.

This avoids leaking per-buffer Python runtime overrides, such as the
Sage-specific `PYTHONPATH`, into the external Jupytext process."
  :type '(repeat string)
  :group 'my/jupytext)

(defvar my/jupytext-pairs (make-hash-table :test #'equal)
  "Session-local registry from script file to paired notebook metadata.")

(defvar-local my/jupytext-notebook-file nil
  "Notebook file paired with the current script buffer.")

(defvar-local my/jupytext-script-format nil
  "Jupytext format string used for the current script buffer.")

(defvar-local my/jupytext--set-formats-pending nil
  "Whether the next sync should run `jupytext --set-formats` first.")

(defvar jupytext-mode)

(defun my/jupytext--script-file ()
  "Return the absolute file name for the current buffer."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (expand-file-name buffer-file-name))

(defun my/jupytext--notebook-file-p (file)
  "Return non-nil when FILE looks like an `.ipynb' notebook."
  (string-suffix-p ".ipynb" file t))

(defun my/jupytext--ensure-script-buffer ()
  "Ensure the current buffer is a script buffer, not a notebook."
  (let ((script-file (my/jupytext--script-file)))
    (when (my/jupytext--notebook-file-p script-file)
      (user-error "Enable `jupytext-mode' in the paired script buffer, not the notebook"))
    script-file))

(defun my/jupytext--default-notebook-file (&optional script-file)
  "Infer the paired notebook path for SCRIPT-FILE or the current buffer."
  (concat (file-name-sans-extension
           (expand-file-name (or script-file (my/jupytext--script-file))))
          my/jupytext-default-notebook-extension))

(defun my/jupytext--default-format (&optional script-file)
  "Infer a Jupytext format string for SCRIPT-FILE or the current buffer."
  (let* ((file (expand-file-name (or script-file (my/jupytext--script-file))))
         (extension (downcase (or (file-name-extension file) ""))))
    (cond
     ((string-empty-p extension)
      (user-error "Cannot infer a Jupytext format for file without extension: %s"
                  (file-name-nondirectory file)))
     ((alist-get extension my/jupytext-format-alist nil nil #'string=))
     (t
      (format "%s:percent" extension)))))

(defun my/jupytext--ensure-executable ()
  "Return the resolved Jupytext executable or raise an error."
  (or (executable-find my/jupytext-command)
      (user-error "Cannot find `%s' in PATH" my/jupytext-command)))

(defun my/jupytext--pair-entry (&optional script-file)
  "Return the registered pair entry for SCRIPT-FILE or the current buffer."
  (gethash (expand-file-name (or script-file (my/jupytext--script-file)))
           my/jupytext-pairs))

(defun my/jupytext--sanitized-process-environment (&optional env)
  "Return ENV without Python runtime overrides that break Jupytext."
  (let ((result (copy-sequence (or env process-environment))))
    (dolist (name my/jupytext-sanitized-environment-variables result)
      (setq result
            (cl-remove-if
             (lambda (entry)
               (string-prefix-p (concat name "=") entry))
             result)))))

(defun my/jupytext--store-pair (script-file notebook-file format
                                            &optional set-formats-pending)
  "Register SCRIPT-FILE with NOTEBOOK-FILE using FORMAT.

When SET-FORMATS-PENDING is non-nil, the next sync will run
`jupytext --set-formats` before `--sync`."
  (let* ((script-file (expand-file-name script-file))
         (notebook-file (expand-file-name notebook-file))
         (entry (list :script script-file
                      :notebook notebook-file
                      :format format)))
    (puthash script-file entry my/jupytext-pairs)
    (setq-local my/jupytext-notebook-file notebook-file
                my/jupytext-script-format format
                my/jupytext--set-formats-pending set-formats-pending)
    entry))

(defun my/jupytext--read-notebook-file (script-file)
  "Prompt for the paired notebook path for SCRIPT-FILE."
  (let* ((default-notebook (my/jupytext--default-notebook-file script-file))
         (notebook-file
          (read-file-name
           (format "Paired notebook for %s: "
                   (file-name-nondirectory script-file))
           (file-name-directory default-notebook)
           default-notebook
           nil
           (file-name-nondirectory default-notebook))))
    (unless (my/jupytext--notebook-file-p notebook-file)
      (user-error "Jupytext workflow expects a paired `.ipynb' notebook"))
    (expand-file-name notebook-file)))

(defun my/jupytext--run (&rest args)
  "Run Jupytext with ARGS and return trimmed output."
  (let* ((program (my/jupytext--ensure-executable))
         (log-buffer (get-buffer-create my/jupytext-log-buffer-name))
         (default-directory (file-name-directory (my/jupytext--script-file)))
         (process-environment (my/jupytext--sanitized-process-environment)))
    (with-current-buffer log-buffer
      (erase-buffer))
    (let ((status (apply #'process-file program nil log-buffer nil args)))
      (with-current-buffer log-buffer
        (let ((output (string-trim (buffer-string))))
          (if (zerop status)
              output
            (display-buffer log-buffer)
            (error "jupytext exited with status %d%s"
                   status
                   (if (string-empty-p output)
                       ""
                     (format ": %s" output)))))))))

(defun jupytext-register-current-buffer (&optional notebook-file format)
  "Register the current script buffer with NOTEBOOK-FILE and FORMAT.

When NOTEBOOK-FILE is omitted, prefer a same-basename notebook if it
already exists and otherwise prompt for one. FORMAT defaults to the
current file extension, for example `py:percent' or `sage:percent'."
  (interactive
   (let* ((script-file (my/jupytext--ensure-script-buffer))
          (notebook-file (my/jupytext--read-notebook-file script-file))
          (format (when current-prefix-arg
                    (read-string
                     (format "Jupytext format for %s: "
                             (file-name-nondirectory script-file))
                     (my/jupytext--default-format script-file)))))
     (list notebook-file format)))
  (let* ((script-file (my/jupytext--ensure-script-buffer))
         (notebook-file
          (expand-file-name
           (or notebook-file
               (let ((default-notebook
                       (my/jupytext--default-notebook-file script-file)))
                 (if (file-exists-p default-notebook)
                     default-notebook
                   (my/jupytext--read-notebook-file script-file))))))
         (format (or format (my/jupytext--default-format script-file))))
    (unless (my/jupytext--notebook-file-p notebook-file)
      (user-error "Jupytext workflow expects a paired `.ipynb' notebook"))
    (my/jupytext--store-pair script-file notebook-file format t)
    (message "Registered Jupytext pair: %s <-> %s (%s)"
             (abbreviate-file-name script-file)
             (abbreviate-file-name notebook-file)
             format)))

(defun my/jupytext--ensure-pair ()
  "Populate buffer-local pairing state for the current script buffer."
  (let* ((script-file (my/jupytext--ensure-script-buffer))
         (entry (my/jupytext--pair-entry script-file))
         (default-notebook (my/jupytext--default-notebook-file script-file)))
    (cond
     ((and my/jupytext-notebook-file my/jupytext-script-format)
      (my/jupytext--store-pair script-file
                               my/jupytext-notebook-file
                               my/jupytext-script-format
                               my/jupytext--set-formats-pending))
     (entry
      (my/jupytext--store-pair script-file
                               (plist-get entry :notebook)
                               (plist-get entry :format)
                               nil))
     ((file-exists-p default-notebook)
      (my/jupytext--store-pair script-file
                               default-notebook
                               (my/jupytext--default-format script-file)
                               t))
     (t
      (call-interactively #'jupytext-register-current-buffer)))))

(defun my/jupytext--sync (&optional announce)
  "Sync the current buffer with its notebook via Jupytext.

When ANNOUNCE is non-nil, show a success message."
  (my/jupytext--ensure-executable)
  (my/jupytext--ensure-pair)
  (let* ((script-file (my/jupytext--script-file))
         (notebook-file (expand-file-name my/jupytext-notebook-file))
         (format my/jupytext-script-format)
         (pairing-p my/jupytext--set-formats-pending)
         (args (if pairing-p
                   (list "--set-formats"
                         (format "ipynb,%s" format)
                         "--sync"
                         script-file)
                 (list "--sync" script-file))))
    (my/jupytext--store-pair script-file notebook-file format pairing-p)
    (apply #'my/jupytext--run args)
    (setq-local my/jupytext--set-formats-pending nil)
    (when announce
      (message "Jupytext synced %s <-> %s%s"
               (abbreviate-file-name script-file)
               (abbreviate-file-name notebook-file)
               (if pairing-p " [set-formats]" "")))))

(defun my/jupytext--revert-buffer-if-needed ()
  "Revert the current buffer if Jupytext rewrote it on disk."
  (when (and buffer-file-name
             (not (buffer-modified-p))
             (not (verify-visited-file-modtime (current-buffer))))
    (revert-buffer :ignore-auto :noconfirm :preserve-modes)))

(defun jupytext-sync-buffer ()
  "Run `jupytext --sync` for the current script buffer now."
  (interactive)
  (my/jupytext--sync t)
  (my/jupytext--revert-buffer-if-needed))

(defun my/jupytext--after-save ()
  "Synchronize the current script buffer with its paired notebook."
  (when jupytext-mode
    (condition-case err
        (progn
          (my/jupytext--sync t)
          (my/jupytext--revert-buffer-if-needed))
      (error
       (message "Jupytext sync failed: %s" (error-message-string err))))))

(define-minor-mode jupytext-mode
  "Buffer-local Jupytext workflow for paired script/notebook editing.

Enable this in the script buffer you want to edit from Emacs. On save,
the current file is synchronized to the paired notebook with Jupytext.
Reload the notebook from disk in JupyterLab to keep running there."
  :init-value nil
  :lighter " Jupytext"
  (if jupytext-mode
      (condition-case err
          (progn
            (my/jupytext--ensure-pair)
            (add-hook 'after-save-hook #'my/jupytext--after-save nil t)
            (message "Jupytext mode enabled: %s <-> %s"
                     (abbreviate-file-name (my/jupytext--script-file))
                     (abbreviate-file-name my/jupytext-notebook-file)))
        (error
         (setq jupytext-mode nil)
         (remove-hook 'after-save-hook #'my/jupytext--after-save t)
         (signal (car err) (cdr err))))
    (remove-hook 'after-save-hook #'my/jupytext--after-save t)
    (message "Jupytext mode disabled")))

;; ----------------------------
;; 安装 emacs-jupyter
;; ----------------------------

(use-package jupyter
  :ensure t
  :commands (jupyter-run-repl
             jupyter-connect-repl
             jupyter-inspect-at-point
             jupyter-eval-line-or-region
             my/jupyter-connect-repl-dwim
             my/jupyter-connect-existing-repl
             my/jupyter-register-language-connection-file
             my/jupyter-use-connection-file-for-org-block)
  :bind (("C-c j r" . my/jupyter-connect-repl-dwim)
         ("C-c j R" . jupyter-run-repl)
         ("C-c j c" . my/jupyter-connect-existing-repl)
         ("C-c j k" . my/jupyter-register-language-connection-file)
         ("C-c j o" . my/jupyter-use-connection-file-for-org-block)
         ("C-c j i" . jupyter-inspect-at-point)
         ("C-c j e" . jupyter-eval-line-or-region)
         ("C-c j y" . jupytext-mode)
         ("C-c j s" . jupytext-sync-buffer)
         ("C-c j p" . jupytext-register-current-buffer))
  :init
  (setq jupyter-log-buffer-name "*jupyter-log*")
  (setq jupyter-repl-interaction-mode-enable-prompt-overlay t)
  (setq jupyter-repl-buffer-name-template "*jupyter-repl[%s]*")
  :config
  (my/jupyter-apply-emacs-jupyter-state-fixes))

(use-package code-cells
  :ensure t
  :commands (code-cells-mode code-cells-eval))

(provide 'init-jupyter)
;;; init-jupyter.el ends here
