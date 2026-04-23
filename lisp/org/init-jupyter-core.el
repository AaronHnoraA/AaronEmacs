;;; init-jupyter-core.el --- Jupyter and Jupytext integration -*- lexical-binding: t -*-

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
(require 'eieio-core)
(require 'init-funcs)
(require 'init-jupyter-lab)
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

(defcustom my/jupyter-language-default-kernels nil
  "User-selected default kernelspec names for managed languages.

Each entry is of the form (LANGUAGE . KERNEL), where LANGUAGE is a
plain Org/Jupyter language such as \"python\" or \"sage\" and KERNEL is
the preferred kernelspec name to launch for that language when no
existing-kernel connection file is in use."
  :type '(alist :key-type string :value-type string)
  :group 'my/jupyter)

(defcustom my/jupyter-kernelspec-log-buffer-name "*jupyter-kernelspec*"
  "Buffer used to capture kernelspec installation output."
  :type 'string
  :group 'my/jupyter)

(defconst my/jupyter-manager-buffer-name "*Jupyter Hub*"
  "Buffer name used by the Jupyter management dashboard.")

(defvar my/jupyter-connection-file-history nil
  "History of Jupyter kernel connection files.")

(defcustom my/jupyter-connection-file-history-limit 40
  "Maximum remembered Jupyter kernel connection files."
  :type 'integer
  :group 'my/jupyter)

(defvar my/jupyter-language-connection-files nil
  "Alist mapping language names to active Jupyter connection files.")

(defvar org-babel-jupyter-language-aliases nil
  "Current `ob-jupyter' source block aliases.")

(defvar org-babel-load-languages nil
  "Enabled Org Babel languages.")

(defvar org-src-lang-modes nil
  "Org source language to major-mode mappings.")

(defvar my/jupyter-manager-setup-functions nil
  "Functions run after the Jupyter Hub keymap is initialized.

Each function is called with no arguments in the Jupyter Hub buffer.")

(defvar my/jupyter-manager-extra-section-functions nil
  "Functions used to append extra sections to the Jupyter Hub buffer.

Each function is called with no arguments after the built-in header is
inserted and should insert any desired text into the current buffer.")

(declare-function jupyter-connect-repl "jupyter-repl"
                  (file &optional repl-name associate-buffer client-class display))
(declare-function jupyter-kernelspec-name "jupyter-kernelspec" (spec))
(declare-function jupyter-kernelspec-plist "jupyter-kernelspec" (spec))
(declare-function jupyter-kernelspecs "jupyter-kernelspec" (&optional directory refresh))
(declare-function jupyter-refresh-kernelspecs "jupyter-kernelspec")
(declare-function jupyter-run-repl "jupyter-repl"
                  (kernel-name &optional repl-name associate-buffer client-class display))
(declare-function jupyter-runtime-directory "jupyter-env")
(declare-function my/org-babel-configure-jupyter-backends "init-org-babel")
(declare-function my/org-babel-jupyter-set-language-session "init-org-babel"
                  (lang session &optional async))
(declare-function org-babel-get-src-block-info "ob-core" (&optional light))
(declare-function org-babel-where-is-src-block-head "ob-core")
(declare-function org-src-get-lang-mode "org-src" (lang))
(declare-function org-in-src-block-p "org-src" (&optional inside))

(defun my/jupyter--ensure-kernel-client (object)
  "Signal unless OBJECT is a `jupyter-kernel-client' instance."
  (unless (and (eieio-object-p object)
               (object-of-class-p object 'jupyter-kernel-client))
    (signal 'wrong-type-argument (list 'jupyter-kernel-client object))))

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

(defun my/jupyter--available-kernels (&optional refresh)
  "Return an alist mapping normalized languages to available kernel names.

When REFRESH is non-nil, refresh the cached kernelspec list first."
  (require 'jupyter-kernelspec)
  (let ((default-directory user-emacs-directory)
        available)
    (dolist (spec (or (with-demoted-errors "Error retrieving kernelspecs: %S"
                        (jupyter-kernelspecs user-emacs-directory refresh))
                      nil))
      (let* ((kernel (jupyter-kernelspec-name spec))
             (language (my/jupyter--normalize-language
                        (plist-get (jupyter-kernelspec-plist spec) :language)))
             (slot (assoc language available)))
        (when (and (stringp kernel) (stringp language))
          (if slot
              (cl-pushnew kernel (cdr slot) :test #'equal)
            (push (cons language (list kernel)) available)))))
    (nreverse available)))

(defun my/jupyter--available-kernel-names (&optional language refresh)
  "Return available kernelspec names.

When LANGUAGE is non-nil, restrict the result to kernels for that
normalized Jupyter language.  When REFRESH is non-nil, refresh the
kernelspec cache first."
  (let* ((language (and language (my/jupyter--normalize-language language)))
         (available (my/jupyter--available-kernels refresh)))
    (if language
        (copy-sequence (cdr (assoc language available)))
      (sort (delete-dups (apply #'append (mapcar #'cdr available))) #'string<))))

(defun my/jupyter-preferred-kernels-for-language (language fallback)
  "Return preferred kernels for LANGUAGE, prepending user overrides to FALLBACK."
  (let* ((language (my/jupyter--normalize-language language))
         (override (alist-get language my/jupyter-language-default-kernels nil nil #'equal))
         (kernels (append (and override (list override))
                          (copy-sequence fallback))))
    (delete-dups (delq nil kernels))))

(defun my/jupyter-read-kernel (&optional prompt language include-auto)
  "Prompt for a kernelspec name and return it.

When LANGUAGE is non-nil, only offer kernels whose kernelspec language
matches LANGUAGE.  When INCLUDE-AUTO is non-nil, include an \"<auto>\"
choice and return nil when that entry is selected."
  (let* ((language (and language (my/jupyter--normalize-language language)))
         (kernels (my/jupyter--available-kernel-names language))
         (choices (if include-auto
                      (cons "<auto>" kernels)
                    kernels))
         (default (or (alist-get language my/jupyter-language-default-kernels nil nil #'equal)
                      (when include-auto "<auto>")
                      (car kernels))))
    (unless choices
      (user-error "No Jupyter kernelspecs are currently available"))
    (let* ((input (completing-read
                   (if default
                       (format "%s (default %s): "
                               (or prompt "Kernel")
                               default)
                     (format "%s: " (or prompt "Kernel")))
                   choices nil t nil nil default))
           (kernel (if (string-empty-p input) default input)))
      (unless (or (null kernel) (string= kernel "<auto>"))
        kernel))))

(defun my/jupyter-refresh-kernelspecs-and-reconfigure (&optional quiet)
  "Refresh kernelspecs and reconfigure Org/Jupyter glue.

When QUIET is non-nil, suppress the final confirmation message."
  (interactive)
  (require 'jupyter-kernelspec)
  (jupyter-refresh-kernelspecs)
  (when (featurep 'init-org-babel)
    (my/org-babel-configure-jupyter-backends))
  (unless quiet
    (message "Refreshed Jupyter kernelspecs and reconfigured Org backends")))

(defun my/jupyter-set-default-kernel-for-language (language kernel)
  "Set LANGUAGE's default kernelspec to KERNEL.

When KERNEL is nil, clear the override and fall back to automatic
kernel selection."
  (interactive
   (let* ((language (my/jupyter-read-language "Plain language"))
          (kernel (my/jupyter-read-kernel
                   (format "Default kernelspec for %s" language)
                   language
                   t)))
     (list language kernel)))
  (setq language (my/jupyter--normalize-language language))
  (setf (alist-get language my/jupyter-language-default-kernels nil 'remove #'equal)
        kernel)
  (when (called-interactively-p 'interactive)
    (customize-save-variable 'my/jupyter-language-default-kernels
                             my/jupyter-language-default-kernels))
  (my/jupyter-refresh-kernelspecs-and-reconfigure t)
  (if kernel
      (message "Jupyter %s now defaults to kernelspec %s" language kernel)
    (message "Jupyter %s now uses automatic kernel selection" language)))

(defun my/jupyter--default-python-kernel-name ()
  "Return a default kernelspec name for the current Python environment."
  (let ((conda (getenv "CONDA_DEFAULT_ENV"))
        (venv (getenv "VIRTUAL_ENV")))
    (cond
     ((and conda (not (string-empty-p conda))) conda)
     ((and venv (not (string-empty-p venv)))
      (file-name-nondirectory (directory-file-name venv)))
     ((and default-directory
           (not (string-empty-p (directory-file-name default-directory))))
     (file-name-nondirectory (directory-file-name default-directory)))
     (t
      "python3"))))

(defun my/jupyter--org-backends ()
  "Return the current plain Org languages routed through Jupyter."
  (when (require 'init-org-babel nil t)
    (and (boundp 'my/org-babel-jupyter-native-backends)
         my/org-babel-jupyter-native-backends)))

(defun my/jupyter--org-backend-entry (language)
  "Return the Org/Jupyter backend entry for LANGUAGE, if any."
  (let ((language (my/jupyter--normalize-language language)))
    (cl-find-if (lambda (entry)
                  (string= language (car entry)))
                (my/jupyter--org-backends))))

(defun my/jupyter--static-preferred-kernels-for-language (language)
  "Return static preferred kernels configured for LANGUAGE."
  (copy-sequence
   (plist-get (cdr (my/jupyter--org-backend-entry language))
              :preferred-kernels)))

(defun my/jupyter--managed-languages ()
  "Return all known managed languages in display order."
  (delete-dups
   (append
    (mapcar #'car (my/jupyter--org-backends))
    (mapcar #'my/jupyter--normalize-language my/jupyter-known-languages)
    (mapcar #'car my/jupyter-language-connection-files)
    (mapcar #'car my/jupyter-language-default-kernels))))

(defun my/jupyter--loaded-org-babel-languages ()
  "Return enabled native Org Babel languages."
  (when (require 'init-org-babel nil t)
    (sort
     (mapcar (lambda (pair) (format "%s" (car pair)))
             (cl-remove-if-not #'cdr org-babel-load-languages))
     #'string<)))

(defun my/jupyter--org-src-mode-for-language (language)
  "Return the Org source edit mode used for LANGUAGE."
  (when (require 'init-org-babel nil t)
    (let ((mode (or (and (fboundp 'org-src-get-lang-mode)
                         (org-src-get-lang-mode language))
                    (cdr (assoc language org-src-lang-modes))
                    (cdr (assoc (downcase language) org-src-lang-modes))
                    (cdr (assoc (capitalize language) org-src-lang-modes)))))
      (if mode
          (format "%s" mode)
        "-"))))

(defun my/jupyter--org-babel-alias-entries ()
  "Return active `jupyter-*' Org source block aliases."
  (when (require 'init-org-babel nil t)
    (sort
     (cl-delete-duplicates
      (cl-loop for pair in org-babel-jupyter-language-aliases
               for kernel-language = (format "%s" (car pair))
               for alias = (format "%s" (cadr pair))
               collect `(:src-language ,(format "jupyter-%s" alias)
                                       :alias ,alias
                                       :kernel-language ,kernel-language))
      :test (lambda (left right)
              (string=
               (plist-get left :src-language)
               (plist-get right :src-language))))
     (lambda (left right)
       (string< (plist-get left :src-language)
                (plist-get right :src-language))))))

(defun my/jupyter--first-available-kernel-for-language (language)
  "Return the best available kernelspec name for LANGUAGE."
  (let* ((language (my/jupyter--normalize-language language))
         (available (my/jupyter--available-kernel-names language))
         (preferred (my/jupyter-preferred-kernels-for-language
                     language
                     (my/jupyter--static-preferred-kernels-for-language language))))
    (or (cl-find-if (lambda (kernel) (member kernel available)) preferred)
        (car available))))

(defun my/jupyter-run-repl-for-language (language)
  "Open a Jupyter REPL for LANGUAGE.

If LANGUAGE has a remembered existing-kernel connection file, prefer
connecting to that running kernel.  Otherwise launch the best matching
kernelspec for the language."
  (interactive (list (my/jupyter-read-language "Run REPL for language")))
  (setq language (my/jupyter--normalize-language language))
  (let ((connection-file (my/jupyter-language-connection-file language)))
    (if (and (stringp connection-file) (file-exists-p connection-file))
        (my/jupyter-connect-existing-repl connection-file nil language)
      (let ((kernel (my/jupyter--first-available-kernel-for-language language)))
        (unless kernel
          (user-error "No kernelspec is available for language %s" language))
        (jupyter-run-repl kernel nil t nil t)))))

(defvar-local my/jupyter-manager-source-buffer nil
  "Buffer from which the current Jupyter manager view was opened.")

(define-derived-mode my/jupyter-manager-mode special-mode "Jupyter-Hub"
  "Major mode for the Jupyter kernels and Org language management dashboard.")

(defun my/jupyter-manager--clear-source-buffer-references-h ()
  "Clear Jupyter manager views that point at the buffer being killed."
  (let ((source (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'my/jupyter-manager-mode)
                     (eq my/jupyter-manager-source-buffer source))
            (setq-local my/jupyter-manager-source-buffer nil)))))))

(defun my/jupyter-manager--watch-source-buffer (source)
  "Install source cleanup for Jupyter manager views."
  (when (buffer-live-p source)
    (with-current-buffer source
      (add-hook 'kill-buffer-hook
                #'my/jupyter-manager--clear-source-buffer-references-h nil t))))

(defun my/jupyter-manager--current-entry ()
  "Return the Jupyter manager entry at point, if any."
  (or (get-text-property (point) 'my/jupyter-entry)
      (get-text-property (line-beginning-position) 'my/jupyter-entry)))

(defun my/jupyter-manager--entry-language (&optional entry)
  "Return the managed language associated with ENTRY or point."
  (let* ((entry (or entry (my/jupyter-manager--current-entry)))
         (kind (plist-get entry :kind)))
    (pcase kind
      ('language (plist-get entry :language))
      ('kernel
       (let* ((kernel-language (plist-get entry :language))
              (candidates
               (cl-remove-if-not
                (lambda (language)
                  (string= kernel-language
                           (or (plist-get (cdr (my/jupyter--org-backend-entry language))
                                          :kernel-language)
                               language)))
                (my/jupyter--managed-languages))))
         (cond
          ((null candidates) nil)
          ((= (length candidates) 1) (car candidates))
          (t
           (my/jupyter-read-language
            (format "Language for kernel %s" (plist-get entry :kernel)))))))
      (_ nil))))

(defun my/jupyter-manager--entry-kernel (&optional entry)
  "Return the kernelspec associated with ENTRY or point."
  (let* ((entry (or entry (my/jupyter-manager--current-entry)))
         (kind (plist-get entry :kind)))
    (pcase kind
      ('kernel (plist-get entry :kernel))
      ('language (my/jupyter--first-available-kernel-for-language
                  (plist-get entry :language)))
      (_ nil))))

(defun my/jupyter-manager--set-entry-properties (start end entry)
  "Mark the region between START and END with manager ENTRY."
  (add-text-properties start end
                       `(my/jupyter-entry ,entry
                                          mouse-face highlight
                                          help-echo "RET: context action")))

(defun my/jupyter-manager--insert-button (label action help)
  "Insert a text button with LABEL, ACTION, and HELP."
  (insert-text-button
   label
   'action action
   'follow-link t
   'help-echo help))

(defun my/jupyter-manager--insert-language-section ()
  "Insert the managed plain-language section."
  (insert "Managed Plain Languages\n")
  (insert "-----------------------\n")
  (if-let* ((languages (my/jupyter--managed-languages)))
      (dolist (language languages)
        (let* ((entry (my/jupyter--org-backend-entry language))
               (plist (cdr entry))
               (kernel-language (or (plist-get plist :kernel-language) language))
               (mode (plist-get plist :mode))
               (extension (plist-get plist :extension))
               (override (alist-get language my/jupyter-language-default-kernels nil nil #'equal))
               (available (my/jupyter--available-kernel-names kernel-language))
               (effective (my/jupyter--first-available-kernel-for-language language))
               (connection (my/jupyter-language-connection-file language))
               (fallback-session (or (plist-get plist :session) language))
               (effective-session (my/jupyter-default-session-for-language
                                   language
                                   fallback-session))
               (row `(:kind language :language ,language))
               (start (point)))
          (insert (format "%-12s kernel-lang=%-8s mode=%-10s ext=%s\n"
                          language
                          kernel-language
                          (or mode "-")
                          (or extension "-")))
          (insert (format "  default kernel: %s%s\n"
                          (or effective "-")
                          (if override
                              (format " (override: %s)" override)
                            "")))
          (insert (format "  available: %s\n"
                          (if available
                              (string-join available ", ")
                            "-")))
          (insert (format "  session: %s\n"
                          (if (stringp effective-session)
                              (abbreviate-file-name effective-session)
                            effective-session)))
          (insert (format "  connection: %s\n"
                          (if connection
                              (abbreviate-file-name connection)
                            "-")))
          (insert "  actions: ")
          (my/jupyter-manager--insert-button
           "[kernel]"
           (lambda (_button)
             (my/jupyter-set-default-kernel-for-language language
                                                         (my/jupyter-read-kernel
                                                          (format "Default kernelspec for %s" language)
                                                          language
                                                          t))
             (my/jupyter-manager-refresh))
           "Set or clear the default kernelspec for this language")
          (insert " ")
          (my/jupyter-manager--insert-button
           "[connection]"
           (lambda (_button)
             (call-interactively
              (lambda (file)
                (interactive
                 (list (my/jupyter-read-connection-file
                        (format "%s connection file: " language)
                        language)))
                (my/jupyter-register-language-connection-file language file)))
             (my/jupyter-manager-refresh))
           "Remember an existing-kernel connection file for this language")
          (when connection
            (insert " ")
            (my/jupyter-manager--insert-button
             "[clear]"
             (lambda (_button)
               (my/jupyter-clear-language-connection-file language)
               (my/jupyter-manager-refresh))
             "Forget the remembered connection file for this language"))
          (insert " ")
          (my/jupyter-manager--insert-button
           "[repl]"
           (lambda (_button)
             (my/jupyter-run-repl-for-language language))
           "Open a REPL for this language")
          (insert "\n\n")
          (my/jupyter-manager--set-entry-properties start (point) row)))
    (insert "No managed Jupyter languages are configured.\n\n")))

(defun my/jupyter-manager--insert-org-babel-section ()
  "Insert a summary of the current Org Babel language surface."
  (insert "Org Babel Surface\n")
  (insert "-----------------\n")
  (let ((native-languages (my/jupyter--loaded-org-babel-languages))
        (plain-languages (my/jupyter--managed-languages))
        (alias-entries (my/jupyter--org-babel-alias-entries)))
    (insert "Native loaded languages\n")
    (if native-languages
        (dolist (language native-languages)
          (insert (format "  %-18s kind=native mode=%s\n"
                          language
                          (my/jupyter--org-src-mode-for-language language))))
      (insert "  -\n"))
    (insert "\n")
    (insert "Plain Jupyter-backed languages\n")
    (if plain-languages
        (dolist (language plain-languages)
          (let* ((entry (cdr (my/jupyter--org-backend-entry language)))
                 (kernel-language (or (plist-get entry :kernel-language) language)))
            (insert (format "  %-18s kind=plain-jupyter kernel-lang=%s mode=%s\n"
                            language
                            kernel-language
                            (my/jupyter--org-src-mode-for-language language)))))
      (insert "  -\n"))
    (insert "\n")
    (insert "Jupyter alias source block names\n")
    (if alias-entries
        (dolist (entry alias-entries)
          (insert (format "  %-18s kind=jupyter-alias kernel-lang=%s\n"
                          (plist-get entry :src-language)
                          (plist-get entry :kernel-language))))
      (insert "  -\n"))
    (insert "\n")))

(defun my/jupyter-manager--kernel-used-by (kernel language)
  "Return plain managed languages using KERNEL for normalized LANGUAGE."
  (cl-remove-if-not
   (lambda (managed-language)
     (and (string= language
                   (or (plist-get (cdr (my/jupyter--org-backend-entry managed-language))
                                  :kernel-language)
                       managed-language))
          (string= kernel
                   (or (alist-get managed-language my/jupyter-language-default-kernels nil nil #'equal)
                       ""))))
   (my/jupyter--managed-languages)))

(defun my/jupyter-manager--insert-kernelspec-section ()
  "Insert the discovered kernelspec section."
  (insert "Available Kernelspecs\n")
  (insert "--------------------\n")
  (require 'jupyter-kernelspec)
  (let ((default-directory user-emacs-directory)
        (specs (or (with-demoted-errors "Error retrieving kernelspecs: %S"
                     (jupyter-kernelspecs user-emacs-directory))
                   nil)))
    (if specs
        (dolist (spec specs)
          (let* ((kernel (jupyter-kernelspec-name spec))
                 (plist (jupyter-kernelspec-plist spec))
                 (language (my/jupyter--normalize-language
                            (plist-get plist :language)))
                 (display-name (or (plist-get plist :display_name) kernel))
                 (resource-dir (or (plist-get plist :resource_dir)
                                   (plist-get plist :resource-dir)))
                 (used-by (my/jupyter-manager--kernel-used-by kernel language))
                 (row `(:kind kernel
                              :kernel ,kernel
                              :language ,language
                              :resource-dir ,resource-dir))
                 (start (point)))
            (insert (format "%-28s [%s]\n" kernel language))
            (insert (format "  display: %s\n" display-name))
            (insert (format "  resource: %s\n"
                            (if resource-dir
                                (abbreviate-file-name resource-dir)
                              "-")))
            (insert (format "  used by override: %s\n"
                            (if used-by
                                (string-join used-by ", ")
                              "-")))
            (insert "  actions: ")
            (my/jupyter-manager--insert-button
             "[repl]"
             (lambda (_button)
               (jupyter-run-repl kernel nil t nil t))
             "Start a REPL using this kernelspec")
            (insert " ")
            (my/jupyter-manager--insert-button
             "[json]"
             (lambda (_button)
               (my/jupyter-edit-kernelspec-json kernel))
             "Edit this kernelspec's kernel.json")
            (insert " ")
            (my/jupyter-manager--insert-button
             "[dir]"
             (lambda (_button)
               (let ((dir (my/jupyter--kernelspec-resource-dir kernel)))
                 (unless dir
                   (user-error "No kernelspec directory found for %s" kernel))
                 (dired dir)))
             "Open this kernelspec's resource directory")
            (insert " ")
            (my/jupyter-manager--insert-button
             "[set default]"
             (lambda (_button)
               (let ((target-language
                      (or (my/jupyter-manager--entry-language row)
                          (my/jupyter-read-language "Plain language"))))
                 (my/jupyter-set-default-kernel-for-language target-language kernel)
                 (my/jupyter-manager-refresh)))
             "Assign this kernelspec to a plain managed language")
            (insert "\n\n")
            (my/jupyter-manager--set-entry-properties start (point) row)))
      (insert "No kernelspecs available.\n\n"))))

(defun my/jupyter-manager-refresh ()
  "Refresh the Jupyter manager dashboard."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Jupyter Hub\n")
    (insert "===========\n\n")
    (insert "Keys: g refresh, x refresh kernelspecs, a start lab, A start+open lab, b open lab, B restart lab, z stop lab, O lab log, RET context action, K set default kernel, d clear override, c set connection, C clear connection, r open REPL, e edit config, j edit kernelspec, m remote board, u add remote kernel, U delete remote kernel, w show remote_ikernel, P install current Python env, D doctor, S prune stale, v runtime, l jupyter log, L kernelspec log, t jupytext log, ? dispatch, o docs, q quit\n\n")
    (run-hooks 'my/jupyter-manager-extra-section-functions)
    (my/jupyter-manager--insert-lab-section)
    (my/jupyter-manager--insert-org-babel-section)
    (my/jupyter-manager--insert-language-section)
    (my/jupyter-manager--insert-kernelspec-section)
    (goto-char (point-min))))

(defun my/jupyter-manager-refresh-kernels ()
  "Refresh kernelspec discovery, then redraw the manager."
  (interactive)
  (my/jupyter-refresh-kernelspecs-and-reconfigure t)
  (my/jupyter-manager-refresh)
  (message "Jupyter Hub refreshed kernelspec discovery"))

(defun my/jupyter-manager-set-default-kernel ()
  "Set the default kernelspec using the manager entry at point."
  (interactive)
  (let* ((entry (my/jupyter-manager--current-entry))
         (language (or (my/jupyter-manager--entry-language entry)
                       (my/jupyter-read-language "Plain language")))
         (kernel (pcase (plist-get entry :kind)
                   ('kernel (plist-get entry :kernel))
                   (_ (my/jupyter-read-kernel
                       (format "Default kernelspec for %s" language)
                       language
                       t)))))
    (my/jupyter-set-default-kernel-for-language language kernel)
    (my/jupyter-manager-refresh)))

(defun my/jupyter-manager-clear-default-kernel ()
  "Clear the default kernelspec override for the entry at point."
  (interactive)
  (let ((language (or (my/jupyter-manager--entry-language)
                      (my/jupyter-read-language "Plain language"))))
    (my/jupyter-set-default-kernel-for-language language nil)
    (my/jupyter-manager-refresh)))

(defun my/jupyter-manager-register-connection ()
  "Remember a connection file for the language at point."
  (interactive)
  (let* ((language (or (my/jupyter-manager--entry-language)
                       (my/jupyter-read-language "Plain language")))
         (file (my/jupyter-read-connection-file
                (format "%s connection file: " language)
                language)))
    (my/jupyter-register-language-connection-file language file)
    (my/jupyter-manager-refresh)))

(defun my/jupyter-manager-run-repl ()
  "Run a REPL using the manager entry at point."
  (interactive)
  (let ((entry (my/jupyter-manager--current-entry)))
    (pcase (plist-get entry :kind)
      ('kernel
       (jupyter-run-repl (plist-get entry :kernel) nil t nil t))
      (_
       (my/jupyter-run-repl-for-language
        (or (my/jupyter-manager--entry-language entry)
            (my/jupyter-read-language "Run REPL for language")))))))

(defun my/jupyter-manager-context-action ()
  "Run the context action for the manager entry at point."
  (interactive)
  (pcase (plist-get (my/jupyter-manager--current-entry) :kind)
    ('kernel (my/jupyter-manager-run-repl))
    (_ (my/jupyter-manager-set-default-kernel))))

(defun my/jupyter-manager-open-docs ()
  "Open the Jupyter workflow documentation."
  (interactive)
  (find-file (expand-file-name "docs/jupyter-workflow.org" user-emacs-directory)))

(defun my/jupyter-manager ()
  "Open the Jupyter kernels and Org language management dashboard."
  (interactive)
  (let ((buffer (get-buffer-create my/jupyter-manager-buffer-name))
        (source (current-buffer)))
    (with-current-buffer buffer
      (my/jupyter-manager-mode)
      (setq-local my/jupyter-manager-source-buffer source)
      (my/jupyter-manager--watch-source-buffer source)
      (let ((map (copy-keymap special-mode-map)))
        (use-local-map map)
        (local-set-key (kbd "g") #'my/jupyter-manager-refresh)
        (local-set-key (kbd "x") #'my/jupyter-manager-refresh-kernels)
        (local-set-key (kbd "a") #'my/jupyter-lab-start)
        (local-set-key (kbd "A") #'my/jupyter-lab-start-and-open)
        (local-set-key (kbd "b") #'my/jupyter-lab-open)
        (local-set-key (kbd "B") #'my/jupyter-lab-restart)
        (local-set-key (kbd "z") #'my/jupyter-lab-stop)
        (local-set-key (kbd "O") #'my/jupyter-lab-open-log)
        (local-set-key (kbd "K") #'my/jupyter-manager-set-default-kernel)
        (local-set-key (kbd "d") #'my/jupyter-manager-clear-default-kernel)
        (local-set-key (kbd "c") #'my/jupyter-manager-register-connection)
        (local-set-key (kbd "r") #'my/jupyter-manager-run-repl)
        (local-set-key (kbd "P") #'my/jupyter-install-current-python-kernel)
        (local-set-key (kbd "o") #'my/jupyter-manager-open-docs)
        (local-set-key (kbd "RET") #'my/jupyter-manager-context-action)
        (run-hooks 'my/jupyter-manager-setup-functions))
      (my/jupyter-manager-refresh))
    (pop-to-buffer buffer)))

(defun my/jupyter-install-current-python-kernel (name display-name)
  "Install the current Python environment as a user Jupyter kernelspec.

This command uses the current Emacs process environment.  In practice
that means `direnv', `conda', `venv', or other project-level Python
selection should already be active before invoking it."
  (interactive
   (let* ((name (read-string "Kernel name: "
                             (my/jupyter--default-python-kernel-name)))
          (display-name (read-string "Display name: "
                                     (format "Python (%s)" name))))
     (list name display-name)))
  (let* ((python (or (executable-find "python3")
                     (executable-find "python")))
         (log-buffer (get-buffer-create my/jupyter-kernelspec-log-buffer-name)))
    (unless python
      (user-error "Cannot find python3 or python in PATH"))
    (with-current-buffer log-buffer
      (erase-buffer))
    (let ((status (process-file python nil log-buffer nil
                                "-m" "ipykernel" "install"
                                "--user"
                                "--name" name
                                "--display-name" display-name)))
      (with-current-buffer log-buffer
        (let ((output (string-trim (buffer-string))))
          (if (zerop status)
              (progn
                (my/jupyter-refresh-kernelspecs-and-reconfigure t)
                (message "Installed kernelspec %s via %s"
                         name
                         (abbreviate-file-name python)))
            (display-buffer log-buffer)
            (error "ipykernel install exited with status %d%s"
                   status
                   (if (string-empty-p output)
                       ""
                     (format ": %s" output)))))))))

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
  (when (and (integerp my/jupyter-connection-file-history-limit)
             (> my/jupyter-connection-file-history-limit 0))
    (when-let* ((tail (nthcdr (1- my/jupyter-connection-file-history-limit)
                              my/jupyter-connection-file-history)))
      (setcdr tail nil)))
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

(defun my/jupyter-clear-language-connection-file (language &optional quiet)
  "Forget any remembered connection file for LANGUAGE.

When QUIET is non-nil, skip the confirmation message."
  (interactive (list (my/jupyter-read-language "Clear connection for language")))
  (setq language (my/jupyter--normalize-language language))
  (dolist (alias (my/jupyter--language-family language))
    (setf (alist-get alias my/jupyter-language-connection-files nil 'remove #'equal)
          nil))
  (my/jupyter-refresh-kernelspecs-and-reconfigure t)
  (unless quiet
    (message "Jupyter %s no longer uses a remembered connection file"
             language)))

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
          (my/jupyter--ensure-kernel-client client)
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
      (my/jupyter--ensure-kernel-client client)
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

(defcustom my/jupytext-auto-mode-file-regexp "\\.ju\\.[^.]+\\'"
  "File name regexp for script buffers that should auto-enable `jupytext-mode'."
  :type 'regexp
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

(defun my/jupytext--canonical-script-file (&optional script-file)
  "Return SCRIPT-FILE with an optional `.ju.' infix normalized away."
  (let ((file (expand-file-name (or script-file (my/jupytext--script-file)))))
    (replace-regexp-in-string "\\.ju\\.\\([^.]+\\)\\'" ".\\1" file)))

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
      ;; Default to a same-basename notebook so brand new `.ju.*' files can
      ;; create their paired `.ipynb' on first save without prompting.
      (my/jupytext--store-pair script-file
                               default-notebook
                               (my/jupytext--default-format script-file)
                               t)))))

(defun my/jupytext--sync (&optional announce)
  "Sync the current buffer with its notebook via Jupytext.

When ANNOUNCE is non-nil, show a success message."
  (my/jupytext--ensure-executable)
  (my/jupytext--ensure-pair)
  (let* ((script-file (my/jupytext--script-file))
         (notebook-file (expand-file-name my/jupytext-notebook-file))
         (format my/jupytext-script-format)
         (pairing-p my/jupytext--set-formats-pending)
         (creating-p (or pairing-p
                         (not (file-exists-p notebook-file))))
         (args (append (unless creating-p
                         (list "--update"))
                       (list "--to" "ipynb"
                             "--output" notebook-file
                             script-file))))
    (my/jupytext--store-pair script-file notebook-file format pairing-p)
    (apply #'my/jupytext--run args)
    (setq-local my/jupytext--set-formats-pending nil)
    (when announce
      (message "Jupytext synced %s <-> %s%s"
               (abbreviate-file-name script-file)
               (abbreviate-file-name notebook-file)
               (if creating-p " [create]" "")))))

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

(defun my/jupytext-auto-enable-mode ()
  "Enable `jupytext-mode' automatically for matching paired script buffers."
  (when (and buffer-file-name
             (not jupytext-mode)
             (string-match-p my/jupytext-auto-mode-file-regexp buffer-file-name))
    (let ((script-file (my/jupytext--script-file)))
      (condition-case err
          (jupytext-mode 1)
        (error
         (message "Skipping automatic jupytext-mode for %s: %s"
                  (file-name-nondirectory script-file)
                  (error-message-string err)))))))

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

(add-hook 'find-file-hook #'my/jupytext-auto-enable-mode)

(require 'init-jupyter-tools)

;; ----------------------------
;; 安装 emacs-jupyter
;; ----------------------------

(use-package jupyter
  :ensure t
  :commands (jupyter-run-repl
             jupyter-connect-repl
             jupyter-inspect-at-point
             jupyter-eval-line-or-region
             my/jupyter-clear-language-connection-file
             my/jupyter-lab-open
             my/jupyter-lab-open-log
             my/jupyter-lab-restart
             my/jupyter-lab-start
             my/jupyter-lab-start-and-open
             my/jupyter-lab-stop
             my/jupyter-connect-repl-dwim
             my/jupyter-connect-existing-repl
             my/jupyter-dispatch
             my/jupyter-doctor
             my/jupyter-edit-current-jupyter-config-file
             my/jupyter-edit-current-kernelspec-json
             my/jupyter-edit-jupyter-config-file
             my/jupyter-edit-kernelspec-json
             my/jupyter-install-current-python-kernel
             my/jupyter-manager
             my/jupyter-open-current-kernelspec-directory
             my/jupyter-open-jupyter-config-directory
             my/jupyter-open-kernelspec-root-directory
             my/jupyter-open-remote-connectboard
             my/jupyter-remote-ikernel-add
             my/jupyter-remote-ikernel-delete
             my/jupyter-remote-ikernel-show
             my/jupyter-prune-stale-connections
             my/jupyter-register-language-connection-file
             my/jupyter-refresh-kernelspecs-and-reconfigure
             my/jupyter-run-repl-for-language
             my/jupyter-edit-remote-connectboard-config
             my/jupyter-set-default-kernel-for-language
             my/jupyter-use-connection-file-for-org-block)
  :bind (("C-c j r" . my/jupyter-connect-repl-dwim)
         ("C-c j R" . jupyter-run-repl)
         ("C-c j a" . my/jupyter-lab-start)
         ("C-c j A" . my/jupyter-lab-start-and-open)
         ("C-c j b" . my/jupyter-lab-open)
         ("C-c j B" . my/jupyter-lab-restart)
         ("C-c j c" . my/jupyter-connect-existing-repl)
         ("C-c j ?" . my/jupyter-dispatch)
         ("C-c j h" . my/jupyter-manager)
         ("C-c j C" . my/jupyter-clear-language-connection-file)
         ("C-c j k" . my/jupyter-register-language-connection-file)
         ("C-c j K" . my/jupyter-set-default-kernel-for-language)
         ("C-c j l" . my/jupyter-lab-open-log)
         ("C-c j o" . my/jupyter-use-connection-file-for-org-block)
         ("C-c j i" . jupyter-inspect-at-point)
         ("C-c j e" . jupyter-eval-line-or-region)
         ("C-c j E" . my/jupyter-open-jupyter-config-directory)
         ("C-c j j" . my/jupyter-edit-current-kernelspec-json)
         ("C-c j J" . my/jupyter-open-current-kernelspec-directory)
         ("C-c j m" . my/jupyter-open-remote-connectboard)
         ("C-c j M" . my/jupyter-edit-remote-connectboard-config)
         ("C-c j u" . my/jupyter-remote-ikernel-add)
         ("C-c j U" . my/jupyter-remote-ikernel-delete)
         ("C-c j w" . my/jupyter-remote-ikernel-show)
         ("C-c j x" . my/jupyter-refresh-kernelspecs-and-reconfigure)
         ("C-c j y" . jupytext-mode)
         ("C-c j s" . jupytext-sync-buffer)
         ("C-c j z" . my/jupyter-lab-stop)
         ("C-c j p" . jupytext-register-current-buffer)
         ("C-c j P" . my/jupyter-install-current-python-kernel))
  :init
  (setq jupyter-log-buffer-name "*jupyter-log*")
  (setq jupyter-repl-interaction-mode-enable-prompt-overlay t)
  (setq jupyter-repl-buffer-name-template "*jupyter-repl[%s]*")
  :config
  (my/jupyter-apply-emacs-jupyter-state-fixes))

(my/leader!
  "o j" '(:def my/jupyter-manager :which-key "jupyter hub")
  "o J" '(:def my/jupyter-lab-start-and-open :which-key "jupyter lab"))

(use-package code-cells
  :ensure t
  :commands (code-cells-mode code-cells-eval))

(provide 'init-jupyter-core)
;;; init-jupyter-core.el ends here
