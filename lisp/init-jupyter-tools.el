;;; init-jupyter-tools.el --- Jupyter maintenance and diagnostics -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'init-funcs)
(require 'init-remote-connectboard nil t)
(require 'subr-x)
(require 'transient)

(defconst my/jupyter-doctor-buffer-name "*Jupyter Doctor*"
  "Buffer name used by the Jupyter doctor report.")

(defvar jupyter-log-buffer-name "*jupyter-log*"
  "Primary `emacs-jupyter' log buffer name.")

(defvar my/jupyter-kernelspec-log-buffer-name "*jupyter-kernelspec*"
  "Buffer used to capture kernelspec installation output.")

(defvar my/jupytext-log-buffer-name "*jupytext*"
  "Buffer used to capture Jupytext stdout and stderr.")

(defvar my/jupytext-command "jupytext"
  "Command used to run Jupytext.")

(defcustom my/jupyter-remote-ikernel-command "/opt/homebrew/anaconda3/bin/remote_ikernel"
  "Absolute path to the `remote_ikernel' management command."
  :type 'string
  :group 'my/jupyter)

(defvar-local my/jupytext-notebook-file nil
  "Notebook file paired with the current script buffer.")

(defvar-local my/jupytext-script-format nil
  "Jupytext format string used for the current script buffer.")

(defvar-local my/jupytext--set-formats-pending nil
  "Whether the next sync should run `jupytext --set-formats` first.")

(defvar jupytext-mode nil
  "Non-nil when `jupytext-mode' is enabled in the current buffer.")

(defvar my/jupyter-language-connection-files nil
  "Alist mapping language names to active Jupyter connection files.")

(defvar my/jupyter-manager-extra-section-functions nil
  "Hook used to extend the Jupyter Hub with extra sections.")

(defvar my/jupyter-manager-setup-functions nil
  "Hook used to extend the Jupyter Hub keymap.")

(defvar my/jupyter-lab-command)
(defvar my/jupyter-remote-ikernel-command)

(declare-function dired "dired" (dirname &optional switches))
(declare-function my/evil-global-leader-set "init-funcs" (key def &optional label))
(declare-function my/jupyter--available-kernel-names "init-jupyter" (&optional language refresh))
(declare-function my/jupyter--connection-files "init-jupyter")
(declare-function my/jupyter--first-available-kernel-for-language "init-jupyter" (language))
(declare-function my/jupyter--managed-languages "init-jupyter")
(declare-function my/jupyter--normalize-language "init-jupyter" (language))
(declare-function my/jupyter--runtime-directory "init-jupyter")
(declare-function my/jupyter-clear-language-connection-file "init-jupyter"
                  (language &optional quiet))
(declare-function my/jupyter-current-language "init-jupyter")
(declare-function my/jupyter-install-current-python-kernel "init-jupyter" (name display-name))
(declare-function my/jupyter-language-connection-file "init-jupyter" (language))
(declare-function my/jupyter-manager "init-jupyter")
(declare-function my/jupyter-manager--current-entry "init-jupyter")
(declare-function my/jupyter-manager--entry-language "init-jupyter" (&optional entry))
(declare-function my/jupyter-manager--insert-button "init-jupyter" (label action help))
(declare-function my/jupyter-manager-refresh "init-jupyter")
(declare-function my/jupyter-manager-open-docs "init-jupyter")
(declare-function my/jupyter-read-language "init-jupyter" (&optional prompt))
(declare-function my/jupyter-read-kernel "init-jupyter" (&optional prompt language include-auto))
(declare-function my/jupyter-refresh-kernelspecs-and-reconfigure "init-jupyter" (&optional quiet))
(declare-function my/jupyter-register-language-connection-file "init-jupyter"
                  (language file &optional quiet))
(declare-function my/jupyter-set-default-kernel-for-language "init-jupyter"
                  (language kernel))
(declare-function my/jupyter-use-connection-file-for-org-block "init-jupyter"
                  (file &optional language))
(declare-function my/jupyter-run-repl-for-language "init-jupyter" (language))
(declare-function my/jupyter-lab--command "init-jupyter-lab")
(declare-function my/jupyter-lab--default-directory "init-jupyter-lab")
(declare-function my/jupyter-lab-open-log "init-jupyter-lab")
(declare-function my/jupyter-lab-url "init-jupyter-lab")
(declare-function my/jupyter-manager--entry-kernel "init-jupyter")

(defun my/jupyter-manager--entry-resource-dir (&optional entry)
  "Return the kernelspec resource directory associated with ENTRY or point."
  (let* ((entry (or entry (my/jupyter-manager--current-entry)))
         (kind (plist-get entry :kind)))
    (pcase kind
      ('kernel
       (or (plist-get entry :resource-dir)
           (let ((kernel (plist-get entry :kernel)))
             (and kernel (my/jupyter--kernelspec-resource-dir kernel)))))
      (_ nil))))

(defcustom my/jupyter-config-file-candidates
  '("jupyter_server_config.py"
    "jupyter_lab_config.py"
    "jupyter_notebook_config.py"
    "jupyter_server_config.json"
    "jupyter_lab_config.json")
  "Likely Jupyter config files to edit from the board."
  :type '(repeat string)
  :group 'my/jupyter)

(defun my/jupyter--jupyter-command ()
  "Return the Jupyter executable used for local config discovery."
  (cond
   ((and (boundp 'my/jupyter-lab-command)
         (stringp my/jupyter-lab-command)
         (file-executable-p my/jupyter-lab-command))
    my/jupyter-lab-command)
   ((and (fboundp 'my/jupyter-lab--command)
         (ignore-errors (my/jupyter-lab--command))))
   ((executable-find "jupyter"))
   (t (user-error "Cannot find a usable Jupyter executable"))))

(defun my/jupyter--command-json (program &rest args)
  "Run PROGRAM with ARGS and parse JSON output."
  (with-temp-buffer
    (let ((status (apply #'process-file program nil (current-buffer) nil args)))
      (unless (zerop status)
        (user-error "%s %s exited with status %d"
                    program (string-join args " ") status))
      (goto-char (point-min))
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol)
            (json-false nil))
        (json-read)))))

(defun my/jupyter--paths ()
  "Return the parsed result of `jupyter --paths --json'."
  (my/jupyter--command-json (my/jupyter--jupyter-command) "--paths" "--json"))

(defun my/jupyter--config-directories ()
  "Return configured Jupyter config directories."
  (alist-get 'config (my/jupyter--paths)))

(defun my/jupyter--data-directories ()
  "Return configured Jupyter data directories."
  (alist-get 'data (my/jupyter--paths)))

(defun my/jupyter--config-directory ()
  "Return the primary local Jupyter config directory."
  (or (car (my/jupyter--config-directories))
      (expand-file-name "~/.jupyter/")))

(defun my/jupyter--kernelspec-root-directory ()
  "Return the primary local kernelspec root directory."
  (let ((data (car (my/jupyter--data-directories))))
    (when data
      (expand-file-name "kernels" data))))

(defun my/jupyter--kernelspec-list-json ()
  "Return parsed output from `jupyter kernelspec list --json'."
  (my/jupyter--command-json (my/jupyter--jupyter-command)
                            "kernelspec" "list" "--json"))

(defun my/jupyter--kernelspec-entry-by-name (kernel)
  "Return the kernelspec JSON entry for KERNEL."
  (let* ((json (my/jupyter--kernelspec-list-json))
         (specs (alist-get 'kernelspecs json nil nil #'equal)))
    (alist-get kernel specs nil nil #'equal)))

(defun my/jupyter--kernelspec-resource-dir (kernel)
  "Return the resource directory for KERNEL."
  (alist-get 'resource_dir (my/jupyter--kernelspec-entry-by-name kernel)))

(defun my/jupyter--ensure-file-directory (file)
  "Ensure the directory for FILE exists."
  (make-directory (file-name-directory file) t))

(defun my/jupyter-open-jupyter-config-directory ()
  "Open the primary local Jupyter config directory."
  (interactive)
  (dired (my/jupyter--config-directory)))

(defun my/jupyter-open-jupyter-data-directory ()
  "Open the primary local Jupyter data directory."
  (interactive)
  (if-let* ((dir (car (my/jupyter--data-directories))))
      (dired dir)
    (user-error "Cannot determine the Jupyter data directory")))

(defun my/jupyter-open-kernelspec-root-directory ()
  "Open the local kernelspec root directory."
  (interactive)
  (let ((dir (my/jupyter--kernelspec-root-directory)))
    (unless dir
      (user-error "Cannot determine the kernelspec root directory"))
    (dired dir)))

(defun my/jupyter-edit-jupyter-config-file (&optional file)
  "Open a local Jupyter config FILE.

When FILE is nil, prompt for one of the standard config filenames."
  (interactive)
  (let* ((dir (my/jupyter--config-directory))
         (file (or file
                   (expand-file-name
                    (completing-read "Jupyter config file: "
                                     my/jupyter-config-file-candidates
                                     nil
                                     t
                                     nil
                                     nil
                                     (car my/jupyter-config-file-candidates))
                    dir))))
    (my/jupyter--ensure-file-directory file)
    (find-file file)))

(defun my/jupyter-edit-current-jupyter-config-file ()
  "Open the most common Jupyter server config file."
  (interactive)
  (my/jupyter-edit-jupyter-config-file
   (expand-file-name (car my/jupyter-config-file-candidates)
                     (my/jupyter--config-directory))))

(defun my/jupyter-edit-kernelspec-json (kernel)
  "Open KERNEL's `kernel.json' file."
  (interactive (list (my/jupyter-read-kernel "Edit kernelspec")))
  (let ((resource-dir (my/jupyter--kernelspec-resource-dir kernel)))
    (unless resource-dir
      (user-error "No kernelspec resource directory found for %s" kernel))
    (find-file (expand-file-name "kernel.json" resource-dir))))

(defun my/jupyter-edit-current-kernelspec-json ()
  "Open the kernelspec JSON for the entry at point."
  (interactive)
  (let* ((entry (my/jupyter-manager--current-entry))
         (kernel (my/jupyter-manager--entry-kernel entry))
         (resource-dir (my/jupyter-manager--entry-resource-dir entry)))
    (unless kernel
      (user-error "No kernelspec at point"))
    (unless resource-dir
      (user-error "Current line is not a concrete kernelspec entry"))
    (find-file (expand-file-name "kernel.json" resource-dir))))

(defun my/jupyter-open-current-kernelspec-directory ()
  "Open the kernelspec directory for the entry at point."
  (interactive)
  (let* ((entry (my/jupyter-manager--current-entry))
         (resource-dir (my/jupyter-manager--entry-resource-dir entry)))
    (unless resource-dir
      (user-error "No kernelspec directory at point"))
    (dired resource-dir)))

(defun my/jupyter-open-remote-connectboard ()
  "Open the remote connectboard, when available."
  (interactive)
  (unless (fboundp 'my/remote-connectboard)
    (user-error "Remote connectboard is not available"))
  (my/remote-connectboard))

(defun my/jupyter-edit-remote-connectboard-config ()
  "Open the remote connectboard configuration file."
  (interactive)
  (unless (fboundp 'my/remote-connectboard-edit-config)
    (user-error "Remote connectboard config editor is not available"))
  (my/remote-connectboard-edit-config))

(defun my/jupyter--remote-ikernel-command ()
  "Return the `remote_ikernel' management executable."
  (cond
   ((and (boundp 'my/jupyter-remote-ikernel-command)
         (stringp my/jupyter-remote-ikernel-command)
         (file-executable-p my/jupyter-remote-ikernel-command))
    my/jupyter-remote-ikernel-command)
   ((executable-find "remote_ikernel"))
   (t (user-error "Cannot find a usable remote_ikernel executable"))))

(defvar my/jupyter-remote-ikernel-buffer-name "*Remote IKernel*"
  "Buffer used for `remote_ikernel manage' output.")

(define-derived-mode my/jupyter-remote-ikernel-mode special-mode "Remote-IKernel"
  "Major mode for `remote_ikernel manage' output.")

(defun my/jupyter--remote-ikernel-run (args &optional buffer)
  "Run `remote_ikernel manage' with ARGS and capture output in BUFFER."
  (let ((buffer (or buffer (get-buffer-create my/jupyter-remote-ikernel-buffer-name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (my/jupyter-remote-ikernel-mode))
    (let ((status
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (apply #'process-file
                      (my/jupyter--remote-ikernel-command)
                      nil
                      buffer
                      nil
                      (append (list "manage") args))))))
      (if (zerop status)
          buffer
        (user-error "remote_ikernel manage failed with status %d" status)))))

(defun my/jupyter-remote-ikernel-installed-names ()
  "Return installed `remote_ikernel' kernelspec names."
  (when-let* ((buffer (ignore-errors
                        (my/jupyter--remote-ikernel-run '("--show")))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (let (names)
        (while (re-search-forward
                "^\\(?:\\['\\([^']+\\)'\\]\\|[[:space:]]*[-*]?[[:space:]]*\\([^ :\n]+\\):?\\)$"
                nil t)
          (push (or (match-string 1)
                    (match-string 2))
                names))
        (nreverse names)))))

(defun my/jupyter-remote-ikernel-read-kernel (prompt)
  "Read one installed remote kernel using PROMPT."
  (let ((kernels (or (my/jupyter-remote-ikernel-installed-names) '())))
    (unless kernels
      (user-error "No installed remote kernels"))
    (completing-read prompt kernels nil t)))

(defun my/jupyter-remote-ikernel-show (&optional kernel)
  "Show `remote_ikernel manage' output, optionally for KERNEL."
  (interactive
   (list (when current-prefix-arg
           (my/jupyter-remote-ikernel-read-kernel "Remote kernel: "))))
  (let ((buffer (my/jupyter--remote-ikernel-run
                 (append (and kernel (list "--show" kernel))
                         (unless kernel (list "--show"))))))
    (pop-to-buffer buffer)))

(defun my/jupyter-remote-ikernel-add ()
  "Add a new remote kernel with a guided prompt."
  (interactive)
  (let* ((name (read-string "Kernel name: "))
         (interface (completing-read
                     "Interface: "
                     '("ssh" "local" "pbs" "sge" "sge_qrsh" "slurm" "lsf")
                     nil
                     t
                     nil
                     nil
                     "ssh"))
         (host (read-string "Host: "))
         (kernel-cmd (read-string "Kernel command: "))
         (language (read-string "Language: "))
         (cpus (read-string "CPUs (optional): "))
         (workdir (read-string "Workdir (optional): "))
         (launch-cmd (read-string "Launch command (optional): "))
         (remote-launch-args (read-string "Remote launch args (optional): "))
         (remote-precmd (read-string "Remote precmd (optional): "))
         (tunnel-hosts (read-string "Tunnel hosts (optional, space-separated): "))
         (system (y-or-n-p "Install system-wide? "))
         (args (append
                (delq nil
                      (list "--add"
                            (and system "--system")
                            (unless (string-empty-p name) (format "--name=%s" name))
                            (unless (string-empty-p interface) (format "--interface=%s" interface))
                            (unless (string-empty-p host) (format "--host=%s" host))
                            (unless (string-empty-p kernel-cmd) (format "--kernel_cmd=%s" kernel-cmd))
                            (unless (string-empty-p language) (format "--language=%s" language))
                            (unless (string-empty-p cpus) (format "--cpus=%s" cpus))
                            (unless (string-empty-p workdir) (format "--workdir=%s" workdir))
                            (unless (string-empty-p launch-cmd) (format "--launch-cmd=%s" launch-cmd))
                            (unless (string-empty-p remote-launch-args)
                              (format "--remote-launch-args=%s" remote-launch-args))
                            (unless (string-empty-p remote-precmd)
                              (format "--remote-precmd=%s" remote-precmd))))
                (when (not (string-empty-p tunnel-hosts))
                  (cons "--tunnel-hosts" (split-string tunnel-hosts "[[:space:]]+" t)))))
         (buffer (my/jupyter--remote-ikernel-run args)))
    (display-buffer buffer)
    (when (fboundp 'my/jupyter-manager-refresh)
      (my/jupyter-manager-refresh))))

(defun my/jupyter-remote-ikernel-delete (kernel)
  "Delete an installed remote kernel KERNEL."
  (interactive
   (list (my/jupyter-remote-ikernel-read-kernel "Delete remote kernel: ")))
  (let ((buffer (my/jupyter--remote-ikernel-run (list "--delete" kernel))))
    (display-buffer buffer)
    (when (fboundp 'my/jupyter-manager-refresh)
      (my/jupyter-manager-refresh))))

(define-derived-mode my/jupyter-doctor-mode special-mode "Jupyter-Doctor"
  "Major mode for the Jupyter doctor report.")

(defun my/jupyter--show-buffer-by-name (name)
  "Display buffer NAME, or raise a user error when it does not exist."
  (if-let* ((buffer (get-buffer name)))
      (pop-to-buffer buffer)
    (user-error "No live buffer named %s" name)))

(defun my/jupyter-show-log-buffer ()
  "Display the primary `emacs-jupyter' log buffer."
  (interactive)
  (my/jupyter--show-buffer-by-name jupyter-log-buffer-name))

(defun my/jupyter-show-kernelspec-log-buffer ()
  "Display the kernelspec installation log buffer."
  (interactive)
  (my/jupyter--show-buffer-by-name my/jupyter-kernelspec-log-buffer-name))

(defun my/jupytext-show-log-buffer ()
  "Display the Jupytext log buffer."
  (interactive)
  (my/jupyter--show-buffer-by-name my/jupytext-log-buffer-name))

(defun my/jupyter-open-runtime-directory ()
  "Open the local Jupyter runtime directory in `dired'."
  (interactive)
  (dired (my/jupyter--runtime-directory)))

(defun my/jupyter--registered-connection-languages ()
  "Return distinct managed languages with remembered connection files."
  (delete-dups
   (mapcar #'my/jupyter--normalize-language
           (mapcar #'car my/jupyter-language-connection-files))))

(defun my/jupyter--stale-connections ()
  "Return remembered connections whose files no longer exist."
  (cl-loop for language in (my/jupyter--registered-connection-languages)
           for file = (my/jupyter-language-connection-file language)
           when (and (stringp file)
                     (not (file-exists-p file)))
           collect (cons language file)))

(defun my/jupyter-prune-stale-connections ()
  "Forget remembered connection files that no longer exist."
  (interactive)
  (let ((stale (my/jupyter--stale-connections)))
    (dolist (entry stale)
      (setq my/jupyter-language-connection-files
            (cl-remove-if
             (lambda (pair)
               (string=
                (my/jupyter--normalize-language (car pair))
                (car entry)))
             my/jupyter-language-connection-files)))
    (when stale
      (my/jupyter-refresh-kernelspecs-and-reconfigure t))
    (message "Pruned %d stale Jupyter connection%s"
             (length stale)
             (if (= (length stale) 1) "" "s"))))

(defun my/jupyter-manager-clear-connection ()
  "Forget the remembered connection file for the language at point."
  (interactive)
  (let ((language (or (my/jupyter-manager--entry-language
                       (my/jupyter-manager--current-entry))
                      (my/jupyter-read-language "Clear connection for language"))))
    (my/jupyter-clear-language-connection-file language)
    (when (fboundp 'my/jupyter-manager-refresh)
      (my/jupyter-manager-refresh))))

(defun my/jupyter-manager-prune-stale-connections ()
  "Prune stale remembered connection files, then refresh the Hub."
  (interactive)
  (my/jupyter-prune-stale-connections)
  (when (fboundp 'my/jupyter-manager-refresh)
    (my/jupyter-manager-refresh)))

(defun my/jupyter--command-output (program &rest args)
  "Run PROGRAM with ARGS and return a plist report."
  (when-let* ((exe (executable-find program)))
    (with-temp-buffer
      (let ((status (apply #'process-file exe nil (current-buffer) nil args))
            (output nil))
        (setq output (string-trim (buffer-string)))
        (list :exe exe
              :ok (zerop status)
              :status status
              :output output)))))

(defun my/jupyter--python-report ()
  "Return Python runtime details for the current Emacs environment."
  (or (my/jupyter--command-output
       "python3"
       "-c"
       (mapconcat
        #'identity
        '("import platform, sys"
          "print(sys.executable)"
          "print(platform.python_version())")
        "; "))
      (my/jupyter--command-output
       "python"
       "-c"
       (mapconcat
        #'identity
        '("import platform, sys"
          "print(sys.executable)"
          "print(platform.python_version())")
        "; "))))

(defun my/jupyter--jupytext-version ()
  "Return a Jupytext version report."
  (my/jupyter--command-output my/jupytext-command "--version"))

(defun my/jupyter--doctor-insert-openable-path (path)
  "Insert PATH as a button that opens it."
  (insert-text-button
   (abbreviate-file-name path)
   'follow-link t
   'help-echo "Open this path"
   'action (lambda (_button)
             (if (file-directory-p path)
                 (dired path)
               (find-file path)))))

(defun my/jupyter--doctor-insert-executables ()
  "Insert the executable report."
  (insert "Executables\n")
  (insert "-----------\n")
  (dolist (entry `(("python3" . ,(executable-find "python3"))
                   ("python" . ,(executable-find "python"))
                   ("jupyter" . ,(executable-find "jupyter"))
                   (,my/jupytext-command . ,(executable-find my/jupytext-command))
                   ("direnv" . ,(executable-find "direnv"))))
    (insert (format "%-12s " (car entry)))
    (if-let* ((path (cdr entry)))
        (my/jupyter--doctor-insert-openable-path path)
      (insert "MISSING"))
    (insert "\n"))
  (insert "\n"))

(defun my/jupyter--doctor-insert-python ()
  "Insert the active Python runtime report."
  (insert "Python Runtime\n")
  (insert "--------------\n")
  (if-let* ((report (my/jupyter--python-report))
            (output (plist-get report :output)))
      (let* ((lines (split-string output "\n" t "[ \t]+"))
             (exe (car lines))
             (version (cadr lines)))
        (insert "python executable: ")
        (if exe
            (my/jupyter--doctor-insert-openable-path exe)
          (insert "-"))
        (insert "\n")
        (insert (format "python version: %s\n" (or version "-"))))
    (insert "python executable: MISSING\n"))
  (when-let* ((report (my/jupyter--jupytext-version))
              (output (plist-get report :output))
              ((not (string-empty-p output))))
    (insert (format "jupytext version: %s\n" output)))
  (insert "\n"))

(defun my/jupyter--doctor-insert-runtime ()
  "Insert runtime and connection-file details."
  (let* ((runtime-dir (my/jupyter--runtime-directory))
         (runtime-files (ignore-errors (my/jupyter--connection-files)))
         (stale (my/jupyter--stale-connections)))
    (insert "Runtime\n")
    (insert "-------\n")
    (insert "runtime dir: ")
    (my/jupyter--doctor-insert-openable-path runtime-dir)
    (insert "\n")
    (insert (format "runtime connection files: %d\n" (length runtime-files)))
    (insert (format "remembered stale connections: %d\n\n" (length stale)))))

(defun my/jupyter--doctor-insert-managed-languages ()
  "Insert managed language details."
  (insert "Managed Languages\n")
  (insert "-----------------\n")
  (if-let* ((languages (my/jupyter--managed-languages)))
      (dolist (language languages)
        (let ((kernel (my/jupyter--first-available-kernel-for-language language))
              (connection (my/jupyter-language-connection-file language))
              (available (my/jupyter--available-kernel-names language)))
          (insert (format "%-12s default=%-20s available=%s\n"
                          language
                          (or kernel "-")
                          (if available
                              (string-join available ", ")
                            "-")))
          (insert (format "  connection: %s%s\n"
                          (or (and connection (abbreviate-file-name connection)) "-")
                          (if (and connection (not (file-exists-p connection)))
                              " [missing]"
                            "")))))
    (insert "No managed languages configured.\n"))
  (insert "\n"))

(defun my/jupyter--doctor-insert-current-buffer (source-buffer)
  "Insert the current SOURCE-BUFFER state."
  (insert "Current Buffer\n")
  (insert "--------------\n")
  (if (buffer-live-p source-buffer)
      (let (buffer-name file language jupytext-enabled notebook format pending)
        (with-current-buffer source-buffer
          (setq buffer-name (buffer-name source-buffer)
                file (or (and buffer-file-name
                              (abbreviate-file-name buffer-file-name))
                         "-")
                language (or (ignore-errors (my/jupyter-current-language))
                             "-")
                jupytext-enabled jupytext-mode
                notebook (or (and my/jupytext-notebook-file
                                  (abbreviate-file-name my/jupytext-notebook-file))
                             "-")
                format (or my/jupytext-script-format "-")
                pending (if my/jupytext--set-formats-pending "yes" "no")))
        (insert (format "buffer: %s\n" buffer-name))
        (insert (format "file: %s\n" file))
        (insert (format "jupyter language: %s\n" language))
        (insert (format "jupytext mode: %s\n" (if jupytext-enabled "on" "off")))
        (when jupytext-enabled
          (insert (format "paired notebook: %s\n" notebook))
          (insert (format "pair format: %s\n" format))
          (insert (format "set-formats pending: %s\n" pending))))
    (insert "No source buffer.\n"))
  (insert "\n"))

(defun my/jupyter-doctor ()
  "Open a doctor report for the current Jupyter/Org/Jupytext setup."
  (interactive)
  (let ((buffer (get-buffer-create my/jupyter-doctor-buffer-name))
        (source (current-buffer)))
    (with-current-buffer buffer
      (my/jupyter-doctor-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Jupyter Doctor\n")
        (insert "==============\n\n")
        (my/jupyter--doctor-insert-executables)
        (my/jupyter--doctor-insert-python)
        (my/jupyter--doctor-insert-runtime)
        (my/jupyter--doctor-insert-managed-languages)
        (my/jupyter--doctor-insert-current-buffer source)
        (use-local-map (copy-keymap special-mode-map))
        (local-set-key (kbd "g") #'my/jupyter-doctor)
        (local-set-key (kbd "v") #'my/jupyter-open-runtime-directory)
        (local-set-key (kbd "l") #'my/jupyter-show-log-buffer)
        (local-set-key (kbd "L") #'my/jupyter-show-kernelspec-log-buffer)
        (local-set-key (kbd "t") #'my/jupytext-show-log-buffer)
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun my/jupyter-manager-insert-maintenance-section ()
  "Insert maintenance shortcuts into the Jupyter Hub."
  (let* ((runtime-files (ignore-errors (my/jupyter--connection-files)))
         (stale (my/jupyter--stale-connections))
         (python (or (executable-find "python3")
                     (executable-find "python")
                     "-"))
         (jupyter (or (executable-find "jupyter") "-"))
         (jupytext (or (executable-find my/jupytext-command) "-")))
    (insert "Maintenance\n")
    (insert "-----------\n")
    (insert (format "python: %s\n" (abbreviate-file-name python)))
    (insert (format "jupyter: %s\n" (abbreviate-file-name jupyter)))
    (insert (format "jupytext: %s\n" (abbreviate-file-name jupytext)))
    (insert (format "runtime files: %d, stale remembered connections: %d\n"
                    (length runtime-files)
                    (length stale)))
    (insert "actions: ")
    (my/jupyter-manager--insert-button
     "[doctor]"
     (lambda (_button) (my/jupyter-doctor))
     "Open the Jupyter doctor report")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[prune stale]"
     (lambda (_button) (my/jupyter-manager-prune-stale-connections))
     "Forget remembered connection files that no longer exist")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[runtime]"
     (lambda (_button) (my/jupyter-open-runtime-directory))
     "Open the local Jupyter runtime directory")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[jupyter log]"
     (lambda (_button) (my/jupyter-show-log-buffer))
     "Show the `emacs-jupyter' log buffer")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[kernelspec log]"
     (lambda (_button) (my/jupyter-show-kernelspec-log-buffer))
     "Show the kernelspec install log")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[jupytext log]"
     (lambda (_button) (my/jupytext-show-log-buffer))
     "Show the Jupytext log buffer")
    (insert "\n\n")))

(defun my/jupyter-manager-insert-configuration-section ()
  "Insert configuration-editing shortcuts into the Jupyter Hub."
  (let* ((config-dir (my/jupyter--config-directory))
         (data-dir (car (my/jupyter--data-directories)))
         (kernelspec-root (my/jupyter--kernelspec-root-directory)))
    (insert "Configuration\n")
    (insert "-------------\n")
    (insert (format "config dir: %s\n" (abbreviate-file-name config-dir)))
    (insert (format "data dir:   %s\n" (abbreviate-file-name (or data-dir "-"))))
    (insert (format "kernelspec: %s\n"
                    (abbreviate-file-name (or kernelspec-root "-"))))
    (insert "actions: ")
    (my/jupyter-manager--insert-button
     "[config]"
     (lambda (_button) (my/jupyter-open-jupyter-config-directory))
     "Open the local Jupyter config directory")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[edit config]"
     (lambda (_button) (my/jupyter-edit-current-jupyter-config-file))
     "Edit the main Jupyter server config file")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[kernelspec root]"
     (lambda (_button) (my/jupyter-open-kernelspec-root-directory))
     "Open the local kernelspec root directory")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[remote board]"
     (lambda (_button) (my/jupyter-open-remote-connectboard))
     "Open the curated remote connectboard")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[remote config]"
     (lambda (_button) (my/jupyter-edit-remote-connectboard-config))
     "Edit the remote connectboard config")
    (insert "\n\n")))

(defun my/jupyter-manager-insert-remote-kernel-section ()
  "Insert remote_ikernel management shortcuts into the Jupyter Hub."
  (let ((remote-kernels (ignore-errors (my/jupyter-remote-ikernel-installed-names))))
    (insert "Remote Kernels\n")
    (insert "--------------\n")
    (insert (format "remote_ikernel: %s\n"
                    (abbreviate-file-name (my/jupyter--remote-ikernel-command))))
    (insert (format "installed: %d\n" (length remote-kernels)))
    (insert "actions: ")
    (my/jupyter-manager--insert-button
     "[show]"
     (lambda (_button) (my/jupyter-remote-ikernel-show))
     "Show the remote_ikernel manage output")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[add]"
     (lambda (_button) (my/jupyter-remote-ikernel-add))
     "Add a remote kernel")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[delete]"
     (lambda (_button)
       (my/jupyter-remote-ikernel-delete
        (my/jupyter-remote-ikernel-read-kernel "Delete remote kernel: ")))
     "Delete an installed remote kernel")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[refresh]"
     (lambda (_button) (my/jupyter-manager-refresh))
     "Refresh the Jupyter Hub")
    (insert "\n\n")))

(defun my/jupyter-manager-setup-maintenance-keys ()
  "Install maintenance keybindings in the Jupyter Hub."
  (local-set-key (kbd "C") #'my/jupyter-manager-clear-connection)
  (local-set-key (kbd "D") #'my/jupyter-doctor)
  (local-set-key (kbd "S") #'my/jupyter-manager-prune-stale-connections)
  (local-set-key (kbd "O") #'my/jupyter-lab-open-log)
  (local-set-key (kbd "e") #'my/jupyter-edit-current-jupyter-config-file)
  (local-set-key (kbd "E") #'my/jupyter-open-jupyter-config-directory)
  (local-set-key (kbd "j") #'my/jupyter-edit-current-kernelspec-json)
  (local-set-key (kbd "J") #'my/jupyter-open-current-kernelspec-directory)
  (local-set-key (kbd "u") #'my/jupyter-remote-ikernel-add)
  (local-set-key (kbd "U") #'my/jupyter-remote-ikernel-delete)
  (local-set-key (kbd "m") #'my/jupyter-open-remote-connectboard)
  (local-set-key (kbd "M") #'my/jupyter-edit-remote-connectboard-config)
  (local-set-key (kbd "w") #'my/jupyter-remote-ikernel-show)
  (local-set-key (kbd "v") #'my/jupyter-open-runtime-directory)
  (local-set-key (kbd "l") #'my/jupyter-show-log-buffer)
  (local-set-key (kbd "L") #'my/jupyter-show-kernelspec-log-buffer)
  (local-set-key (kbd "t") #'my/jupytext-show-log-buffer)
  (local-set-key (kbd "?") #'my/jupyter-dispatch))

(transient-define-prefix my/jupyter-dispatch ()
  "Jupyter command surface."
  [["Views"
    ("h" "hub" my/jupyter-manager)
    ("D" "doctor" my/jupyter-doctor)
    ("o" "docs" my/jupyter-manager-open-docs)
    ("O" "lab log" my/jupyter-lab-open-log)]
   ["Config"
    ("e" "edit jupyter config" my/jupyter-edit-current-jupyter-config-file)
    ("E" "open jupyter config dir" my/jupyter-open-jupyter-config-directory)
    ("j" "edit current kernelspec" my/jupyter-edit-current-kernelspec-json)
    ("J" "open current kernelspec dir" my/jupyter-open-current-kernelspec-directory)
    ("m" "remote board" my/jupyter-open-remote-connectboard)
    ("M" "remote config" my/jupyter-edit-remote-connectboard-config)]
   ["Remote"
    ("u" "add remote kernel" my/jupyter-remote-ikernel-add)
    ("U" "delete remote kernel" my/jupyter-remote-ikernel-delete)
    ("w" "show remote_ikernel" my/jupyter-remote-ikernel-show)
    ("b" "remote board" my/jupyter-open-remote-connectboard)
    ("B" "remote config" my/jupyter-edit-remote-connectboard-config)]
   ["Kernel"
    ("r" "repl" my/jupyter-run-repl-for-language)
    ("k" "remember connection" my/jupyter-register-language-connection-file)
    ("C" "clear connection" my/jupyter-clear-language-connection-file)
    ("K" "default kernel" my/jupyter-set-default-kernel-for-language)
    ("P" "install current Python env" my/jupyter-install-current-python-kernel)
    ("x" "refresh kernels" my/jupyter-refresh-kernelspecs-and-reconfigure)]
   ["Maintenance"
    ("S" "prune stale connections" my/jupyter-prune-stale-connections)
    ("v" "open runtime dir" my/jupyter-open-runtime-directory)
    ("l" "show jupyter log" my/jupyter-show-log-buffer)
    ("L" "show kernelspec log" my/jupyter-show-kernelspec-log-buffer)
    ("t" "show jupytext log" my/jupytext-show-log-buffer)]])

(add-hook 'my/jupyter-manager-extra-section-functions
          #'my/jupyter-manager-insert-configuration-section)
(add-hook 'my/jupyter-manager-extra-section-functions
          #'my/jupyter-manager-insert-remote-kernel-section)
(add-hook 'my/jupyter-manager-extra-section-functions
          #'my/jupyter-manager-insert-maintenance-section)
(add-hook 'my/jupyter-manager-setup-functions
          #'my/jupyter-manager-setup-maintenance-keys)

(my/evil-global-leader-set "o j ?" #'my/jupyter-dispatch "jupyter dispatch")

(provide 'init-jupyter-tools)
;;; init-jupyter-tools.el ends here
