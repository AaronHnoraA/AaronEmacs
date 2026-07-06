;;; init-jupyter-board.el --- Jupyter kernelspec management board -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual management for local kernelspecs and the vendored remote_ikernel CLI.

;;; Code:

(require 'aaron-ui-board)
(require 'cl-lib)
(require 'config)
(require 'json)
(require 'subr-x)
(require 'transient)

(defgroup my/jupyter-board nil
  "Jupyter kernelspec and remote kernel management."
  :group 'tools)

(config-defvar my/jupyter-board-jupyter-command nil
  "Jupyter executable used to inspect and remove kernelspecs."
  :type 'file)

(config-defvar my/jupyter-board-python-command nil
  "Python executable containing the editable remote_ikernel install."
  :type 'file)

(config-defvar my/jupyter-remote-ikernel-command nil
  "Vendored remote_ikernel command."
  :type 'file)

(config-defvar my/jupyter-remote-ikernel-source-directory nil
  "Vendored remote_ikernel source directory."
  :type 'directory)

(config-defvar my/jupyter-remote-ikernel-install-script nil
  "Script that installs the vendored remote_ikernel source."
  :type 'file)

(defconst my/jupyter-board-buffer-name "*Jupyter Board*")
(defconst my/jupyter-board-log-buffer-name "*Jupyter Board Log*")
(defconst my/jupyter-board-detail-buffer-name "*Jupyter Kernel Detail*")

(defvar-local my/jupyter-board--entries nil)
(defvar-local my/jupyter-board--error nil)
(defvar my/jupyter-board--edit-origin nil)

(define-derived-mode my/jupyter-board-mode aaron-ui-board-mode "Jupyter-Board"
  "Major mode for the Jupyter management board.")

(defun my/jupyter-board--alist-get (key object)
  "Return KEY from JSON alist OBJECT."
  (and (listp object) (alist-get key object)))

(defun my/jupyter-board--capture (program &rest args)
  "Run PROGRAM with ARGS and return stdout, signaling on failure."
  (unless (and program (file-executable-p program))
    (error "Executable is unavailable: %s" (or program "<unset>")))
  (let ((stderr-file (make-temp-file "jupyter-board-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'process-file program nil
                               (list (current-buffer) stderr-file) nil args)))
            (if (zerop status)
                (buffer-string)
              (let ((stdout (buffer-string)))
                (error "%s"
                       (string-trim
                        (concat
                         stdout "\n"
                         (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (buffer-string)))))))))
      (delete-file stderr-file))))

(defun my/jupyter-board--json-file (file)
  "Read FILE as a JSON alist, returning nil when unreadable."
  (when (file-readable-p file)
    (condition-case nil
        (json-parse-string
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string))
         :object-type 'alist :array-type 'list
         :null-object nil :false-object nil)
      (error nil))))

(defun my/jupyter-board--remote-p (name argv raw)
  "Return non-nil when NAME, ARGV, or RAW identifies remote_ikernel."
  (or (string-prefix-p "rik_" name)
      (cl-some (lambda (arg)
                 (string-match-p "remote_ikernel" (format "%s" arg)))
               argv)
      (my/jupyter-board--alist-get 'remote_ikernel_argv raw)))

(defun my/jupyter-board--remote-metadata (spec)
  "Return Aaron remote-kernel metadata from SPEC."
  (let* ((metadata (my/jupyter-board--alist-get 'metadata spec))
         (aaron (my/jupyter-board--alist-get 'aaron metadata)))
    (my/jupyter-board--alist-get 'remote_kernel aaron)))

(defun my/jupyter-board--argv-option (argv option)
  "Return OPTION's value from ARGV, supporting split and equals forms."
  (let ((tail argv) value)
    (while (and tail (not value))
      (let ((arg (format "%s" (car tail))))
        (cond
         ((string= arg option)
          (setq value (and (cdr tail) (format "%s" (cadr tail)))))
         ((string-prefix-p (concat option "=") arg)
          (setq value (substring arg (1+ (length option)))))))
      (setq tail (cdr tail)))
    value))

(defun my/jupyter-board--entry (name object)
  "Normalize kernelspec NAME and JSON OBJECT into a plist."
  (let* ((resource-dir (my/jupyter-board--alist-get 'resource_dir object))
         (spec (my/jupyter-board--alist-get 'spec object))
         (argv (my/jupyter-board--alist-get 'argv spec))
         (raw (my/jupyter-board--json-file
               (expand-file-name "kernel.json" resource-dir)))
         (remote (my/jupyter-board--remote-p name argv raw))
         (remote-meta (my/jupyter-board--remote-metadata spec))
         (group (and remote
                     (or (my/jupyter-board--alist-get 'group remote-meta)
                         "core"))))
    (list :name name
          :display-name (or (my/jupyter-board--alist-get 'display_name spec) name)
          :language (or (my/jupyter-board--alist-get 'language spec) "")
          :resource-dir resource-dir
          :spec spec
          :raw raw
          :argv argv
          :remote remote
          :remote-meta remote-meta
          :group group
          :interface (and remote (my/jupyter-board--argv-option argv "--interface"))
          :host (and remote (my/jupyter-board--argv-option argv "--host"))
          :kernel-command (and remote (my/jupyter-board--argv-option argv "--kernel_cmd"))
          :workdir (and remote (my/jupyter-board--argv-option argv "--workdir")))))

(defun my/jupyter-board--load-entries ()
  "Return normalized entries from the configured Jupyter command."
  (let* ((output (my/jupyter-board--capture
                  my/jupyter-board-jupyter-command "kernelspec" "list" "--json"))
         (data (json-parse-string output :object-type 'alist :array-type 'list
                                  :null-object nil :false-object nil))
         (kernelspecs (my/jupyter-board--alist-get 'kernelspecs data)))
    (sort
     (mapcar (lambda (cell)
               (my/jupyter-board--entry (symbol-name (car cell)) (cdr cell)))
             kernelspecs)
     (lambda (a b) (string-lessp (plist-get a :name) (plist-get b :name))))))

(defun my/jupyter-board--current-entry ()
  "Return the kernel entry at point."
  (get-text-property (point) 'my/jupyter-board-entry))

(defun my/jupyter-board--require-entry (&optional remote-only)
  "Return current entry, requiring a remote entry when REMOTE-ONLY."
  (let ((entry (my/jupyter-board--current-entry)))
    (unless entry (user-error "No kernel entry at point"))
    (when (and remote-only (not (plist-get entry :remote)))
      (user-error "This action requires a remote kernel"))
    entry))

(defun my/jupyter-board--command-version (program &rest args)
  "Return one-line PROGRAM version using ARGS."
  (condition-case nil
      (let ((output (string-trim (apply #'my/jupyter-board--capture program args))))
        (if (string-match "version[[:space:]]+\\([[:alnum:]+._-]+\\)" output)
            (match-string 1 output)
          (car (split-string output "\n" t))))
    (error "unavailable")))

(defun my/jupyter-board--module-source ()
  "Return the installed remote_ikernel module source path."
  (condition-case nil
      (string-trim
       (my/jupyter-board--capture
        my/jupyter-board-python-command "-c"
        "import remote_ikernel; print(remote_ikernel.__file__)"))
    (error "unavailable")))

(defun my/jupyter-board--insert-toolchain ()
  "Insert the toolchain status section."
  (aaron-ui-board-insert-section "Toolchain")
  (aaron-ui-board-insert-field "Jupyter"
                               (my/jupyter-board--command-version
                                my/jupyter-board-jupyter-command
                                "kernelspec" "--version"))
  (aaron-ui-board-insert-field "remote_ikernel"
                               (my/jupyter-board--command-version
                                my/jupyter-remote-ikernel-command "--version"))
  (aaron-ui-board-insert-field "module source" (my/jupyter-board--module-source))
  (aaron-ui-board-insert-field "vendored source" my/jupyter-remote-ikernel-source-directory)
  (insert "   ")
  (aaron-ui-board-insert-actions
   '((:label "Reinstall" :command my/jupyter-board-reinstall :help "Reinstall editable source")
     (:label "Source" :command my/jupyter-board-open-source :help "Open vendored source")
     (:label "Log" :command my/jupyter-board-open-log :help "Open command log")))
  (insert "\n\n"))

(defun my/jupyter-board--entry-detail (entry)
  "Return a compact detail string for ENTRY."
  (if (plist-get entry :remote)
      (string-join
       (delq nil
             (list (and (plist-get entry :interface)
                        (format "interface=%s" (plist-get entry :interface)))
                   (and (plist-get entry :host)
                        (format "host=%s" (plist-get entry :host)))
                   (and (plist-get entry :workdir)
                        (format "workdir=%s" (plist-get entry :workdir)))
                   (and (plist-get entry :kernel-command)
                        (format "command=%s" (plist-get entry :kernel-command)))))
       "  ")
    (format "%s" (plist-get entry :resource-dir))))

(defun my/jupyter-board--insert-entry (entry)
  "Insert one kernelspec ENTRY."
  (aaron-ui-board-insert-row
   :id (plist-get entry :name)
   :icon (if (plist-get entry :remote) 'remote 'jupyter)
   :badge (if (plist-get entry :remote)
              (upcase (plist-get entry :group))
            (or (plist-get entry :language) "LOCAL"))
   :badge-tone (pcase (plist-get entry :group)
                 ("core" 'success)
                 ("temporary" 'warning)
                 (_ 'muted))
   :title (plist-get entry :display-name)
   :meta (format "%s  %s" (plist-get entry :name)
                 (or (plist-get entry :language) ""))
   :detail (my/jupyter-board--entry-detail entry)
   :action (lambda (_button) (my/jupyter-board-describe))
   :help "RET: details; e: edit; d: delete"
   :properties (list 'my/jupyter-board-entry entry)))

(defun my/jupyter-board--insert-group (title entries &optional tone)
  "Insert TITLE section containing ENTRIES, with optional badge TONE."
  (aaron-ui-board-insert-section title (length entries) tone)
  (if entries
      (progn
        (mapc #'my/jupyter-board--insert-entry entries)
        (insert "\n"))
    (aaron-ui-board-insert-empty "No kernels in this group.")))

(defun my/jupyter-board-refresh ()
  "Refresh the Jupyter management board."
  (interactive)
  (unless (derived-mode-p 'my/jupyter-board-mode)
    (user-error "Not in a Jupyter Board"))
  (setq my/jupyter-board--error nil)
  (condition-case err
      (setq my/jupyter-board--entries (my/jupyter-board--load-entries))
    (error
     (setq my/jupyter-board--entries nil
           my/jupyter-board--error (error-message-string err))))
  (let* ((remote (cl-remove-if-not (lambda (entry) (plist-get entry :remote))
                                    my/jupyter-board--entries))
         (core (cl-remove-if-not (lambda (entry)
                                   (equal (plist-get entry :group) "core")) remote))
         (temporary (cl-remove-if-not (lambda (entry)
                                        (equal (plist-get entry :group) "temporary")) remote))
         (local (cl-remove-if (lambda (entry) (plist-get entry :remote))
                              my/jupyter-board--entries)))
    (let ((inhibit-read-only t))
      (aaron-ui-board-render
       (lambda ()
         (aaron-ui-board-insert-page-header
          "Jupyter Board"
          :icon 'jupyter
          :subtitle "Kernelspec and vendored remote_ikernel management"
          :stats `((,(format "%d kernels" (length my/jupyter-board--entries)) . info)
                   (,(format "%d remote" (length remote)) . success)
                   (,(format "%d temporary" (length temporary)) . warning))
          :actions '((:label "Add Remote" :command my/jupyter-remote-add :primary t
                              :help "Add a remote kernelspec")
                     (:label "Refresh" :command my/jupyter-board-refresh :help "Refresh")
                     (:label "Clean Temp" :command my/jupyter-board-clean-temporary
                              :help "Remove all temporary remote kernels")))
         (my/jupyter-board--insert-toolchain)
         (when my/jupyter-board--error
           (aaron-ui-board-insert-section "Error")
           (aaron-ui-board-insert-field "kernelspec scan" my/jupyter-board--error
                                        'aaron-ui-board-bad)
           (insert "\n"))
         (my/jupyter-board--insert-group "Core Remote Kernels" core 'success)
         (my/jupyter-board--insert-group "Temporary Remote Kernels" temporary 'warning)
         (my/jupyter-board--insert-group "Local / Other Kernels" local 'muted)
         (aaron-ui-board-insert-key-hints
          "Keys: g refresh  a add  RET detail  e edit  m group  d delete  C clean temporary  o open path  ? menu  q quit"))))))

(defun my/jupyter-board ()
  "Open the Jupyter management board."
  (interactive)
  (let ((buffer (get-buffer-create my/jupyter-board-buffer-name)))
    (with-current-buffer buffer
      (my/jupyter-board-mode)
      (aaron-ui-board-set-header "Jupyter Board" 'jupyter)
      (setq-local aaron-ui-board-refresh-function #'my/jupyter-board-refresh)
      (use-local-map (copy-keymap special-mode-map))
      (my/jupyter-board--setup-keys)
      (my/jupyter-board-refresh))
    (pop-to-buffer buffer)))

(defun my/jupyter-board-open-log ()
  "Open the Jupyter Board command log."
  (interactive)
  (pop-to-buffer (get-buffer-create my/jupyter-board-log-buffer-name)))

(defun my/jupyter-board-open-source ()
  "Open the vendored remote_ikernel source directory."
  (interactive)
  (dired my/jupyter-remote-ikernel-source-directory))

(defun my/jupyter-board-open-resource ()
  "Open the current kernelspec resource directory."
  (interactive)
  (dired (plist-get (my/jupyter-board--require-entry) :resource-dir)))

(defun my/jupyter-board--log-command (program args)
  "Append PROGRAM and ARGS to the command log and return its buffer."
  (let ((buffer (get-buffer-create my/jupyter-board-log-buffer-name)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format-time-string "\n[%Y-%m-%d %H:%M:%S] "))
      (insert (mapconcat #'shell-quote-argument (cons program args) " ") "\n"))
    buffer))

(defun my/jupyter-board--start-command (program args &optional callback)
  "Run PROGRAM ARGS asynchronously, then invoke CALLBACK with output."
  (unless (and program (file-executable-p program))
    (user-error "Executable is unavailable: %s" (or program "<unset>")))
  (let* ((buffer (my/jupyter-board--log-command program args))
         (process
          (make-process
           :name "jupyter-board-command"
           :buffer buffer
           :stderr buffer
           :command (cons program args)
           :noquery t
           :filter
           (lambda (process chunk)
             (process-put process 'my/output
                          (concat (or (process-get process 'my/output) "") chunk))
             (when-let* ((log (process-buffer process)))
               (with-current-buffer log
                 (goto-char (point-max))
                 (insert chunk))))
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (let ((ok (zerop (process-exit-status process)))
                     (output (or (process-get process 'my/output) "")))
                 (if ok
                     (progn
                       (when callback (funcall callback output))
                       (when (get-buffer my/jupyter-board-buffer-name)
                         (with-current-buffer my/jupyter-board-buffer-name
                           (my/jupyter-board-refresh)))
                       (message "Jupyter command completed"))
                   (display-buffer (process-buffer process))
                   (message "Jupyter command failed; see %s"
                            my/jupyter-board-log-buffer-name))))))))
    process))

(defun my/jupyter-board-reinstall ()
  "Reinstall vendored remote_ikernel into the configured Python."
  (interactive)
  (my/jupyter-board--start-command
   my/jupyter-remote-ikernel-install-script '("install")))

(defun my/jupyter-board-describe ()
  "Describe the kernelspec at point."
  (interactive)
  (let* ((entry (my/jupyter-board--require-entry))
         (buffer (get-buffer-create my/jupyter-board-detail-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n\n" (plist-get entry :display-name)))
        (insert (format "Name: %s\n" (plist-get entry :name)))
        (insert (format "Language: %s\n" (plist-get entry :language)))
        (insert (format "Resource: %s\n" (plist-get entry :resource-dir)))
        (when (plist-get entry :remote)
          (insert (format "Group: %s\nInterface: %s\nHost: %s\n"
                          (plist-get entry :group)
                          (or (plist-get entry :interface) "-")
                          (or (plist-get entry :host) "-"))))
        (insert "\nkernel.json\n-----------\n")
        (let ((json-encoding-pretty-print t))
          (insert (json-encode (or (plist-get entry :raw)
                                   (plist-get entry :spec)))))
        (insert "\n")
        (special-mode)))
    (pop-to-buffer buffer)))

(defun my/jupyter-board-delete ()
  "Delete the kernelspec at point with appropriate protection."
  (interactive)
  (let* ((entry (my/jupyter-board--require-entry))
         (name (plist-get entry :name))
         (remote (plist-get entry :remote))
         (core (equal (plist-get entry :group) "core"))
         (prompt (if core
                     (format "Delete protected Core kernel %s? " name)
                   (format "Delete kernel %s? " name))))
    (when (if core (yes-or-no-p prompt) (y-or-n-p prompt))
      (if remote
          (my/jupyter-board--start-command
           my/jupyter-remote-ikernel-command (list "manage" "--delete" name))
        (my/jupyter-board--start-command
         my/jupyter-board-jupyter-command (list "kernelspec" "remove" "-f" name))))))

(defun my/jupyter-board-set-group ()
  "Set the current remote kernel's Core/Temporary group."
  (interactive)
  (let* ((entry (my/jupyter-board--require-entry t))
         (current (plist-get entry :group))
         (group (completing-read "Remote kernel group: " '("core" "temporary")
                                 nil t nil nil current)))
    (unless (equal current group)
      (my/jupyter-board--start-command
       my/jupyter-remote-ikernel-command
       (list "manage" "--set-group" (plist-get entry :name) group)))))

(defun my/jupyter-board-clean-temporary ()
  "Delete all Temporary remote kernels after confirmation."
  (interactive)
  (let ((names (mapcar (lambda (entry) (plist-get entry :name))
                       (cl-remove-if-not
                        (lambda (entry)
                          (and (plist-get entry :remote)
                               (equal (plist-get entry :group) "temporary")))
                        my/jupyter-board--entries))))
    (if (null names)
        (message "No Temporary remote kernels")
      (when (yes-or-no-p (format "Delete %d Temporary remote kernels? " (length names)))
        (my/jupyter-board--start-command
         my/jupyter-remote-ikernel-command
         (append '("manage" "--delete") names))))))

(defun my/jupyter-board--transient-config-args (entry)
  "Return transient arguments that reproduce remote kernelspec ENTRY."
  (let* ((meta (plist-get entry :remote-meta))
         (config (my/jupyter-board--alist-get 'config meta))
         (argv (plist-get entry :argv))
         (value (lambda (key option)
                  (or (my/jupyter-board--alist-get key config)
                      (my/jupyter-board--argv-option argv option))))
         (interface (funcall value 'interface "--interface"))
         (host (funcall value 'host "--host"))
         (kernel-command (funcall value 'kernel_cmd "--kernel_cmd"))
         (name (or (my/jupyter-board--alist-get 'name config)
                   (let ((display (plist-get entry :display-name)))
                     (if (and host (string-prefix-p (format "SSH %s " host) display))
                         (substring display (length (format "SSH %s " host)))
                       display))))
         args)
    (dolist (pair `(("--interface=" . ,interface)
                    ("--name=" . ,name)
                    ("--kernel_cmd=" . ,kernel-command)
                    ("--group=" . ,(plist-get entry :group))
                    ("--host=" . ,host)
                    ("--language=" . ,(or (funcall value 'language "--language")
                                            (plist-get entry :language)))
                    ("--workdir=" . ,(funcall value 'workdir "--workdir"))
                    ("--cpus=" . ,(funcall value 'cpus "--cpus"))
                    ("--pe=" . ,(funcall value 'pe "--pe"))
                    ("--remote-precmd=" . ,(funcall value 'remote_precmd "--remote-precmd"))
                    ("--launch-cmd=" . ,(funcall value 'launch_cmd "--launch-cmd"))
                    ("--remote-launch-args=" . ,(funcall value 'remote_launch_args "--remote-launch-args"))))
      (when (and (cdr pair) (not (equal (cdr pair) "")))
        (push (concat (car pair) (format "%s" (cdr pair))) args)))
    (when (my/jupyter-board--alist-get 'verbose config) (push "--verbose" args))
    (when (my/jupyter-board--alist-get 'system config) (push "--system" args))
    (let ((tunnels (my/jupyter-board--alist-get 'tunnel_hosts config)))
      (when tunnels (push (concat "--tunnel-hosts=" (string-join tunnels ",")) args)))
    (nreverse args)))

(defun my/jupyter-board--remote-args-normalize (args)
  "Convert transient ARGS to remote_ikernel CLI arguments."
  (apply #'append
         (mapcar
          (lambda (arg)
            (if (string-prefix-p "--tunnel-hosts=" arg)
                (cons "--tunnel-hosts"
                      (split-string (substring arg (length "--tunnel-hosts=")) "," t "[[:space:]]*"))
              (list arg)))
          args)))

(defun my/jupyter-board--arg-value (args prefix)
  "Return the value of PREFIX from transient ARGS."
  (when-let* ((arg (cl-find-if (lambda (item) (string-prefix-p prefix item)) args)))
    (substring arg (length prefix))))

(defun my/jupyter-remote-add-run ()
  "Create or replace a remote kernelspec from the active transient."
  (interactive)
  (let* ((args (transient-args 'my/jupyter-remote-add-dispatch))
         (interface (my/jupyter-board--arg-value args "--interface="))
         (name (my/jupyter-board--arg-value args "--name="))
         (kernel-command (my/jupyter-board--arg-value args "--kernel_cmd="))
         (host (my/jupyter-board--arg-value args "--host="))
         (origin my/jupyter-board--edit-origin))
    (unless (and interface (not (string-empty-p interface)))
      (user-error "--interface is required"))
    (unless (and name (not (string-empty-p name)))
      (user-error "--name is required"))
    (unless (and kernel-command (not (string-empty-p kernel-command)))
      (user-error "--kernel_cmd is required"))
    (when (and (equal interface "ssh") (or (null host) (string-empty-p host)))
      (user-error "--host is required for SSH kernels"))
    (setq my/jupyter-board--edit-origin nil)
    (my/jupyter-board--start-command
     my/jupyter-remote-ikernel-command
     (append '("manage" "--add") (my/jupyter-board--remote-args-normalize args))
     (lambda (output)
       (when (and origin
                  (string-match "Added kernel \\['\\([^']+\\)'\\]" output))
         (let ((created (match-string 1 output)))
           (when (and (not (equal created origin))
                      (yes-or-no-p (format "New kernel %s created; delete old %s? " created origin)))
             (my/jupyter-board--start-command
              my/jupyter-remote-ikernel-command
              (list "manage" "--delete" origin)))))))))

(defun my/jupyter-remote-add ()
  "Open the remote kernel creation transient."
  (interactive)
  (setq my/jupyter-board--edit-origin nil)
  (transient-setup 'my/jupyter-remote-add-dispatch nil nil
                   :value '("--interface=ssh" "--group=temporary")))

(defun my/jupyter-remote-edit ()
  "Edit the current remote kernel through the creation transient."
  (interactive)
  (let ((entry (my/jupyter-board--require-entry t)))
    (setq my/jupyter-board--edit-origin (plist-get entry :name))
    (transient-setup 'my/jupyter-remote-add-dispatch nil nil
                     :value (my/jupyter-board--transient-config-args entry))))

(transient-define-prefix my/jupyter-remote-add-dispatch ()
  "Create or edit a remote_ikernel kernelspec."
  [["Identity"
    ("-i" "Interface" "--interface=" :choices ("ssh" "local" "pbs" "sge" "sge_qrsh" "slurm" "lsf"))
    ("-n" "Name" "--name=")
    ("-k" "Kernel command" "--kernel_cmd=")
    ("-g" "Group" "--group=" :choices ("core" "temporary"))]
   ["Remote"
    ("-x" "Host" "--host=")
    ("-l" "Language" "--language=")
    ("-w" "Workdir" "--workdir=")
    ("-t" "Tunnel hosts CSV" "--tunnel-hosts=")]
   ["Scheduler"
    ("-c" "CPUs" "--cpus=")
    ("-p" "Parallel env" "--pe=")
    ("-P" "Remote pre-command" "--remote-precmd=")
    ("-L" "Launch command" "--launch-cmd=")
    ("-A" "Remote launch args" "--remote-launch-args=")]
   ["Flags"
    ("-v" "Verbose" "--verbose")
    ("-s" "System kernelspec" "--system")]
   ["Apply"
    ("a" "Add / replace" my/jupyter-remote-add-run)]])

(transient-define-prefix my/jupyter-board-dispatch ()
  "Jupyter Board command surface."
  [["Board"
    ("j" "Open board" my/jupyter-board)
    ("g" "Refresh" my/jupyter-board-refresh)
    ("RET" "Describe" my/jupyter-board-describe)
    ("o" "Open resource" my/jupyter-board-open-resource)]
   ["Remote"
    ("a" "Add" my/jupyter-remote-add)
    ("e" "Edit" my/jupyter-remote-edit)
    ("m" "Set group" my/jupyter-board-set-group)
    ("C" "Clean temporary" my/jupyter-board-clean-temporary)]
   ["Maintenance"
    ("d" "Delete" my/jupyter-board-delete)
    ("i" "Reinstall tool" my/jupyter-board-reinstall)
    ("S" "Open source" my/jupyter-board-open-source)
    ("l" "Open log" my/jupyter-board-open-log)]])

(defun my/jupyter-board--setup-keys ()
  "Install local keybindings for the Jupyter Board."
  (local-set-key (kbd "g") #'my/jupyter-board-refresh)
  (local-set-key (kbd "a") #'my/jupyter-remote-add)
  (local-set-key (kbd "RET") #'my/jupyter-board-describe)
  (local-set-key (kbd "e") #'my/jupyter-remote-edit)
  (local-set-key (kbd "m") #'my/jupyter-board-set-group)
  (local-set-key (kbd "d") #'my/jupyter-board-delete)
  (local-set-key (kbd "C") #'my/jupyter-board-clean-temporary)
  (local-set-key (kbd "o") #'my/jupyter-board-open-resource)
  (local-set-key (kbd "?") #'my/jupyter-board-dispatch))

(provide 'init-jupyter-board)
;;; init-jupyter-board.el ends here
