;;; init-jupyter-tools.el --- Jupyter maintenance and diagnostics -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-funcs)
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

(defun my/jupyter-manager-setup-maintenance-keys ()
  "Install maintenance keybindings in the Jupyter Hub."
  (local-set-key (kbd "C") #'my/jupyter-manager-clear-connection)
  (local-set-key (kbd "D") #'my/jupyter-doctor)
  (local-set-key (kbd "S") #'my/jupyter-manager-prune-stale-connections)
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
    ("o" "docs" my/jupyter-manager-open-docs)]
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
          #'my/jupyter-manager-insert-maintenance-section)
(add-hook 'my/jupyter-manager-setup-functions
          #'my/jupyter-manager-setup-maintenance-keys)

(my/evil-global-leader-set "o J" #'my/jupyter-dispatch "jupyter dispatch")

(provide 'init-jupyter-tools)
;;; init-jupyter-tools.el ends here
