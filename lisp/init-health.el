;;; init-health.el --- Config health checks -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'subr-x)
(require 'transient)

(declare-function my/compile-board "init-compile" ())
(declare-function my/byte-compile-config "init-compile" (&optional force))
(declare-function my/native-compile-config "init-compile" (&optional force))
(declare-function claude-code-ide-emacs-tools-setup "claude-code-ide" ())
(declare-function my/package-lock-audit "init-package-utils" ())
(declare-function my/maintenance-state-report "init-maintenance" ())

(defgroup my/health nil
  "Health checks for the Emacs config."
  :group 'convenience)

(defconst my/health--module-file
  (or load-file-name buffer-file-name)
  "Absolute path of `init-health.el' at load time.")

(defcustom my/health-executables
  '("rg" "git" "python3" "node" "cargo" "go" "just" "make" "latexmk" "gdb" "lldb-dap" "dlv")
  "Executables checked by `my/health-report'."
  :type '(repeat string)
  :group 'my/health)

(defconst my/health-important-commands
  '(my/compile-board
    my/compile-dispatch
    my/byte-compile-config
    my/native-compile-config
    show-imenu
    my/test-dispatch
    my/task-dispatch
    my/project-run-dispatch
    my/debug-profile-dispatch
    my/output-dispatch
    my/diagnostics-dispatch
    my/language-server-dispatch
    my/language-server-manager
    my/language-server-doctor)
  "Interactive commands checked in `my/health-report'.")

(defconst my/health-critical-libraries
  '((ligature . "ligature")
    (theme . "aaron-ui")
    (vterm . "vterm")
    (vterm-module . "vterm-module")
    (pdf-tools . "pdf-tools")
    (tramp-rpc . "tramp-rpc")
    (claude-code-ide . "claude-code-ide")
    (codex-cli . "codex-cli"))
  "Libraries that should be available after a healthy bootstrap.")

(defvar my/health-startup-time nil
  "Elapsed startup time in seconds for the current Emacs session.")

(defvar my/health-startup-gcs nil
  "GC count recorded for the current Emacs session startup.")

(defvar my/health-startup-package-count nil
  "Installed package count recorded after startup.")

(defconst my/health-startup-bytecode-files
  '("early-init.el" "init.el" "bootstrap.el")
  "Top-level startup files whose stale bytecode should fail health checks.")

(defun my/health--bundled-epdfinfo ()
  "Return the configured bundled epdfinfo path."
  (expand-file-name "elpa/pdf-tools-20260102.1101/epdfinfo"
                    user-emacs-directory))

(defun my/health--claude-cli-path ()
  "Return the configured Claude CLI path."
  (if (boundp 'claude-code-ide-cli-path)
      claude-code-ide-cli-path
    (executable-find "claude")))

(defun my/health--codex-executable ()
  "Return the configured Codex CLI executable path."
  (executable-find
   (if (boundp 'codex-cli-executable) codex-cli-executable "codex")))

(define-derived-mode my/health-mode special-mode "Health"
  "Major mode for config health reports.")

(defun my/health-config-root ()
  "Return the root of the current Emacs config."
  (expand-file-name
   ".."
   (file-name-directory my/health--module-file)))

(defun my/health--stale-startup-bytecode-files ()
  "Return stale top-level startup `.elc' files as absolute paths."
  (let (stale)
    (dolist (file my/health-startup-bytecode-files (nreverse stale))
      (let* ((source (expand-file-name file (my/health-config-root)))
             (bytecode (concat source "c")))
        (when (and (file-exists-p source)
                   (file-exists-p bytecode)
                   (file-newer-than-file-p source bytecode))
          (push bytecode stale))))))

(defun my/health--startup-bytecode-freshness-result (stale-files)
  "Return a failed health result for STALE-FILES."
  (list :ok nil
        :status 'stale-bytecode
        :output
        (format "Stale startup bytecode: %s"
                (string-join
                 (mapcar (lambda (file)
                           (file-relative-name file (my/health-config-root)))
                         stale-files)
                 ", "))))

(defun my/health--emacs-program ()
  "Return the Emacs executable used for batch health checks."
  (or (and invocation-directory invocation-name
           (expand-file-name invocation-name invocation-directory))
      (executable-find "emacs")
      "emacs"))

(defun my/health--run-batch (&rest args)
  "Run Emacs in batch with ARGS and return a plist report."
  (let ((default-directory (my/health-config-root)))
    (with-temp-buffer
      (let ((status (apply #'call-process
                           (my/health--emacs-program)
                           nil
                           (current-buffer)
                           nil
                           args))
            (output (string-trim (buffer-string))))
        (list :ok (eq status 0)
              :status status
              :output output)))))

(defun my/health--record-startup-metrics-h ()
  "Capture startup metrics for the current Emacs session."
  (setq my/health-startup-time
        (float-time (time-subtract after-init-time before-init-time))
        my/health-startup-gcs gcs-done
        my/health-startup-package-count (length package-alist))
  (unless noninteractive
    (message "Emacs ready in %.2fs with %d GCs (%d packages)"
             my/health-startup-time
             my/health-startup-gcs
             my/health-startup-package-count)))

(add-hook 'emacs-startup-hook #'my/health--record-startup-metrics-h 90)

(defun my/health-startup-summary ()
  "Display a concise summary of current-session startup metrics."
  (interactive)
  (if (and my/health-startup-time my/health-startup-gcs)
      (message "Startup: %.2fs, %d GCs, %d packages"
               my/health-startup-time
               my/health-startup-gcs
               (or my/health-startup-package-count 0))
    (message "Startup metrics are not available yet")))

(defun my/health-startup-check ()
  "Run a batch startup smoke test."
  (interactive)
  (let* ((stale-files (my/health--stale-startup-bytecode-files))
         (result
          (if stale-files
              (my/health--startup-bytecode-freshness-result stale-files)
            (my/health--run-batch
             "--batch" "--no-site-file" "--no-site-lisp" "--no-splash"
             (format "--init-directory=%s"
                     (directory-file-name (my/health-config-root)))
             "-q"
             "-L" "."
             "-l" "early-init"
             "-l" "init"
             "--eval"
             "(progn
                (unless (featurep 'init-modules)
                  (error \"init-modules did not load\"))
                (when (eq system-type 'darwin)
                  (unless (featurep 'init-macos)
                    (error \"init-macos did not load\")))
                (message \"INIT-OK\"))"))))
    (if (called-interactively-p 'interactive)
        (message "Startup smoke %s"
                 (if (plist-get result :ok) "passed" "failed"))
      result)))

(defun my/health-byte-compile-check ()
  "Run a batch byte-compile smoke test."
  (interactive)
  (let ((result (my/health--run-batch
                 "--batch" "-Q"
                 "-l" "./init.el"
                 "--eval" "(setq debug-on-error t)"
                 "--eval" "(my/byte-compile-config)")))
    (if (called-interactively-p 'interactive)
        (message "Byte-compile smoke %s"
                 (if (plist-get result :ok) "passed" "failed"))
      result)))

(defun my/health-native-compile-check ()
  "Run a batch native-compile smoke test."
  (interactive)
  (let ((result (my/health--run-batch
                 "--batch" "-Q"
                 "-l" "./init.el"
                 "--eval" "(setq debug-on-error t)"
                 "--eval"
                 "(if (and (fboundp 'native-comp-available-p)
                           (native-comp-available-p)
                           (fboundp 'native-compile))
                      (progn
                        (native-compile
                         (expand-file-name \"lisp/init-compile.el\" user-emacs-directory))
                        (message \"NATIVE-OK\"))
                    (message \"NATIVE-SKIP\"))")))
    (if (called-interactively-p 'interactive)
        (message "Native-compile smoke %s"
                 (if (plist-get result :ok) "passed" "failed"))
      result)))

(defun my/health--executable-report ()
  "Return executable availability as an alist."
  (mapcar (lambda (name)
            (cons name (executable-find name)))
          my/health-executables))

(defun my/health--command-report ()
  "Return command availability as an alist."
  (mapcar (lambda (command)
            (cons command (fboundp command)))
          my/health-important-commands))

(defun my/health--library-report ()
  "Return critical library availability as an alist."
  (mapcar (lambda (entry)
            (cons (car entry)
                  (ignore-errors (locate-library (cdr entry)))))
          my/health-critical-libraries))

(defun my/health--artifact-report ()
  "Return critical runtime artifact availability as an alist."
  (list
   (cons 'epdfinfo
         (let ((path (my/health--bundled-epdfinfo)))
           (and (file-executable-p path) path)))
   (cons 'claude-cli
         (let ((path (my/health--claude-cli-path)))
           (and path (file-executable-p path) path)))
   (cons 'codex-cli
         (my/health--codex-executable))))

(defun my/health--feature-report ()
  "Return lightweight feature/runtime checks for critical subsystems."
  (list
   (cons 'rpc-method
         (ignore-errors
           (require 'tramp)
           (or (assoc "rpc" tramp-methods)
               (assoc 'rpc tramp-methods))))
   (cons 'claude-code-ide-loaded
         (ignore-errors
           (require 'claude-code-ide)
           (fboundp 'claude-code-ide-menu)))
   (cons 'codex-cli-loaded
         (ignore-errors
           (require 'codex-cli)
           (fboundp 'codex-cli-toggle)))
   (cons 'theme-loaded
         (ignore-errors
           (memq 'kanagawa-wave custom-enabled-themes)))
   (cons 'ligature-enabled
         (ignore-errors
           (or (bound-and-true-p global-ligature-mode)
               (and (featurep 'ligature)
                    (fboundp 'ligature-mode)))))))

(defun my/health-critical-check ()
  "Return a compact batch-friendly report for critical bootstrap features."
  (interactive)
  (let ((report
         (list
          :libraries (my/health--library-report)
          :artifacts (my/health--artifact-report)
          :features (my/health--feature-report)
          :lock (when (fboundp 'my/package-lock-audit)
                  (my/package-lock-audit))
          :state (when (fboundp 'my/maintenance-state-report)
                   (my/maintenance-state-report)))))
    (if (called-interactively-p 'interactive)
        (message "%S" report)
      report)))

(defun my/health--insert-check (label result)
  "Insert LABEL and RESULT into the current health buffer."
  (insert (format "%-20s %s\n"
                  label
                  (if (plist-get result :ok) "OK" "FAIL")))
  (when-let* ((status (plist-get result :status)))
    (insert (format "  exit: %s\n" status)))
  (when-let* ((output (plist-get result :output))
              ((not (string-empty-p output))))
    (insert (format "  %s\n" output))))

(defun my/health-report ()
  "Open a health report buffer for this Emacs config."
  (interactive)
  (let ((buffer (get-buffer-create "*Health*"))
        (startup (my/health-startup-check))
        (compile (my/health-byte-compile-check))
        (native (my/health-native-compile-check))
        (executables (my/health--executable-report))
        (commands (my/health--command-report))
        (libraries (my/health--library-report))
        (artifacts (my/health--artifact-report))
        (features (my/health--feature-report))
        (lock-report (and (fboundp 'my/package-lock-audit)
                          (my/package-lock-audit)))
        (state-report (and (fboundp 'my/maintenance-state-report)
                           (my/maintenance-state-report))))
    (with-current-buffer buffer
      (my/health-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Health report for %s\n\n"
                        (abbreviate-file-name (my/health-config-root))))
        (insert "Current session\n")
        (insert "---------------\n")
        (insert (format "%-20s %s\n" "Startup time"
                        (if my/health-startup-time
                            (format "%.2fs" my/health-startup-time)
                          "N/A")))
        (insert (format "%-20s %s\n" "GCs"
                        (if my/health-startup-gcs
                            (number-to-string my/health-startup-gcs)
                          "N/A")))
        (insert (format "%-20s %s\n\n" "Packages"
                        (if my/health-startup-package-count
                            (number-to-string my/health-startup-package-count)
                          "N/A")))
        (insert "Batch checks\n")
        (insert "------------\n")
        (my/health--insert-check "Startup smoke" startup)
        (insert "\n")
        (my/health--insert-check "Byte compile" compile)
        (insert "\n")
        (my/health--insert-check "Native compile" native)
        (insert "\nExecutables\n")
        (insert "-----------\n")
        (dolist (entry executables)
          (insert (format "%-20s %s\n"
                          (car entry)
                          (or (cdr entry) "MISSING"))))
        (insert "\nCommands\n")
        (insert "--------\n")
        (dolist (entry commands)
          (insert (format "%-28s %s\n"
                          (car entry)
                          (if (cdr entry) "OK" "MISSING"))))
        (insert "\nLibraries\n")
        (insert "---------\n")
        (dolist (entry libraries)
          (insert (format "%-28s %s\n"
                          (car entry)
                          (or (cdr entry) "MISSING"))))
        (insert "\nArtifacts\n")
        (insert "---------\n")
        (dolist (entry artifacts)
          (insert (format "%-28s %s\n"
                          (car entry)
                          (or (cdr entry) "MISSING"))))
        (insert "\nCritical features\n")
        (insert "-----------------\n")
        (dolist (entry features)
          (insert (format "%-28s %s\n"
                          (car entry)
                          (if (cdr entry) "OK" "MISSING"))))
        (when lock-report
          (insert "\nLock Audit\n")
          (insert "----------\n")
          (insert (format "%-28s %s\n" "lock-status"
                          (if (plist-get lock-report :ok) "OK" "DRIFT")))
          (insert (format "%-28s %s\n" "lock-version"
                          (or (plist-get lock-report :lock-version) "MISSING")))
          (insert (format "%-28s %S\n" "archive-missing"
                          (plist-get lock-report :archive-missing-in-lock)))
          (insert (format "%-28s %S\n" "archive-extra"
                          (plist-get lock-report :archive-extra-in-lock)))
          (insert (format "%-28s %S\n" "vc-missing"
                          (plist-get lock-report :vc-missing-in-lock)))
          (insert (format "%-28s %S\n" "vc-extra"
                          (plist-get lock-report :vc-extra-in-lock))))
        (when state-report
          (insert "\nState Snapshot\n")
          (insert "--------------\n")
          (insert (format "%-28s %s\n" "tar"
                          (or (plist-get state-report :tar) "MISSING")))
          (insert (format "%-28s %s\n" "backup-dir"
                          (or (plist-get state-report :backup-dir) "MISSING")))
          (insert (format "%-28s %S\n" "paths"
                          (plist-get state-report :paths)))
          (insert (format "%-28s %S\n" "missing-paths"
                          (plist-get state-report :missing-paths))))
        (goto-char (point-min))
        (use-local-map (copy-keymap special-mode-map))
        (local-set-key (kbd "g") #'my/health-report)))
    (pop-to-buffer buffer)))

(transient-define-prefix my/health-dispatch ()
  "Health check workflow."
  [["Checks"
   ("h" "full report" my/health-report)
    ("d" "critical doctor" my/health-critical-check)
    ("l" "lock audit" my/package-lock-audit)
    ("i" "init stats" my/health-startup-summary)
    ("s" "startup smoke" my/health-startup-check)
    ("c" "byte compile smoke" my/health-byte-compile-check)
    ("n" "native compile smoke" my/health-native-compile-check)]
   ["Ops"
    ("b" "compile board" my/compile-board)]])

(my/leader!
  "h H" '(:def my/health-dispatch :which-key "health")
  "h i" '(:def my/health-startup-summary :which-key "init stats"))

(provide 'init-health)
;;; init-health.el ends here
