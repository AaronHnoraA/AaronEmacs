;;; init-health.el --- Config health checks -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-ui-board)
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

(config-defvar my/health-executables nil
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

(define-derived-mode my/health-mode aaron-ui-board-mode "Health"
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
        my/health-startup-package-count (length package-alist)))

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
   (cons 'aaronnote-latex-agent
         ;; OK unless the Noema LaTeX export engine wants an AI backend but
         ;; that backend's binary is missing (export still works via mechanical
         ;; fallback).
         (or (equal (bound-and-true-p my/noema-latex-export-engine) "mechanical")
             (let ((backend (or (bound-and-true-p my/noema-latex-export-agent) "codex")))
               (cond
                ((equal backend "claude") (and (my/health--claude-cli-path) t))
                ((equal backend "opencode")
                 (and (executable-find (or (bound-and-true-p my/noema-opencode-executable) "opencode")) t))
                (t (and (my/health--codex-executable) t))))))
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
  (let ((ok (plist-get result :ok)))
    (insert "   "
            (propertize (format "%-20s" label) 'face 'aaron-ui-board-meta)
            (propertize (if ok "OK" "FAIL")
                        'face (if ok 'aaron-ui-board-good 'aaron-ui-board-bad))
            "\n")
    (when-let* ((status (plist-get result :status)))
      (insert "   "
              (propertize (format "exit: %s" status) 'face 'aaron-ui-board-detail)
              "\n"))
    (when-let* ((output (plist-get result :output))
                ((not (string-empty-p output))))
      (insert "   "
              (propertize output 'face 'aaron-ui-board-detail)
              "\n"))))

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
      (aaron-ui-board-set-header "Config Health" 'health)
      (setq-local aaron-ui-board-refresh-function #'my/health-report)
      (let ((inhibit-read-only t))
        (aaron-ui-board-render
         (lambda ()
           (aaron-ui-board-insert-page-header
            "Config Health"
            :icon 'health
            :subtitle (abbreviate-file-name (my/health-config-root)))

           ;; --- Session ---
           (aaron-ui-board-insert-section "Current Session")
           (aaron-ui-board-insert-field
            "Startup time"
            (if my/health-startup-time (format "%.2fs" my/health-startup-time) "N/A"))
           (aaron-ui-board-insert-field
            "GCs"
            (if my/health-startup-gcs (number-to-string my/health-startup-gcs) "N/A"))
           (aaron-ui-board-insert-field
            "Packages"
            (if my/health-startup-package-count
                (number-to-string my/health-startup-package-count) "N/A"))
           (insert "\n")

           ;; --- Batch checks ---
           (aaron-ui-board-insert-section "Batch Checks")
           (my/health--insert-check "Startup smoke" startup)
           (my/health--insert-check "Byte compile"  compile)
           (my/health--insert-check "Native compile" native)
           (insert "\n")

           ;; --- Executables ---
           (aaron-ui-board-insert-section "Executables" (length executables))
           (dolist (entry executables)
             (aaron-ui-board-insert-field
              (symbol-name (car entry))
              (or (cdr entry) "MISSING")
              (if (cdr entry) 'aaron-ui-board-good 'aaron-ui-board-bad)))
           (insert "\n")

           ;; --- Commands ---
           (aaron-ui-board-insert-section "Commands" (length commands))
           (dolist (entry commands)
             (aaron-ui-board-insert-field
              (symbol-name (car entry))
              (if (cdr entry) "OK" "MISSING")
              (if (cdr entry) 'aaron-ui-board-good 'aaron-ui-board-bad)))
           (insert "\n")

           ;; --- Libraries ---
           (aaron-ui-board-insert-section "Libraries" (length libraries))
           (dolist (entry libraries)
             (aaron-ui-board-insert-field
              (symbol-name (car entry))
              (if (cdr entry) (abbreviate-file-name (cdr entry)) "MISSING")
              (if (cdr entry) 'aaron-ui-board-good 'aaron-ui-board-bad)))
           (insert "\n")

           ;; --- Artifacts ---
           (aaron-ui-board-insert-section "Artifacts" (length artifacts))
           (dolist (entry artifacts)
             (if (cdr entry)
                 (progn
                   (insert "   "
                           (propertize (format "%-16s" (symbol-name (car entry)))
                                       'face 'aaron-ui-board-meta))
                   (aaron-ui-board-insert-openable-path
                    (if (stringp (cdr entry)) (cdr entry)
                      (format "%s" (cdr entry))))
                   (insert "\n"))
               (aaron-ui-board-insert-field
                (symbol-name (car entry)) "MISSING" 'aaron-ui-board-bad)))
           (insert "\n")

           ;; --- Features ---
           (aaron-ui-board-insert-section "Critical Features" (length features))
           (dolist (entry features)
             (aaron-ui-board-insert-field
              (symbol-name (car entry))
              (if (cdr entry) "OK" "MISSING")
              (if (cdr entry) 'aaron-ui-board-good 'aaron-ui-board-bad)))
           (insert "\n")

           ;; --- Lock audit ---
           (when lock-report
             (aaron-ui-board-insert-section
              "Lock Audit" nil
              (if (plist-get lock-report :ok) 'success 'warning))
             (aaron-ui-board-insert-field
              "lock-status"
              (if (plist-get lock-report :ok) "OK" "DRIFT")
              (if (plist-get lock-report :ok) 'aaron-ui-board-good 'aaron-ui-board-warn))
             (aaron-ui-board-insert-field
              "lock-version"
              (or (plist-get lock-report :lock-version) "MISSING"))
             (aaron-ui-board-insert-field
              "archive-missing"
              (format "%S" (plist-get lock-report :archive-missing-in-lock)))
             (aaron-ui-board-insert-field
              "archive-extra"
              (format "%S" (plist-get lock-report :archive-extra-in-lock)))
             (aaron-ui-board-insert-field
              "vc-missing"
              (format "%S" (plist-get lock-report :vc-missing-in-lock)))
             (aaron-ui-board-insert-field
              "vc-extra"
              (format "%S" (plist-get lock-report :vc-extra-in-lock)))
             (insert "\n"))

           ;; --- State snapshot ---
           (when state-report
             (aaron-ui-board-insert-section "State Snapshot")
             (aaron-ui-board-insert-field
              "tar" (or (plist-get state-report :tar) "MISSING"))
             (aaron-ui-board-insert-field
              "backup-dir" (or (plist-get state-report :backup-dir) "MISSING"))
             (aaron-ui-board-insert-field
              "paths" (format "%S" (plist-get state-report :paths)))
             (aaron-ui-board-insert-field
              "missing-paths" (format "%S" (plist-get state-report :missing-paths)))
             (insert "\n"))

           ;; --- External gateway ---
           (aaron-ui-board-insert-section "Emacs Gateway")
           (let* ((live (and (fboundp 'remote-gateway-live-p)
                             (remote-gateway-live-p)))
                  (info (and live
                             (remote-gateway-connection-info)))
                  (clients (and live
                                (remote-gateway-client-list))))
             (aaron-ui-board-insert-field
              "listener"
              (if live
                  (format "%s:%s"
                          (plist-get info :host)
                          (plist-get info :port))
                "stopped")
              (if live 'aaron-ui-board-good 'aaron-ui-board-bad))
             (aaron-ui-board-insert-field
              "rpc" (or (plist-get info :http-url) "unavailable"))
             (aaron-ui-board-insert-field
              "websocket" (or (plist-get info :websocket-url) "unavailable"))
             (aaron-ui-board-insert-field
              "clients" (format "%d" (length clients)))
             (aaron-ui-board-insert-field
              "discovery"
              (if (and live
                       (fboundp 'remote-gateway--discovery-file))
                  (remote-gateway--discovery-file)
                "unavailable")))
           (insert "\n")

           ;; --- Noema ---
           (aaron-ui-board-insert-section "Noema")
           (let ((running (and (boundp 'my/noema--process)
                               (processp my/noema--process)
                               (process-live-p my/noema--process)))
                 (ready (and (boundp 'my/noema--ready) my/noema--ready)))
             (aaron-ui-board-insert-field
              "process" (if running "running" "stopped")
              (if running 'aaron-ui-board-good 'aaron-ui-board-meta))
             (aaron-ui-board-insert-field
              "ready"
              (if ready
                  (format "yes (port %s)"
                          (if (boundp 'my/noema--port)
                              (number-to-string my/noema--port) "?"))
                "no")
              (if ready 'aaron-ui-board-good 'aaron-ui-board-meta)))
           (aaron-ui-board-insert-field
            "runtime"
            (if (and (fboundp 'my/noema-roam--runtime-available-p)
                     (my/noema-roam--runtime-available-p))
                "available" "missing"))
           (aaron-ui-board-insert-field
            "last-sync"
            (if (and (boundp 'my/noema--last-sync-stats)
                     my/noema--last-sync-stats)
                my/noema--last-sync-stats
              "never"))
           (insert "\n")
           (aaron-ui-board-insert-key-hints "Keys: g refresh  q quit"))))
      (pop-to-buffer buffer))))

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
