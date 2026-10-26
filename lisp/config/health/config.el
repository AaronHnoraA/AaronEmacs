;;; config/health/config.el --- Config health checks -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-ui-board)
(require 'aaron-funcs)
(require 'subr-x)
(require 'transient)
(require 'aaron-modules)
(require 'aaron-doctor)

(declare-function +compile/board "config/compile/config" ())
(declare-function +compile/byte-compile-config "config/compile/config" (&optional force))
(declare-function +compile/native-compile-config "config/compile/config" (&optional force))
(declare-function aaron/package-lock-audit "aaron-packages" ())
(declare-function +maintenance/state-report "config/maintenance/config" ())

(defgroup +health nil
  "Health checks for the Emacs config."
  :group 'convenience)

(defconst +health--module-file
  (or load-file-name buffer-file-name)
  "Absolute path of `config/health/config.el' at load time.")

(config-defvar +health-executables nil
  "Executables checked by `+health/report'."
  :type '(repeat string)
  :group '+health)

(defconst +health-important-commands
  '(+compile/board
    config-compile-menu
    +compile/byte-compile-config
    +compile/native-compile-config
    show-imenu
    test-menu
    task-menu
    project-run-menu
    debug-profile-menu
    output-menu
    diagnostics-menu
    language-server-menu
    +language-server/manager
    +language-server/doctor
    editing-menu
    file-menu
    +calendar/calendar-year
    +github/topic-open
    bazel-target-menu)
  "Interactive commands checked in `+health/report'.")

(defconst +health-critical-libraries
  '((ligature . "ligature")
    (theme . "aaron-ui")
    (pdf-tools . "pdf-tools")
    (tramp-rpc . "tramp-rpc"))
  "Libraries that should be available after a healthy bootstrap.")

(defvar +health-startup-time nil
  "Elapsed startup time in seconds for the current Emacs session.")

(defvar +health-startup-gcs nil
  "GC count recorded for the current Emacs session startup.")

(defvar +health-startup-package-count nil
  "Installed package count recorded after startup.")

(defvar +health--batch-results nil
  "Results of batch checks explicitly run during this Emacs session.")

(defconst +health-startup-bytecode-files
  '("early-init.el" "init.el" "bootstrap.el")
  "Top-level startup files whose stale bytecode should fail health checks.")

(defun +health--bundled-epdfinfo ()
  "Return the configured bundled epdfinfo path."
  (expand-file-name "elpa/pdf-tools-20260102.1101/epdfinfo"
                    user-emacs-directory))

(defun +health--claude-cli-path ()
  "Return the configured Claude CLI path."
  (executable-find
   (if (boundp 'claude-code-ide-cli-path) claude-code-ide-cli-path "claude")))

(defun +health--codex-executable ()
  "Return the configured Codex CLI executable path."
  (executable-find
   (if (boundp 'codex-cli-executable) codex-cli-executable "codex")))

(define-derived-mode +health-mode aaron-ui-board-mode "Health"
  "Major mode for config health reports.")

(defun +health-config-root ()
  "Return the root of the current Emacs config."
  (expand-file-name
   "../../.."
   (file-name-directory +health--module-file)))

(defun +health--stale-startup-bytecode-files ()
  "Return stale top-level startup `.elc' files as absolute paths."
  (let (stale)
    (dolist (file +health-startup-bytecode-files (nreverse stale))
      (let* ((source (expand-file-name file (+health-config-root)))
             (bytecode (concat source "c")))
        (when (and (file-exists-p source)
                   (file-exists-p bytecode)
                   (file-newer-than-file-p source bytecode))
          (push bytecode stale))))))

(defun +health--startup-bytecode-freshness-result (stale-files)
  "Return a failed health result for STALE-FILES."
  (list :ok nil
        :status 'stale-bytecode
        :output
        (format "Stale startup bytecode: %s"
                (string-join
                 (mapcar (lambda (file)
                           (file-relative-name file (+health-config-root)))
                         stale-files)
                 ", "))))

(defun +health--emacs-program ()
  "Return the Emacs executable used for batch health checks."
  (or (and invocation-directory invocation-name
           (expand-file-name invocation-name invocation-directory))
      (executable-find "emacs")
      "emacs"))

(defun +health--run-batch (&rest args)
  "Run Emacs in batch with ARGS and return a plist report."
  (let ((default-directory (+health-config-root))
        (process-environment (default-value 'process-environment))
        (exec-path (default-value 'exec-path)))
    (with-temp-buffer
      (let ((status (apply #'call-process
                           (+health--emacs-program)
                           nil
                           (current-buffer)
                           nil
                           args))
            (output (string-trim (buffer-string))))
        (list :ok (eq status 0)
              :status status
              :output output)))))

(defun +health--record-batch-result (name result)
  "Remember RESULT for batch check NAME and refresh a visible health board."
  (setf (alist-get name +health--batch-results) result)
  (when-let* ((buffer (get-buffer "*Health*"))
              ((get-buffer-window buffer t)))
    (+health/report t))
  result)

(defun +health--record-startup-metrics-h ()
  "Capture startup metrics for the current Emacs session."
  (setq +health-startup-time
        (float-time (time-subtract after-init-time before-init-time))
        +health-startup-gcs gcs-done
        +health-startup-package-count (length package-alist)))

(add-hook 'emacs-startup-hook #'+health--record-startup-metrics-h 90)

(defun +health/startup-summary ()
  "Display a concise summary of current-session startup metrics."
  (interactive)
  (if (and +health-startup-time +health-startup-gcs)
      (message "Startup: %.2fs, %d GCs, %d packages"
               +health-startup-time
               +health-startup-gcs
               (or +health-startup-package-count 0))
    (message "Startup metrics are not available yet")))

(defun +health/startup-check ()
  "Run a batch startup smoke test."
  (interactive)
  (let* ((stale-files (+health--stale-startup-bytecode-files))
         (result
          (if stale-files
              (+health--startup-bytecode-freshness-result stale-files)
            (+health--run-batch
             "--batch" "--no-site-file" "--no-site-lisp" "--no-splash"
             (format "--init-directory=%s"
                     (directory-file-name (+health-config-root)))
             "-q"
             "-L" "."
             "-l" "early-init"
             "-l" "init"
             "--eval"
             "(progn
                (unless (featurep 'init-modules)
                  (error \"init-modules did not load\"))
                (when (eq system-type 'darwin)
                  (unless (featurep 'os/macos/config)
                    (error \"init-macos did not load\")))
                (message \"INIT-OK\"))"))))
    (+health--record-batch-result 'startup result)
    (if (called-interactively-p 'interactive)
        (message "Startup smoke %s"
                 (if (plist-get result :ok) "passed" "failed"))
      result)))

(defun +health/byte-compile-check ()
  "Run a batch byte-compile smoke test."
  (interactive)
  (let ((result (+health--run-batch
                 "--batch" "-Q"
                 "-l" "./init.el"
                 "--eval" "(setq debug-on-error t)"
                 "--eval" "(+compile/byte-compile-config)")))
    (+health--record-batch-result 'byte-compile result)
    (if (called-interactively-p 'interactive)
        (message "Byte-compile smoke %s"
                 (if (plist-get result :ok) "passed" "failed"))
      result)))

(defun +health/native-compile-check ()
  "Run a batch native-compile smoke test."
  (interactive)
  (let ((result (+health--run-batch
                 "--batch" "-Q"
                 "-l" "./init.el"
                 "--eval" "(setq debug-on-error t)"
                 "--eval"
                 "(if (and (fboundp 'native-comp-available-p)
                           (native-comp-available-p)
                           (fboundp 'native-compile))
                      (progn
                        (native-compile
                         (expand-file-name \"lisp/config/compile/config.el\" user-emacs-directory))
                        (message \"NATIVE-OK\"))
                    (message \"NATIVE-SKIP\"))")))
    (+health--record-batch-result 'native-compile result)
    (if (called-interactively-p 'interactive)
        (message "Native-compile smoke %s"
                 (if (plist-get result :ok) "passed" "failed"))
      result)))

(defun +health--executable-report ()
  "Return executable availability as an alist."
  (mapcar (lambda (name)
            (cons name (executable-find name)))
          +health-executables))

(defun +health--command-report ()
  "Return interactive command availability as an alist."
  (mapcar (lambda (command)
            (cons command (commandp command)))
          +health-important-commands))

(defun +health--library-report ()
  "Return critical library availability as an alist."
  (mapcar (lambda (entry)
            (cons (car entry)
                  (ignore-errors (locate-library (cdr entry)))))
          +health-critical-libraries))

(defconst +health-noema-asset-links
  '("etc/katex-macros"
    "etc/prose-accepted-words.txt"
    "templates/latex"
    "templates/tex"
    "templates/noema")
  "Links from this config into `site-lisp/noema/resources/'.

These are the single source of truth for math macros, prose dictionaries and
LaTeX templates.  They were once committed with absolute paths into a Noema
checkout that later moved, and a dangling link fails silently: Emacs exports
the dead path to the Noema server, which then loads no macros and no export
templates at all.  Check them so that cannot happen quietly again.")

(defun +health--noema-asset-link-report ()
  "Return each shared Noema asset link and whether it still resolves."
  (mapcar
   (lambda (relative)
     (let ((path (expand-file-name relative user-emacs-directory)))
       (cons (intern relative)
             (cond
              ((file-exists-p path) t)
              ((file-symlink-p path)
               (format "dangling -> %s" (file-symlink-p path)))
              (t "missing")))))
   +health-noema-asset-links))

(defun +health--artifact-report ()
  "Return critical runtime artifact availability as an alist."
  (list
   (cons 'noema-asset-links (+health--noema-asset-link-report))
   (cons 'epdfinfo
         (let ((path (+health--bundled-epdfinfo)))
           (and (file-executable-p path) path)))
   (cons 'claude-cli
         (let ((path (+health--claude-cli-path)))
           (and path (file-executable-p path) path)))
   (cons 'codex-cli
         (+health--codex-executable))))

(defun +health--feature-report ()
  "Return lightweight feature/runtime checks for critical subsystems."
  (list
   (cons 'rpc-method
         (ignore-errors
           (require 'tramp)
           (or (assoc "rpc" tramp-methods)
               (assoc 'rpc tramp-methods))))
   (cons 'noema-latex-agent
         ;; OK unless the Noema LaTeX export engine wants an AI backend but
         ;; that backend's binary is missing (export still works via mechanical
         ;; fallback).
         (or (equal (bound-and-true-p +noema-latex-export-engine) "mechanical")
             (let ((backend (or (bound-and-true-p +noema-latex-export-agent) "codex")))
               (cond
                ((equal backend "claude") (and (+health--claude-cli-path) t))
                ((equal backend "opencode")
                 (and (executable-find (or (bound-and-true-p +noema-opencode-executable) "opencode")) t))
                (t (and (+health--codex-executable) t))))))
   (cons 'theme-loaded
         (ignore-errors
           (memq 'kanagawa-wave custom-enabled-themes)))
   (cons 'ligature-enabled
         (ignore-errors
           (or (bound-and-true-p global-ligature-mode)
               (and (featurep 'ligature)
                    (fboundp 'ligature-mode)))))))

(defun +health/critical-check ()
  "Return a compact batch-friendly report for critical bootstrap features."
  (interactive)
  (let* ((default-directory (+health-config-root))
         (process-environment (default-value 'process-environment))
         (exec-path (default-value 'exec-path))
         (report
          (list
           :libraries (+health--library-report)
           :artifacts (+health--artifact-report)
           :features (+health--feature-report)
           :lock (when (fboundp 'aaron/package-lock-audit)
                   (aaron/package-lock-audit))
           :state (when (fboundp '+maintenance/state-report)
                    (+maintenance/state-report)))))
    (if (called-interactively-p 'interactive)
        (message "%S" report)
      report)))

(defun +health--insert-check (label result)
  "Insert LABEL and RESULT into the current health buffer."
  (let ((ok (plist-get result :ok)))
    (insert "   "
            (propertize (format "%-20s" label) 'face 'aaron-ui-board-meta)
            (propertize (cond ((null result) "NOT RUN")
                              (ok "OK")
                              (t "FAIL"))
                        'face (cond ((null result) 'aaron-ui-board-meta)
                                    (ok 'aaron-ui-board-good)
                                    (t 'aaron-ui-board-bad)))
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

(defun +health/reload-project-state ()
  "Reload repaired project state and refresh this health board."
  (interactive)
  (project-reload-ignored-roots)
  (+health/report t))

(defun +health/report (&optional no-display)
  "Open a health report buffer without running batch checks.
With NO-DISPLAY, refresh an already visible board without selecting it."
  (interactive)
  (let* ((default-directory (+health-config-root))
         (process-environment (default-value 'process-environment))
         (exec-path (default-value 'exec-path))
         (buffer (get-buffer-create "*Health*"))
         (startup (alist-get 'startup +health--batch-results))
         (compile (alist-get 'byte-compile +health--batch-results))
         (native (alist-get 'native-compile +health--batch-results))
         (executables (+health--executable-report))
         (commands (+health--command-report))
         (libraries (+health--library-report))
         (artifacts (+health--artifact-report))
         (features (+health--feature-report))
         (settings-issues
          (when (fboundp 'config--integrity-issues)
            (condition-case err
                (config--integrity-issues)
              (error (list (format "Settings check failed: %s"
                                   (error-message-string err)))))))
         (project-state-error
          (bound-and-true-p +project-ignored-roots-load-error))
         (lock-report (and (fboundp 'aaron/package-lock-audit)
                           (aaron/package-lock-audit)))
         (state-report (and (fboundp '+maintenance/state-report)
                            (+maintenance/state-report))))
    (with-current-buffer buffer
      (+health-mode)
      (setq-local default-directory (+health-config-root)
                  process-environment (default-value 'process-environment)
                  exec-path (default-value 'exec-path))
      (aaron-ui-board-set-header "Config Health" 'health)
      (setq-local aaron-ui-board-refresh-function #'+health/report)
      (let ((inhibit-read-only t))
        (aaron-ui-board-render
         (lambda ()
           (aaron-ui-board-insert-page-header
            "Config Health"
            :icon 'health
            :subtitle (abbreviate-file-name (+health-config-root)))

           (insert "   ")
           (aaron-ui-board-insert-actions
            '((:label "Startup" :command +health/startup-check
               :help "Run the startup smoke test")
              (:label "Byte check" :command +health/byte-compile-check
               :help "Run byte compilation in a batch Emacs")
              (:label "Native check" :command +health/native-compile-check
               :help "Run native compilation in a batch Emacs")
              (:label "Compile board" :command +compile/board
               :help "Open compilation and cleanup tools")))
           (insert "\n\n")

           ;; --- Session ---
           (aaron-ui-board-insert-section "Current Session")
           (aaron-ui-board-insert-field
            "Startup time"
            (if +health-startup-time (format "%.2fs" +health-startup-time) "N/A"))
           (aaron-ui-board-insert-field
            "GCs"
            (if +health-startup-gcs (number-to-string +health-startup-gcs) "N/A"))
           (aaron-ui-board-insert-field
            "Packages"
            (if +health-startup-package-count
                (number-to-string +health-startup-package-count) "N/A"))
           (insert "\n")

           ;; The module registry (lisp/core/aaron-modules.el) is the source
           ;; of truth; the module board holds the full detail.  Deferred
           ;; modules are healthy while they wait for their trigger.
           (aaron-ui-board-insert-section "Modules")
           (let* ((keys (aaron-module-list))
                  (states (mapcar (lambda (k) (aaron-module-state (aaron-module-get k)))
                                  keys))
                  (summary (aaron-doctor-summary)))
             (aaron-ui-board-insert-field
              "Declared"
              (format "%d (%d loaded, %d waiting, %d autoload, %d failed) in %s"
                      (length keys) (cl-count 'loaded states) (cl-count 'waiting states)
                      (cl-count 'autoload states) (cl-count 'failed states)
                      (if aaron-modules-init-time
                          (format "%.2fs" aaron-modules-init-time) "-"))
              (if (memq 'failed states) 'aaron-ui-board-bad 'aaron-ui-board-good))
             (aaron-ui-board-insert-field
              "Key conflicts" (number-to-string (length aaron-module-key-conflicts))
              (if aaron-module-key-conflicts 'aaron-ui-board-warn 'aaron-ui-board-good))
             (aaron-ui-board-insert-field
              "Doctor"
              (if (zerop (plist-get summary :modules))
                  "not run"
                (format "%d errors, %d warnings, %d policy hits"
                        (plist-get summary :errors) (plist-get summary :warnings)
                        (plist-get summary :violations)))
              (cond ((zerop (plist-get summary :modules)) 'shadow)
                    ((> (plist-get summary :errors) 0) 'aaron-ui-board-bad)
                    ((> (plist-get summary :violations) 0) 'aaron-ui-board-warn)
                    (t 'aaron-ui-board-good)))
             (dolist (module (delete-dups
                              (append (copy-sequence aaron-module-optional-modules)
                                      (mapcar #'car aaron-module-load-failures))))
               (let* ((failure (alist-get module aaron-module-load-failures))
                      (triggers (alist-get module aaron-module-deferred-triggers))
                      (status
                       (cond
                        (failure failure)
                        ((featurep module) "loaded")
                        (triggers
                         (format "waiting for %s"
                                 (mapconcat #'symbol-name triggers ", ")))
                        (t "not loaded"))))
                 (aaron-ui-board-insert-field
                  (format "%s" module) status
                  (cond (failure 'aaron-ui-board-bad)
                        ((featurep module) 'aaron-ui-board-good)
                        (t 'aaron-ui-board-warn)))))
             (insert "   ")
             (aaron-ui-board-insert-actions
              '((:label "Module board" :command aaron/modules
                 :help "Every module's state, footprint and findings")
                (:label "Doctor" :command aaron/doctor
                 :help "Diagnose every module"))))
           (insert "\n\n")

           ;; Settings and the health board share the same integrity check.
           (aaron-ui-board-insert-section "Settings")
           (aaron-ui-board-insert-field
            "stores"
            (if settings-issues
                (format "%d issue(s)" (length settings-issues))
              "OK")
            (if settings-issues 'aaron-ui-board-bad 'aaron-ui-board-good))
           (insert "   ")
           (aaron-ui-board-insert-actions
            '((:label "Settings" :command config-dispatch
               :help "Open the settings menu")
              (:label "Inspect" :command config-check
               :help "Show store and registry problems")))
           (insert "\n\n")

           (when project-state-error
             (aaron-ui-board-insert-section "Project State" nil 'warning)
             (aaron-ui-board-insert-field
              "ignored roots" project-state-error 'aaron-ui-board-bad)
             (when (boundp '+project-ignored-roots-file)
               (insert "   ")
               (aaron-ui-board-insert-openable-path
                +project-ignored-roots-file)
               (insert "\n"))
             (insert "   ")
             (aaron-ui-board-insert-actions
              '((:label "Reload" :command +health/reload-project-state
                 :help "Reload the repaired ignored-project file")))
             (insert "\n\n"))

           ;; --- Batch checks ---
           (aaron-ui-board-insert-section "Batch Checks")
           (+health--insert-check "Startup smoke" startup)
           (+health--insert-check "Byte compile"  compile)
           (+health--insert-check "Native compile" native)
           (insert "\n")

           ;; --- Executables ---
           (aaron-ui-board-insert-section "Executables" (length executables))
           (dolist (entry executables)
             (aaron-ui-board-insert-field
              (format "%s" (car entry))
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
           (let ((running (and (boundp '+noema--process)
                               (processp +noema--process)
                               (process-live-p +noema--process)))
                 (ready (and (boundp '+noema--ready) +noema--ready)))
             (aaron-ui-board-insert-field
              "process" (if running "running" "stopped")
              (if running 'aaron-ui-board-good 'aaron-ui-board-meta))
             (aaron-ui-board-insert-field
              "ready"
              (if ready
                  (format "yes (port %s)"
                          (if (boundp '+noema--port)
                              (number-to-string +noema--port) "?"))
                "no")
              (if ready 'aaron-ui-board-good 'aaron-ui-board-meta)))
           (aaron-ui-board-insert-field
            "runtime"
            (if (and (boundp '+noema--runtime-root)
                     (file-exists-p
                      (expand-file-name "web-host.mjs" +noema--runtime-root)))
                "available" "missing"))
           (aaron-ui-board-insert-field
            "last-index"
            (if (and (boundp '+noema--last-sync-stats)
                     +noema--last-sync-stats)
                +noema--last-sync-stats
              "never"))
           (insert "\n")
           (aaron-ui-board-insert-key-hints
            "Keys: g refresh  s startup  c byte  N native  b compile board  ? menu  q quit")))))
    (unless no-display
      (pop-to-buffer buffer))))

(transient-define-prefix config-health-menu ()
  "Health check workflow."
  [["Checks"
   ("h" "full report" +health/report)
    ("d" "critical doctor" +health/critical-check)
    ("l" "lock audit" aaron/package-lock-audit)
    ("i" "init stats" +health/startup-summary)
    ("s" "startup smoke" +health/startup-check)
    ("c" "byte compile smoke" +health/byte-compile-check)
    ("n" "native compile smoke" +health/native-compile-check)]
   ["Ops"
    ("b" "compile board" +compile/board)]])

(define-key +health-mode-map (kbd "s") #'+health/startup-check)
(define-key +health-mode-map (kbd "c") #'+health/byte-compile-check)
(define-key +health-mode-map (kbd "N") #'+health/native-compile-check)
(define-key +health-mode-map (kbd "b") #'+compile/board)
(define-key +health-mode-map (kbd "?") #'config-health-menu)

;; Public M-x names are discoverable by purpose.  Keep `my/' names for
;; internal callers and existing keybindings.
(defalias 'config-health #'+health/report
  "Open the Emacs configuration health board.")
(aaron-leader!
  "h H" '(:def config-health-menu :which-key "health")
  "h i" '(:def +health/startup-summary :which-key "init stats"))

(provide 'config/health/config)
;;; config/health/config.el ends here
