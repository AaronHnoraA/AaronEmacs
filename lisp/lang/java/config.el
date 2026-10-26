;;; lang/java/config.el --- Java config -*- lexical-binding: t -*-

;;; Commentary:
;; Prefer `lsp-mode' for Java buffers.  `lsp-java' manages the JDT LS
;; integration, including Gradle project import.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tools/lsp/toolchain)
(require 'remote-core)
(require 'remote-channel)
(require 'remote-fs)
(require 'remote-process)
(require 'remote-service)

(declare-function dape-cwd "dape" ())
(declare-function lsp-can-execute-command? "lsp-mode" (command-name))
(declare-function lsp-find-workspace "lsp-mode" (server-id &optional buffer-or-file))
(declare-function lsp-send-execute-command "lsp-mode" (command &optional args))
(declare-function lsp--workspace-buffers "lsp-mode" (workspace))
(declare-function lsp--client-multi-root "lsp-mode" (client))
(declare-function lsp--persist-session "lsp-mode" (session))
(declare-function lsp-session "lsp-mode" ())
(declare-function lsp-session-server-id->folders "lsp-mode" (session))
(declare-function lsp-java--get-root "lsp-java" ())
(declare-function +debug-register-adapter-spec "tools/debugger/config" (name &rest plist))
(declare-function +register-language-server-feature "tools/lsp/config")
(declare-function +lsp/mode-ensure "tools/lsp/config" ())
(declare-function +language-server--set-struct-slot
                  "tools/lsp/config" (object type slot value))
(declare-function +language-server--project-root-for-buffer "tools/lsp/config" ())
(declare-function +language-server-executable-find "tools/lsp/config" (program))
(declare-function +language-server-toolchain-set-local-variable
                  "tools/lsp/toolchain" (variable value))
(defvar dape-configs)
(defvar lsp-managed-mode)
(defvar lsp--cur-workspace)
(defvar lsp-clients)
(defvar lsp-java-bundles)
(defvar lsp-java-server-config-dir)
(defvar lsp-java-workspace-dir)
(defvar lsp-java-jdt-ls-prefer-native-command)
(defvar lsp-java-jdt-ls-command)
(defvar lsp-java-server-install-dir)
(defvar lsp-enabled-clients)
(defvar +debug-after-register-common-configs-hook)
(defvar +java-debug--forwards (make-hash-table :test #'equal)
  "Active Java debug forwards keyed by logical project root.")

(defcustom +lsp-java-target-cache-root
  "~/.cache/emacs-remote/lsp-java/"
  "Target-native cache root for provisioned lsp-java assets."
  :type 'string
  :group '+language-server)

(defcustom +lsp-java-local-bundle-directory
  (expand-file-name
   "var/lsp-java/eclipse.jdt.ls/server/" user-emacs-directory)
  "Local complete JDTLS/lsp-java bundle used to provision trusted targets."
  :type 'directory
  :group '+language-server)

(defcustom +lsp-java-auto-provision-target t
  "Whether Java buffers may provision JDTLS on trusted targets."
  :type 'boolean
  :group '+language-server)

(defun +lsp-java--bundle-version ()
  "Return a stable version ID for the local JDTLS bundle."
  (let* ((plugins
          (expand-file-name "plugins/" +lsp-java-local-bundle-directory))
         (launcher
          (and
           (file-directory-p plugins)
           (car (directory-files
                 plugins nil
                 "\\`org\\.eclipse\\.equinox\\.launcher_.*\\.jar\\'")))))
    (if launcher
        (substring (secure-hash 'sha1 launcher) 0 12)
      (substring
       (secure-hash 'sha1
                    (expand-file-name +lsp-java-local-bundle-directory))
       0 12))))

(defun +lsp-java--context (&optional root)
  "Return the Java target context for ROOT."
  (remote-context (or root default-directory)))

(defun +lsp-java--cache-path (context relative)
  "Return logical target cache path RELATIVE for CONTEXT."
  (remote-expand-file-name
   (concat
    (file-name-as-directory +lsp-java-target-cache-root)
    relative)
   nil context))

(defun +lsp-java--target-install-directory (context)
  "Return the versioned logical JDTLS directory for CONTEXT."
  (file-name-as-directory
   (+lsp-java--cache-path
    context (format "servers/%s/" (+lsp-java--bundle-version)))))

(defun +lsp-java--client-bundle-on-target (context)
  "Return the client JDTLS bundle when CONTEXT shares that filesystem.
Backend placement decides this; the Java consumer does not inspect the target
or transport kind."
  (let ((candidate
         (remote-expand-file-name
          +lsp-java-local-bundle-directory nil context)))
    (when (and
           (remote-client-file-name candidate)
           (file-directory-p candidate))
      (file-name-as-directory candidate))))

(defun +lsp-java--server-directory (context)
  "Return an available logical JDTLS directory for CONTEXT."
  (or
   (+lsp-java--client-bundle-on-target context)
   (let ((directory
          (+lsp-java--target-install-directory context)))
     (unless (+lsp-java--target-install-ready-p directory)
       (+lsp-java--provision-target context directory))
     directory)))

(defun +lsp-java--target-workspace-directory (context root)
  "Return a target-side JDTLS data directory for ROOT."
  (file-name-as-directory
   (+lsp-java--cache-path
    context
    (format
     "workspaces/%s/"
     (substring
      (secure-hash
       'sha1
       (format "%s:%s"
               (remote-context-target-id context)
               (remote-file-local-name root)))
      0 16)))))

(defun +lsp-java--target-install-ready-p (directory)
  "Return non-nil when logical JDTLS DIRECTORY is complete."
  (file-executable-p (expand-file-name "bin/jdtls" directory)))

(defun +lsp-java--ensure-target-python (context)
  "Signal a clear error unless CONTEXT's target has a `python3'.
`bin/jdtls' is `#!/usr/bin/env python3': native-launcher mode silently
fails to start when the target lacks it, and the resulting error surfaces
far from its cause.  Fail here instead, at toolchain-apply time."
  (let ((remote-current-adapter-id "language-server"))
    (unless (remote-executable-find "python3" context)
      (error "JDTLS native launcher requires python3 on target %s"
             (remote-context-target-id context)))))

(defun +lsp-java--provision-target (context install-directory)
  "Provision the complete JDTLS bundle for CONTEXT at INSTALL-DIRECTORY."
  (unless +lsp-java-auto-provision-target
    (error "JDTLS is missing on target and automatic provisioning is disabled"))
  (remote-service-provision-directory
   "lsp-java" +lsp-java-local-bundle-directory install-directory
   :context context
   :adapter "language-server"
   :ready-file "bin/jdtls"
   :ready-kind 'executable
   :prepare
   (lambda (provision-context staging)
     (remote-exec
      "chmod"
      :args
      (list "u+x"
            (remote-file-local-name
             (expand-file-name "bin/jdtls" staging)))
      :context provision-context :adapter "language-server" :check t)))
  (unless (+lsp-java--target-install-ready-p install-directory)
    (error "Target JDTLS provisioning did not produce bin/jdtls")))

(defun +lsp-java--java-profile (root)
  "Discover the target Java toolchain for ROOT."
  (let* ((context (+lsp-java--context root))
         (java
          (let ((remote-current-adapter-id "language-server"))
            (remote-executable-find "java" context))))
    (when java
      (let* ((home-result
              (remote-exec
               "sh"
               :args
               (list
                "-c"
                (concat
                 "java_path=$(readlink -f \"$1\" 2>/dev/null || printf '%s' \"$1\")\n"
                 "dirname \"$(dirname \"$java_path\")\"\n"
                 "uname -s 2>/dev/null || printf 'unknown\\n'\n"
                 "uname -m 2>/dev/null || printf 'unknown\\n'\n")
                "java-home" java)
               :context context :adapter "language-server" :trim t))
             (target-facts
              (and
               (zerop (remote-exec-result-status home-result))
               (split-string
                (remote-exec-result-stdout home-result) "\n" t)))
             (java-home
              (car target-facts))
             (target-system (nth 1 target-facts))
             (target-architecture (nth 2 target-facts))
             (version-result
              (remote-exec
               java :args '("-version")
               :context context :adapter "language-server" :trim t))
             (version-output
              (or (remote-exec-result-stderr version-result)
                  (remote-exec-result-stdout version-result))))
        (list
         (list
          :id (intern
               (format "target-java-%s"
                       (substring
                        (secure-hash
                         'sha1 (format "%s:%s" java java-home))
                        0 8)))
          :label
          (if (string-match
               "version[[:space:]]+\"\\([^\"]+\\)\"" version-output)
              (format "Target Java %s" (match-string 1 version-output))
            "Target Java")
          :default t
          :executable java
          :env (and java-home (list (cons "JAVA_HOME" java-home)))
          :java-home java-home
          :target-system target-system
          :target-architecture target-architecture))))))

(defun +lsp-java--runtime-available-p ()
  "Return whether this buffer's selected Java runtime exists on its target.
The JDTLS launcher may already be installed on the Emacs client, so lsp-mode's
ordinary binary check alone cannot prove that target-side `java' is usable."
  (when-let* ((profile +language-server-toolchain--applied-profile)
              (java (plist-get profile :executable)))
    (and (+language-server-executable-find java) t)))

(defun +lsp-java--target-config-name (profile)
  "Return the JDTLS configuration directory name for target PROFILE."
  (let* ((system
          (downcase (or (plist-get profile :target-system) "")))
         (architecture
          (downcase (or (plist-get profile :target-architecture) "")))
         (arm-p
          (string-match-p
           "\\`\\(?:aarch64\\|arm64\\|armv[0-9]+\\)"
           architecture)))
    (cond
     ((string-match-p "\\`\\(?:darwin\\|mac\\)" system)
      (if arm-p "config_mac_arm" "config_mac"))
     ((string-match-p "\\`\\(?:mingw\\|msys\\|cygwin\\|windows\\)" system)
      "config_win")
     (t
      (if arm-p "config_linux_arm" "config_linux")))))

(defun +lsp-java--target-command-a (function &rest arguments)
  "Project lsp-java's server COMMAND into its target-native namespace.
In native-launcher mode, `lsp-java--ls-command' only emits the JDTLS
binary plus `--jvm-arg' flags: it never passes `-data' or
`-configuration'.  `bin/jdtls' then derives its own workspace directory
from `sha1(basename(cwd))', so two Java projects that merely share a
directory basename collide on one JDTLS workspace and silently reuse a
half-imported one.  Appending the toolchain's own per-root
`lsp-java-workspace-dir'/`lsp-java-server-config-dir'
\(`+lsp-java--apply-toolchain' already computes and sets these
buffer-locally, but native mode ignores them\) restores the workspace
identity the jar launcher would have used."
  (let ((command
         (mapcar
          (lambda (argument)
            (if (stringp argument)
                (remote-file-local-name argument)
              argument))
          (apply function arguments))))
    (if (and (bound-and-true-p lsp-java-jdt-ls-prefer-native-command)
             (bound-and-true-p lsp-java-workspace-dir))
        (append
         command
         (list "-data" (remote-file-local-name lsp-java-workspace-dir))
         (and (bound-and-true-p lsp-java-server-config-dir)
              (list "-configuration"
                    (remote-file-local-name lsp-java-server-config-dir))))
      command)))

(defun +lsp-java--locate-server-command-a (function &rest arguments)
  "Find a target JDTLS launcher through the logical file handler.
`locate-file' can return nil for an executable under `/fs:' even when
`file-executable-p' succeeds.  Use the target installation checked by the
toolchain before letting lsp-java fall back to its jar launcher."
  (let ((candidate
         (and (bound-and-true-p lsp-java-jdt-ls-prefer-native-command)
              (stringp lsp-java-server-install-dir)
              (stringp lsp-java-jdt-ls-command)
              (expand-file-name
               lsp-java-jdt-ls-command
               (expand-file-name "bin/" lsp-java-server-install-dir)))))
    (or (and candidate
             (remote-fs-file-name-p candidate)
             (file-executable-p candidate)
             candidate)
        (apply function arguments))))

(defvar +lsp-java--single-root-enforced nil
  "Canonical root last handled by `+lsp-java--enforce-single-root', or nil.
lsp-mode re-populates `lsp-session-server-id->folders' as JDTLS attaches
each newly visited buffer to an already-live workspace, so this function
previously ran its folder-removal and `lsp--persist-session' on every
single Java buffer visit, not just once per project: it kept racing that
live bookkeeping and re-writing the session file for no reason.  Tracking
only the current root (rather than every root ever seen) keeps the
\"enforce ONE root\" semantics correct when switching between projects:
returning to an earlier root after visiting a different one must still
re-run the wipe, since the folder table now belongs to that other root.")

(defun +lsp-java--enforce-single-root ()
  "Give every Java project one JDTLS workspace and data directory.
Upstream lsp-java marks JDTLS as multi-root and persists all previously opened
Java roots under one server ID.  That couples unrelated Gradle projects,
shares progress/failure state, and defeats the Remote workspace owner model.
Composite builds remain supported because JDTLS discovers their subprojects
inside the one selected root."
  (when (derived-mode-p 'java-mode 'java-ts-mode)
    ;; Java has one explicit workspace owner.  In particular, lsp-mode's
    ;; semgrep client declares itself an add-on for Java and can otherwise be
    ;; started beside JDTLS even when `semgrep' is unavailable on the target.
    ;; Its exit/restart cleanup then races the healthy JDTLS buffer.  This
    ;; buffer-local assignment is cheap and must still run for every buffer.
    ;; `lsp-auto-register-remote-clients' is nil (tools/lsp/config.el), so JDTLS never
    ;; gets cloned into a `jdtls-tramp' alias -- one client id serves every
    ;; target.
    (setq-local lsp-enabled-clients '(jdtls))
    (require 'lsp-java)
    (let ((root (+language-server--project-root-for-buffer)))
      (unless (and root (equal root +lsp-java--single-root-enforced))
        (setq +lsp-java--single-root-enforced root)
        (when-let* ((client (gethash 'jdtls lsp-clients)))
          (+language-server--set-struct-slot
           client 'lsp--client 'multi-root nil))
        (let* ((session (lsp-session))
               (folders (lsp-session-server-id->folders session)))
          (when (gethash 'jdtls folders)
            (remhash 'jdtls folders)
            (lsp--persist-session session)))))))

(defun +lsp-java--apply-toolchain (profile root)
  "Apply Java PROFILE for ROOT to lsp-java."
  (let* ((context (+lsp-java--context root))
         (java (plist-get profile :executable))
         (java-home (plist-get profile :java-home))
         (install-directory
          (+lsp-java--server-directory context)))
    (+language-server-toolchain-set-local-variable
     'lsp-java-java-path java)
    (+language-server-toolchain-set-local-variable
     'lsp-java-import-gradle-java-home java-home)
    (+language-server-toolchain-set-local-variable
     'lsp-java-import-gradle-wrapper-enabled t)
    (+language-server-toolchain-set-local-variable
     'lsp-java-configuration-update-build-configuration "automatic")
    (+language-server-toolchain-set-local-variable
     'lsp-java-project-import-on-first-time-startup "automatic")
    (+language-server-toolchain-set-local-variable
     'lsp-java-format-comments-enabled nil)
    ;; The same native JDTLS launcher contract is used for every target.  A
    ;; backend placement query may expose the existing client bundle to target
    ;; `local'; other targets receive the same bundle through provisioning.
    (let ((jdtls (expand-file-name "bin/jdtls" install-directory)))
      (when (and (file-exists-p jdtls)
                 (not (file-executable-p jdtls)))
        (remote-exec
         "chmod"
         :args (list "u+x" (remote-file-local-name jdtls))
         :context context :adapter "language-server" :check t)))
    (unless (+lsp-java--target-install-ready-p install-directory)
      (error "JDTLS launcher is unavailable in %s" install-directory))
    (+lsp-java--ensure-target-python context)
    (+language-server-toolchain-set-local-variable
     'lsp-java-server-install-dir install-directory)
    ;; Configuration is selected from facts probed on the execution target,
    ;; never from the client Emacs' `system-type'.
    (+language-server-toolchain-set-local-variable
     'lsp-java-server-config-dir
     (expand-file-name
      (+lsp-java--target-config-name profile)
      install-directory))
    (+language-server-toolchain-set-local-variable
     'lsp-java-bundles
     (let ((bundles-directory
            (expand-file-name "bundles/" install-directory)))
       ;; Not every JDTLS distribution ships debug/test-runner bundles;
       ;; a missing directory here must not abort the whole toolchain
       ;; apply and leave the buffer with no language server at all.
       (if (file-directory-p bundles-directory)
           (mapcar
            #'remote-file-local-name
            (seq-remove
             (lambda (file)
               (string-match-p
                "com\\.microsoft\\.java\\.test\\.runner\\.jar\\'"
                file))
             (directory-files bundles-directory t "\\.jar\\'")))
         nil)))
    (+language-server-toolchain-set-local-variable
     'lsp-java-jdt-ls-prefer-native-command t)
    (+language-server-toolchain-set-local-variable
     'lsp-java-workspace-dir
     (+lsp-java--target-workspace-directory context root))
    (+language-server-toolchain-set-local-variable
     'lsp-java-workspace-cache-dir
     (expand-file-name
      ".cache/"
      (+lsp-java--target-workspace-directory context root)))))

(+register-language-server-toolchain-provider
 'java '(java-mode java-ts-mode)
 #'+lsp-java--java-profile
 :apply #'+lsp-java--apply-toolchain
 :label "Java / JDTLS"
 :source (or load-file-name buffer-file-name))

(with-eval-after-load 'lsp-java
  (dolist (server-id '(jdtls jdtls-tramp))
    (when-let* ((client (gethash server-id lsp-clients)))
      (+language-server--set-struct-slot
       client 'lsp--client 'multi-root nil)))
  (unless (advice-member-p
           #'+lsp-java--target-command-a 'lsp-java--ls-command)
    (advice-add
     'lsp-java--ls-command :around #'+lsp-java--target-command-a))
  (when (fboundp 'lsp-java--locate-server-command)
    (unless (advice-member-p
             #'+lsp-java--locate-server-command-a
             'lsp-java--locate-server-command)
      (advice-add 'lsp-java--locate-server-command :around
                  #'+lsp-java--locate-server-command-a))))

(add-hook
 '+language-server-lsp-local-settings-hook
 #'+lsp-java--enforce-single-root)

;; `lsp-java' registers the JDTLS clients itself, so this records the route
;; for the Hub/Doctor and declares the feature that must be loadable before
;; a Java buffer may start a server.
(when (fboundp '+register-language-server-feature)
  (+register-language-server-feature
   '(java-mode java-ts-mode) 'lsp-java
   :label "Eclipse JDT LS (lsp-java)"
   :executables '("java")
   :available-p #'+lsp-java--runtime-available-p
   :note "JDTLS is provisioned onto the workspace target when it is trusted."))

;; `+lsp/mode-ensure' previously ran a second time here, directly on
;; `java-mode-hook'/`java-ts-mode-hook', ahead of the shared
;; `prog-mode-hook' -> `+language-server-ensure-deferred' path that
;; every other language already goes through.  It ran synchronously during
;; mode setup and bypassed the runtime-provider layer
;; (`+language-server-runtime-prepare') entirely, since it never passed
;; through `+language-server/ensure'.  The shared path already routes
;; every `prog-mode' buffer to lsp-mode, so no separate hook is needed.

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :defer t)

(defun +java-debug--workspace ()
  "Return the active `lsp-java' JDTLS workspace."
  (unless (require 'lsp-java nil t)
    (user-error "lsp-java is not available"))
  (or (and (bound-and-true-p lsp-managed-mode)
           (lsp-find-workspace 'jdtls))
      (lsp-find-workspace 'jdtls buffer-file-name)
      (user-error "No active lsp-java JDTLS workspace; run `M-x +language-server/ensure' first")))

(defun +java-debug--with-workspace (fn)
  "Call FN in a buffer attached to the active JDTLS workspace."
  (let* ((workspace (+java-debug--workspace))
         (buffers (ignore-errors (lsp--workspace-buffers workspace)))
         (buffer (or (and (memq (current-buffer) buffers)
                          (current-buffer))
                     (seq-find #'buffer-live-p buffers)
                     (current-buffer))))
    (with-current-buffer buffer
      (let ((lsp--cur-workspace workspace))
        (funcall fn workspace)))))

(defun +java-debug--execute (command &optional args)
  "Execute JDTLS workspace COMMAND with optional ARGS."
  (+java-debug--with-workspace
   (lambda (_workspace)
     (lsp-send-execute-command command args))))

(defun +java-debug--get (object key)
  "Return KEY from OBJECT, accepting plist or hash-table results."
  (cond
   ((hash-table-p object) (gethash (substring (symbol-name key) 1) object))
   ((listp object) (plist-get object key))
   (t nil)))

(defun +java-debug--seq-ref (object index)
  "Return INDEX from OBJECT when OBJECT is a vector or list."
  (cond
   ((vectorp object) (aref object index))
   ((listp object) (nth index object))))

(defun +java-debug--main-class-candidates ()
  "Return JDTLS main-class candidates."
  (let ((result (+java-debug--execute "vscode.java.resolveMainClass")))
    (cond
     ((vectorp result) (append result nil))
     ((listp result) result)
     ((null result) nil)
     (t (list result)))))

(defun +java-debug--select-main-class ()
  "Select the Java main class for the current file or workspace."
  (let* ((candidates (+java-debug--main-class-candidates))
         (current-file (and buffer-file-name (expand-file-name buffer-file-name)))
         (candidate
          (cond
           ((null candidates)
            (user-error "JDTLS did not find a Java main class; wait for import/build to finish"))
           ((= (length candidates) 1)
            (car candidates))
           ((seq-find
             (lambda (it)
               (let ((file (+java-debug--get it :filePath)))
                 (and file current-file
                      (string= (expand-file-name file) current-file))))
             candidates))
           (t
            (let* ((labels
                    (mapcar
                     (lambda (it)
                       (cons (format "%s (%s)"
                                     (or (+java-debug--get it :mainClass) "<unknown>")
                                     (or (+java-debug--get it :projectName) "project"))
                             it))
                     candidates))
                   (label (completing-read "Java main class: "
                                           (mapcar #'car labels)
                                           nil t)))
              (cdr (assoc label labels)))))))
    (unless (and (+java-debug--get candidate :mainClass)
                 (+java-debug--get candidate :projectName))
      (user-error "Bad JDTLS main-class response: %S" candidate))
    candidate))

(defun +java-debug--forward-live-p (forward remote-port)
  "Return non-nil when FORWARD still reaches REMOTE-PORT."
  (and
   (remote-forward-p forward)
   (eq (remote-forward-state forward) 'open)
   (equal
    (plist-get (remote-forward-remote-endpoint forward) :port)
    remote-port)
   (when-let* ((process (remote-forward-handle forward)))
     (process-live-p process))))

(defun +java-debug--access-port (root remote-port)
  "Return a routed client-accessible port for ROOT's target REMOTE-PORT."
  (let ((forward (gethash root +java-debug--forwards)))
    (unless (+java-debug--forward-live-p forward remote-port)
      (when forward
        (ignore-errors (remote-close-channel forward)))
      (setq forward
            (remote-port-forward
             (list :host "127.0.0.1" :port remote-port)
             :context root
             :adapter "network"
             :local-endpoint '(:host "127.0.0.1" :port 0)
             :metadata '(:owner lsp-java-debug)))
      (puthash root forward +java-debug--forwards))
    (plist-get (remote-forward-local-endpoint forward) :port)))

(defun +java-debug--config (config)
  "Populate a Dape Java CONFIG using `lsp-java'."
  (let* ((main (+java-debug--select-main-class))
         (main-class (+java-debug--get main :mainClass))
         (project-name (+java-debug--get main :projectName))
         (classpath (+java-debug--execute
                     "vscode.java.resolveClasspath"
                     (vector main-class project-name)))
         (module-paths (+java-debug--seq-ref classpath 0))
         (class-paths (+java-debug--seq-ref classpath 1))
         (root
          (or
           (ignore-errors
             (+java-debug--with-workspace
              (lambda (_workspace)
                (lsp-java--get-root))))
           (dape-cwd)))
         (remote-port
          (+java-debug--execute "vscode.java.startDebugSession"))
         (port
          (and
           (integerp remote-port)
           (> remote-port 0)
           (+java-debug--access-port root remote-port)))
         (config (copy-tree config)))
    (unless (and (integerp port) (> port 0))
      (user-error "JDTLS did not return a debug server port: %S" port))
    (unless class-paths
      (user-error "JDTLS could not resolve classpath for %s" main-class))
    (setq config (plist-put config 'port port))
    (setq config (plist-put config 'host "localhost"))
    (setq config (plist-put config :mainClass main-class))
    (setq config (plist-put config :projectName project-name))
    (setq config (plist-put config :modulePaths (or module-paths [])))
    (setq config (plist-put config :classPaths class-paths))
    (setq config
          (plist-put config :cwd
                     (remote-file-local-name root)))
    (plist-put config :name (format "Java: %s (%s)" main-class project-name))))

(defun +java-debug--ensure (config)
  "Ensure lsp-java can supply CONFIG's DAP server."
  (ignore config)
  (+java-debug--with-workspace
   (lambda (_workspace)
     (unless (and (lsp-can-execute-command? "vscode.java.startDebugSession")
                  (lsp-can-execute-command? "vscode.java.resolveClasspath"))
       (user-error "JDTLS is running but java-debug bundle is not active; restart lsp-java")))))

(defun +java-debug-register-dape-configs ()
  "Register Java Dape configs backed by `lsp-java'."
  (when (fboundp '+debug-register-adapter-spec)
    (+debug-register-adapter-spec
     'java
     :title "Java"
     :configs '(lsp-java-main java-main jdtls)
     :commands '("java")
     :install "Use lsp-java/JDTLS with vscode-java-debug support."))
  (when (boundp 'dape-configs)
    (setf (alist-get 'lsp-java-main dape-configs nil nil #'eq)
          '(modes (java-mode java-ts-mode)
            ensure +java-debug--ensure
            fn +java-debug--config
            :type "java"
            :request "launch"
            :args ""
            :stopOnEntry nil
            :console "integratedConsole"
            :internalConsoleOptions "neverOpen"
            :vmArgs " -XX:+ShowCodeDetailsInExceptionMessages"))
    (setf (alist-get 'java-main dape-configs nil nil #'eq)
          (copy-tree (alist-get 'lsp-java-main dape-configs nil nil #'eq)))))

(add-hook '+debug-after-register-common-configs-hook
          #'+java-debug-register-dape-configs)

(with-eval-after-load 'dape
  (when (featurep 'tools/debugger/config)
    (+java-debug-register-dape-configs)))

(provide 'lang/java/config)
;;; lang/java/config.el ends here
