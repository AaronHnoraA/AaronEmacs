;;; init-java.el --- Java config -*- lexical-binding: t -*-

;;; Commentary:
;; Prefer `lsp-mode' for Java buffers.  `lsp-java' manages the JDT LS
;; integration, including Gradle project import.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'init-lsp-toolchain)
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
(declare-function my/debug-register-adapter-spec "init-debug" (name &rest plist))
(declare-function my/register-language-server-feature "init-lsp")
(declare-function my/lsp-mode-ensure "init-lsp" ())
(declare-function my/language-server--set-struct-slot
                  "init-lsp" (object type slot value))
(declare-function my/language-server--project-root-for-buffer "init-lsp" ())
(declare-function my/language-server-toolchain-set-local-variable
                  "init-lsp-toolchain" (variable value))
(defvar dape-configs)
(defvar lsp-managed-mode)
(defvar lsp--cur-workspace)
(defvar lsp-clients)
(defvar lsp-java-bundles)
(defvar lsp-java-server-config-dir)
(defvar lsp-java-workspace-dir)
(defvar lsp-java-jdt-ls-prefer-native-command)
(defvar lsp-enabled-clients)
(defvar my/debug-after-register-common-configs-hook)
(defvar my/java-debug--forwards (make-hash-table :test #'equal)
  "Active Java debug forwards keyed by logical project root.")

(defcustom my/lsp-java-target-cache-root
  "~/.cache/emacs-remote/lsp-java/"
  "Target-native cache root for provisioned lsp-java assets."
  :type 'string
  :group 'my/language-server)

(defcustom my/lsp-java-local-bundle-directory
  (expand-file-name
   "var/lsp-java/eclipse.jdt.ls/server/" user-emacs-directory)
  "Local complete JDTLS/lsp-java bundle used to provision trusted targets."
  :type 'directory
  :group 'my/language-server)

(defcustom my/lsp-java-auto-provision-target t
  "Whether Java buffers may provision JDTLS on trusted targets."
  :type 'boolean
  :group 'my/language-server)

(defun my/lsp-java--bundle-version ()
  "Return a stable version ID for the local JDTLS bundle."
  (let* ((plugins
          (expand-file-name "plugins/" my/lsp-java-local-bundle-directory))
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
                    (expand-file-name my/lsp-java-local-bundle-directory))
       0 12))))

(defun my/lsp-java--context (&optional root)
  "Return the Java target context for ROOT."
  (remote-context (or root default-directory)))

(defun my/lsp-java--cache-path (context relative)
  "Return logical target cache path RELATIVE for CONTEXT."
  (remote-expand-file-name
   (concat
    (file-name-as-directory my/lsp-java-target-cache-root)
    relative)
   nil context))

(defun my/lsp-java--target-install-directory (context)
  "Return the versioned logical JDTLS directory for CONTEXT."
  (file-name-as-directory
   (my/lsp-java--cache-path
    context (format "servers/%s/" (my/lsp-java--bundle-version)))))

(defun my/lsp-java--client-bundle-on-target (context)
  "Return the client JDTLS bundle when CONTEXT shares that filesystem.
Backend placement decides this; the Java consumer does not inspect the target
or transport kind."
  (let ((candidate
         (remote-expand-file-name
          my/lsp-java-local-bundle-directory nil context)))
    (when (and
           (remote-client-file-name candidate)
           (file-directory-p candidate))
      (file-name-as-directory candidate))))

(defun my/lsp-java--server-directory (context)
  "Return an available logical JDTLS directory for CONTEXT."
  (or
   (my/lsp-java--client-bundle-on-target context)
   (let ((directory
          (my/lsp-java--target-install-directory context)))
     (unless (my/lsp-java--target-install-ready-p directory)
       (my/lsp-java--provision-target context directory))
     directory)))

(defun my/lsp-java--target-workspace-directory (context root)
  "Return a target-side JDTLS data directory for ROOT."
  (file-name-as-directory
   (my/lsp-java--cache-path
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

(defun my/lsp-java--target-trusted-p (context)
  "Return non-nil when CONTEXT permits target-side provisioning."
  (when-let* ((target
               (remote-get-target
                (remote-context-target-id context))))
    (remote-target-trusted target)))

(defun my/lsp-java--target-install-ready-p (directory)
  "Return non-nil when logical JDTLS DIRECTORY is complete."
  (file-executable-p (expand-file-name "bin/jdtls" directory)))

(defun my/lsp-java--ensure-target-python (context)
  "Signal a clear error unless CONTEXT's target has a `python3'.
`bin/jdtls' is `#!/usr/bin/env python3': native-launcher mode silently
fails to start when the target lacks it, and the resulting error surfaces
far from its cause.  Fail here instead, at toolchain-apply time."
  (let ((remote-current-adapter-id "language-server"))
    (unless (remote-executable-find "python3" context)
      (error "JDTLS native launcher requires python3 on target %s"
             (remote-context-target-id context)))))

(defun my/lsp-java--archive-local-bundle ()
  "Create and return a temporary archive of the local lsp-java bundle."
  (unless (file-directory-p my/lsp-java-local-bundle-directory)
    (error "Local lsp-java bundle is missing: %s"
           my/lsp-java-local-bundle-directory))
  (let ((archive (make-temp-file "emacs-lsp-java-" nil ".tar.gz")))
    (unless
        (zerop
         (call-process
          "tar" nil nil nil "-czf" archive
          "-C" my/lsp-java-local-bundle-directory "."))
      (delete-file archive)
      (error "Could not package local lsp-java bundle"))
    archive))

(defun my/lsp-java--provision-target (context install-directory)
  "Provision the complete JDTLS bundle for CONTEXT at INSTALL-DIRECTORY."
  (unless my/lsp-java-auto-provision-target
    (error "JDTLS is missing on target and automatic provisioning is disabled"))
  (unless (my/lsp-java--target-trusted-p context)
    (signal
     'remote-service-untrusted
     (list "lsp-java" (remote-context-target-id context))))
  (let* ((parent (file-name-directory
                  (directory-file-name install-directory)))
         (archive (my/lsp-java--archive-local-bundle))
         (remote-archive
          (expand-file-name
           (format ".lsp-java-%s.tar.gz" (my/lsp-java--bundle-version))
           parent))
         (native-archive (remote-file-local-name remote-archive))
         (native-install
          (remote-file-local-name
           (directory-file-name install-directory)))
         (native-parent
          (remote-file-local-name
           (directory-file-name parent))))
    (unwind-protect
        (progn
          (remote-exec
           "mkdir" :args (list "-p" native-parent)
           :context context :adapter "language-server" :check t)
          ;; Cross-target copy remains an ordinary Emacs file operation.  The
          ;; `/fs:' handler selects native/TRAMP/RPC mechanics without exposing
          ;; a backend ID to this language consumer.
          (let ((large-file-warning-threshold nil))
            (copy-file archive remote-archive t))
          (remote-exec
           "sh"
           :args
           (list
            "-c"
            (concat
             "set -eu\n"
             "archive=$1\n"
             "install=$2\n"
             "staging=${install}.tmp.$$\n"
             "if test -x \"$install/bin/jdtls\"; then exit 0; fi\n"
             "rm -rf \"$staging\"\n"
             "mkdir -p \"$staging\"\n"
             "tar -xzf \"$archive\" -C \"$staging\"\n"
             "chmod u+x \"$staging/bin/jdtls\"\n"
             "if test -e \"$install\"; then\n"
             "  rm -rf \"$staging\"\n"
             "else\n"
             "  mv \"$staging\" \"$install\"\n"
             "fi\n")
            "lsp-java-provision" native-archive native-install)
           :context context :adapter "language-server" :check t))
      (when (file-exists-p archive)
        (delete-file archive))
      (when (file-exists-p remote-archive)
        (delete-file remote-archive))))
  (unless (my/lsp-java--target-install-ready-p install-directory)
    (error "Target JDTLS provisioning did not produce bin/jdtls")))

(defun my/lsp-java--java-profile (root)
  "Discover the target Java toolchain for ROOT."
  (let* ((context (my/lsp-java--context root))
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

(defun my/lsp-java--target-config-name (profile)
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

(defun my/lsp-java--target-command-a (function &rest arguments)
  "Project lsp-java's server COMMAND into its target-native namespace.
In native-launcher mode, `lsp-java--ls-command' only emits the JDTLS
binary plus `--jvm-arg' flags: it never passes `-data' or
`-configuration'.  `bin/jdtls' then derives its own workspace directory
from `sha1(basename(cwd))', so two Java projects that merely share a
directory basename collide on one JDTLS workspace and silently reuse a
half-imported one.  Appending the toolchain's own per-root
`lsp-java-workspace-dir'/`lsp-java-server-config-dir'
\(`my/lsp-java--apply-toolchain' already computes and sets these
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

(defvar my/lsp-java--single-root-enforced nil
  "Canonical root last handled by `my/lsp-java--enforce-single-root', or nil.
lsp-mode re-populates `lsp-session-server-id->folders' as JDTLS attaches
each newly visited buffer to an already-live workspace, so this function
previously ran its folder-removal and `lsp--persist-session' on every
single Java buffer visit, not just once per project: it kept racing that
live bookkeeping and re-writing the session file for no reason.  Tracking
only the current root (rather than every root ever seen) keeps the
\"enforce ONE root\" semantics correct when switching between projects:
returning to an earlier root after visiting a different one must still
re-run the wipe, since the folder table now belongs to that other root.")

(defun my/lsp-java--enforce-single-root ()
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
    ;; `lsp-auto-register-remote-clients' is nil (init-lsp.el), so JDTLS never
    ;; gets cloned into a `jdtls-tramp' alias -- one client id serves every
    ;; target.
    (setq-local lsp-enabled-clients '(jdtls))
    (require 'lsp-java)
    (let ((root (my/language-server--project-root-for-buffer)))
      (unless (and root (equal root my/lsp-java--single-root-enforced))
        (setq my/lsp-java--single-root-enforced root)
        (when-let* ((client (gethash 'jdtls lsp-clients)))
          (my/language-server--set-struct-slot
           client 'lsp--client 'multi-root nil))
        (let* ((session (lsp-session))
               (folders (lsp-session-server-id->folders session)))
          (when (gethash 'jdtls folders)
            (remhash 'jdtls folders)
            (lsp--persist-session session)))))))

(defun my/lsp-java--apply-toolchain (profile root)
  "Apply Java PROFILE for ROOT to lsp-java."
  (let* ((context (my/lsp-java--context root))
         (java (plist-get profile :executable))
         (java-home (plist-get profile :java-home))
         (install-directory
          (my/lsp-java--server-directory context)))
    (my/language-server-toolchain-set-local-variable
     'lsp-java-java-path java)
    (my/language-server-toolchain-set-local-variable
     'lsp-java-import-gradle-java-home java-home)
    (my/language-server-toolchain-set-local-variable
     'lsp-java-import-gradle-wrapper-enabled t)
    (my/language-server-toolchain-set-local-variable
     'lsp-java-configuration-update-build-configuration "automatic")
    (my/language-server-toolchain-set-local-variable
     'lsp-java-project-import-on-first-time-startup "automatic")
    (my/language-server-toolchain-set-local-variable
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
    (unless (my/lsp-java--target-install-ready-p install-directory)
      (error "JDTLS launcher is unavailable in %s" install-directory))
    (my/lsp-java--ensure-target-python context)
    (my/language-server-toolchain-set-local-variable
     'lsp-java-server-install-dir install-directory)
    ;; Configuration is selected from facts probed on the execution target,
    ;; never from the client Emacs' `system-type'.
    (my/language-server-toolchain-set-local-variable
     'lsp-java-server-config-dir
     (expand-file-name
      (my/lsp-java--target-config-name profile)
      install-directory))
    (my/language-server-toolchain-set-local-variable
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
    (my/language-server-toolchain-set-local-variable
     'lsp-java-jdt-ls-prefer-native-command t)
    (my/language-server-toolchain-set-local-variable
     'lsp-java-workspace-dir
     (my/lsp-java--target-workspace-directory context root))
    (my/language-server-toolchain-set-local-variable
     'lsp-java-workspace-cache-dir
     (expand-file-name
      ".cache/"
      (my/lsp-java--target-workspace-directory context root)))))

(my/register-language-server-toolchain-provider
 'java '(java-mode java-ts-mode)
 #'my/lsp-java--java-profile
 :apply #'my/lsp-java--apply-toolchain
 :label "Java / JDTLS"
 :source (or load-file-name buffer-file-name))

(with-eval-after-load 'lsp-java
  (dolist (server-id '(jdtls jdtls-tramp))
    (when-let* ((client (gethash server-id lsp-clients)))
      (my/language-server--set-struct-slot
       client 'lsp--client 'multi-root nil)))
  (unless (advice-member-p
           #'my/lsp-java--target-command-a 'lsp-java--ls-command)
    (advice-add
     'lsp-java--ls-command :around #'my/lsp-java--target-command-a)))

(add-hook
 'my/language-server-lsp-local-settings-hook
 #'my/lsp-java--enforce-single-root)

;; `lsp-java' registers the JDTLS clients itself, so this records the route
;; for the Hub/Doctor and declares the feature that must be loadable before
;; a Java buffer may start a server.
(when (fboundp 'my/register-language-server-feature)
  (my/register-language-server-feature
   '(java-mode java-ts-mode) 'lsp-java
   :label "Eclipse JDT LS (lsp-java)"
   :executables '("java")
   :note "JDTLS is provisioned onto the workspace target when it is trusted."))

;; `my/lsp-mode-ensure' previously ran a second time here, directly on
;; `java-mode-hook'/`java-ts-mode-hook', ahead of the shared
;; `prog-mode-hook' -> `my/language-server-ensure-deferred' path that
;; every other language already goes through.  It ran synchronously during
;; mode setup and bypassed the runtime-provider layer
;; (`my/language-server-runtime-prepare') entirely, since it never passed
;; through `my/language-server-ensure'.  The shared path already routes
;; every `prog-mode' buffer to lsp-mode, so no separate hook is needed.

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :defer t)

(defun my/java-debug--workspace ()
  "Return the active `lsp-java' JDTLS workspace."
  (unless (require 'lsp-java nil t)
    (user-error "lsp-java is not available"))
  (or (and (bound-and-true-p lsp-managed-mode)
           (lsp-find-workspace 'jdtls))
      (lsp-find-workspace 'jdtls buffer-file-name)
      (user-error "No active lsp-java JDTLS workspace; run `M-x my/language-server-ensure' first")))

(defun my/java-debug--with-workspace (fn)
  "Call FN in a buffer attached to the active JDTLS workspace."
  (let* ((workspace (my/java-debug--workspace))
         (buffers (ignore-errors (lsp--workspace-buffers workspace)))
         (buffer (or (and (memq (current-buffer) buffers)
                          (current-buffer))
                     (seq-find #'buffer-live-p buffers)
                     (current-buffer))))
    (with-current-buffer buffer
      (let ((lsp--cur-workspace workspace))
        (funcall fn workspace)))))

(defun my/java-debug--execute (command &optional args)
  "Execute JDTLS workspace COMMAND with optional ARGS."
  (my/java-debug--with-workspace
   (lambda (_workspace)
     (lsp-send-execute-command command args))))

(defun my/java-debug--get (object key)
  "Return KEY from OBJECT, accepting plist or hash-table results."
  (cond
   ((hash-table-p object) (gethash (substring (symbol-name key) 1) object))
   ((listp object) (plist-get object key))
   (t nil)))

(defun my/java-debug--seq-ref (object index)
  "Return INDEX from OBJECT when OBJECT is a vector or list."
  (cond
   ((vectorp object) (aref object index))
   ((listp object) (nth index object))))

(defun my/java-debug--main-class-candidates ()
  "Return JDTLS main-class candidates."
  (let ((result (my/java-debug--execute "vscode.java.resolveMainClass")))
    (cond
     ((vectorp result) (append result nil))
     ((listp result) result)
     ((null result) nil)
     (t (list result)))))

(defun my/java-debug--select-main-class ()
  "Select the Java main class for the current file or workspace."
  (let* ((candidates (my/java-debug--main-class-candidates))
         (current-file (and buffer-file-name (expand-file-name buffer-file-name)))
         (candidate
          (cond
           ((null candidates)
            (user-error "JDTLS did not find a Java main class; wait for import/build to finish"))
           ((= (length candidates) 1)
            (car candidates))
           ((seq-find
             (lambda (it)
               (let ((file (my/java-debug--get it :filePath)))
                 (and file current-file
                      (string= (expand-file-name file) current-file))))
             candidates))
           (t
            (let* ((labels
                    (mapcar
                     (lambda (it)
                       (cons (format "%s (%s)"
                                     (or (my/java-debug--get it :mainClass) "<unknown>")
                                     (or (my/java-debug--get it :projectName) "project"))
                             it))
                     candidates))
                   (label (completing-read "Java main class: "
                                           (mapcar #'car labels)
                                           nil t)))
              (cdr (assoc label labels)))))))
    (unless (and (my/java-debug--get candidate :mainClass)
                 (my/java-debug--get candidate :projectName))
      (user-error "Bad JDTLS main-class response: %S" candidate))
    candidate))

(defun my/java-debug--forward-live-p (forward remote-port)
  "Return non-nil when FORWARD still reaches REMOTE-PORT."
  (and
   (remote-forward-p forward)
   (eq (remote-forward-state forward) 'open)
   (equal
    (plist-get (remote-forward-remote-endpoint forward) :port)
    remote-port)
   (when-let* ((process (remote-forward-handle forward)))
     (process-live-p process))))

(defun my/java-debug--access-port (root remote-port)
  "Return a routed client-accessible port for ROOT's target REMOTE-PORT."
  (let ((forward (gethash root my/java-debug--forwards)))
    (unless (my/java-debug--forward-live-p forward remote-port)
      (when forward
        (ignore-errors (remote-close-channel forward)))
      (setq forward
            (remote-port-forward
             (list :host "127.0.0.1" :port remote-port)
             :context root
             :adapter "network"
             :local-endpoint '(:host "127.0.0.1" :port 0)
             :metadata '(:owner lsp-java-debug)))
      (puthash root forward my/java-debug--forwards))
    (plist-get (remote-forward-local-endpoint forward) :port)))

(defun my/java-debug--config (config)
  "Populate a Dape Java CONFIG using `lsp-java'."
  (let* ((main (my/java-debug--select-main-class))
         (main-class (my/java-debug--get main :mainClass))
         (project-name (my/java-debug--get main :projectName))
         (classpath (my/java-debug--execute
                     "vscode.java.resolveClasspath"
                     (vector main-class project-name)))
         (module-paths (my/java-debug--seq-ref classpath 0))
         (class-paths (my/java-debug--seq-ref classpath 1))
         (root
          (or
           (ignore-errors
             (my/java-debug--with-workspace
              (lambda (_workspace)
                (lsp-java--get-root))))
           (dape-cwd)))
         (remote-port
          (my/java-debug--execute "vscode.java.startDebugSession"))
         (port
          (and
           (integerp remote-port)
           (> remote-port 0)
           (my/java-debug--access-port root remote-port)))
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

(defun my/java-debug--ensure (config)
  "Ensure lsp-java can supply CONFIG's DAP server."
  (ignore config)
  (my/java-debug--with-workspace
   (lambda (_workspace)
     (unless (and (lsp-can-execute-command? "vscode.java.startDebugSession")
                  (lsp-can-execute-command? "vscode.java.resolveClasspath"))
       (user-error "JDTLS is running but java-debug bundle is not active; restart lsp-java")))))

(defun my/java-debug-register-dape-configs ()
  "Register Java Dape configs backed by `lsp-java'."
  (when (fboundp 'my/debug-register-adapter-spec)
    (my/debug-register-adapter-spec
     'java
     :title "Java"
     :configs '(lsp-java-main java-main jdtls)
     :commands '("java")
     :install "Use lsp-java/JDTLS with vscode-java-debug support."))
  (when (boundp 'dape-configs)
    (setf (alist-get 'lsp-java-main dape-configs nil nil #'eq)
          '(modes (java-mode java-ts-mode)
            ensure my/java-debug--ensure
            fn my/java-debug--config
            :type "java"
            :request "launch"
            :args ""
            :stopOnEntry nil
            :console "integratedConsole"
            :internalConsoleOptions "neverOpen"
            :vmArgs " -XX:+ShowCodeDetailsInExceptionMessages"))
    (setf (alist-get 'java-main dape-configs nil nil #'eq)
          (copy-tree (alist-get 'lsp-java-main dape-configs nil nil #'eq)))))

(add-hook 'my/debug-after-register-common-configs-hook
          #'my/java-debug-register-dape-configs)

(with-eval-after-load 'dape
  (when (featurep 'init-debug)
    (my/java-debug-register-dape-configs)))

(provide 'init-java)
;;; init-java.el ends here
