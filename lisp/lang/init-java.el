;;; init-java.el --- Java config -*- lexical-binding: t -*-

;;; Commentary:
;; Prefer `lsp-mode' for Java buffers.  `lsp-java' manages the JDT LS
;; integration, including Gradle project import.

;;; Code:

(require 'seq)

(declare-function dape-cwd "dape" ())
(declare-function lsp-can-execute-command? "lsp-mode" (command-name))
(declare-function lsp-find-workspace "lsp-mode" (server-id &optional buffer-or-file))
(declare-function lsp-send-execute-command "lsp-mode" (command &optional args))
(declare-function lsp--workspace-buffers "lsp-mode" (workspace))
(declare-function lsp-java--get-root "lsp-java" ())
(declare-function my/debug-register-adapter-spec "init-debug" (name &rest plist))
(declare-function my/register-lsp-mode-preference "init-lsp" (mode &optional feature source note))
(declare-function my/lsp-mode-ensure "init-lsp" ())

(defvar dape-configs)
(defvar lsp-managed-mode)
(defvar lsp--cur-workspace)
(defvar my/debug-after-register-common-configs-hook)

(when (fboundp 'my/register-lsp-mode-preference)
  (my/register-lsp-mode-preference 'java-mode 'lsp-java)
  (my/register-lsp-mode-preference 'java-ts-mode 'lsp-java))

(add-hook 'java-mode-hook #'my/lsp-mode-ensure)
(add-hook 'java-ts-mode-hook #'my/lsp-mode-ensure)

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
         (port (my/java-debug--execute "vscode.java.startDebugSession"))
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
                     (or (ignore-errors
                           (my/java-debug--with-workspace
                            (lambda (_workspace)
                              (lsp-java--get-root))))
                         (dape-cwd))))
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
