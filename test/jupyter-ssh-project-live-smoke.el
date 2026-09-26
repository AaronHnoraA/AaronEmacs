;;; jupyter-ssh-project-live-smoke.el --- Real SSH notebook project smoke -*- lexical-binding: t; -*-
;; Opt-in: JUPYTER_SSH_TARGET=aaron-pc JUPYTER_SSH_ROOT=/home/.../COMP9444
;; The probe creates one temporary notebook in ROOT and removes it on exit.

(require 'init-aaronnote)
(require 'init-aaronnote-jupyter-notebook)
(require 'init-aaronnote-jupyter-runtime)
(require 'init-aaronnote-jupyter-lsp)
(require 'init-lsp)
(require 'init-project)
(load (expand-file-name "test/lsp-live-smoke.el" user-emacs-directory) nil t)
(require 'remote-config)
(require 'remote-framework)

(defun my/jupyter-ssh-live--client (connection code)
  "Execute CODE through CONNECTION using the existing real-kernel client."
  (let* ((buffer (generate-new-buffer " *jupyter-ssh-live-client*"))
         (default-directory user-emacs-directory)
         (process-environment (remote-client-process-environment))
         (process
          (make-process
           :name "jupyter-ssh-live-client" :buffer buffer :connection-type 'pipe
           :command
           (list (or (getenv "JUPYTER_SSH_NODE") (executable-find "node"))
                 (expand-file-name "test/jupyter-remote-live-client.mjs"
                                   user-emacs-directory))
           :noquery t))
         (deadline (+ (float-time) 75)))
    (unwind-protect
        (progn
          (process-send-string
           process
           (json-serialize
            `((mode . "raw") (label . "SSH project notebook")
              (connectionInfo . ,connection) (code . ,code) (expected . "42"))))
          (process-send-eof process)
          (while (and (process-live-p process) (< (float-time) deadline))
            (accept-process-output nil 0.05))
          (unless (and (not (process-live-p process))
                       (zerop (process-exit-status process)))
            (error "SSH Jupyter execution failed: %s"
                   (with-current-buffer buffer (buffer-string))))
          (princ (with-current-buffer buffer (buffer-string))))
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(defun my/jupyter-ssh-project-live-run ()
  "Exercise remote notebook editing and a project-owned raw kernel."
  (let* ((ssh (or (getenv "JUPYTER_SSH_SSH") "/usr/bin/ssh"))
         (exec-path (cons (file-name-directory ssh) exec-path))
         (remote--client-exec-path exec-path)
         (process-environment (copy-sequence process-environment))
         (remote--client-process-environment process-environment)
         (host-sandbox (make-temp-file "noema-ssh-host-" t))
         (my/noema--state-root (expand-file-name "state" host-sandbox))
         (my/noema--tmp-root (expand-file-name "tmp" host-sandbox))
         (my/noema-web-port 0))
    (setenv "PATH" (concat (file-name-directory ssh) ":" (getenv "PATH")))
    (setenv "NOEMA_ROOT" (expand-file-name "notes" host-sandbox))
    (remote-config-load)
    (remote-fs-install)
    (let* ((target (or (getenv "JUPYTER_SSH_TARGET") "aaron-pc"))
           (native-root (file-name-as-directory
                         (or (getenv "JUPYTER_SSH_ROOT")
                             "/home/aaron/Desktop/UNSW/COMP9444")))
           (root (remote-make-file-name target native-root))
           (python (concat native-root ".conda/bin/python"))
           (file (expand-file-name
                  (format ".noema-ssh-smoke-%s.ipynb"
                          (substring (secure-hash 'sha256
                                                  (format "%s-%s" (float-time) (emacs-pid)))
                                     0 12))
                  root))
           (source (concat (file-name-sans-extension file) ".py"))
           (moved (concat (file-name-sans-extension file) "-moved.py"))
           workspace notebook directory-buffer runtime)
      (unless (file-directory-p root) (error "SSH project is unavailable: %s" root))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'my/project-switch)
                       (lambda (selected &optional _arg)
                         (unless (equal selected root)
                           (error "SSH entry switched to another project: %s" selected)))))
              (my/jupyter-ssh-open-project
               (list :name "COMP9444" :target target :root native-root)))
            (setq workspace (remote-workspace-for-path root))
            (unless (and workspace (remote-workspace-environment workspace))
              (error "SSH project entry did not load its environment"))
            (with-temp-file file
              (insert
               "{\"cells\":[{\"cell_type\":\"code\",\"execution_count\":null,"
               "\"id\":\"ssh-smoke\",\"metadata\":{},\"outputs\":[],"
               "\"source\":[\"import numpy as np\\n\",\"import torch\\n\",\"import sys\\n\",\"array = np.array([1, 2])\\n\",\"print('SSH_SOURCE_BEFORE')\\n\",\"print(sys.executable)\\n\",\"print(torch.__version__)\\n\",\"print(41 + 1)\\n\"]}],"
               "\"metadata\":{\"kernelspec\":{\"display_name\":\"Python 3\","
               "\"language\":\"python\",\"name\":\"python3\"},"
               "\"language_info\":{\"name\":\"python\"}},"
               "\"nbformat\":4,\"nbformat_minor\":5}\n"))
            (setq notebook (find-file-noselect file))
            (with-current-buffer notebook
              (unless (bound-and-true-p my/noema-jupyter-notebook--projection-p)
                (error "Remote ipynb did not open as a notebook projection"))
              (goto-char (point-min))
              (search-forward "SSH_SOURCE_BEFORE")
              (replace-match "SSH_SOURCE_AFTER")
              (save-buffer))
            (with-temp-buffer
              (insert-file-contents file)
              (unless (string-match-p "SSH_SOURCE_AFTER" (buffer-string))
                (error "Remote notebook save was not persisted")))
            (princ "PASS remote ipynb visit, projection, edit and save\n")
            (with-temp-file source (insert "value = 42\n"))
            (rename-file source moved)
            (unless (member (file-name-nondirectory moved)
                            (directory-files root))
              (error "Remote project directory did not list the renamed source"))
            (setq directory-buffer (dired-noselect root))
            (unless (with-current-buffer directory-buffer
                      (derived-mode-p 'dired-mode))
              (error "Remote project did not open in Dired"))
            (delete-file moved)
            (princ "PASS SSH project Dired, create, list, rename and delete\n")
            (let ((deadline (+ (float-time) 30)))
              (while (and (not my/noema--ready) (< (float-time) deadline))
                (accept-process-output nil 0.1))
              (unless my/noema--ready
                (error "Isolated Noema web-host did not start: %s"
                       (my/noema--web-host-log-tail 16))))
            (princ "PASS isolated Noema web-host serves the SSH notebook\n")
            (with-current-buffer notebook
              (my/noema-jupyter-cell-select-kernel nil "python3")
              (princ "PASS Noema selected the target kernelspec\n")
              (unless (my/noema-jupyter-cell--api-sync
                       "aaronnote:api:jupyter:script-action"
                       (append (my/noema-jupyter-cell--document-detail)
                               '((action . "run-all"))) 60)
                (error "Noema did not execute the SSH notebook")))
            (with-temp-buffer
              (insert-file-contents file)
              (let* ((document (json-parse-buffer))
                     (cell (aref (gethash "cells" document) 0))
                     (outputs (gethash "outputs" cell)))
                (unless (and (> (length outputs) 0)
                             (string-match-p "42" (format "%S" outputs))
                             (string-match-p (regexp-quote python)
                                             (format "%S" outputs)))
                  (error "Noema did not save the remote execution output: %S"
                         outputs))))
            (princ "PASS Noema executes PyTorch in the project kernel\n")
            (switch-to-buffer notebook)
            (with-current-buffer notebook
              (setq-local my/language-server--manual-start t
                          lsp-auto-guess-root t
                          lsp-guess-root-without-session t)
              (my/noema-jupyter-project-lsp)
              (when (and (bound-and-true-p lsp--buffer-deferred)
                         (fboundp 'lsp--init-if-visible))
                (lsp--init-if-visible))
              (unless (my/lsp-live-smoke--wait 45)
                (error "Remote notebook source LSP did not connect: env-wait=%S root=%S"
                       my/lsp-mode--waiting-for-direnv
                       (my/language-server--project-root-for-buffer)))
              (unless (equal (my/language-server--project-root-for-buffer) root)
                (error "LSP selected another project root"))
              (let ((profile (my/language-server-current-toolchain-profile)))
                (unless (and profile
                             (string-prefix-p (concat native-root ".conda/bin/")
                                              (plist-get profile :executable)))
                  (error "LSP did not use the project env Python: %S" profile)))
              (goto-char (point-min))
              (search-forward "np.array")
              (backward-char 2)
              (unless (string-match-p
                       "array"
                       (format "%S" (lsp-request "textDocument/hover"
                                                 (lsp--text-document-position-params))))
                (error "Remote notebook LSP did not resolve numpy hover")))
            (princ "PASS target Pyright analyzes the file-owned remote environment\n")
            (let* ((context (my/noema-jupyter--context file))
                   (command (my/jupyter-target-command context))
                   (spec (seq-find
                          (lambda (item)
                            (equal (my/noema-jupyter--get 'name item) "python3"))
                          (my/noema-jupyter--kernelspecs file)))
                   (argv (and spec
                              (append (my/noema-jupyter--get
                                       'argv (my/noema-jupyter--get 'spec spec)) nil))))
              (unless (equal (remote-context-workspace-root context) root)
                (error "Notebook lost project root: %s"
                       (remote-context-workspace-root context)))
              (unless (equal (car command) (concat native-root ".conda/bin/jupyter"))
                (error "Wrong Jupyter environment: %S" command))
              (unless (equal (car argv) python)
                (error "Wrong kernel interpreter: %S" argv)))
            (princ "PASS nested Remote context and project Python discovery\n")
            (setq runtime
                  (my/noema-jupyter--launch-runtime
                   `((kernelId . "ssh-project-smoke")
                     (sourceFile . ,file) (kernelName . "python3"))))
            (my/jupyter-ssh-live--client
             (my/noema-jupyter-runtime-client-connection runtime)
             (format
              "import os,sys,torch; assert os.getcwd() == %S; assert sys.executable == %S; noema_audit_state = 41; print(noema_audit_state + 1)"
              (directory-file-name native-root) python))
            (princ "PASS target kernel executes in the same project and Python\n"))
        (when runtime (ignore-errors (my/noema-jupyter--shutdown-runtime runtime)))
        (when (and my/noema--ready (buffer-live-p notebook))
          (ignore-errors
            (my/noema--api-call-sync
             "aaronnote:api:jupyter:script-action"
             (vector `((scriptFile . ,file) (action . "shutdown"))) 20)))
        (when (buffer-live-p notebook)
          (with-current-buffer notebook
            (dolist (lsp-workspace (ignore-errors (lsp-workspaces)))
              (ignore-errors (my/lsp-mode-shutdown-workspace lsp-workspace 'live-smoke))))
          (with-current-buffer notebook (set-buffer-modified-p nil))
          (kill-buffer notebook))
        (when (buffer-live-p directory-buffer)
          (kill-buffer directory-buffer))
        (when (file-exists-p source) (delete-file source))
        (when (file-exists-p moved) (delete-file moved))
        (when (file-exists-p file) (delete-file file))
        (when workspace (ignore-errors (remote-workspace-close workspace 'test-cleanup)))
        (let ((host-process my/noema--process))
          (ignore-errors (my/noema-stop))
          (let ((deadline (+ (float-time) 4)))
            (while (and host-process (process-live-p host-process)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05))))
        (let ((deadline (+ (float-time) 3)))
          (while (and (file-directory-p host-sandbox)
                      (< (float-time) deadline))
            (unless (ignore-errors (delete-directory host-sandbox t) t)
              (accept-process-output nil 0.1)))
          (when (file-directory-p host-sandbox)
            (error "Isolated Noema host state was not removed: %s"
                   host-sandbox)))))))

(my/jupyter-ssh-project-live-run)
