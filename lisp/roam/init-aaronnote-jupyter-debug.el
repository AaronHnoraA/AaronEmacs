;;; init-aaronnote-jupyter-debug.el --- Notebook DAP through Noema -*- lexical-binding: t; -*-
;;; Commentary:
;; Noema owns the kernel and debugger protocol. Dape owns breakpoints, stack,
;; stepping, watches and variable inspection. The DAP listener is client-local
;; even when notebook storage or compute belongs to a Remote target.
;;; Code:
(require 'cl-lib)
(require 'init-aaronnote-jupyter-cell)
(require 'remote-core)

(declare-function dape "dape" (config &optional skip-compile))
(declare-function dape-next "dape" (conn))
(declare-function dape-continue "dape" (conn))
(declare-function jsonrpc-running-p "jsonrpc" (conn))
(defvar dape--connections)
(defvar dape-start-hook)
(defvar-local my/noema-jupyter-debug--id nil)
(defvar my/noema-jupyter-debug--sessions (make-hash-table :test #'equal))

(defun my/noema-jupyter-debug--projection ()
  "Return saved code cells with their actual Emacs source coordinates."
  (vconcat
   (cl-loop for cell in (my/noema-jupyter-notebook-projection-cells)
            when (equal (plist-get cell :type) "code")
            collect `((id . ,(plist-get cell :id))
                      (line . ,(plist-get cell :line))
                      (code . ,(plist-get cell :code))))))

(defun my/noema-jupyter-debug--release (id)
  "Release ID's editing guard and timer."
  (when-let* ((session (gethash id my/noema-jupyter-debug--sessions)))
    (remhash id my/noema-jupyter-debug--sessions)
    (when-let* ((timer (plist-get session :timer))) (cancel-timer timer))
    (when-let* ((buffer (plist-get session :buffer)) ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (when (equal id my/noema-jupyter-debug--id)
          (setq my/noema-jupyter-debug--id nil
                buffer-read-only (plist-get session :read-only))
          (remove-hook 'kill-buffer-hook #'my/noema-jupyter-debug-stop t)
          (force-mode-line-update))))))

(defun my/noema-jupyter-debug-handle-ended (payload)
  "Handle Noema's debugger-ended PAYLOAD."
  (my/noema-jupyter-debug--release (my/noema-jupyter-notebook--get 'id payload)))

(defun my/noema-jupyter-debug--watch (id)
  "Release ID even when the gateway ended before its final event arrived."
  (when-let* ((session (gethash id my/noema-jupyter-debug--sessions)))
    (unless (jsonrpc-running-p (plist-get session :connection))
      (my/noema-jupyter-debug--release id))))

(defun my/noema-jupyter-debug-start (&optional by-line)
  "Debug the current code cell, or stop at entry when BY-LINE is non-nil."
  (interactive)
  (unless (and my/noema-jupyter-cell-mode my/noema-jupyter-notebook--projection-p)
    (user-error "Open a Jupyter notebook source buffer first"))
  (when my/noema-jupyter-debug--id (user-error "This notebook already has an active debugger"))
  (require 'dape)
  (when (buffer-modified-p) (save-buffer))
  (let* ((cell (my/noema-jupyter-cell--bounds-at-point))
         (id (plist-get cell :id))
         (origin (current-buffer))
         (was-read-only buffer-read-only)
         (reply (my/noema-jupyter-cell--api-sync
                 "aaronnote:api:jupyter:debug-start"
                 `((scriptFile . ,buffer-file-name) (cellId . ,id)
                   (projection . ,(my/noema-jupyter-debug--projection))
                   (runByLine . ,(if by-line t :json-false))) 60))
         (debug-id (my/noema-jupyter-notebook--get 'id reply))
         (port (my/noema-jupyter-notebook--get 'port reply)))
    (unless (and (stringp debug-id) (integerp port) (< 0 port 65536))
      (user-error "Noema did not return a debugger endpoint"))
    (condition-case err
        (let* ((default-directory user-emacs-directory)
               (dape-start-hook (unless by-line dape-start-hook)))
          (remote-with-client-environment
            (dape (list 'host "127.0.0.1" 'port port
                        'command-cwd user-emacs-directory
                        :name (if by-line "Notebook: Run by Line" "Notebook: Debug Cell")
                        :type "python" :request "attach" :justMyCode t)))
          (let ((session (list :buffer origin :read-only was-read-only
                               :connection (car dape--connections))))
            (puthash debug-id session my/noema-jupyter-debug--sessions)
            (setf (plist-get session :timer)
                  (run-at-time 1 1 #'my/noema-jupyter-debug--watch debug-id)))
          (with-current-buffer origin
            ;; The debugger runs a fixed source snapshot. Keep stack locations
            ;; stable until it finishes; output updates remain server-owned.
            (setq my/noema-jupyter-debug--id debug-id buffer-read-only t)
            (add-hook 'kill-buffer-hook #'my/noema-jupyter-debug-stop nil t))
          debug-id)
      (error
       (ignore-errors
         (my/noema-jupyter-cell--api-sync "aaronnote:api:jupyter:debug-stop" `((id . ,debug-id)) 15))
       (signal (car err) (cdr err))))))

(defun my/noema-jupyter-run-by-line ()
  "Start the current cell at its first statement, or advance one statement."
  (interactive)
  (if-let* ((session (gethash my/noema-jupyter-debug--id my/noema-jupyter-debug--sessions)))
      (dape-next (plist-get session :connection))
    (my/noema-jupyter-debug-start t)))

(defun my/noema-jupyter-debug-continue ()
  "Continue this notebook's debugger until the next breakpoint or completion."
  (interactive)
  (if-let* ((session (gethash my/noema-jupyter-debug--id my/noema-jupyter-debug--sessions)))
      (dape-continue (plist-get session :connection))
    (user-error "This notebook has no active debugger")))

(defun my/noema-jupyter-debug-stop ()
  "Stop this notebook's debug run while preserving its kernel."
  (interactive)
  (when-let* ((id my/noema-jupyter-debug--id))
    (unwind-protect
        (my/noema-jupyter-cell--api-sync "aaronnote:api:jupyter:debug-stop" `((id . ,id)) 20)
      (my/noema-jupyter-debug--release id))))

(keymap-set my/noema-jupyter-cell-mode-map "C-c i g" #'my/noema-jupyter-debug-start)
(keymap-set my/noema-jupyter-cell-mode-map "C-c i ." #'my/noema-jupyter-run-by-line)
(keymap-set my/noema-jupyter-cell-mode-map "C-c i c" #'my/noema-jupyter-debug-continue)
(keymap-set my/noema-jupyter-cell-mode-map "C-c i q" #'my/noema-jupyter-debug-stop)

(provide 'init-aaronnote-jupyter-debug)
;;; init-aaronnote-jupyter-debug.el ends here
