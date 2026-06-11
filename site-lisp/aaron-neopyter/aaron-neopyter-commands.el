;;; aaron-neopyter-commands.el --- Interactive commands -*- lexical-binding: t -*-

;;; Commentary:
;; All user-facing interactive commands for aaron-neopyter-mode.

;;; Code:

(require 'cl-lib)
(require 'aaron-neopyter-rpc)
(require 'aaron-neopyter-parser)
(require 'aaron-neopyter-sync)
(require 'aaron-neopyter-jupyter)
(require 'aaron-neopyter-ui)

(defun aaron-neopyter--conn ()
  "Return the current connection or nil."
  (and (boundp 'aaron-neopyter--connection) aaron-neopyter--connection))

(defun aaron-neopyter--require-conn ()
  "Return the connection, signaling an error if not connected."
  (let ((conn (aaron-neopyter--conn)))
    (unless (aaron-neopyter-rpc-connected-p conn)
      (user-error "Neopyter is not connected (run aaron-neopyter-connect first)"))
    conn))

(defun aaron-neopyter--notebook-path ()
  "Return the notebook path for the current session, or signal an error."
  (unless aaron-neopyter--session
    (user-error "No active Neopyter session in this buffer"))
  (let ((path (aaron-neopyter--session-notebook-path aaron-neopyter--session)))
    (unless path
      (user-error "No notebook path for this buffer (run aaron-neopyter-open-notebook)"))
    path))

;;;###autoload
(defun aaron-neopyter-connect ()
  "Start the Neopyter WebSocket server and wait for the extension to connect."
  (interactive)
  (require 'aaron-neopyter)
  (aaron-neopyter--start-server))

;;;###autoload
(defun aaron-neopyter-disconnect ()
  "Stop the Neopyter WebSocket server."
  (interactive)
  (require 'aaron-neopyter)
  (aaron-neopyter--stop-server))

;;;###autoload
(defun aaron-neopyter-status ()
  "Show the current Neopyter connection status."
  (interactive)
  (aaron-neopyter-ui-show-status))

;;;###autoload
(defun aaron-neopyter-sync-current ()
  "Immediately sync the current buffer to JupyterLab."
  (interactive)
  (let ((conn (aaron-neopyter--require-conn)))
    (unless aaron-neopyter--session
      (user-error "No active Neopyter session in this buffer"))
    (aaron-neopyter-sync-now conn)
    (message "Neopyter: synced %s" (buffer-name))))

;;;###autoload
(defun aaron-neopyter-run-cell ()
  "Run the current cell in JupyterLab."
  (interactive)
  (let ((conn (aaron-neopyter--require-conn))
        (path (aaron-neopyter--notebook-path)))
    (when aaron-neopyter--session
      (aaron-neopyter-cursor-now conn))
    (aaron-neopyter-jupyter-run-cell
     conn path
     (lambda (_r err)
       (if err
           (message "Neopyter: run-cell error: %s" err)
         (message "Neopyter: cell executed"))))))

;;;###autoload
(defun aaron-neopyter-run-all-above ()
  "Run all cells above the current cell."
  (interactive)
  (let ((conn (aaron-neopyter--require-conn))
        (path (aaron-neopyter--notebook-path)))
    (when aaron-neopyter--session (aaron-neopyter-cursor-now conn))
    (aaron-neopyter-jupyter-run-all-above
     conn path
     (lambda (_r err) (when err (message "Neopyter: run-all-above error: %s" err))))))

;;;###autoload
(defun aaron-neopyter-run-all-below ()
  "Run the current cell and all cells below."
  (interactive)
  (let ((conn (aaron-neopyter--require-conn))
        (path (aaron-neopyter--notebook-path)))
    (when aaron-neopyter--session (aaron-neopyter-cursor-now conn))
    (aaron-neopyter-jupyter-run-all-below
     conn path
     (lambda (_r err) (when err (message "Neopyter: run-all-below error: %s" err))))))

;;;###autoload
(defun aaron-neopyter-run-all ()
  "Run all cells in the notebook."
  (interactive)
  (let ((conn (aaron-neopyter--require-conn))
        (path (aaron-neopyter--notebook-path)))
    (aaron-neopyter-jupyter-run-all
     conn path
     (lambda (_r err)
       (if err
           (message "Neopyter: run-all error: %s" err)
         (message "Neopyter: all cells running"))))))

;;;###autoload
(defun aaron-neopyter-restart-kernel ()
  "Restart the Jupyter kernel."
  (interactive)
  (when (yes-or-no-p "Restart kernel? (all output will be lost) ")
    (let ((conn (aaron-neopyter--require-conn))
          (path (aaron-neopyter--notebook-path)))
      (aaron-neopyter-jupyter-restart-kernel
       conn path
       (lambda (_r err)
         (if err
             (message "Neopyter: restart error: %s" err)
           (message "Neopyter: kernel restarting")))))))

;;;###autoload
(defun aaron-neopyter-restart-kernel-run-all ()
  "Restart the kernel and run all cells."
  (interactive)
  (when (yes-or-no-p "Restart kernel and run all cells? ")
    (let ((conn (aaron-neopyter--require-conn))
          (path (aaron-neopyter--notebook-path)))
      (aaron-neopyter-jupyter-restart-run-all
       conn path
       (lambda (_r err)
         (if err
             (message "Neopyter: restart+run-all error: %s" err)
           (message "Neopyter: kernel restarting, running all cells")))))))

;;;###autoload
(defun aaron-neopyter-open-notebook ()
  "Open or create the notebook paired with the current buffer."
  (interactive)
  (let ((conn (aaron-neopyter--require-conn)))
    ;; Always re-init so a changed jupyter-root or pairing takes effect.
    (aaron-neopyter-sync-init-session)
    (let ((path (aaron-neopyter--session-notebook-path aaron-neopyter--session)))
      (unless path
        (user-error "Cannot determine notebook path for this buffer"))
      (aaron-neopyter-sync-attach
       conn
       (lambda (_r err)
         (if err
             (message "Neopyter: open-notebook error: %s" err)
           (message "Neopyter: notebook open and synced: %s"
                    (file-name-nondirectory path))))))))

;;;###autoload
(defun aaron-neopyter-toggle-follow-point ()
  "Toggle whether cursor position drives the active JupyterLab cell."
  (interactive)
  (if aaron-neopyter--session
      (let ((new-val (not (aaron-neopyter--session-follow-point
                           aaron-neopyter--session))))
        (setf (aaron-neopyter--session-follow-point aaron-neopyter--session) new-val)
        (message "Neopyter: follow-point %s" (if new-val "enabled" "disabled")))
    (message "Neopyter: no active session in this buffer")))

;;;###autoload
(defun aaron-neopyter-save-notebook ()
  "Save the active notebook in JupyterLab."
  (interactive)
  (let ((conn (aaron-neopyter--require-conn))
        (path (aaron-neopyter--notebook-path)))
    (aaron-neopyter-jupyter-save
     conn path
     (lambda (_r err)
       (if err
           (message "Neopyter: save error: %s" err)
         (message "Neopyter: notebook saved"))))))

;;;###autoload
(defun aaron-neopyter-show-log ()
  "Show the Neopyter debug log buffer."
  (interactive)
  (aaron-neopyter-ui-show-log))

;;;###autoload
(defun aaron-neopyter-detect-jupyter-root ()
  "Auto-detect `aaron-neopyter-jupyter-root' from the currently open notebook.
Calls getCurrentNotebook to get the JupyterLab-relative path, then computes
the root by stripping that relative path from the expected absolute path.

Requires: (1) Neopyter connected, (2) the paired notebook open in JupyterLab,
(3) `aaron-neopyter-mode' active in the current buffer."
  (interactive)
  (let ((conn (aaron-neopyter--require-conn)))
    (unless aaron-neopyter--session
      (user-error "No active Neopyter session in this buffer"))
    ;; Compute what the absolute path should be (before any root stripping)
    (let ((abs-path
           (and buffer-file-name
                (let* ((file (expand-file-name buffer-file-name))
                       (without-ju
                        (replace-regexp-in-string
                         "\\.ju\\.\\([^.]+\\)\\'" ".\\1" file))
                       (sans-ext (file-name-sans-extension without-ju)))
                  (concat sans-ext ".ipynb")))))
      (unless abs-path
        (user-error "Cannot determine absolute notebook path for this buffer"))
      (aaron-neopyter-jupyter-get-current-notebook
       conn
       (lambda (rel-path err)
         (if err
             (message "Neopyter: detect-root error: %s" err)
           (if (or (null rel-path) (string-empty-p rel-path))
               (message "Neopyter: no notebook currently open in JupyterLab")
             ;; Normalize: JupyterLab sometimes prefixes with "/"
             (let* ((rel (string-trim-left rel-path "/"))
                    (abs (expand-file-name abs-path))
                    (abs-dir (file-name-directory abs))
                    (rel-dir (file-name-directory rel))
                    (root
                     (when (and rel-dir
                                (string-suffix-p
                                 (file-name-as-directory rel-dir) abs-dir))
                       (substring abs-dir
                                  0
                                  (- (length abs-dir)
                                     (length (file-name-as-directory rel-dir)))))))
               (if root
                   (progn
                     (setq aaron-neopyter-jupyter-root root)
                     ;; Re-initialize the session with the corrected path
                     (aaron-neopyter-sync-init-session)
                     (message "Neopyter: jupyter-root set to \"%s\" (add to init file to persist)"
                              root))
                 (message
                  "Neopyter: could not infer root.\n  abs=%s\n  rel=%s\nSet `aaron-neopyter-jupyter-root' manually."
                  abs rel))))))))))

(provide 'aaron-neopyter-commands)
;;; aaron-neopyter-commands.el ends here
