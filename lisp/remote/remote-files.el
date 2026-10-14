;;; remote-files.el --- Two-pane file manager over bulk transfers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `remote-files' puts a folder of this machine beside a folder of a target,
;; both ordinary Dired buffers, with the transfer queue underneath.  Its
;; commands move what is marked in one pane into the other pane's folder
;; through `remote-transfer', which never blocks the editor.
;;
;; A session belongs to its frame.  Quitting it ends what it started and
;; nothing else: its running transfers, the workspace it opened
;; and, when the session made the connection and nothing else uses the
;; target, the connection itself.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-workspace)
(require 'remote-transfer)

(declare-function remote-read-target "remote-board" (&optional prompt omit-local))

(defgroup remote-files nil
  "Two-pane file manager for targets."
  :group 'remote)

(defcustom remote-files-local-directory "~/"
  "Folder of this machine first shown beside a target's folder."
  :type 'directory
  :group 'remote-files)

(defcustom remote-files-queue-height 8
  "Lines of the transfer queue window in a file manager session."
  :type 'integer
  :group 'remote-files)

(cl-defstruct (remote-files-session
               (:constructor remote-files-session--create))
  target-id frame configuration local-window remote-window
  existing-buffers workspace connected-before)

(defvar remote-files-local-history (make-hash-table :test #'equal)
  "Last folder of this machine used beside each target.")

(defun remote-files--session (&optional frame)
  "Return FRAME's live file manager session, if any."
  (let ((session (frame-parameter frame 'remote-files-session)))
    (and (remote-files-session-p session) session)))

(defun remote-files--require-session ()
  "Return the selected frame's session or signal a `user-error'."
  (or (remote-files--session)
      (user-error "No file manager session here; start one with M-x remote-files")))

(defun remote-files--pane-window (session side)
  "Return SESSION's live window for SIDE, `local' or `remote'."
  (let ((window (if (eq side 'local)
                    (remote-files-session-local-window session)
                  (remote-files-session-remote-window session))))
    (unless (window-live-p window)
      (user-error "The %s pane is gone; quit the session and start again"
                  side))
    window))

(defun remote-files--side (session &optional window)
  "Return which of SESSION's panes WINDOW is, or signal a `user-error'."
  (let ((window (or window (selected-window))))
    (cond
     ((eq window (remote-files-session-local-window session)) 'local)
     ((eq window (remote-files-session-remote-window session)) 'remote)
     (t (user-error "Select one of the two folder panes first")))))

(defun remote-files--other (side)
  "Return the pane opposite SIDE."
  (if (eq side 'local) 'remote 'local))

(defun remote-files--pane-directory (session side)
  "Return the folder SESSION's SIDE pane shows."
  (with-current-buffer (window-buffer (remote-files--pane-window session side))
    (unless (derived-mode-p 'dired-mode)
      (user-error "The %s pane does not show a folder" side))
    (file-name-as-directory (expand-file-name default-directory))))

(defun remote-files--show (session side directory)
  "Show DIRECTORY in SESSION's SIDE pane."
  (let ((window (remote-files--pane-window session side)))
    (set-window-buffer window (dired-noselect directory))
    (when (eq side 'local)
      (puthash (remote-files-session-target-id session)
               (file-name-as-directory directory)
               remote-files-local-history))))

(defun remote-files--label (directory)
  "Return a short name for logical or native DIRECTORY."
  (let ((canonical (remote-canonicalize-file-name directory)))
    (if (remote-client-file-name canonical)
        (abbreviate-file-name (remote-client-file-name canonical))
      (format "%s:%s" (remote-file-name-target canonical)
              (remote-file-local-name canonical)))))

(defun remote-files--revert-directory (directory)
  "Refresh the Dired buffers that show DIRECTORY."
  (let ((directory (file-name-as-directory directory)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'dired-mode)
                   (equal (file-name-as-directory
                           (expand-file-name default-directory))
                          directory))
          (condition-case nil
              (revert-buffer)
            (error nil)))))))

(defun remote-files--finished (job)
  "Report JOB and refresh the folder it wrote into."
  (pcase (remote-transfer-job-state job)
    ('done
     (remote-files--revert-directory (remote-transfer-job-destination job))
     (message "Transfer finished: %s" (remote-transfer-job-name job)))
    ('failed
     (message "Transfer failed (%s): %s; RET on its row shows why"
              (remote-transfer-job-exit-status job)
              (remote-transfer-job-name job)))
    (_ (message "Transfer cancelled: %s" (remote-transfer-job-name job)))))

;;; Transfers between the panes

(defun remote-files--arrow (side)
  "Return the direction word for moving out of the SIDE pane."
  (if (eq side 'local) "Upload" "Download"))

(defun remote-files-copy ()
  "Copy the marked files, or the one at point, into the other pane's folder."
  (interactive)
  (let* ((session (remote-files--require-session))
         (side (remote-files--side session))
         (files (dired-get-marked-files))
         (destination
          (remote-files--pane-directory session (remote-files--other side)))
         (summary (if (cdr files)
                      (format "%d items" (length files))
                    (file-name-nondirectory
                     (directory-file-name (car files))))))
    (when (y-or-n-p (format "%s %s to %s? "
                            (remote-files--arrow side) summary
                            (remote-files--label destination)))
      (remote-transfer-start
       files destination
       :name (format "%s %s -> %s" (remote-files--arrow side)
                     summary (remote-files--label destination))
       :callback #'remote-files--finished)
      (message "%s of %s queued" (remote-files--arrow side) summary))))

(defvar-local remote-files--apply nil
  "Function that carries out the changes a comparison buffer lists.")

(defun remote-files-apply ()
  "Carry out the changes listed in this comparison."
  (interactive)
  (let ((apply (or remote-files--apply
                   (user-error "Nothing to apply here"))))
    (setq remote-files--apply nil)
    (quit-window)
    (funcall apply)))

(defvar-keymap remote-files-compare-mode-map
  "C-c C-c" #'remote-files-apply)

(define-minor-mode remote-files-compare-mode
  "Minor mode of a comparison whose changes can be applied with \\[remote-files-apply]."
  :lighter " Compare")

(defun remote-files--synchronize (delete apply)
  "Compare this pane's folder with the other pane's, then offer the changes.
DELETE also lists what only the other folder has.  With APPLY nil the
comparison is only shown."
  (let* ((session (remote-files--require-session))
         (side (remote-files--side session))
         (source (remote-files--pane-directory session side))
         (destination
          (remote-files--pane-directory session (remote-files--other side)))
         (name (format "%s %s -> %s"
                       (if delete "Mirror" "Sync")
                       (remote-files--label source)
                       (remote-files--label destination)))
         (run
          (lambda ()
            (remote-transfer-start
             (list source) destination :contents t :delete delete
             :name name :callback #'remote-files--finished)
            (message "%s queued" name))))
    (remote-transfer-start
     (list source) destination :contents t :delete delete :dry-run t
     :name (concat "Compare: " name)
     :callback
     (lambda (job)
       (if (not (eq (remote-transfer-job-state job) 'done))
           (remote-files--finished job)
         (let ((buffer (remote-transfer-job-log-buffer job)))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (insert
                  (if apply
                      (substitute-command-keys
                       "\nEach line above is one change; none means the folders already match.\n\\<remote-files-compare-mode-map>\\[remote-files-apply] applies them, q leaves the folders as they are.\n")
                    "\nEach line above is one difference; none means the folders match.\n")))
               (goto-char (point-min))
               (when apply
                 (setq remote-files--apply run)
                 (remote-files-compare-mode 1)))
             (display-buffer buffer))))))
    (message "Comparing %s with %s..."
             (remote-files--label source) (remote-files--label destination))))

(defun remote-files-sync ()
  "Bring the other pane's folder up to date with this pane's folder.
Nothing is deleted.  The changes are listed first and applied on request."
  (interactive)
  (remote-files--synchronize nil t))

(defun remote-files-mirror ()
  "Make the other pane's folder identical to this pane's folder.
Files only the other folder has are deleted.  The changes are listed first
and applied on request."
  (interactive)
  (remote-files--synchronize t t))

(defun remote-files-compare ()
  "List what differs between this pane's folder and the other pane's."
  (interactive)
  (remote-files--synchronize t nil))

;;; Panes

(defun remote-files-other-pane ()
  "Select the other folder pane."
  (interactive)
  (let* ((session (remote-files--require-session))
         (side (condition-case nil
                   (remote-files--side session)
                 (user-error 'remote))))
    (select-window (remote-files--pane-window
                    session (remote-files--other side)))))

(defun remote-files-set-local (directory)
  "Show DIRECTORY of this machine in the local pane."
  (interactive
   (let ((session (remote-files--require-session)))
     (list (read-directory-name
            "Local folder: "
            (remote-files--pane-directory session 'local)))))
  (remote-files--show (remote-files--require-session) 'local directory))

(defun remote-files-set-remote (directory)
  "Show DIRECTORY of the session's target in the remote pane."
  (interactive
   (let ((session (remote-files--require-session)))
     (list (read-directory-name
            "Remote folder: "
            (remote-files--pane-directory session 'remote)))))
  (let ((session (remote-files--require-session)))
    (unless (equal (remote-file-name-target directory)
                   (remote-files-session-target-id session))
      (user-error "This session manages %s; start another for a different target"
                  (remote-files-session-target-id session)))
    (remote-files--show session 'remote directory)))

(defun remote-files-show-queue ()
  "Show the transfer queue under the panes."
  (interactive)
  (display-buffer
   (remote-transfer-list-buffer)
   `((display-buffer-in-side-window)
     (side . bottom)
     (window-height . ,remote-files-queue-height))))

;;; Session lifetime

(defun remote-files--target-in-use-p (target-id ignored-buffers)
  "Return non-nil when something besides IGNORED-BUFFERS uses TARGET-ID."
  (or (seq-some (lambda (workspace)
                  (equal (plist-get workspace :target) target-id))
                (remote-workspace-list))
      (seq-some (lambda (buffer)
                  (and (not (memq buffer ignored-buffers))
                       (equal (ignore-errors (remote-buffer-target buffer))
                              target-id)))
                (buffer-list))))

(defun remote-files-quit (&optional force)
  "End this frame's file manager session and release what it started.
Running transfers are cancelled after a question, or at once with FORCE."
  (interactive)
  (let* ((session (remote-files--require-session))
         (target-id (remote-files-session-target-id session))
         (running (remote-transfer-jobs-for-target target-id t))
         (panes
          (delq nil
                (mapcar (lambda (window)
                          (and (window-live-p window) (window-buffer window)))
                        (list (remote-files-session-local-window session)
                              (remote-files-session-remote-window session)))))
         (owned
          (seq-remove
           (lambda (buffer)
             (memq buffer (remote-files-session-existing-buffers session)))
           panes)))
    (when (and running
               (not force)
               (not (y-or-n-p
                     (format "Cancel %d running transfer%s and quit? "
                             (length running)
                             (if (cdr running) "s" "")))))
      (user-error "Session kept; its transfers are still running"))
    (mapc #'remote-transfer-cancel running)
    (set-frame-parameter nil 'remote-files-session nil)
    (when-let* ((queue (get-buffer "*Remote Transfers*")))
      (dolist (window (get-buffer-window-list queue nil (selected-frame)))
        (when (window-parameter window 'window-side)
          (delete-window window))))
    (when (window-configuration-p
           (remote-files-session-configuration session))
      (set-window-configuration (remote-files-session-configuration session)))
    (dolist (buffer owned)
      (when (and (buffer-live-p buffer)
                 (not (get-buffer-window buffer t)))
        (kill-buffer buffer)))
    (when-let* ((workspace (remote-files-session-workspace session))
                ((remote-get-workspace workspace)))
      (remote-workspace-close workspace 'remote-files-quit))
    (if (or (remote-files-session-connected-before session)
            (remote-files--target-in-use-p target-id nil))
        (message "File manager closed; %s stays connected" target-id)
      (remote-workspace-disconnect-target target-id 'remote-files-quit)
      (message "File manager closed and %s disconnected" target-id))))

(defun remote-files--default-remote ()
  "Return the current folder when this machine cannot read it directly."
  (let ((directory (expand-file-name default-directory)))
    (unless (ignore-errors (remote-client-file-name directory))
      (file-name-as-directory (remote-canonicalize-file-name directory)))))

;;;###autoload
(defun remote-files (&optional remote-directory local-directory)
  "Manage REMOTE-DIRECTORY beside LOCAL-DIRECTORY of this machine.
In a folder of a target, that folder is managed.  Anywhere else a target is
asked for, temporary machines included.  With a session already open on this
frame, show its commands instead."
  (interactive
   (unless (remote-files--session)
     (let ((remote (remote-files--default-remote)))
       (if remote
           (list remote)
         (list (remote-target-file-name
                (remote-read-target "Manage files on: " t))
               (file-name-as-directory
                (expand-file-name default-directory)))))))
  (if (remote-files--session)
      (remote-files-dispatch)
    (let* ((remote (file-name-as-directory
                    (remote-canonicalize-file-name remote-directory)))
           (target-id (remote-file-name-target remote))
           (local (file-name-as-directory
                   (expand-file-name
                    (or local-directory
                        (gethash target-id remote-files-local-history)
                        remote-files-local-directory))))
           (session
            (remote-files-session--create
             :target-id target-id :frame (selected-frame)
             :configuration (current-window-configuration)
             :existing-buffers (buffer-list)
             ;; A folder this machine reads directly has no connection that
             ;; the session could own.
             :connected-before
             (and (or (remote-client-file-name remote)
                      (file-remote-p remote nil t))
                  t)))
           (started nil))
      (unless (remote-client-file-name local)
        (user-error "The local pane needs a folder of this machine: %s" local))
      (unwind-protect
          (let ((existing (remote-get-workspace remote)))
            (unless existing
              (setf (remote-files-session-workspace session)
                    (remote-workspace-open
                     remote :connect t :adapter "emacs-file"
                     :capability 'file-read)))
            (unless (file-directory-p remote)
              (user-error "Folder is unavailable: %s"
                          (remote-files--label remote)))
            (delete-other-windows)
            (setf (remote-files-session-local-window session)
                  (selected-window)
                  (remote-files-session-remote-window session)
                  (split-window-right))
            (set-frame-parameter nil 'remote-files-session session)
            (remote-files--show session 'local local)
            (remote-files--show session 'remote remote)
            (remote-files-show-queue)
            (select-window (remote-files-session-remote-window session))
            (setq started t)
            (message
             (substitute-command-keys
              "File manager on %s: \\[remote-files] shows its commands, q in that menu quits")
             target-id))
        (unless started
          (set-frame-parameter nil 'remote-files-session nil)
          (set-window-configuration
           (remote-files-session-configuration session))
          (when-let* ((workspace (remote-files-session-workspace session))
                      ((remote-get-workspace workspace)))
            (remote-workspace-close workspace 'remote-files-failed)))))))

(defun remote-files--delete-frame-h (frame)
  "End FRAME's file manager session when the frame goes away."
  (when (remote-files--session frame)
    (with-selected-frame frame
      (condition-case nil
          (remote-files-quit t)
        (error nil)))))

(add-hook 'delete-frame-functions #'remote-files--delete-frame-h)

;;;###autoload (autoload 'remote-files-dispatch "remote-files" nil t)
(transient-define-prefix remote-files-dispatch ()
  "Commands of the file manager session on this frame."
  [:description
   (lambda ()
     (if-let* ((session (remote-files--session)))
         (format "Files on %s" (remote-files-session-target-id session))
       "Files"))
   ["Transfer to the other pane"
    ("c" "Copy marked" remote-files-copy)
    ("s" "Sync folder" remote-files-sync)
    ("S" "Mirror folder (deletes extras)" remote-files-mirror)
    ("d" "Compare folders" remote-files-compare)]
   ["Panes"
    ("o" "Other pane" remote-files-other-pane)
    ("l" "Local folder" remote-files-set-local)
    ("r" "Remote folder" remote-files-set-remote)
    ("t" "Transfer queue" remote-files-show-queue)]
   ["Session"
    ("q" "Quit and release" remote-files-quit)]])

(provide 'remote-files)
;;; remote-files.el ends here
