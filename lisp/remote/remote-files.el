;;; remote-files.el --- Bulk transfers from inside Dired -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A Dired extension, not a screen of its own.  In any Dired or Dirvish
;; buffer `remote-files' pops up a menu for the folder shown: send what is
;; marked to the other machine, fetch files from it, or synchronize the two
;; folders.  The other side is chosen in a second, pop-up Dired: mark files or
;; walk to a folder there and confirm.  It closes again once the choice is
;; made, and the transfer runs in `remote-transfer' without blocking.
;;
;; Which side is "the other" follows placement.  From a folder this machine
;; cannot read directly, the pop-up shows this machine; from a folder of this
;; machine, a target is asked for and the pop-up shows it.
;;
;; There is no session to quit.  A transfer uses a connection of its own that
;; ends with it; the pop-up kills the buffers it made.  The menu can also
;; disconnect the folder's target.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-workspace)
(require 'remote-terminal)
(require 'remote-transfer)

(declare-function remote-read-target "remote-board" (&optional prompt omit-local))
(declare-function remote-open-target "remote-board" (&optional target))

(defgroup remote-files nil
  "Bulk transfers from inside Dired."
  :group 'remote)

(defcustom remote-files-local-directory "~/"
  "Folder of this machine first offered as the other side of a target."
  :type 'directory
  :group 'remote-files)

(defvar remote-files-terminal-function #'remote-files-default-terminal
  "Function that opens a terminal on a folder.
It receives a buffer name and a logical directory, and returns the terminal
buffer.  Called again while that buffer lives, it moves the shell to the
directory instead of starting another.")

(defvar remote-files-other-history (make-hash-table :test #'equal)
  "Last folder used as the other side, keyed by the target of this side.")

;;; Folders and names

(defun remote-files--client-directory (directory)
  "Return the logical name of DIRECTORY, a folder of this machine.
A bare path typed or kept by a command means this machine.  Canonicalized
from inside a target's buffer it would name a folder of that target instead,
so it is resolved from a directory that is the client's own."
  (let ((default-directory temporary-file-directory))
    (file-name-as-directory
     (remote-canonicalize-file-name (expand-file-name directory)))))

(defun remote-files--here ()
  "Return the logical name of the folder the current Dired buffer shows."
  (unless (derived-mode-p 'dired-mode)
    (user-error "Use this in a Dired folder"))
  ;; Resolved in the folder's own buffer, which is what gives a bare path
  ;; its machine.
  (file-name-as-directory
   (remote-canonicalize-file-name (expand-file-name default-directory))))

(defun remote-files--label (directory)
  "Return a short name for logical DIRECTORY."
  (if-let* ((client (remote-client-file-name directory)))
      (abbreviate-file-name client)
    (format "%s:%s" (remote-file-name-target directory)
            (remote-file-local-name directory))))

(defun remote-files--revert-directory (directory)
  "Refresh the Dired buffers that show logical DIRECTORY."
  (let ((directory (file-name-as-directory directory)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'dired-mode)
                   (equal (file-name-as-directory
                           (remote-canonicalize-file-name
                            (expand-file-name default-directory)))
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
     (message "Transfer failed (%s): %s; M-x remote-transfer-list shows why"
              (remote-transfer-job-exit-status job)
              (remote-transfer-job-name job)))
    (_ (message "Transfer cancelled: %s" (remote-transfer-job-name job)))))

(defun remote-files--summary (files)
  "Return a short description of FILES."
  (if (cdr files)
      (format "%d items" (length files))
    (file-name-nondirectory (directory-file-name (car files)))))

;;; The other side

(defun remote-files--other-start (here)
  "Return the folder first shown as the other side of logical HERE.
From a folder this machine cannot read, that is a folder of this machine.
From a folder of this machine, a target is asked for."
  (let ((remembered
         (gethash (remote-file-name-target here) remote-files-other-history)))
    (if (not (remote-client-file-name here))
        (remote-files--client-directory
         (or remembered remote-files-local-directory))
      (let* ((target (remote-read-target "Other side, target: " t))
             (id (remote-target-id target)))
        (if (and remembered (equal (remote-file-name-target remembered) id))
            remembered
          (file-name-as-directory (remote-target-file-name target)))))))

(defun remote-files--pick-state (&optional window)
  "Return the pending choice WINDOW was popped up for, if any."
  (window-parameter window 'remote-files-pick))

(defun remote-files--pick-close (window)
  "Close pop-up WINDOW and kill the Dired buffers its choice made."
  (let* ((state (remote-files--pick-state window))
         (origin (plist-get state :origin))
         (made
          (seq-remove
           (lambda (buffer) (memq buffer (plist-get state :existing)))
           (seq-filter
            (lambda (buffer)
              (and (buffer-live-p buffer)
                   (with-current-buffer buffer (derived-mode-p 'dired-mode))))
            (cons (window-buffer window)
                  (mapcar #'car (window-prev-buffers window)))))))
    (set-window-parameter window 'remote-files-pick nil)
    (when (window-live-p window)
      (if (plist-get state :created)
          (ignore-errors (delete-window window))
        (when (buffer-live-p (plist-get state :previous))
          (set-window-buffer window (plist-get state :previous)))))
    (dolist (buffer made)
      (unless (get-buffer-window buffer t)
        (kill-buffer buffer)))
    (when (window-live-p origin)
      (select-window origin))))

(defun remote-files--pick (directory want description callback)
  "Pop up DIRECTORY for a choice and call CALLBACK with it once confirmed.
WANT is `files', for the marked files or the one at point, or `directory',
for the folder shown when the choice is confirmed.  DESCRIPTION says what
confirming does."
  (let* ((origin (selected-window))
         (windows (window-list nil 'no-mini))
         (existing (buffer-list))
         (buffer
          ;; A folder this machine reads directly is visited under its native
          ;; name, from a native directory, like any other local Dired.
          (let* ((client (remote-client-file-name directory))
                 (default-directory
                  (if client temporary-file-directory default-directory)))
            (dired-noselect (or client directory))))
         (window
          (display-buffer
           buffer
           '((display-buffer-pop-up-window display-buffer-use-some-window)
             (inhibit-same-window . t)))))
    (set-window-parameter
     window 'remote-files-pick
     (list :want want :description description :callback callback
           :origin origin :existing existing
           :created (not (memq window windows))
           :previous (car (car (window-prev-buffers window)))))
    (select-window window)
    (message "%s"
             (substitute-command-keys
              (format "%s: %s, then \\[remote-files-pick-confirm] (\\[remote-files-pick-cancel] cancels)"
                      description
                      (if (eq want 'files)
                          "mark files"
                        "go to the folder"))))))

(defun remote-files-pick-confirm ()
  "Confirm the choice this pop-up folder was opened for."
  (interactive)
  (let* ((window (selected-window))
         (state (or (remote-files--pick-state window)
                    (user-error "Nothing is being chosen in this window")))
         (choice
          (if (eq (plist-get state :want) 'files)
              (mapcar #'remote-canonicalize-file-name
                      (or (dired-get-marked-files)
                          (user-error "Mark the files first")))
            (remote-files--here))))
    (remote-files--pick-close window)
    (funcall (plist-get state :callback) choice)))

(defun remote-files-pick-cancel ()
  "Close this pop-up folder without choosing."
  (interactive)
  (unless (remote-files--pick-state)
    (user-error "Nothing is being chosen in this window"))
  (remote-files--pick-close (selected-window))
  (message "Cancelled"))

(defun remote-files--remember-other (here other)
  "Remember logical OTHER as the other side of logical HERE's target."
  (puthash (remote-file-name-target here) (file-name-as-directory other)
           remote-files-other-history))

;;; Transfers

(defun remote-files--queue (files destination)
  "Queue copying logical FILES into logical DESTINATION."
  (let ((name (format "%s -> %s" (remote-files--summary files)
                      (remote-files--label destination))))
    (remote-transfer-start files destination
                           :name name :callback #'remote-files--finished)
    (message "Queued: %s" name)))

(defun remote-files-send ()
  "Copy the marked files, or the one at point, to a folder of the other side.
The folder is chosen in a pop-up Dired."
  (interactive)
  (let ((here (remote-files--here))
        (files (mapcar #'remote-canonicalize-file-name
                       (dired-get-marked-files))))
    (remote-files--pick
     (remote-files--other-start here) 'directory
     (format "Destination of %s" (remote-files--summary files))
     (lambda (destination)
       (remote-files--remember-other here destination)
       (remote-files--queue files destination)))))

(defun remote-files-fetch ()
  "Copy files of the other side into this folder.
The files are marked in a pop-up Dired."
  (interactive)
  (let ((here (remote-files--here)))
    (remote-files--pick
     (remote-files--other-start here) 'files
     (format "Files for %s" (remote-files--label here))
     (lambda (files)
       (remote-files--remember-other
        here (file-name-directory (directory-file-name (car files))))
       (remote-files--queue files here)))))

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

(defun remote-files--synchronize (source destination delete apply)
  "List what bringing DESTINATION up to date with SOURCE would change.
Both are logical folders.  DELETE also lists what only DESTINATION has.
With APPLY the listing offers to carry the changes out."
  (let* ((name (format "%s %s -> %s"
                       (if delete "Mirror" "Sync")
                       (remote-files--label source)
                       (remote-files--label destination)))
         (run
          (lambda ()
            (remote-transfer-start
             (list source) destination :contents t :delete delete
             :name name :callback #'remote-files--finished)
            (message "Queued: %s" name))))
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
             (pop-to-buffer buffer))))))
    (message "Comparing %s with %s..."
             (remote-files--label source) (remote-files--label destination))))

(defun remote-files--with-other-folder (description function)
  "Choose a folder of the other side, then call FUNCTION with here and it.
DESCRIPTION says what the folder is chosen for."
  (let ((here (remote-files--here)))
    (remote-files--pick
     (remote-files--other-start here) 'directory description
     (lambda (other)
       (remote-files--remember-other here other)
       (funcall function here other)))))

(defun remote-files--delete-p ()
  "Return non-nil when the menu's delete switch is on."
  (and (member "--delete" (transient-args 'remote-files-dispatch)) t))

(defun remote-files-push ()
  "Bring a folder of the other side up to date with this folder.
The changes are listed first and applied on request.  With the menu's delete
switch, files only the other folder has are deleted."
  (interactive)
  (let ((delete (remote-files--delete-p)))
    (remote-files--with-other-folder
     "Folder to update from here"
     (lambda (here other)
       (remote-files--synchronize here other delete t)))))

(defun remote-files-pull ()
  "Bring this folder up to date with a folder of the other side.
The changes are listed first and applied on request.  With the menu's delete
switch, files only this folder has are deleted."
  (interactive)
  (let ((delete (remote-files--delete-p)))
    (remote-files--with-other-folder
     "Folder to update this one from"
     (lambda (here other)
       (remote-files--synchronize other here delete t)))))

(defun remote-files-compare ()
  "List what differs between this folder and a folder of the other side."
  (interactive)
  (remote-files--with-other-folder
   "Folder to compare with"
   (lambda (here other)
     (remote-files--synchronize here other t nil))))

;;; Terminal and connection

(defun remote-files-default-terminal (name directory)
  "Open or reuse the routed terminal buffer NAME in logical DIRECTORY."
  (let ((buffer (get-buffer name)))
    (if (and buffer
             (remote-terminal-p
              (buffer-local-value 'remote-terminal-instance buffer)))
        (remote-terminal-send-string
         (buffer-local-value 'remote-terminal-instance buffer)
         (format "cd -- %s\n"
                 (shell-quote-argument (remote-file-local-name directory))))
      (setq buffer
            (remote-terminal-buffer (remote-terminal-open directory)))
      (with-current-buffer buffer (rename-buffer name t)))
    buffer))

(defun remote-files-terminal ()
  "Open a terminal in this folder, on the machine the folder is on."
  (interactive)
  (let ((here (remote-files--here)))
    (pop-to-buffer
     (funcall remote-files-terminal-function
              (format "*files:%s*" (remote-file-name-target here))
              here))))

(defun remote-files-disconnect ()
  "Disconnect this folder's target, ending its transfers and buffers."
  (interactive)
  (let ((here (remote-files--here)))
    (when (remote-client-file-name here)
      (user-error "This folder is on this machine; there is nothing to disconnect"))
    (let* ((target-id (remote-file-name-target here))
           (running (remote-transfer-jobs-for-target target-id t)))
      (when (yes-or-no-p
             (format "Disconnect %s%s? "
                     target-id
                     (if running
                         (format " and cancel %d running transfer%s"
                                 (length running) (if (cdr running) "s" ""))
                       "")))
        (remote-workspace-disconnect-target target-id 'remote-files)
        (message "Disconnected %s" target-id)))))

;;; Mode line

(defun remote-files-mode-line-text ()
  "Return what a Dired mode line says about transfers in its window.
A pop-up folder names the pending choice and its keys; any other folder
reports the transfers running on its target.  This runs on every redisplay
of a Dirvish window, so it only reads state that is already in memory."
  (if-let* ((state (remote-files--pick-state)))
      (propertize
       (substitute-command-keys
        (format " %s: \\[remote-files-pick-confirm] confirm, \\[remote-files-pick-cancel] cancel "
                (plist-get state :description)))
       'face 'warning)
    (when-let* ((running (remote-transfer--running))
                (target-id (ignore-errors
                             (remote-file-name-target default-directory)))
                (mine (seq-filter
                       (lambda (job)
                         (member target-id
                                 (remote-transfer-job-target-ids job)))
                       running)))
      (let ((percents (delq nil (mapcar #'remote-transfer-job-percent mine))))
        (format " Transfers:%d%s " (length mine)
                (if percents
                    (format " %d%%" (/ (apply #'+ percents) (length percents)))
                  ""))))))

;;; Entry

(defun remote-files--menu-title ()
  "Return the menu heading for the current folder."
  (condition-case nil
      (let ((here (remote-files--here)))
        (format "%s  <->  %s"
                (remote-files--label here)
                (if (remote-client-file-name here)
                    "a target (asked for)"
                  "this machine")))
    (error "Files")))

;;;###autoload (autoload 'remote-files-dispatch "remote-files" nil t)
(transient-define-prefix remote-files-dispatch ()
  "Transfer commands for the folder the current Dired buffer shows."
  [:description remote-files--menu-title
   ["Files"
    ("c" "Send marked to the other side" remote-files-send)
    ("g" "Fetch from the other side" remote-files-fetch)]
   ["Folders"
    ("-d" "Delete extras (mirror)" "--delete")
    ("s" "Sync this folder to the other" remote-files-push)
    ("r" "Sync the other folder to this" remote-files-pull)
    ("=" "Compare" remote-files-compare)]
   ["More"
    ("t" "Transfer queue" remote-transfer-list)
    ("e" "Terminal here" remote-files-terminal)
    ("x" "Disconnect target" remote-files-disconnect)]])

;;;###autoload (autoload 'remote-files-pick-dispatch "remote-files" nil t)
(transient-define-prefix remote-files-pick-dispatch ()
  "Confirm or cancel the choice a pop-up folder was opened for."
  [:description
   (lambda ()
     (or (plist-get (remote-files--pick-state) :description) "Choose"))
   ("c" "Confirm" remote-files-pick-confirm)
   ("k" "Cancel" remote-files-pick-cancel)])

;;;###autoload
(defun remote-files (&optional directory)
  "Show the transfer menu for the current Dired folder.
In a pop-up folder opened for a choice, show its confirm menu instead.
Outside Dired, or with DIRECTORY, open that folder first; without DIRECTORY
a target is asked for, temporary machines included."
  (interactive)
  (cond
   (directory
    (dired directory)
    (remote-files-dispatch))
   ((remote-files--pick-state)
    (remote-files-pick-dispatch))
   ((derived-mode-p 'dired-mode)
    (remote-files-dispatch))
   (t
    (remote-open-target (remote-read-target "Manage files on: " t))
    (when (derived-mode-p 'dired-mode)
      (remote-files-dispatch)))))

(provide 'remote-files)
;;; remote-files.el ends here
