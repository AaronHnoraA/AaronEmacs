;;; init-aaronnote-jupyter-cell.el --- Noema ipynb source buffers -*- lexical-binding: t -*-

;;; Commentary:
;; Noema owns Jupyter kernels, protocol traffic, execution, and outputs.
;; Emacs only edits the native source projection of Noema's standard ipynb and
;; forwards explicit editor commands to the running Noema application.

;;; Code:

(require 'cl-lib)
(require 'ansi-color)
(require 'subr-x)
(require 'init-aaronnote-jupyter-notebook)

(declare-function my/noema-command "init-aaronnote" (command &optional detail))
(declare-function my/noema--ensure-server "init-aaronnote" (&optional callback))
(declare-function my/noema--api-call-sync "init-aaronnote"
                  (channel args &optional timeout))
(declare-function my/noema-api-call "init-aaronnote" (channel args callback))
(declare-function my/noema-jupyter-output-open "init-aaronnote"
                  (&optional cell-id focus view))
(declare-function my/noema--host-file "init-aaronnote" (file))
(declare-function my/noema-jupyter-cell-lsp-runtime-changing
                  "init-aaronnote-jupyter-lsp" ())
(declare-function my/language-server-ensure-deferred "init-lsp" ())

(defvar-local my/noema-jupyter-cell-source-file nil
  "Markdown note file that owns the current Noema notebook.")

(defvar-local my/noema-jupyter-cell-kernel nil
  "Jupyter kernelspec recorded in the current notebook.")

(defvar-local my/noema-jupyter-cell-session nil
  "Noema session name recorded in the current notebook.")

(defvar-local my/noema-jupyter-cell-language nil
  "Jupyter language recorded in the current notebook.")

(defvar-local my/noema-jupyter-cell-storage nil
  "Storage mode for the current notebook; always ipynb.")

(defvar-local my/noema-jupyter-cell-kernel-spec nil
  "Resolved kernelspec sent by the owning Noema runtime.")

(defvar-local my/noema-jupyter-cell-kernel-spec-error nil
  "Kernelspec discovery error sent by Noema, when any.")

(defvar-local my/noema-jupyter-cell-current-id nil
  "Cell id at point in the current notebook source projection.")

(defvar-local my/noema-jupyter-cell--start-overlay nil
  "Overlay highlighting the current Jupyter cell marker.")

(defvar-local my/noema-jupyter-cell--saved-header-line-format nil
  "Header line active before Noema notebook controls were installed.")

(defun my/noema-jupyter-cell--header-line-installed-p ()
  "Return non-nil when this mode's header line is already the current one."
  (equal header-line-format
         '(:eval (my/noema-jupyter-cell--header-line))))

(defvar-local my/noema-jupyter-cell--kernel-status "not-started"
  "Last status reported by Noema's Jupyter service.")

(defvar-local my/noema-jupyter-cell--host-ready-requested nil
  "Non-nil while this notebook already awaits Noema web-host readiness.")

(defvar-local my/noema-jupyter-cell--session-refresh-pending nil
  "Non-nil while a session snapshot request is in flight.")

(defvar my/noema-jupyter-cell-mode)
(defvar my/noema--ready nil)
(defvar my/snippet-action-functions)

(defface my/noema-jupyter-cell-marker-face
  '((t :background "#1f6f43" :foreground "#eafff1" :weight bold))
  "Face for the active Noema Jupyter cell marker."
  :group 'my/noema)

(defvar-keymap my/noema-jupyter-cell-mode-map
  "C-c C-c" #'my/noema-jupyter-cell-run-current
  "C-c C-o" #'my/noema-jupyter-cell-jump-output
  "M-RET" #'my/noema-jupyter-cell-jump-output
  "s-RET" #'my/noema-jupyter-cell-jump-output
  "s-<return>" #'my/noema-jupyter-cell-jump-output
  "C-RET" #'my/noema-jupyter-cell-run-current
  "S-RET" #'my/noema-jupyter-cell-run-current-next
  "C-c C-r" #'my/noema-jupyter-cell-restart-run-all
  "C-c C-k" #'my/noema-jupyter-cell-interrupt
  "C-c C-p" #'my/noema-jupyter-output-page
  "C-c C-s" #'my/noema-jupyter-cell-sync-buffer
  "C-c C-i" #'my/noema-jupyter-cell-run-current
  "<backtab>" #'my/noema-jupyter-cell-inspect
  "C-c i n" #'my/noema-jupyter-cell-next
  "C-c i N" #'my/noema-jupyter-cell-previous
  "C-c i a" #'my/noema-jupyter-cell-insert-above
  "C-c i b" #'my/noema-jupyter-cell-insert-below
  "C-c i d" #'my/noema-jupyter-cell-delete
  "C-c i D" #'my/noema-jupyter-cell-duplicate
  "C-c i u" #'my/noema-jupyter-cell-move-up
  "C-c i o" #'my/noema-jupyter-cell-move-down
  "C-c i s" #'my/noema-jupyter-cell-split
  "C-c i m" #'my/noema-jupyter-cell-merge-above
  "C-c i M" #'my/noema-jupyter-cell-merge-below
  "C-c i A" #'my/noema-jupyter-cell-run-all
  "C-c i <" #'my/noema-jupyter-cell-run-above
  "C-c i >" #'my/noema-jupyter-cell-run-below
  "C-c i x" #'my/noema-jupyter-cell-clear-output
  "C-c i X" #'my/noema-jupyter-cell-clear-all-outputs
  "C-c i r" #'my/noema-jupyter-cell-restart
  "C-c i k" #'my/noema-jupyter-cell-shutdown
  "C-c i K" #'my/noema-jupyter-cell-select-kernel
  "C-c i l" #'my/noema-jupyter-cell-select-editor-mode
  "C-c i v" #'my/noema-jupyter-cell-variables
  "C-c i t" #'my/noema-jupyter-cell-tasks
  "C-c i ?" #'my/noema-jupyter-cell-command-menu)

(defconst my/noema-jupyter-cell--start-re
  "^[ \t]*\\(?://\\|--\\|#\\|;\\)[ \t]*%%\\(?:[ \t]+\\[\\(?:markdown\\|raw\\)\\]\\)?\\(?:[ \t]+id=\\([A-Za-z0-9_-]+\\)\\)?[ \t]*$"
  "Standard percent marker in an ipynb source projection.")

(defconst my/noema-jupyter-cell-snippet-actions
  '(("jcode" . my/noema-jupyter-cell-insert-code-below)
    ("jmd" . my/noema-jupyter-cell-insert-markdown-below)
    ("jabove" . my/noema-jupyter-cell-insert-code-above)
    ("jdup" . my/noema-jupyter-cell-duplicate)
    ("jsplit" . my/noema-jupyter-cell-split)
    ("jmerge" . my/noema-jupyter-cell-merge-below)
    ("jrun" . my/noema-jupyter-cell-run-current)
    ("jrunnext" . my/noema-jupyter-cell-run-current-next)
    ("jall" . my/noema-jupyter-cell-run-all)
    ("jrunabove" . my/noema-jupyter-cell-run-above)
    ("jrunbelow" . my/noema-jupyter-cell-run-below)
    ("jclear" . my/noema-jupyter-cell-clear-output)
    ("jclearall" . my/noema-jupyter-cell-clear-all-outputs)
    ("jout" . my/noema-jupyter-output-page)
    ("jvars" . my/noema-jupyter-cell-variables)
    ("jmanage" . my/noema-jupyter-cell-tasks)
    ("jkernel" . my/noema-jupyter-cell-select-kernel))
  "Snippet-like commands available in an Emacs ipynb source projection.")

(defun my/noema-jupyter-cell--snippet-token-bounds ()
  "Return bounds of the Jupyter action token immediately before point."
  (unless (use-region-p)
    (let ((end (point)) start)
      (save-excursion
        (skip-chars-backward "A-Za-z0-9_-" (line-beginning-position))
        (setq start (point)))
      (when (< start end) (cons start end)))))

(defun my/noema-jupyter-cell-expand-snippet-action ()
  "Expand a Jupyter action token through Noema and return non-nil.
This is an Emacs snippet-workflow provider, not a Yasnippet template.  A
recognized token is removed from the cell source before dispatch, while cell
creation and the new standard `cell.id' are owned by Noema's document API."
  (when-let* (((bound-and-true-p my/noema-jupyter-cell-mode))
              (bounds (my/noema-jupyter-cell--snippet-token-bounds))
              (token (buffer-substring-no-properties
                      (car bounds) (cdr bounds)))
              (command (alist-get token my/noema-jupyter-cell-snippet-actions
                                  nil nil #'string=)))
    (let ((start (copy-marker (car bounds)))
          (original token))
      (delete-region (car bounds) (cdr bounds))
      (condition-case err
          (progn
            ;; Structural and execution actions must observe the source after
            ;; the trigger itself has disappeared.
            (save-buffer)
            (call-interactively command)
            (set-marker start nil)
            t)
        (error
         (goto-char start)
         (insert original)
         (set-marker start nil)
         (signal (car err) (cdr err)))))))

(defun my/noema-jupyter-cell-snippet-menu ()
  "Select an Emacs Jupyter snippet action by its short trigger name."
  (interactive)
  (unless (bound-and-true-p my/noema-jupyter-cell-mode)
    (user-error "Jupyter actions are available in an ipynb source buffer"))
  (let* ((trigger (completing-read "Jupyter action: "
                                   my/noema-jupyter-cell-snippet-actions
                                   nil t))
         (command (alist-get trigger my/noema-jupyter-cell-snippet-actions
                            nil nil #'string=)))
    (call-interactively command)))

(defun my/noema-jupyter-cell--ensure-overlay ()
  "Return the buffer-local marker overlay."
  (or my/noema-jupyter-cell--start-overlay
      (setq my/noema-jupyter-cell--start-overlay
            (let ((overlay (make-overlay (point-min) (point-min) nil nil t)))
              (overlay-put overlay 'face 'my/noema-jupyter-cell-marker-face)
              (overlay-put overlay 'evaporate t)
              overlay))))

(defun my/noema-jupyter-cell--hide-overlay ()
  "Hide the current cell marker overlay."
  (when (overlayp my/noema-jupyter-cell--start-overlay)
    (delete-overlay my/noema-jupyter-cell--start-overlay)))

(defun my/noema-jupyter-cell--matched-id ()
  "Return the explicit or transient id on the marker in current match data."
  (or (match-string-no-properties 1)
      (get-text-property (line-beginning-position)
                         'my/noema-jupyter-cell-id)))

(defun my/noema-jupyter-cell--bounds-at-point ()
  "Return a plist describing the projected Jupyter cell around point."
  (save-excursion
    (save-restriction
      (widen)
      (let ((origin (point)) id block-beg start-end body-beg raw-end body-end)
        (goto-char (line-beginning-position))
        (when (or (looking-at my/noema-jupyter-cell--start-re)
                  (re-search-backward my/noema-jupyter-cell--start-re nil t))
          (setq id (my/noema-jupyter-cell--matched-id)
                block-beg (line-beginning-position)
                start-end (line-end-position)
                body-beg (progn (forward-line 1) (point))
                raw-end (if (re-search-forward my/noema-jupyter-cell--start-re nil t)
                            (line-beginning-position)
                          (point-max))
                body-end (my/noema-jupyter-notebook--body-end body-beg raw-end))
          (when (and (<= block-beg origin)
                     (or (< origin raw-end)
                         (and (= origin (point-max)) (= raw-end (point-max)))))
            (list :id id :start-beg block-beg :start-end start-end
                  :body-beg body-beg :body-end body-end
                  :block-beg block-beg :block-end raw-end)))))))

(defun my/noema-jupyter-cell--update-highlight ()
  "Update the local cell marker without contacting Noema."
  (if-let* ((bounds (my/noema-jupyter-cell--bounds-at-point)))
      (let ((overlay (my/noema-jupyter-cell--ensure-overlay)))
        (setq-local my/noema-jupyter-cell-current-id (plist-get bounds :id))
        (move-overlay overlay
                      (plist-get bounds :start-beg)
                      (plist-get bounds :start-end)))
    (setq-local my/noema-jupyter-cell-current-id nil)
    (my/noema-jupyter-cell--hide-overlay)))

(defun my/noema-jupyter-cell--command-detail (&optional cell-id)
  "Return Noema command detail for CELL-ID or the cell at point."
  (let ((cell-id (or cell-id
                     my/noema-jupyter-cell-current-id
                     (plist-get (my/noema-jupyter-cell--bounds-at-point) :id))))
    (unless (and cell-id (not (string-empty-p cell-id)))
      (user-error "Point is not inside a Noema Jupyter cell"))
    `((file . ,my/noema-jupyter-cell-source-file)
      (scriptFile . ,buffer-file-name)
      (cellId . ,cell-id)
      (kernel . ,(or my/noema-jupyter-cell-kernel ""))
      (session . ,(or my/noema-jupyter-cell-session ""))
      (language . ,(or my/noema-jupyter-cell-language ""))
      (storage . "ipynb"))))

(defun my/noema-jupyter-cell--document-detail ()
  "Return document parameters understood by Noema's Jupyter API."
  `((file . ,my/noema-jupyter-cell-source-file)
    (scriptFile . ,buffer-file-name)
    (sourceFile . ,my/noema-jupyter-cell-source-file)
    (kernel . ,(or my/noema-jupyter-cell-kernel ""))
    (session . ,(or my/noema-jupyter-cell-session ""))
    (language . ,(or my/noema-jupyter-cell-language ""))))

(defun my/noema-jupyter-cell--api-sync (channel body &optional timeout)
  "Synchronously call Noema CHANNEL with BODY."
  (unless (fboundp 'my/noema--api-call-sync)
    (user-error "Noema API bridge is unavailable"))
  (unless (bound-and-true-p my/noema--ready)
    (when (fboundp 'my/noema--ensure-server)
      (my/noema--ensure-server))
    (user-error "Noema Jupyter is starting; try again when the header is ready"))
  (or (my/noema--api-call-sync channel (vector body) timeout)
      (user-error "Noema Jupyter did not answer")))

(defun my/noema-jupyter-cell--api-async
    (channel body success-message &optional callback timeout)
  "Call Noema CHANNEL with BODY and report SUCCESS-MESSAGE.
Invoke CALLBACK with the decoded result after a successful response.
TIMEOUT is passed to `my/noema-api-call'; channels that run user code need
one long enough for the whole run, not the default request deadline."
  (unless (fboundp 'my/noema-api-call)
    (user-error "Noema API bridge is unavailable"))
  (let ((source-buffer (current-buffer)))
    (if (and (not (bound-and-true-p my/noema--ready))
             (fboundp 'my/noema--ensure-server))
        (progn
          (my/noema--ensure-server
           (lambda ()
             (when (buffer-live-p source-buffer)
               (with-current-buffer source-buffer
                 ;; Only retry once the host is actually up.  Recursing while
                 ;; it is still not ready would spin `my/noema--ensure-server'
                 ;; against itself.
                 (if (bound-and-true-p my/noema--ready)
                     (my/noema-jupyter-cell--api-async
                      channel body success-message callback timeout)
                   (message
                    "Noema Jupyter: web-host did not come up; %s was dropped"
                    channel))))))
          (message "Noema Jupyter: starting web-host…"))
      (my/noema-api-call
       channel (vector body)
       (lambda (result error-object)
         (when (buffer-live-p source-buffer)
           (with-current-buffer source-buffer
             (if error-object
                 (message "Noema Jupyter: %s"
                          (or (my/noema-jupyter-notebook--get
                               'message error-object)
                              "request failed"))
               (message "Noema Jupyter: %s" success-message)
               (when callback (funcall callback result))))))
       timeout))))

(defun my/noema-jupyter-cell--file-identity (file)
  "Return Noema's comparable logical identity for FILE."
  (when (and file (not (string-empty-p (format "%s" file))))
    (condition-case nil
        (if (fboundp 'my/noema--host-file)
            (my/noema--host-file (format "%s" file))
          (expand-file-name (format "%s" file)))
      (error (format "%s" file)))))

(defun my/noema-jupyter-cell--same-script-p (left right)
  "Return non-nil when LEFT and RIGHT name the same Noema notebook."
  (equal (my/noema-jupyter-cell--file-identity left)
         (my/noema-jupyter-cell--file-identity right)))

(defun my/noema-jupyter-cell--apply-session-snapshot (snapshot)
  "Apply Noema's authoritative document SNAPSHOT to the current buffer."
  (when-let* ((document (my/noema-jupyter-notebook--get 'document snapshot)))
    (let* ((kernel-value (my/noema-jupyter-notebook--get 'kernel document))
           (language-value (my/noema-jupyter-notebook--get 'language document))
           (session-value (my/noema-jupyter-notebook--get 'session document))
           (source-value (my/noema-jupyter-notebook--get 'sourceFile document))
           (kernel-text (and kernel-value (format "%s" kernel-value)))
           (kernel (and kernel-text
                        (not (string-empty-p kernel-text))
                        kernel-text))
           (language (and language-value (format "%s" language-value)))
           (runtime-changed
            (or (not (equal kernel my/noema-jupyter-cell-kernel))
                (and language
                     (not (equal language my/noema-jupyter-cell-language))))))
      (when (and runtime-changed
                 (fboundp 'my/noema-jupyter-cell-lsp-runtime-changing))
        (my/noema-jupyter-cell-lsp-runtime-changing))
      (setq-local my/noema-jupyter-cell-kernel kernel)
      (when language
        (setq-local my/noema-jupyter-cell-language language))
      (when session-value
        (setq-local my/noema-jupyter-cell-session
                    (format "%s" session-value)))
      (when (and source-value
                 (not (string-empty-p (format "%s" source-value))))
        (setq-local my/noema-jupyter-cell-source-file
                    (format "%s" source-value)))
      (setq-local my/noema-jupyter-cell--kernel-status
                  (format "%s"
                          (or (my/noema-jupyter-notebook--get
                               'kernelStatus snapshot)
                              "not-started")))
      (when runtime-changed
        (setq-local my/noema-jupyter-cell-kernel-spec nil)
        (setq-local my/noema-jupyter-cell-kernel-spec-error nil)
        (when (and (derived-mode-p 'prog-mode)
                   (fboundp 'my/language-server-ensure-deferred))
          (my/language-server-ensure-deferred)))
      ;; sessionSelect rewrites only notebook metadata.  Source saving merges
      ;; against the disk notebook, so accepting its new mtime here prevents a
      ;; false supersession prompt without discarding edited source.
      (when (and buffer-file-name (not (buffer-modified-p)))
        (ignore-errors (set-visited-file-modtime)))
      (force-mode-line-update t))))

(defun my/noema-jupyter-cell-handle-session-event (snapshot)
  "Apply a Noema Jupyter session SNAPSHOT to its open Emacs buffer."
  (let* ((document (my/noema-jupyter-notebook--get 'document snapshot))
         (script-file (my/noema-jupyter-notebook--get 'scriptFile document)))
    (when script-file
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (bound-and-true-p my/noema-jupyter-cell-mode)
                     buffer-file-name
                     (my/noema-jupyter-cell--same-script-p
                      buffer-file-name script-file))
            (my/noema-jupyter-cell--apply-session-snapshot snapshot)))))))

(defun my/noema-jupyter-cell-refresh-status ()
  "Refresh kernel/session state from Noema without creating a kernel."
  (interactive)
  (when (and my/noema-jupyter-cell-mode
             buffer-file-name
             (bound-and-true-p my/noema--ready)
             (fboundp 'my/noema-api-call)
             (not my/noema-jupyter-cell--session-refresh-pending))
    (setq-local my/noema-jupyter-cell--session-refresh-pending t)
    (let ((source-buffer (current-buffer)))
      (my/noema-api-call
       "aaronnote:api:jupyter:script-snapshot"
       (vector `((scriptFile . ,buffer-file-name)))
       (lambda (result error-object)
         (when (buffer-live-p source-buffer)
           (with-current-buffer source-buffer
             (setq-local my/noema-jupyter-cell--session-refresh-pending nil)
             (unless error-object
               (my/noema-jupyter-cell--apply-session-snapshot result)))))))))

(defun my/noema-jupyter-cell-refresh-open-buffers ()
  "Refresh open notebooks after the Noema gateway reports ready.
This is a one-shot reconnect reaction, never a timer or polling loop."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p my/noema-jupyter-cell-mode)
        (my/noema-jupyter-cell-refresh-status)))))

(defun my/noema-jupyter-cell--ensure-noema-host ()
  "Reuse or start Noema web-host once for this notebook buffer."
  (cond
   ((bound-and-true-p my/noema--ready)
    (setq-local my/noema-jupyter-cell--host-ready-requested nil)
    (my/noema-jupyter-cell-refresh-status))
   ((and (fboundp 'my/noema--ensure-server)
         (not my/noema-jupyter-cell--host-ready-requested))
    (setq-local my/noema-jupyter-cell--host-ready-requested t)
    (let ((source-buffer (current-buffer)))
      (my/noema--ensure-server
       (lambda ()
         (when (buffer-live-p source-buffer)
           (with-current-buffer source-buffer
             (setq-local my/noema-jupyter-cell--host-ready-requested nil)
             (my/noema-jupyter-cell-refresh-status)))))))))

(defun my/noema-jupyter-cell--kernel-choices (catalog)
  "Return Noema's canonical Web/Emacs kernel selections from CATALOG."
  (let ((entries
         (append
          (or (my/noema-jupyter-notebook--get 'selections catalog)
              (mapcar
               (lambda (entry)
                 `((kind . "start")
                   (value . ,(my/noema-jupyter-notebook--get 'name entry))
                   (name . ,(my/noema-jupyter-notebook--get 'name entry))
                   (displayName . ,(my/noema-jupyter-notebook--get
                                    'displayName entry))
                   (group . ,(or (my/noema-jupyter-notebook--get 'group entry)
                                 "Kernel Specs"))))
               (append
                (or (my/noema-jupyter-notebook--get 'choices catalog)
                    (append
                     (or (my/noema-jupyter-notebook--get 'kernels catalog) [])
                     (or (my/noema-jupyter-notebook--get 'attachable catalog) [])))
                nil)))
          nil))
        choices)
    (dolist (entry entries)
      (let* ((kind (format "%s" (or (my/noema-jupyter-notebook--get
                                      'kind entry)
                                     "start")))
             (value (format "%s" (or (my/noema-jupyter-notebook--get
                                       'value entry)
                                      (my/noema-jupyter-notebook--get
                                       'name entry)
                                      "")))
             (name (format "%s" (or (my/noema-jupyter-notebook--get
                                      'name entry)
                                     value)))
             (display (format "%s" (or (my/noema-jupyter-notebook--get
                                          'displayName entry)
                                         name)))
             (group (format "%s" (or (my/noema-jupyter-notebook--get
                                        'group entry)
                                       "Kernel Specs")))
             (label-value (my/noema-jupyter-notebook--get 'label entry))
             (label (if label-value
                        (format "%s" label-value)
                      (if (equal kind "none")
                          "No Kernel"
                        (format "%s · %s · %s  [%s]"
                                (capitalize kind) group display value)))))
        (when (or (equal kind "none") (not (string-empty-p value)))
          (push (cons label `((kind . ,kind) (value . ,value)))
                choices))))
    (nreverse choices)))

(defun my/noema-jupyter-cell-select-kernel ()
  "Select from the same Noema-owned kernel catalog used by the Web UI."
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (let* ((catalog (my/noema-jupyter-cell--api-sync
                   "aaronnote:api:jupyter-cell:kernels"
                   (my/noema-jupyter-cell--document-detail) 30))
         (choices (my/noema-jupyter-cell--kernel-choices catalog))
         (label (completing-read "Noema Jupyter kernel: " choices nil t))
         (choice (cdr (assoc label choices)))
         (kind (my/noema-jupyter-notebook--get 'kind choice))
         (value (my/noema-jupyter-notebook--get 'value choice))
         (body `((scriptFile . ,buffer-file-name)
                 (kind . ,kind)
                 ,@(pcase kind
                     ("start" `((kernelSpecName . ,value)))
                     ("connect" `((kernelId . ,value))))))
         (reply (my/noema-jupyter-cell--api-sync
                 "aaronnote:api:jupyter:session-select" body 60)))
    (my/noema-jupyter-cell--apply-session-snapshot reply)
    (message "Noema Jupyter: %s" label)))

(defcustom my/noema-jupyter-cell-execute-timeout 86400
  "Seconds to wait for a Noema execution or kernel-control reply.
Noema answers a run only after every cell in it has finished, so this is a
bound on how long a notebook may legitimately compute — not a health check.
The ordinary request deadline would report a running notebook as failed;
kernel death is detected by Noema\='s heartbeat instead."
  :type 'number
  :group 'my/noema)

(defun my/noema-jupyter-cell--action-body (action &optional require-cell extra)
  "Return Noema API body for ACTION.
When REQUIRE-CELL is non-nil, require the cell at point.  Append EXTRA fields."
  (my/noema-jupyter-cell--update-highlight)
  (let ((cell-id my/noema-jupyter-cell-current-id))
    (when (and require-cell (not cell-id))
      (user-error "Point is not inside a Noema Jupyter cell"))
    (append (my/noema-jupyter-cell--document-detail)
            `((action . ,action))
            (when cell-id `((cellId . ,cell-id)))
            extra)))

(defun my/noema-jupyter-cell--execute (mode &optional after)
  "Ask Noema to execute MODE and invoke AFTER immediately after dispatch."
  (when (buffer-modified-p) (save-buffer))
  (let* ((action (format "run-%s" mode))
         (needs-cell (not (equal mode "all")))
         (cell-id (and needs-cell
                       (plist-get (my/noema-jupyter-cell--bounds-at-point) :id))))
    (when needs-cell
      (my/noema-jupyter-output-open cell-id nil "outputs"))
    (my/noema-jupyter-cell--api-async
     "aaronnote:api:jupyter:script-action"
     (my/noema-jupyter-cell--action-body action needs-cell)
     (format "%s finished" action)
     nil my/noema-jupyter-cell-execute-timeout)
    (when after (funcall after))))

(defun my/noema-jupyter-cell-run-current ()
  "Run the current cell through Noema's Jupyter service."
  (interactive)
  (my/noema-jupyter-cell--execute "current"))

(defun my/noema-jupyter-cell-run-current-next ()
  "Run the current cell through Noema, then select the next cell."
  (interactive)
  (my/noema-jupyter-cell--execute "current" #'my/noema-jupyter-cell-next))

(defun my/noema-jupyter-cell-run-all ()
  "Run every cell through Noema in document order."
  (interactive)
  (my/noema-jupyter-cell--execute "all"))

(defun my/noema-jupyter-cell-run-above ()
  "Run the current cell and every cell above it through Noema."
  (interactive)
  (my/noema-jupyter-cell--execute "above"))

(defun my/noema-jupyter-cell-run-below ()
  "Run the current cell and every cell below it through Noema."
  (interactive)
  (my/noema-jupyter-cell--execute "below"))

(defun my/noema-jupyter-cell--control (action message-text)
  "Ask Noema to perform kernel ACTION and report MESSAGE-TEXT."
  (my/noema-jupyter-cell--api-async
   "aaronnote:api:jupyter:script-action"
   (my/noema-jupyter-cell--action-body action)
   message-text
   nil my/noema-jupyter-cell-execute-timeout))

(defun my/noema-jupyter-cell-interrupt ()
  "Interrupt this notebook's Noema-owned kernel."
  (interactive)
  (my/noema-jupyter-cell--control "interrupt" "interrupt requested"))

(defun my/noema-jupyter-cell-restart ()
  "Restart this notebook's Noema-owned kernel."
  (interactive)
  (my/noema-jupyter-cell--control "restart" "kernel restarted"))

(defun my/noema-jupyter-cell-restart-run-all ()
  "Restart this notebook's Noema-owned kernel and run every cell."
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (my/noema-jupyter-cell--api-async
   "aaronnote:api:jupyter:script-action"
   (my/noema-jupyter-cell--action-body "restart-run-all")
   "kernel restarted and all cells finished"))

(defun my/noema-jupyter-cell-shutdown ()
  "Shut down this notebook's Noema-owned kernel."
  (interactive)
  (my/noema-jupyter-cell--control "shutdown" "kernel shut down"))

(defun my/noema-jupyter-cell-open-outputs ()
  "Open Noema's Jupyter UI on the Outputs view."
  (interactive)
  (my/noema-jupyter-output-open my/noema-jupyter-cell-current-id t "outputs"))

(defun my/noema-jupyter-cell-open-variables ()
  "Open Noema's Jupyter UI on the Variables view."
  (interactive)
  (my/noema-jupyter-output-open my/noema-jupyter-cell-current-id t "variables"))

(defun my/noema-jupyter-cell-open-manager ()
  "Open Noema's Jupyter management UI."
  (interactive)
  (my/noema-jupyter-output-open my/noema-jupyter-cell-current-id t "manage"))

(defalias 'my/noema-jupyter-output-page #'my/noema-jupyter-cell-open-outputs)
(defalias 'my/noema-jupyter-cell-jump-output #'my/noema-jupyter-cell-open-outputs)
(defalias 'my/noema-jupyter-cell-variables #'my/noema-jupyter-cell-open-variables)
(defalias 'my/noema-jupyter-cell-tasks #'my/noema-jupyter-cell-open-manager)

(defun my/noema-jupyter-cell--header-button (label command help &optional enabled)
  "Return a header LABEL invoking COMMAND with HELP."
  (if (eq enabled 'disabled)
      (propertize (format " %s " label) 'face 'shadow)
    (let ((map (make-sparse-keymap)))
      (define-key map [header-line mouse-1] command)
      (propertize (format " %s " label)
                  'face 'mode-line-highlight
                  'mouse-face 'highlight
                  'help-echo help
                  'local-map map))))

(defun my/noema-jupyter-cell--editor-mode-choices ()
  "Return available display-name/major-mode choices for this notebook."
  (let* ((language (or my/noema-jupyter-cell-language
                       (my/noema-jupyter-notebook--language
                        my/noema-jupyter-notebook--document)
                       "python"))
         (default (my/noema-jupyter-notebook--major-mode language))
         (candidates
          `((,(format "Notebook default (%s)" default) . ,default)
            ("Python (Tree-sitter)" . python-ts-mode)
            ("Python" . python-mode)
            ("Shell" . sh-mode)
            ("JavaScript (Tree-sitter)" . js-ts-mode)
            ("TypeScript (Tree-sitter)" . typescript-ts-mode)
            ("Lean 4" . lean4-mode)
            ("Emacs Lisp" . emacs-lisp-mode)
            ("Text (LSP off)" . text-mode)
            ("Fundamental (LSP off)" . fundamental-mode))))
    (seq-uniq
     (seq-filter (lambda (entry) (fboundp (cdr entry))) candidates)
     (lambda (left right) (eq (cdr left) (cdr right))))))

(defun my/noema-jupyter-cell-select-editor-mode ()
  "Select the Emacs major mode, and therefore LSP hooks, for this ipynb.
This does not change the notebook language, kernelspec, or Noema session."
  (interactive)
  (let* ((choices (my/noema-jupyter-cell--editor-mode-choices))
         (selection (completing-read "Jupyter LSP/editor mode: "
                                     choices nil t))
         (mode (alist-get selection choices nil nil #'string=)))
    (my/noema-jupyter-notebook-switch-editor-mode mode)
    (message "Noema Jupyter: Emacs mode is %s; kernel unchanged" mode)))

(defun my/noema-jupyter-cell--header-line ()
  "Render Noema-owned Jupyter controls for the current notebook."
  (let ((cell (or my/noema-jupyter-cell-current-id
                  (plist-get (my/noema-jupyter-cell--bounds-at-point) :id))))
    (list
     (propertize " Noema Jupyter " 'face 'mode-line-buffer-id)
     (my/noema-jupyter-cell--header-button
      (format "Kernel:%s · %s"
              (or my/noema-jupyter-cell-kernel "No Kernel")
              my/noema-jupyter-cell--kernel-status)
      #'my/noema-jupyter-cell-select-kernel "Select a Noema-managed kernel")
     (my/noema-jupyter-cell--header-button
      "Run" #'my/noema-jupyter-cell-run-current "Run current cell in Noema"
      (unless cell 'disabled))
     (my/noema-jupyter-cell--header-button
      "All" #'my/noema-jupyter-cell-run-all "Run all cells in Noema")
     (my/noema-jupyter-cell--header-button
      "Stop" #'my/noema-jupyter-cell-interrupt "Interrupt Noema kernel")
     (my/noema-jupyter-cell--header-button
      "Restart" #'my/noema-jupyter-cell-restart "Restart Noema kernel")
     (my/noema-jupyter-cell--header-button
      "Outputs" #'my/noema-jupyter-cell-open-outputs "Open Noema outputs")
     (my/noema-jupyter-cell--header-button
      "Vars" #'my/noema-jupyter-cell-open-variables "Open Noema variables")
     (my/noema-jupyter-cell--header-button
      "Manage" #'my/noema-jupyter-cell-open-manager "Open Noema Jupyter manager"))))

(defun my/noema-jupyter-cell--post-command-h ()
  "Track the current Emacs cell locally without moving the Web workspace."
  (when my/noema-jupyter-cell-mode
    (my/noema-jupyter-cell--update-highlight)))

(defun my/noema-jupyter-cell--goto-id (cell-id)
  "Move point to CELL-ID's body and return non-nil when found."
  (goto-char (point-min))
  (let (found)
    (while (and (not found)
                (re-search-forward my/noema-jupyter-cell--start-re nil t))
      (when (equal (my/noema-jupyter-cell--matched-id) cell-id)
        (setq found t)))
    (when found
      (forward-line 1)
      (my/noema-jupyter-cell--update-highlight)
      t)))

(defun my/noema-jupyter-cell-next ()
  "Select the next cell in the source projection."
  (interactive)
  (let* ((bounds (or (my/noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Noema Jupyter cell")))
         (end (plist-get bounds :block-end)))
    (goto-char end)
    (when (looking-at my/noema-jupyter-cell--start-re)
      (forward-line 1))
    (my/noema-jupyter-cell--update-highlight)))

(defun my/noema-jupyter-cell-previous ()
  "Select the previous cell in the source projection."
  (interactive)
  (let* ((bounds (or (my/noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Noema Jupyter cell")))
         (start (plist-get bounds :start-beg)))
    (goto-char start)
    (when (re-search-backward my/noema-jupyter-cell--start-re nil t)
      (forward-line 1))
    (my/noema-jupyter-cell--update-highlight)))

(defun my/noema-jupyter-cell--reload-after-mutation (active-cell-id)
  "Reload the ipynb projection and select ACTIVE-CELL-ID."
  (revert-buffer :ignore-auto :noconfirm)
  (my/noema-jupyter-cell-activate-buffer)
  (if (and active-cell-id (not (string-empty-p active-cell-id)))
      (my/noema-jupyter-cell--goto-id active-cell-id)
    (goto-char (point-min))
    (my/noema-jupyter-cell--update-highlight)))

(defun my/noema-jupyter-cell--mutate (operation &optional extra)
  "Ask Noema to apply structural cell OPERATION with EXTRA fields."
  (when (buffer-modified-p) (save-buffer))
  (let* ((result (my/noema-jupyter-cell--api-sync
                  "aaronnote:api:jupyter:script-action"
                  (my/noema-jupyter-cell--action-body operation t extra)
                  30))
         (active (my/noema-jupyter-notebook--get 'activeCellId result)))
    (my/noema-jupyter-cell--reload-after-mutation
     (and active (format "%s" active)))
    (message "Noema Jupyter: %s" operation)
    result))

(defun my/noema-jupyter-cell-insert-above ()
  "Insert an empty cell above the current cell through Noema."
  (interactive)
  (my/noema-jupyter-cell-insert-code-above))

(defun my/noema-jupyter-cell-insert-below ()
  "Insert an empty cell below the current cell through Noema."
  (interactive)
  (my/noema-jupyter-cell-insert-code-below))

(defun my/noema-jupyter-cell-insert-code-above ()
  "Insert a standard code cell above through Noema.
Noema creates and persists the new cell's standard `cell.id'."
  (interactive)
  (my/noema-jupyter-cell--mutate
   "insertAbove" '((cellType . "code"))))

(defun my/noema-jupyter-cell-insert-code-below ()
  "Insert a standard code cell below through Noema.
Noema creates and persists the new cell's standard `cell.id'."
  (interactive)
  (my/noema-jupyter-cell--mutate
   "insertBelow" '((cellType . "code"))))

(defun my/noema-jupyter-cell-insert-markdown-below ()
  "Insert a standard Markdown cell below through Noema.
Noema creates and persists the new cell's standard `cell.id'."
  (interactive)
  (my/noema-jupyter-cell--mutate
   "insertBelow" '((cellType . "markdown"))))

(defun my/noema-jupyter-cell-duplicate ()
  "Duplicate the current cell through Noema."
  (interactive)
  (my/noema-jupyter-cell--mutate "duplicate"))

(defun my/noema-jupyter-cell-delete ()
  "Delete the current cell and its persisted output through Noema."
  (interactive)
  (let* ((bounds (or (my/noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Noema Jupyter cell")))
         (code (buffer-substring-no-properties
                (plist-get bounds :body-beg) (plist-get bounds :body-end))))
    (when (or (string-empty-p (string-trim code))
              (yes-or-no-p "Delete this non-empty Jupyter cell? "))
      (my/noema-jupyter-cell--mutate "delete"))))

(defun my/noema-jupyter-cell-move-up ()
  "Move the current cell upward through Noema."
  (interactive)
  (my/noema-jupyter-cell--mutate "moveUp"))

(defun my/noema-jupyter-cell-move-down ()
  "Move the current cell downward through Noema."
  (interactive)
  (my/noema-jupyter-cell--mutate "moveDown"))

(defun my/noema-jupyter-cell-split ()
  "Split the current cell at point through Noema."
  (interactive)
  (let* ((bounds (or (my/noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Noema Jupyter cell")))
         (offset (- (max (plist-get bounds :body-beg)
                         (min (point) (plist-get bounds :body-end)))
                    (plist-get bounds :body-beg))))
    (my/noema-jupyter-cell--mutate "split" `((offset . ,offset)))))

(defun my/noema-jupyter-cell-merge-above ()
  "Merge the current cell into the cell above through Noema."
  (interactive)
  (my/noema-jupyter-cell--mutate "mergeAbove"))

(defun my/noema-jupyter-cell-merge-below ()
  "Merge the cell below into the current cell through Noema."
  (interactive)
  (my/noema-jupyter-cell--mutate "mergeBelow"))

(defun my/noema-jupyter-cell-clear-output ()
  "Clear the current cell's persisted output through Noema."
  (interactive)
  (my/noema-jupyter-cell--api-async
   "aaronnote:api:jupyter:script-action"
   (my/noema-jupyter-cell--action-body "clear-output" t)
   "output cleared"))

(defun my/noema-jupyter-cell-clear-all-outputs ()
  "Clear every persisted output in this notebook through Noema."
  (interactive)
  (my/noema-jupyter-cell--api-async
   "aaronnote:api:jupyter:script-action"
   (my/noema-jupyter-cell--action-body "clear-all-outputs")
   "all outputs cleared"))

(defun my/noema-jupyter-cell-command-menu ()
  "Prompt for Noema Jupyter document and kernel UI commands."
  (interactive)
  (let* ((choices
          '(("Run Cell" . my/noema-jupyter-cell-run-current)
            ("Run Cell and Select Next" . my/noema-jupyter-cell-run-current-next)
            ("Run Above" . my/noema-jupyter-cell-run-above)
            ("Run Below" . my/noema-jupyter-cell-run-below)
            ("Run All" . my/noema-jupyter-cell-run-all)
            ("Open Output Page" . my/noema-jupyter-output-page)
            ("Insert Above" . my/noema-jupyter-cell-insert-above)
            ("Insert Below" . my/noema-jupyter-cell-insert-below)
            ("Duplicate" . my/noema-jupyter-cell-duplicate)
            ("Delete" . my/noema-jupyter-cell-delete)
            ("Move Up" . my/noema-jupyter-cell-move-up)
            ("Move Down" . my/noema-jupyter-cell-move-down)
            ("Split" . my/noema-jupyter-cell-split)
            ("Merge Above" . my/noema-jupyter-cell-merge-above)
            ("Merge Below" . my/noema-jupyter-cell-merge-below)
            ("Clear Output" . my/noema-jupyter-cell-clear-output)
            ("Clear All Outputs" . my/noema-jupyter-cell-clear-all-outputs)
            ("Select Kernel" . my/noema-jupyter-cell-select-kernel)
            ("Select Emacs LSP / Mode" . my/noema-jupyter-cell-select-editor-mode)
            ("Variables" . my/noema-jupyter-cell-variables)
            ("Manager / Tasks" . my/noema-jupyter-cell-tasks)
            ("Interrupt Kernel" . my/noema-jupyter-cell-interrupt)
            ("Restart Kernel" . my/noema-jupyter-cell-restart)
            ("Restart and Run All" . my/noema-jupyter-cell-restart-run-all)
            ("Shut Down Kernel" . my/noema-jupyter-cell-shutdown)))
         (selection (completing-read "Noema Jupyter: " choices nil t)))
    (call-interactively (alist-get selection choices nil nil #'equal))))

(defun my/noema-jupyter-cell--read-metadata ()
  "Read metadata from the current standard notebook.
An ordinary ipynb uses itself as its source document."
  (when buffer-file-name
    (let* ((metadata (my/noema-jupyter-notebook-metadata buffer-file-name))
           (source (plist-get metadata :source)))
      (plist-put metadata :source
                 (if (and source (not (string-empty-p source)))
                     source
                   buffer-file-name)))))

(defun my/noema-jupyter-cell-sync-buffer ()
  "Notify Noema that the notebook source projection was saved."
  (interactive)
  (unless (and my/noema-jupyter-cell-source-file
               (not (string-empty-p my/noema-jupyter-cell-source-file)))
    (user-error "This notebook is not linked to a Noema source note"))
  (my/noema-command
   "jupyter-cell-script-saved"
   `((file . ,my/noema-jupyter-cell-source-file)
     (scriptFile . ,buffer-file-name)
     (kernel . ,(or my/noema-jupyter-cell-kernel ""))
     (session . ,(or my/noema-jupyter-cell-session ""))
     (storage . "ipynb")))
  t)

(defun my/noema-jupyter-cell-after-save-h ()
  "Notify Noema after saving a Noema notebook projection."
  (when my/noema-jupyter-cell-mode
    (condition-case err
        (my/noema-jupyter-cell-sync-buffer)
      (error
       (message "Noema notebook sync failed: %s" (error-message-string err))))))

(defcustom my/noema-jupyter-cell-introspect-timeout 1.5
  "Seconds to wait for low-latency Noema kernel completion."
  :type 'number
  :group 'my/noema)

(defcustom my/noema-jupyter-cell-inspect-timeout 7
  "Seconds to wait for an explicit Noema kernel inspection.
The server permits inspect requests up to six seconds by default, so the
interactive bridge must allow enough time for that bounded reply."
  :type 'number
  :group 'my/noema)

(defun my/noema-jupyter-cell--json-true-p (object key)
  "Return non-nil when OBJECT's JSON KEY is true."
  (let ((value (my/noema-jupyter-notebook--get key object)))
    (and value (not (eq value :json-false)))))

(defun my/noema-jupyter-cell--inspect-expression (body-beg body-end)
  "Return the dotted expression at point between BODY-BEG and BODY-END."
  (save-restriction
    (narrow-to-region body-beg body-end)
    (let ((table (copy-syntax-table (syntax-table))))
      ;; Treat attribute separators as part of one inspectable expression, so
      ;; `np.array' is retried as a unit rather than only `array'.
      (modify-syntax-entry ?. "w" table)
      (with-syntax-table table
        (when-let* ((value (thing-at-point 'symbol t))
                    ((not (string-empty-p value))))
          value)))))

(defun my/noema-jupyter-cell--expression-before (position body-beg)
  "Return a dotted identifier ending before POSITION, after BODY-BEG."
  (save-excursion
    (goto-char position)
    (skip-chars-backward " \t\n\r" body-beg)
    (let ((end (point)))
      (skip-chars-backward "[:alnum:]_." body-beg)
      (let ((value (buffer-substring-no-properties (point) end)))
        (when (string-match-p
               "\\`[[:alpha:]_][[:alnum:]_]*\\(?:\\.[[:alpha:]_][[:alnum:]_]*\\)*\\'"
               value)
          value)))))

(defun my/noema-jupyter-cell--enclosing-call-expression (body-beg body-end)
  "Return the nearest enclosing call target between BODY-BEG and BODY-END.
This makes inspection inside arguments deterministic: point inside
`print(\"text\")' inspects `print', not the string contents."
  (save-restriction
    (narrow-to-region body-beg body-end)
    (let ((position (point))
          target)
      ;; Walk out through nested lists until an opening parenthesis with a
      ;; callable dotted identifier immediately before it is found.
      (while (and (not target) (> position (point-min)))
        (let ((open (nth 1 (syntax-ppss position))))
          (if (not open)
              (setq position (point-min))
            (when (eq (char-after open) ?\()
              (setq target
                    (my/noema-jupyter-cell--expression-before
                     open (point-min))))
            (setq position (max (point-min) (1- open))))))
      ;; Point is commonly left just after a completed call.  At that
      ;; position syntax-ppss is no longer inside the parentheses, so resolve
      ;; the immediately preceding balanced list explicitly.
      (unless target
        (save-excursion
          (goto-char (point))
          (skip-chars-backward " \t\n\r" (point-min))
          (when (eq (char-before) ?\))
            (condition-case nil
                (progn
                  (backward-list)
                  (when (eq (char-after) ?\()
                    (setq target
                          (my/noema-jupyter-cell--expression-before
                           (point) (point-min)))))
              (scan-error nil)))))
      target)))

(defun my/noema-jupyter-cell--python-literal-inspect-target (expression)
  "Return the Python type target appropriate for literal EXPRESSION."
  (when (and expression
             (string-match-p
              "python"
              (downcase (or my/noema-jupyter-cell-language ""))))
    (cond
     ((string-match-p "\\`[+-]?[0-9]+\\'" expression) "int")
     ((string-match-p
       "\\`[+-]?\\(?:[0-9]+\\.[0-9]*\\|[0-9]*\\.[0-9]+\\|[0-9]+[eE][+-]?[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?\\'"
       expression)
      "float")
     ((member expression '("True" "False")) "bool"))))

(defun my/noema-jupyter-cell--inspect-target (body-beg body-end)
  "Return the deterministic inspect target at point in the current cell."
  (let* ((state (syntax-ppss))
         (in-string (nth 3 state))
         (expression (unless (or in-string (nth 4 state))
                       (my/noema-jupyter-cell--inspect-expression
                        body-beg body-end))))
    (cond
     ;; Inside an argument string or another literal, documentation for the
     ;; enclosing callable is the useful and stable answer.
     (in-string
      (or (my/noema-jupyter-cell--enclosing-call-expression body-beg body-end)
          (and (string-match-p
                "python"
                (downcase (or my/noema-jupyter-cell-language "")))
               "str")))
     ((my/noema-jupyter-cell--python-literal-inspect-target expression))
     (expression)
     ((my/noema-jupyter-cell--enclosing-call-expression body-beg body-end)))))

(defun my/noema-jupyter-cell--introspect (kind extra &optional explicit)
  "Ask Noema's live kernel for introspection KIND with EXTRA fields.
This never starts a kernel as a typing side effect.  When EXPLICIT is non-nil,
surface transport errors and use the explicit-inspection timeout."
  (when-let* (((bound-and-true-p my/noema--ready))
              (bounds (my/noema-jupyter-cell--bounds-at-point))
              (body-beg (plist-get bounds :body-beg))
              (body-end (plist-get bounds :body-end))
              (inspect-target
               (or (and explicit
                        (equal kind "inspect")
                        (my/noema-jupyter-cell--inspect-target
                         body-beg body-end))
                   :no-inspect-target))
              (code (cond
                     ((stringp inspect-target) inspect-target)
                     ;; Never ask the kernel to guess from an entire cell for
                     ;; an explicit inspection.  That is what made identical
                     ;; C-c C-i presses sometimes return a nearby callable and
                     ;; sometimes `found=false'.
                     ((and explicit (equal kind "inspect")) "")
                     (t (buffer-substring-no-properties body-beg body-end))))
              (request
               (append
                (my/noema-jupyter-cell--document-detail)
                `((cellId . ,(plist-get bounds :id))
                  (code . ,code)
                  (cursorPos . ,(cond
                                 ((stringp inspect-target)
                                  (length inspect-target))
                                 ((and explicit (equal kind "inspect")) 0)
                                 (t
                                  (length
                                   (buffer-substring-no-properties
                                    body-beg (min (point) body-end)))))))
                extra))
              (channel (format "aaronnote:api:jupyter-cell:%s" kind))
              (timeout (if explicit
                           my/noema-jupyter-cell-inspect-timeout
                         my/noema-jupyter-cell-introspect-timeout))
              (call
               (lambda (body)
                 (condition-case err
                     (my/noema-jupyter-cell--api-sync channel body timeout)
                   (error
                    (if explicit
                        (user-error "Noema Jupyter %s failed: %s"
                                    kind (error-message-string err))
                      nil)))))
              (reply (funcall call request)))
    reply))

(defun my/noema-jupyter-cell-capf ()
  "Complete at point using Noema's live Jupyter kernel when available."
  (when my/noema-jupyter-cell-mode
    (when-let* ((bounds (my/noema-jupyter-cell--bounds-at-point))
                (body-beg (plist-get bounds :body-beg))
                (reply (my/noema-jupyter-cell--introspect "complete" nil))
                (matches (my/noema-jupyter-notebook--get 'matches reply))
                ((> (length matches) 0)))
      (let* ((items (append (or (my/noema-jupyter-notebook--get
                                  'items reply) []) nil))
             (start (+ body-beg (or (my/noema-jupyter-notebook--get
                                      'cursorStart reply) 0)))
             (end (+ body-beg (or (my/noema-jupyter-notebook--get
                                    'cursorEnd reply) 0)))
             (annotations (make-hash-table :test #'equal)))
        (dolist (item items)
          (when-let* ((candidate
                       (my/noema-jupyter-notebook--get 'text item)))
            (puthash
             candidate
             (string-trim
              (concat
               (format "%s" (or (my/noema-jupyter-notebook--get
                                   'signature item) ""))
               (when-let* ((type (my/noema-jupyter-notebook--get 'type item))
                           ((not (string-empty-p (format "%s" type)))))
                 (format " [%s]" type))))
             annotations)))
        (list (min start (point)) (max end (point))
              (append matches nil)
              :exclusive 'no
              :company-kind (lambda (_) 'text)
              :annotation-function
              (lambda (candidate)
                (when-let* ((detail (gethash candidate annotations))
                            ((not (string-empty-p detail))))
                  (concat " " detail))))))))

(defun my/noema-jupyter-cell-inspect (&optional detailed)
  "Show Noema kernel documentation for the symbol at point.
With DETAILED non-nil, request source-level detail."
  (interactive "P")
  (let* ((bounds (my/noema-jupyter-cell--bounds-at-point))
         (target (and bounds
                      (my/noema-jupyter-cell--inspect-target
                       (plist-get bounds :body-beg)
                       (plist-get bounds :body-end))))
         (reply (my/noema-jupyter-cell--introspect
                 "inspect" `((detailLevel . ,(if detailed 1 0))) t)))
    (cond
     ((null reply)
      (user-error "Noema Jupyter inspect did not receive a reply"))
     ((not (my/noema-jupyter-cell--json-true-p reply 'supported))
      (message "Noema Jupyter: no running kernel for this notebook"))
     ((not (my/noema-jupyter-cell--json-true-p reply 'found))
      (if target
          (message
           "Noema Jupyter: kernel has no runtime definition for `%s'; execute/import it first"
           target)
        (message "Noema Jupyter: no inspectable object at point")))
     (t
      (let* ((data (my/noema-jupyter-notebook--get 'data reply))
             (plain (my/noema-jupyter-notebook--get "text/plain" data))
             (content (format "%s" (or plain ""))))
        (if (string-empty-p content)
            (message "Noema Jupyter: kernel returned no documentation")
          (with-current-buffer (get-buffer-create "*Noema Jupyter Inspect*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert content)
              (ansi-color-apply-on-region (point-min) (point-max))
              (goto-char (point-min))
              (special-mode))
            (display-buffer (current-buffer)))
          ;; `display-buffer' does not replace an existing echo-area message.
          ;; Without this, a previous failed inspection remains visible even
          ;; though the current request succeeded and opened documentation.
          (message "Noema Jupyter: showing kernel documentation for `%s'"
                   (or target "object"))))))))

;;;###autoload
(define-minor-mode my/noema-jupyter-cell-mode
  "Minor mode that forwards notebook commands to Noema.
This mode never starts, connects to, or speaks the Jupyter protocol itself."
  :lighter " NCell"
  (if my/noema-jupyter-cell-mode
      (progn
        ;; `my/noema-jupyter-cell-activate-buffer' is called again on every
        ;; reload, so this branch runs while the mode is already on.  Only the
        ;; first pass may record the original header line; re-recording would
        ;; save this mode's own :eval form and leave it installed for good.
        (unless (my/noema-jupyter-cell--header-line-installed-p)
          (setq-local my/noema-jupyter-cell--saved-header-line-format
                      header-line-format))
        (setq-local header-line-format
                    '(:eval (my/noema-jupyter-cell--header-line)))
        (add-hook 'after-save-hook #'my/noema-jupyter-cell-after-save-h nil t)
        (add-hook 'post-command-hook #'my/noema-jupyter-cell--post-command-h nil t)
        (add-hook 'completion-at-point-functions
                  #'my/noema-jupyter-cell-capf -10 t)
        (add-hook 'my/snippet-action-functions
                  #'my/noema-jupyter-cell-expand-snippet-action nil t)
        (my/noema-jupyter-cell--update-highlight)
        (my/noema-jupyter-cell--ensure-noema-host))
    (remove-hook 'after-save-hook #'my/noema-jupyter-cell-after-save-h t)
    (remove-hook 'post-command-hook #'my/noema-jupyter-cell--post-command-h t)
    (remove-hook 'completion-at-point-functions
                 #'my/noema-jupyter-cell-capf t)
    (remove-hook 'my/snippet-action-functions
                 #'my/noema-jupyter-cell-expand-snippet-action t)
    (setq-local my/noema-jupyter-cell--session-refresh-pending nil)
    (setq-local header-line-format
                my/noema-jupyter-cell--saved-header-line-format)
    (my/noema-jupyter-cell--hide-overlay)))

(with-eval-after-load 'init-snippets
  (keymap-set my/snippet-map "j" #'my/noema-jupyter-cell-snippet-menu))

(defun my/noema-jupyter-cell--candidate-file-p ()
  "Return non-nil for any standard ipynb file."
  (when-let* ((file buffer-file-name))
    (string-match-p "\\.ipynb\\'" file)))

;;;###autoload
(defun my/noema-jupyter-cell-activate-buffer (&optional payload)
  "Enable Noema's Emacs UI for the current Noema-owned notebook.
PAYLOAD, when non-nil, is the originating Noema open event.  Activation only
installs editing controls and API forwarding; Noema remains the sole Jupyter
kernel and protocol owner."
  (interactive (list nil))
  (let ((get (lambda (key) (or (alist-get key payload)
                               (alist-get (symbol-name key) payload
                                          nil nil #'string=)))))
    (when (and payload buffer-file-name
               (not (buffer-modified-p))
               (not (verify-visited-file-modtime (current-buffer))))
      (revert-buffer :ignore-auto :noconfirm)
      (goto-char (point-min))
      (forward-line (max 0 (1- (truncate (or (funcall get 'line) 1)))))
      (move-to-column (max 0 (truncate (or (funcall get 'col) 0)))))
    (when-let* (((my/noema-jupyter-cell--candidate-file-p))
                (metadata (my/noema-jupyter-cell--read-metadata)))
      (setq-local my/noema-jupyter-cell-source-file (plist-get metadata :source))
      (setq-local my/noema-jupyter-cell-kernel
                  (or (funcall get 'kernel) (plist-get metadata :kernel)))
      (setq-local my/noema-jupyter-cell-session
                  (or (funcall get 'session) (plist-get metadata :session)))
      (setq-local my/noema-jupyter-cell-language (plist-get metadata :language))
      (setq-local my/noema-jupyter-cell-storage "ipynb")
      (setq-local my/noema-jupyter-cell-kernel-spec (funcall get 'kernelSpec))
      (setq-local my/noema-jupyter-cell-kernel-spec-error
                  (funcall get 'kernelSpecError))
      (my/noema-jupyter-cell-mode 1))))

(provide 'init-aaronnote-jupyter-cell)

;;; init-aaronnote-jupyter-cell.el ends here
