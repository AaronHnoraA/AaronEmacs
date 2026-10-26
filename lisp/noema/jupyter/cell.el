;;; noema/jupyter/cell.el --- Noema ipynb source buffers -*- lexical-binding: t -*-

;;; Commentary:
;; Noema owns Jupyter kernels, protocol traffic, execution, and outputs.
;; Emacs only edits the native source projection of Noema's standard ipynb and
;; forwards explicit editor commands to the running Noema application.

;;; Code:

(require 'cl-lib)
(require 'ansi-color)
(require 'seq)
(require 'subr-x)
(require 'aaron-open)
(require 'noema/jupyter/notebook)

(declare-function +noema-jupyter/open-project-directory "noema/jupyter/project" ())
(declare-function +noema-jupyter/open-project-file "noema/jupyter/project" ())
(declare-function +noema-jupyter/open-project-shell "noema/jupyter/project" ())
(declare-function +noema-jupyter/project-lsp "noema/jupyter/project" ())
(declare-function +noema-jupyter/project-inspect "noema/jupyter/project" ())
(declare-function +jupyter/ssh-open-project "tools/project/config" (&optional project))
(declare-function +noema/command "noema/bridge/config" (command &optional detail))
(declare-function +noema--ensure-server "noema/bridge/config" (&optional callback))
(declare-function +noema--api-call-sync "noema/bridge/config"
                  (channel args &optional timeout))
(declare-function +noema-api-call "noema/bridge/config" (channel args callback &optional timeout))
(declare-function +noema-jupyter/output-open "noema/bridge/config"
                  (&optional cell-id focus view))
(declare-function noema-research-accept-jupyter-runtime
                  "noema-research-mode" (snapshot))
(declare-function noema-research-goto-cell "noema-research-mode" (id))
(declare-function +noema--host-file "noema/bridge/config" (file))
(declare-function +language-server-ensure-deferred "tools/lsp/config" ())
(declare-function noema-project-root "noema-research" (path))
(declare-function noema-project-ensure "noema-research" (path))
(defvar +noema--notes-root)

(defvar-local +noema-jupyter-cell-source-file nil
  "Markdown note file that owns the current Noema notebook.")

(defvar-local +noema-jupyter-cell-kernel nil
  "Jupyter kernelspec recorded in the current notebook.")

(defvar-local +noema-jupyter-cell-session nil
  "Noema session name recorded in the current notebook.")

(defvar-local +noema-jupyter-cell-language nil
  "Jupyter language recorded in the current notebook.")
(defvar-local +noema-jupyter-cell--editor-mode-manual-p nil
  "Non-nil after the user explicitly chooses an editor mode.")

(defvar-local +noema-jupyter-cell-storage nil
  "Storage mode for the current notebook; always ipynb.")

(defvar-local +noema-jupyter-cell-kernel-spec nil
  "Resolved kernelspec sent by the owning Noema runtime.")

(defvar-local +noema-jupyter-cell-kernel-spec-error nil
  "Kernelspec discovery error sent by Noema, when any.")

(defvar-local +noema-jupyter-cell-current-id nil
  "Cell id at point in the current notebook source projection.")

(defvar-local +noema-jupyter-cell--start-overlay nil
  "Overlay highlighting the current Jupyter cell marker.")

(defvar-local +noema-jupyter-cell--saved-header-line-format nil
  "Header line active before Noema notebook controls were installed.")

(defun +noema-jupyter-cell--header-line-installed-p ()
  "Return non-nil when this mode's header line is already the current one."
  (equal header-line-format
         '(:eval (+noema-jupyter-cell--header-line))))

(defvar-local +noema-jupyter-cell--kernel-status "not-started"
  "Last status reported by Noema's Jupyter service.")

(defvar-local +noema-jupyter-cell--host-ready-requested nil
  "Non-nil while this notebook already awaits Noema web-host readiness.")

(defvar-local +noema-jupyter-cell--session-refresh-pending nil
  "Non-nil while a session snapshot request is in flight.")

(defvar +noema-jupyter-cell-mode)
(defvar +noema--ready nil)
(defvar +snippet-action-functions)

(defface +noema-jupyter-cell-marker-face
  '((t :background "#1f6f43" :foreground "#eafff1" :weight bold))
  "Face for the active Noema Jupyter cell marker."
  :group '+noema)

(defvar-keymap +noema-jupyter-cell-mode-map
  "C-c C-c" #'+noema-jupyter/cell-run-current
  "C-c C-o" #'+noema-jupyter/cell-jump-output
  "M-RET" #'+noema-jupyter/cell-jump-output
  "s-RET" #'+noema-jupyter/cell-jump-output
  "s-<return>" #'+noema-jupyter/cell-jump-output
  "C-RET" #'+noema-jupyter/cell-run-current
  "S-RET" #'+noema-jupyter/cell-run-current-next
  "C-c C-r" #'+noema-jupyter/cell-restart-run-all
  "C-c C-k" #'+noema-jupyter/cell-interrupt
  "C-c C-p" #'+noema-jupyter/output-page
  "C-c C-s" #'+noema-jupyter/cell-sync-buffer
  "C-c C-i" #'+noema-jupyter/cell-run-current
  "<backtab>" #'+noema-jupyter/cell-inspect
  "C-c i n" #'+noema-jupyter/cell-next
  "C-c i N" #'+noema-jupyter/cell-previous
  "C-c i a" #'+noema-jupyter/cell-insert-above
  "C-c i b" #'+noema-jupyter/cell-insert-below
  "C-c i d" #'+noema-jupyter/cell-delete
  "C-c i D" #'+noema-jupyter/cell-duplicate
  "C-c i u" #'+noema-jupyter/cell-move-up
  "C-c i o" #'+noema-jupyter/cell-move-down
  "C-c i s" #'+noema-jupyter/cell-split
  "C-c i m" #'+noema-jupyter/cell-merge-above
  "C-c i M" #'+noema-jupyter/cell-merge-below
  "C-c i A" #'+noema-jupyter/cell-run-all
  "C-c i <" #'+noema-jupyter/cell-run-above
  "C-c i >" #'+noema-jupyter/cell-run-below
  "C-c i x" #'+noema-jupyter/cell-clear-output
  "C-c i X" #'+noema-jupyter/cell-clear-all-outputs
  "C-c i r" #'+noema-jupyter/cell-restart
  "C-c i k" #'+noema-jupyter/cell-shutdown
  "C-c i K" #'+noema-jupyter/cell-select-kernel
  "C-c i l" #'+noema-jupyter-cell-select-editor-mode
  "C-c i v" #'+noema-jupyter/cell-variables
  "C-c i t" #'+noema-jupyter/cell-tasks
  "C-c i ?" #'+noema-jupyter/cell-command-menu)

(defconst +noema-jupyter-cell--start-re
  "^[ \t]*\\(?://\\|--\\|#\\|;\\)[ \t]*%%\\(?:[ \t]+\\[\\(?:markdown\\|raw\\)\\]\\)?\\(?:[ \t]+id=\\([A-Za-z0-9_-]+\\)\\)?[ \t]*$"
  "Standard percent marker in an ipynb source projection.")

(defconst +noema-jupyter-cell-snippet-actions
  '(("jcode" . +noema-jupyter/cell-insert-code-below)
    ("jmd" . +noema-jupyter/cell-insert-markdown-below)
    ("jraw" . +noema-jupyter/cell-insert-raw-below)
    ("jabove" . +noema-jupyter/cell-insert-code-above)
    ("jdup" . +noema-jupyter/cell-duplicate)
    ("jsplit" . +noema-jupyter/cell-split)
    ("jmerge" . +noema-jupyter/cell-merge-below)
    ("jrun" . +noema-jupyter/cell-run-current)
    ("jrunnext" . +noema-jupyter/cell-run-current-next)
    ("jall" . +noema-jupyter/cell-run-all)
    ("jrunabove" . +noema-jupyter/cell-run-above)
    ("jrunbelow" . +noema-jupyter/cell-run-below)
    ("jclear" . +noema-jupyter/cell-clear-output)
    ("jclearall" . +noema-jupyter/cell-clear-all-outputs)
    ("jout" . +noema-jupyter/output-page)
    ("jvars" . +noema-jupyter/cell-variables)
    ("jmanage" . +noema-jupyter/cell-tasks)
    ("jkernel" . +noema-jupyter/cell-select-kernel))
  "Snippet-like commands available in an Emacs ipynb source projection.")

(defun +noema-jupyter-cell--snippet-token-bounds ()
  "Return bounds of the Jupyter action token immediately before point."
  (unless (use-region-p)
    (let ((end (point)) start)
      (save-excursion
        (skip-chars-backward "A-Za-z0-9_-" (line-beginning-position))
        (setq start (point)))
      (when (< start end) (cons start end)))))

(defun +noema-jupyter-cell-expand-snippet-action ()
  "Expand a Jupyter action token through Noema and return non-nil.
This is an Emacs snippet-workflow provider, not a Yasnippet template.  A
recognized token is removed from the cell source before dispatch, while cell
creation and the new standard `cell.id' are owned by Noema's document API."
  (when-let* (((bound-and-true-p +noema-jupyter-cell-mode))
              (bounds (+noema-jupyter-cell--snippet-token-bounds))
              (token (buffer-substring-no-properties
                      (car bounds) (cdr bounds)))
              (command (alist-get token +noema-jupyter-cell-snippet-actions
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

(defun +noema-jupyter/cell-snippet-menu ()
  "Select an Emacs Jupyter snippet action by its short trigger name."
  (interactive)
  (unless (bound-and-true-p +noema-jupyter-cell-mode)
    (user-error "Jupyter actions are available in an ipynb source buffer"))
  (let* ((trigger (completing-read "Jupyter action: "
                                   +noema-jupyter-cell-snippet-actions
                                   nil t))
         (command (alist-get trigger +noema-jupyter-cell-snippet-actions
                            nil nil #'string=)))
    (call-interactively command)))

(defun +noema-jupyter-cell--ensure-overlay ()
  "Return the buffer-local marker overlay."
  (or +noema-jupyter-cell--start-overlay
      (setq +noema-jupyter-cell--start-overlay
            (let ((overlay (make-overlay (point-min) (point-min) nil nil t)))
              (overlay-put overlay 'face '+noema-jupyter-cell-marker-face)
              (overlay-put overlay 'evaporate t)
              overlay))))

(defun +noema-jupyter-cell--hide-overlay ()
  "Hide the current cell marker overlay."
  (when (overlayp +noema-jupyter-cell--start-overlay)
    (delete-overlay +noema-jupyter-cell--start-overlay)))

(defun +noema-jupyter-cell--matched-id ()
  "Return the explicit or transient id on the marker in current match data."
  (or (match-string-no-properties 1)
      (get-text-property (line-beginning-position)
                         '+noema-jupyter-cell-id)))

(defun +noema-jupyter-cell--bounds-at-point ()
  "Return a plist describing the projected Jupyter cell around point."
  (save-excursion
    (save-restriction
      (widen)
      (let ((origin (point)) id block-beg start-end body-beg raw-end body-end)
        (goto-char (line-beginning-position))
        (when (or (looking-at +noema-jupyter-cell--start-re)
                  (re-search-backward +noema-jupyter-cell--start-re nil t))
          (setq id (+noema-jupyter-cell--matched-id)
                block-beg (line-beginning-position)
                start-end (line-end-position)
                body-beg (progn (forward-line 1) (point))
                raw-end (if (re-search-forward +noema-jupyter-cell--start-re nil t)
                            (line-beginning-position)
                          (point-max))
                body-end (+noema-jupyter-notebook--body-end body-beg raw-end))
          (when (and (<= block-beg origin)
                     (or (< origin raw-end)
                         (and (= origin (point-max)) (= raw-end (point-max)))))
            (list :id id :start-beg block-beg :start-end start-end
                  :body-beg body-beg :body-end body-end
                  :block-beg block-beg :block-end raw-end)))))))

(defun +noema-jupyter-cell--update-highlight ()
  "Update the local cell marker without contacting Noema."
  (if-let* ((bounds (+noema-jupyter-cell--bounds-at-point)))
      (let ((overlay (+noema-jupyter-cell--ensure-overlay)))
        (setq-local +noema-jupyter-cell-current-id (plist-get bounds :id))
        (move-overlay overlay
                      (plist-get bounds :start-beg)
                      (plist-get bounds :start-end)))
    (setq-local +noema-jupyter-cell-current-id nil)
    (+noema-jupyter-cell--hide-overlay)))

(defun +noema-jupyter-cell--command-detail (&optional cell-id)
  "Return Noema command detail for CELL-ID or the cell at point."
  (let ((cell-id (or cell-id
                     +noema-jupyter-cell-current-id
                     (plist-get (+noema-jupyter-cell--bounds-at-point) :id))))
    (unless (and cell-id (not (string-empty-p cell-id)))
      (user-error "Point is not inside a Noema Jupyter cell"))
    `((file . ,+noema-jupyter-cell-source-file)
      (scriptFile . ,buffer-file-name)
      (cellId . ,cell-id)
      (kernel . ,(or +noema-jupyter-cell-kernel ""))
      (session . ,(or +noema-jupyter-cell-session ""))
      (language . ,(or +noema-jupyter-cell-language ""))
      (storage . "ipynb"))))

(defun +noema-jupyter-cell--document-detail ()
  "Return document parameters understood by Noema's Jupyter API."
  `((file . ,+noema-jupyter-cell-source-file)
    (scriptFile . ,buffer-file-name)
    (sourceFile . ,+noema-jupyter-cell-source-file)
    (kernel . ,(or +noema-jupyter-cell-kernel ""))
    (session . ,(or +noema-jupyter-cell-session ""))
    (language . ,(or +noema-jupyter-cell-language ""))))

(defvar-local +noema-jupyter-cell--project-root-cache nil
  "Noema project root found for the current notebook, once known.")

(defun +noema-jupyter-cell--project-root (&optional ensure)
  "Return the Noema project root that authorizes this notebook, or nil.
Noema accepts a native notebook outside its notes root only inside a
validated Noema project (a `noema.toml' ancestor).  When ENSURE is non-nil
and no project exists for such a notebook, `noema-project-ensure' asks where
to create one; quitting aborts the command.  Passive requests never ask."
  (when-let* ((file buffer-file-name)
              ;; Both logical and native TRAMP spellings use the Remote
              ;; gateway; neither belongs to the local research-project gate.
              ((not (or (string-prefix-p "/fs:" file)
                        (file-remote-p file)))))
    (require 'noema-research)
    (or +noema-jupyter-cell--project-root-cache
        (setq +noema-jupyter-cell--project-root-cache
              (or (noema-project-root file)
                  (and ensure
                       (not (and (bound-and-true-p +noema--notes-root)
                                 (file-in-directory-p
                                  file +noema--notes-root)))
                       (noema-project-ensure file)))))))

(defun +noema-jupyter-cell--with-project-root (body &optional ensure)
  "Return BODY with this notebook's `projectRoot' when one applies.
Project remote file identities for the gateway without changing the buffer.
ENSURE is passed to `+noema-jupyter-cell--project-root'."
  (setq body (copy-tree body))
  (dolist (key '(file scriptFile sourceFile))
    (when-let* ((value (alist-get key body))
                ((stringp value))
                ((file-remote-p value)))
      (setf (alist-get key body) (+noema--host-file value))))
  (if-let* (((not (assq 'projectRoot body)))
            (root (+noema-jupyter-cell--project-root ensure)))
      (append body `((projectRoot . ,(expand-file-name root))))
    body))

(defun +noema-jupyter-cell--api-sync (channel body &optional timeout passive)
  "Synchronously call Noema CHANNEL with BODY.
PASSIVE requests, such as keystroke introspection, never ask to create a
Noema project for the notebook."
  (unless (fboundp '+noema--api-call-sync)
    (user-error "Noema API bridge is unavailable"))
  (unless (bound-and-true-p +noema--ready)
    (when (fboundp '+noema--ensure-server)
      (+noema--ensure-server))
    (user-error "Noema Jupyter is starting; try again when the header is ready"))
  (setq body (+noema-jupyter-cell--with-project-root body (not passive)))
  (or (+noema--api-call-sync channel (vector body) timeout)
      (user-error "Noema Jupyter did not answer")))

(defun +noema-jupyter-cell--api-async
    (channel body success-message &optional callback timeout)
  "Call Noema CHANNEL with BODY and report SUCCESS-MESSAGE.
Invoke CALLBACK with the decoded result after a successful response.
TIMEOUT is passed to `+noema-api-call'; channels that run user code need
one long enough for the whole run, not the default request deadline."
  (unless (fboundp '+noema-api-call)
    (user-error "Noema API bridge is unavailable"))
  (setq body (+noema-jupyter-cell--with-project-root body t))
  (let ((source-buffer (current-buffer)))
    (if (and (not (bound-and-true-p +noema--ready))
             (fboundp '+noema--ensure-server))
        (progn
          (+noema--ensure-server
           (lambda ()
             (when (buffer-live-p source-buffer)
               (with-current-buffer source-buffer
                 ;; Only retry once the host is actually up.  Recursing while
                 ;; it is still not ready would spin `+noema--ensure-server'
                 ;; against itself.
                 (if (bound-and-true-p +noema--ready)
                     (+noema-jupyter-cell--api-async
                      channel body success-message callback timeout)
                   (message
                    "Noema Jupyter: web-host did not come up; %s was dropped"
                    channel))))))
          (message "Noema Jupyter: starting web-host…"))
      (+noema-api-call
       channel (vector body)
       (lambda (result error-object)
         (when (buffer-live-p source-buffer)
           (with-current-buffer source-buffer
             (if error-object
                 (message "Noema Jupyter: %s"
                          (or (+noema-jupyter-notebook--get
                               'message error-object)
                              "request failed"))
               (message "Noema Jupyter: %s" success-message)
               (when callback (funcall callback result))))))
       timeout))))

(defun +noema-jupyter-cell--file-identity (file)
  "Return Noema's comparable logical identity for FILE."
  (when (and file (not (string-empty-p (format "%s" file))))
    (condition-case nil
        (if (fboundp '+noema--host-file)
            (+noema--host-file (format "%s" file))
          (expand-file-name (format "%s" file)))
      (error (format "%s" file)))))

(defun +noema-jupyter-cell--same-script-p (left right)
  "Return non-nil when LEFT and RIGHT name the same Noema notebook."
  (equal (+noema-jupyter-cell--file-identity left)
         (+noema-jupyter-cell--file-identity right)))

(defun +noema-jupyter-cell--auto-switch-language-mode (language)
  "Align the source mode with LANGUAGE unless the user chose one explicitly.
Kernel selection controls the default editor/LSP family, while the explicit
Editor/LSP command remains an escape hatch for mixed-language notebooks."
  (when (and (not +noema-jupyter-cell--editor-mode-manual-p)
             (stringp language))
    (let ((mode (+noema-jupyter-notebook--major-mode language)))
      (when (and (fboundp mode)
                 (not (derived-mode-p mode)))
        (let ((state (list +noema-jupyter-cell-kernel
                           +noema-jupyter-cell-session
                           +noema-jupyter-cell-language
                           +noema-jupyter-cell-source-file
                           +noema-jupyter-cell-kernel-spec
                           +noema-jupyter-cell-kernel-spec-error
                           +noema-jupyter-cell--kernel-status)))
          (when +noema-jupyter-cell-mode
            (+noema-jupyter-cell-mode -1))
          (funcall mode)
          (setq-local +noema-jupyter-cell-kernel (nth 0 state)
                      +noema-jupyter-cell-session (nth 1 state)
                      +noema-jupyter-cell-language (nth 2 state)
                      +noema-jupyter-cell-source-file (nth 3 state)
                      +noema-jupyter-cell-kernel-spec (nth 4 state)
                      +noema-jupyter-cell-kernel-spec-error (nth 5 state)
                      +noema-jupyter-cell--kernel-status (nth 6 state))
          (+noema-jupyter-cell-mode 1))))))

(defun +noema-jupyter-cell--apply-session-snapshot (snapshot)
  "Apply Noema's authoritative document SNAPSHOT to the current buffer."
  (when-let* ((document (+noema-jupyter-notebook--get 'document snapshot)))
    (let* ((kernel-value (+noema-jupyter-notebook--get 'kernel document))
           (language-value (+noema-jupyter-notebook--get 'language document))
           (session-value (+noema-jupyter-notebook--get 'session document))
           (source-value (+noema-jupyter-notebook--get 'sourceFile document))
           (kernel-text (and kernel-value (format "%s" kernel-value)))
           (kernel (and kernel-text
                        (not (string-empty-p kernel-text))
                        kernel-text))
           ;; A gateway event may omit language while still carrying a newly
           ;; selected kernel.  Derive it from that kernel instead of keeping
           ;; the previous buffer-local Python value.
           (language (or (and language-value (format "%s" language-value))
                         (and kernel
                              (+noema-jupyter-notebook--language-for-kernel
                               kernel))))
           (runtime-changed
            (or (not (equal kernel +noema-jupyter-cell-kernel))
                (and language
                     (not (equal language +noema-jupyter-cell-language))))))
      ;; The language server follows the notebook file and its buffer-local
      ;; environment.  Selecting a different execution kernel leaves it alone.
      (setq-local +noema-jupyter-cell-kernel kernel)
      (when language
        (setq-local +noema-jupyter-cell-language language))
      (when session-value
        (setq-local +noema-jupyter-cell-session
                    (format "%s" session-value)))
      (when (and source-value
                 (not (string-empty-p (format "%s" source-value))))
        (setq-local +noema-jupyter-cell-source-file
                    (format "%s" source-value)))
      (when (and runtime-changed language)
        (+noema-jupyter-cell--auto-switch-language-mode language))
      (setq-local +noema-jupyter-cell--kernel-status
                  (format "%s"
                          (or (+noema-jupyter-notebook--get
                               'kernelStatus snapshot)
                              "not-started")))
      (when runtime-changed
        (setq-local +noema-jupyter-cell-kernel-spec nil)
        (setq-local +noema-jupyter-cell-kernel-spec-error nil)
        (when (and (derived-mode-p 'prog-mode)
                   (fboundp '+language-server-ensure-deferred))
          (+language-server-ensure-deferred)))
      ;; sessionSelect rewrites only notebook metadata.  Source saving merges
      ;; against the disk notebook, so accepting its new mtime here prevents a
      ;; false supersession prompt without discarding edited source.
      (when (and buffer-file-name (not (buffer-modified-p)))
        (ignore-errors (set-visited-file-modtime)))
      (force-mode-line-update t))))

(defun +noema-jupyter-cell-handle-session-event (snapshot)
  "Apply a Noema Jupyter session SNAPSHOT to its open Emacs buffer."
  (let* ((document (+noema-jupyter-notebook--get 'document snapshot))
         (script-file (+noema-jupyter-notebook--get 'scriptFile document)))
    (when script-file
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and buffer-file-name
                     (+noema-jupyter-cell--same-script-p
                      buffer-file-name script-file))
            (cond
             ((bound-and-true-p +noema-jupyter-cell-mode)
              (+noema-jupyter-cell--apply-session-snapshot snapshot))
             ((and (derived-mode-p 'noema-research-mode)
                   (fboundp 'noema-research-accept-jupyter-runtime))
              (noema-research-accept-jupyter-runtime snapshot)))))))))

(defun +noema-jupyter/cell-refresh-status ()
  "Refresh kernel/session state from Noema without creating a kernel."
  (interactive)
  (when (and +noema-jupyter-cell-mode
             buffer-file-name
             (bound-and-true-p +noema--ready)
             (fboundp '+noema-api-call)
             (not +noema-jupyter-cell--session-refresh-pending))
    (setq-local +noema-jupyter-cell--session-refresh-pending t)
    (let ((source-buffer (current-buffer)))
      (+noema-api-call
       "noema:api:jupyter:script-snapshot"
       (vector (+noema-jupyter-cell--with-project-root
                `((scriptFile . ,buffer-file-name))))
       (lambda (result error-object)
         (when (buffer-live-p source-buffer)
           (with-current-buffer source-buffer
             (setq-local +noema-jupyter-cell--session-refresh-pending nil)
             (unless error-object
               (+noema-jupyter-cell--apply-session-snapshot result)))))))))

(defun +noema-jupyter-cell-refresh-open-buffers ()
  "Refresh open notebooks after the Noema gateway reports ready.
This is a one-shot reconnect reaction, never a timer or polling loop."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p +noema-jupyter-cell-mode)
        (+noema-jupyter/cell-refresh-status)))))

(defun +noema-jupyter-cell--ensure-noema-host ()
  "Reuse or start Noema web-host once for this notebook buffer."
  (cond
   ((bound-and-true-p +noema--ready)
    (setq-local +noema-jupyter-cell--host-ready-requested nil)
    (+noema-jupyter/cell-refresh-status))
   ((and (fboundp '+noema--ensure-server)
         (not +noema-jupyter-cell--host-ready-requested))
    (setq-local +noema-jupyter-cell--host-ready-requested t)
    (let ((source-buffer (current-buffer)))
      (+noema--ensure-server
       (lambda ()
         (when (buffer-live-p source-buffer)
           (with-current-buffer source-buffer
             (setq-local +noema-jupyter-cell--host-ready-requested nil)
             (+noema-jupyter/cell-refresh-status)))))))))

(defun +noema-jupyter-cell--kernel-choices (catalog)
  "Return Noema's canonical Web/Emacs kernel selections from CATALOG."
  (let ((entries
         (append
          (or (+noema-jupyter-notebook--get 'selections catalog)
              (mapcar
               (lambda (entry)
                 `((kind . "start")
                   (value . ,(+noema-jupyter-notebook--get 'name entry))
                   (name . ,(+noema-jupyter-notebook--get 'name entry))
                   (displayName . ,(+noema-jupyter-notebook--get
                                    'displayName entry))
                   (group . ,(or (+noema-jupyter-notebook--get 'group entry)
                                 "Kernel Specs"))))
               (append
                (or (+noema-jupyter-notebook--get 'choices catalog)
                    (append
                     (or (+noema-jupyter-notebook--get 'kernels catalog) [])
                     (or (+noema-jupyter-notebook--get 'attachable catalog) [])))
                nil)))
          nil))
        choices)
    (dolist (entry entries)
      (let* ((kind (format "%s" (or (+noema-jupyter-notebook--get
                                      'kind entry)
                                     "start")))
             (value (format "%s" (or (+noema-jupyter-notebook--get
                                       'value entry)
                                      (+noema-jupyter-notebook--get
                                       'name entry)
                                      "")))
             (name (format "%s" (or (+noema-jupyter-notebook--get
                                      'name entry)
                                     value)))
             (display (format "%s" (or (+noema-jupyter-notebook--get
                                          'displayName entry)
                                         name)))
             (group (format "%s" (or (+noema-jupyter-notebook--get
                                        'group entry)
                                       "Kernel Specs")))
             (label-value (+noema-jupyter-notebook--get 'label entry))
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

(defun +noema-jupyter/cell-select-kernel (&optional connections-only kernel-name)
  "Select a kernel, or an existing connection when CONNECTIONS-ONLY is non-nil.
When KERNEL-NAME is given, select that discovered kernelspec directly."
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (let* ((catalog (+noema-jupyter-cell--api-sync
                   "noema:api:jupyter-cell:kernels"
                   (append (+noema-jupyter-cell--document-detail)
                           (when connections-only '((includeConnections . t)))) 30))
         (entries (+noema-jupyter-cell--kernel-choices catalog))
         (choices
          (if connections-only
              (cl-remove-if-not
               (lambda (entry) (string-prefix-p "attach:" (alist-get 'value (cdr entry)))) entries)
            (append
             (cl-remove-if (lambda (entry) (string-prefix-p "attach:" (alist-get 'value (cdr entry)))) entries)
             (when (eq t (+noema-jupyter-notebook--get 'supportsConnectionFiles catalog))
               '(("Connect to Existing Kernel…" (kind . "browse-connections")))))))
         (_ (unless choices (user-error "No Jupyter kernels are available")))
         (label (if kernel-name
                    (or (car (seq-find
                              (lambda (item)
                                (and (equal (+noema-jupyter-notebook--get
                                             'value (cdr item)) kernel-name)
                                     (equal (+noema-jupyter-notebook--get
                                             'kind (cdr item)) "start")))
                              choices))
                        (user-error "Kernel %s is unavailable for this notebook" kernel-name))
                  (completing-read "Noema Jupyter kernel: " choices nil t)))
         (choice (cdr (assoc label choices)))
         (kind (+noema-jupyter-notebook--get 'kind choice))
         (value (+noema-jupyter-notebook--get 'value choice))
         (body `((scriptFile . ,buffer-file-name)
                 (kind . ,kind)
                 ,@(pcase kind
                     ("start" `((kernelSpecName . ,value)))
                     ("connect" `((kernelId . ,value))))))
         (reply (unless (equal kind "browse-connections")
                  (+noema-jupyter-cell--api-sync
                   "noema:api:jupyter:session-select" body 60))))
    (if (equal kind "browse-connections")
        (+noema-jupyter/cell-select-kernel t)
      (+noema-jupyter-cell--apply-session-snapshot reply)
      (message "Noema Jupyter: %s" label))))

(defcustom +noema-jupyter-cell-execute-timeout 86400
  "Seconds to wait for a Noema execution or kernel-control reply.
Noema answers a run only after every cell in it has finished, so this is a
bound on how long a notebook may legitimately compute — not a health check.
The ordinary request deadline would report a running notebook as failed;
kernel death is detected by Noema\='s heartbeat instead."
  :type 'number
  :group '+noema)

(defun +noema-jupyter-cell--action-body (action &optional require-cell extra)
  "Return Noema API body for ACTION.
When REQUIRE-CELL is non-nil, require the cell at point.  Append EXTRA fields."
  (+noema-jupyter-cell--update-highlight)
  (let ((cell-id +noema-jupyter-cell-current-id))
    (when (and require-cell (not cell-id))
      (user-error "Point is not inside a Noema Jupyter cell"))
    (append (+noema-jupyter-cell--document-detail)
            `((action . ,action))
            (when cell-id `((cellId . ,cell-id)))
            extra)))

(defun +noema-jupyter-cell--execute (mode &optional after)
  "Ask Noema to execute MODE and invoke AFTER immediately after dispatch."
  (when (buffer-modified-p) (save-buffer))
  (let ((action (format "run-%s" mode))
        (needs-cell (not (equal mode "all"))))
    (+noema-jupyter-cell--api-async
     "noema:api:jupyter:script-action"
     (+noema-jupyter-cell--action-body action needs-cell)
     (format "%s finished" action)
     nil +noema-jupyter-cell-execute-timeout)
    (when after (funcall after))))

(defun +noema-jupyter/cell-run-current ()
  "Run the current cell through Noema's Jupyter service."
  (interactive)
  (+noema-jupyter-cell--execute "current"))

(defun +noema-jupyter/cell-run-current-next ()
  "Run the current cell through Noema, then select the next cell."
  (interactive)
  (+noema-jupyter-cell--execute "current" #'+noema-jupyter/cell-next))

(defun +noema-jupyter/cell-run-all ()
  "Run every cell through Noema in document order."
  (interactive)
  (+noema-jupyter-cell--execute "all"))

(defun +noema-jupyter/cell-run-above ()
  "Run the current cell and every cell above it through Noema."
  (interactive)
  (+noema-jupyter-cell--execute "above"))

(defun +noema-jupyter/cell-run-below ()
  "Run the current cell and every cell below it through Noema."
  (interactive)
  (+noema-jupyter-cell--execute "below"))

(defun +noema-jupyter-cell--control (action message-text)
  "Ask Noema to perform kernel ACTION and report MESSAGE-TEXT."
  (+noema-jupyter-cell--api-async
   "noema:api:jupyter:script-action"
   (+noema-jupyter-cell--action-body action)
   message-text
   nil +noema-jupyter-cell-execute-timeout))

(defun +noema-jupyter/cell-interrupt ()
  "Interrupt this notebook's Noema-owned kernel."
  (interactive)
  (+noema-jupyter-cell--control "interrupt" "interrupt requested"))

(defun +noema-jupyter/cell-restart ()
  "Restart this notebook's Noema-owned kernel."
  (interactive)
  (+noema-jupyter-cell--control "restart" "kernel restarted"))

(defun +noema-jupyter/cell-restart-run-all ()
  "Restart this notebook's Noema-owned kernel and run every cell."
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (+noema-jupyter-cell--api-async
   "noema:api:jupyter:script-action"
   (+noema-jupyter-cell--action-body "restart-run-all")
   "kernel restarted and all cells finished"))

(defun +noema-jupyter/cell-shutdown ()
  "Shut down this notebook's Noema-owned kernel."
  (interactive)
  (+noema-jupyter-cell--control "shutdown" "kernel shut down"))

(defun +noema-jupyter/cell-open-outputs ()
  "Open Noema's Jupyter UI on the Outputs view."
  (interactive)
  (+noema-jupyter/output-open +noema-jupyter-cell-current-id t "outputs"))

(defun +noema-jupyter/cell-open-variables ()
  "Open Noema's Jupyter UI on the Variables view."
  (interactive)
  (+noema-jupyter/output-open +noema-jupyter-cell-current-id t "variables"))

(defun +noema-jupyter/cell-open-manager ()
  "Open Noema's Jupyter management UI."
  (interactive)
  (+noema-jupyter/output-open +noema-jupyter-cell-current-id t "manage"))

(defalias '+noema-jupyter/output-page #'+noema-jupyter/cell-open-outputs)
(defalias '+noema-jupyter/cell-jump-output #'+noema-jupyter/cell-open-outputs)
(defalias '+noema-jupyter/cell-variables #'+noema-jupyter/cell-open-variables)
(defalias '+noema-jupyter/cell-tasks #'+noema-jupyter/cell-open-manager)

(defun +noema-jupyter-cell--header-button (label command help &optional enabled)
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

(defun +noema-jupyter-cell--editor-mode-choices ()
  "Return available display-name/major-mode choices for this notebook."
  (let* ((language (or +noema-jupyter-cell-language
                       (+noema-jupyter-notebook--language
                        +noema-jupyter-notebook--document)
                       "python"))
         (default (+noema-jupyter-notebook--major-mode language))
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

(defun +noema-jupyter-cell-select-editor-mode ()
  "Select the Emacs major mode, and therefore LSP hooks, for this ipynb.
This does not change the notebook language, kernelspec, or Noema session."
  (interactive)
  (let* ((choices (+noema-jupyter-cell--editor-mode-choices))
         (selection (completing-read "Jupyter LSP/editor mode: "
                                     choices nil t))
         (mode (alist-get selection choices nil nil #'string=)))
    (+noema-jupyter-notebook-switch-editor-mode mode)
    (setq-local +noema-jupyter-cell--editor-mode-manual-p t)
    (message "Noema Jupyter: Emacs mode is %s; kernel unchanged" mode)))

(defun +noema-jupyter-cell--header-line ()
  "Render Noema-owned Jupyter controls for the current notebook."
  (let ((cell (or +noema-jupyter-cell-current-id
                  (plist-get (+noema-jupyter-cell--bounds-at-point) :id)))
        (lsp-description
         (cond ((bound-and-true-p lsp-managed-mode) "connected")
               ((bound-and-true-p +lsp-mode--waiting-for-direnv) "loading env")
               ((bound-and-true-p +language-server--waiting-for-runtime) "preparing")
               ((bound-and-true-p lsp--buffer-deferred) "starting")
               (t "project env"))))
    (list
     (propertize " Noema Jupyter " 'face 'mode-line-buffer-id)
     (+noema-jupyter-cell--header-button
      (format "Kernel:%s · %s"
              (or +noema-jupyter-cell-kernel "No Kernel")
              (if (equal +noema-jupyter-cell--kernel-status "error")
                  "last run error"
                +noema-jupyter-cell--kernel-status))
      #'+noema-jupyter/cell-select-kernel "Select a Noema-managed kernel")
     (+noema-jupyter-cell--header-button
      "Files" #'+noema-jupyter/open-project-directory "Open the configured execution project in Dired")
     (+noema-jupyter-cell--header-button
      "Shell" #'+noema-jupyter/open-project-shell "Open a shell at the execution project root")
     (+noema-jupyter-cell--header-button
      "Project" #'+noema-jupyter/project-inspect "Inspect configured target, root and Python")
     (when lsp-description
       (+noema-jupyter-cell--header-button
        (format "LSP:%s" (truncate-string-to-width lsp-description 28 nil nil t))
        #'+noema-jupyter/project-lsp
        (concat lsp-description "; restart LSP in this file's environment")))
     (+noema-jupyter-cell--header-button
      "Run" #'+noema-jupyter/cell-run-current "Run current cell in Noema"
      (unless cell 'disabled))
     (+noema-jupyter-cell--header-button
      "All" #'+noema-jupyter/cell-run-all "Run all cells in Noema")
     (+noema-jupyter-cell--header-button
      "Debug" #'+noema-jupyter/debug-start "Debug this cell with Dape breakpoints")
     (+noema-jupyter-cell--header-button
      "Step" #'+noema-jupyter/run-by-line "Run by Line: start or advance one statement")
     (+noema-jupyter-cell--header-button
      "Stop" (if (bound-and-true-p +noema-jupyter-debug--id)
                  #'+noema-jupyter/debug-stop #'+noema-jupyter/cell-interrupt)
      "Stop the active debug run or interrupt the kernel")
     (when (bound-and-true-p +noema-jupyter-debug--id)
       (+noema-jupyter-cell--header-button
        "Continue" #'+noema-jupyter/debug-continue "Continue the paused notebook"))
     (+noema-jupyter-cell--header-button
      "Restart" #'+noema-jupyter/cell-restart "Restart Noema kernel")
     (+noema-jupyter-cell--header-button
      "Outputs" #'+noema-jupyter/cell-open-outputs "Open Noema outputs")
     (+noema-jupyter-cell--header-button
      "Vars" #'+noema-jupyter/cell-open-variables "Open Noema variables")
     (+noema-jupyter-cell--header-button
      "Manage" #'+noema-jupyter/cell-open-manager "Open Noema Jupyter manager"))))

(defun +noema-jupyter-cell--post-command-h ()
  "Track the current Emacs cell locally without moving the Web workspace."
  (when +noema-jupyter-cell-mode
    (+noema-jupyter-cell--update-highlight)))

(defun +noema-jupyter-cell--goto-id (cell-id)
  "Move point to CELL-ID's body and return non-nil when found."
  (goto-char (point-min))
  (let (found)
    (while (and (not found)
                (re-search-forward +noema-jupyter-cell--start-re nil t))
      (when (equal (+noema-jupyter-cell--matched-id) cell-id)
        (setq found t)))
    (when found
      (forward-line 1)
      (+noema-jupyter-cell--update-highlight)
      t)))

(defun +noema-jupyter-cell--source-buffer (file)
  "Return the existing editor buffer for Noema's host FILE, or visit it.
Remote buffers can have a logical Emacs name while Noema knows their host
path, so compare both identities before opening a new buffer."
  (or (find-buffer-visiting file)
      (seq-find
       (lambda (buffer)
         (with-current-buffer buffer
           (and buffer-file-name
                (fboundp '+noema--host-file)
                (equal (ignore-errors (+noema--host-file buffer-file-name))
                       file))))
       (buffer-list))
      (aaron-open-visit-file-noselect (aaron-open-resolve-file-name file))))

(defun +noema-jupyter-cell-select-source (payload)
  "Visit the source Cell identified by PAYLOAD from the Web output renderer.
The renderer sends stable identity, never a projected line number.  Emacs then
uses the active document mode to resolve that identity into an editable
location."
  (let ((file (alist-get 'scriptFile payload))
        (cell-id (alist-get 'cellId payload)))
    (unless (and (stringp file) (not (string-empty-p file))
                 (stringp cell-id) (not (string-empty-p cell-id)))
      (user-error "Noema output source selection needs a file and cell ID"))
    (let* ((buffer (+noema-jupyter-cell--source-buffer file))
           (window (or (get-buffer-window buffer (selected-frame))
                       (get-buffer-window buffer 'visible)
                       (display-buffer
                        buffer
                        '((display-buffer-reuse-window
                           display-buffer-use-some-window
                           display-buffer-pop-up-window)
                          (inhibit-same-window . t))))))
      (unless (window-live-p window)
        (user-error "Could not display Noema source buffer %s" file))
      (unless (eq (window-frame window) (selected-frame))
        (select-frame-set-input-focus (window-frame window)))
      (select-window window)
      (with-current-buffer buffer
        (cond
         ((derived-mode-p 'noema-research-mode)
          (unless (fboundp 'noema-research-goto-cell)
            (user-error "Noema research navigation is unavailable"))
          (noema-research-goto-cell cell-id))
         ((bound-and-true-p +noema-jupyter-cell-mode)
          (unless (+noema-jupyter-cell--goto-id cell-id)
            (user-error "Jupyter cell %s is not present in %s" cell-id file)))
         (t
          (user-error "%s is not an editable Noema/Jupyter source buffer" file)))))))

(defun +noema-jupyter/cell-next ()
  "Select the next cell in the source projection."
  (interactive)
  (let* ((bounds (or (+noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Noema Jupyter cell")))
         (end (plist-get bounds :block-end)))
    (goto-char end)
    (when (looking-at +noema-jupyter-cell--start-re)
      (forward-line 1))
    (+noema-jupyter-cell--update-highlight)))

(defun +noema-jupyter/cell-previous ()
  "Select the previous cell in the source projection."
  (interactive)
  (let* ((bounds (or (+noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Noema Jupyter cell")))
         (start (plist-get bounds :start-beg)))
    (goto-char start)
    (when (re-search-backward +noema-jupyter-cell--start-re nil t)
      (forward-line 1))
    (+noema-jupyter-cell--update-highlight)))

(defun +noema-jupyter-cell--reload-after-mutation (active-cell-id)
  "Reload the ipynb projection and select ACTIVE-CELL-ID."
  (revert-buffer :ignore-auto :noconfirm)
  (+noema-jupyter/cell-activate-buffer)
  (if (and active-cell-id (not (string-empty-p active-cell-id)))
      (+noema-jupyter-cell--goto-id active-cell-id)
    (goto-char (point-min))
    (+noema-jupyter-cell--update-highlight)))

(defun +noema-jupyter-cell--mutate (operation &optional extra)
  "Ask Noema to apply structural cell OPERATION with EXTRA fields."
  (when (buffer-modified-p) (save-buffer))
  (let* ((result (+noema-jupyter-cell--api-sync
                  "noema:api:jupyter:script-action"
                  (+noema-jupyter-cell--action-body operation t extra)
                  30))
         (active (+noema-jupyter-notebook--get 'activeCellId result)))
    (+noema-jupyter-cell--reload-after-mutation
     (and active (format "%s" active)))
    (message "Noema Jupyter: %s" operation)
    result))

(defun +noema-jupyter/cell-insert-above ()
  "Insert an empty cell above the current cell through Noema."
  (interactive)
  (+noema-jupyter/cell-insert-code-above))

(defun +noema-jupyter/cell-insert-below ()
  "Insert an empty cell below the current cell through Noema."
  (interactive)
  (+noema-jupyter/cell-insert-code-below))

(defun +noema-jupyter/cell-insert-code-above ()
  "Insert a standard code cell above through Noema.
Noema creates and persists the new cell's standard `cell.id'."
  (interactive)
  (+noema-jupyter-cell--mutate
   "insertAbove" '((cellType . "code"))))

(defun +noema-jupyter/cell-insert-code-below ()
  "Insert a standard code cell below through Noema.
Noema creates and persists the new cell's standard `cell.id'."
  (interactive)
  (+noema-jupyter-cell--mutate
   "insertBelow" '((cellType . "code"))))

(defun +noema-jupyter/cell-insert-markdown-below ()
  "Insert a standard Markdown cell below through Noema.
Noema creates and persists the new cell's standard `cell.id'."
  (interactive)
  (+noema-jupyter-cell--mutate
   "insertBelow" '((cellType . "markdown"))))

(defun +noema-jupyter/cell-insert-raw-below ()
  "Insert a standard raw cell below through Noema."
  (interactive)
  (+noema-jupyter-cell--mutate
   "insertBelow" '((cellType . "raw"))))

(defun +noema-jupyter/cell-duplicate ()
  "Duplicate the current cell through Noema."
  (interactive)
  (+noema-jupyter-cell--mutate "duplicate"))

(defun +noema-jupyter/cell-delete ()
  "Delete the current cell and its persisted output through Noema."
  (interactive)
  (let* ((bounds (or (+noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Noema Jupyter cell")))
         (code (buffer-substring-no-properties
                (plist-get bounds :body-beg) (plist-get bounds :body-end))))
    (when (or (string-empty-p (string-trim code))
              (yes-or-no-p "Delete this non-empty Jupyter cell? "))
      (+noema-jupyter-cell--mutate "delete"))))

(defun +noema-jupyter/cell-move-up ()
  "Move the current cell upward through Noema."
  (interactive)
  (+noema-jupyter-cell--mutate "moveUp"))

(defun +noema-jupyter/cell-move-down ()
  "Move the current cell downward through Noema."
  (interactive)
  (+noema-jupyter-cell--mutate "moveDown"))

(defun +noema-jupyter/cell-split ()
  "Split the current cell at point through Noema."
  (interactive)
  (let* ((bounds (or (+noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Noema Jupyter cell")))
         (offset (- (max (plist-get bounds :body-beg)
                         (min (point) (plist-get bounds :body-end)))
                    (plist-get bounds :body-beg))))
    (+noema-jupyter-cell--mutate "split" `((offset . ,offset)))))

(defun +noema-jupyter/cell-merge-above ()
  "Merge the current cell into the cell above through Noema."
  (interactive)
  (+noema-jupyter-cell--mutate "mergeAbove"))

(defun +noema-jupyter/cell-merge-below ()
  "Merge the cell below into the current cell through Noema."
  (interactive)
  (+noema-jupyter-cell--mutate "mergeBelow"))

(defun +noema-jupyter/cell-clear-output ()
  "Clear the current cell's persisted output through Noema."
  (interactive)
  (+noema-jupyter-cell--api-async
   "noema:api:jupyter:script-action"
   (+noema-jupyter-cell--action-body "clear-output" t)
   "output cleared"))

(defun +noema-jupyter/cell-clear-all-outputs ()
  "Clear every persisted output in this notebook through Noema."
  (interactive)
  (+noema-jupyter-cell--api-async
   "noema:api:jupyter:script-action"
   (+noema-jupyter-cell--action-body "clear-all-outputs")
   "all outputs cleared"))

(defun +noema-jupyter/cell-command-menu ()
  "Prompt for Noema Jupyter document and kernel UI commands."
  (interactive)
  (let* ((choices
          '(("Run Cell" . +noema-jupyter/cell-run-current)
            ("Project Files (Dired)" . +noema-jupyter/open-project-directory)
            ("Open Project File" . +noema-jupyter/open-project-file)
            ("Open SSH Jupyter Project" . +jupyter/ssh-open-project)
            ("Project Shell" . +noema-jupyter/open-project-shell)
            ("Project LSP / Restart" . +noema-jupyter/project-lsp)
            ("Inspect Project Environment" . +noema-jupyter/project-inspect)
            ("Export Notebook" . +noema-jupyter/export)
            ("Compare Notebook" . +noema-jupyter/compare)
            ("Debug Cell" . +noema-jupyter/debug-start)
            ("Run by Line / Next Statement" . +noema-jupyter/run-by-line)
            ("Continue Debugging" . +noema-jupyter/debug-continue)
            ("Stop Debugging" . +noema-jupyter/debug-stop)
            ("Run Cell and Select Next" . +noema-jupyter/cell-run-current-next)
            ("Run Above" . +noema-jupyter/cell-run-above)
            ("Run Below" . +noema-jupyter/cell-run-below)
            ("Run All" . +noema-jupyter/cell-run-all)
            ("Open Output Page" . +noema-jupyter/output-page)
            ("Insert Above" . +noema-jupyter/cell-insert-above)
            ("Insert Below" . +noema-jupyter/cell-insert-below)
            ("Insert Markdown" . +noema-jupyter/cell-insert-markdown-below)
            ("Insert Raw" . +noema-jupyter/cell-insert-raw-below)
            ("Duplicate" . +noema-jupyter/cell-duplicate)
            ("Delete" . +noema-jupyter/cell-delete)
            ("Move Up" . +noema-jupyter/cell-move-up)
            ("Move Down" . +noema-jupyter/cell-move-down)
            ("Split" . +noema-jupyter/cell-split)
            ("Merge Above" . +noema-jupyter/cell-merge-above)
            ("Merge Below" . +noema-jupyter/cell-merge-below)
            ("Clear Output" . +noema-jupyter/cell-clear-output)
            ("Clear All Outputs" . +noema-jupyter/cell-clear-all-outputs)
            ("Select Kernel" . +noema-jupyter/cell-select-kernel)
            ("Select Emacs LSP / Mode" . +noema-jupyter-cell-select-editor-mode)
            ("Variables" . +noema-jupyter/cell-variables)
            ("Manager / Tasks" . +noema-jupyter/cell-tasks)
            ("Interrupt Kernel" . +noema-jupyter/cell-interrupt)
            ("Restart Kernel" . +noema-jupyter/cell-restart)
            ("Restart and Run All" . +noema-jupyter/cell-restart-run-all)
            ("Shut Down Kernel" . +noema-jupyter/cell-shutdown)))
         (selection (completing-read "Noema Jupyter: " choices nil t)))
    (call-interactively (alist-get selection choices nil nil #'equal))))

(defun +noema-jupyter-cell--read-metadata ()
  "Read metadata from the current standard notebook.
An ordinary ipynb uses itself as its source document."
  (when buffer-file-name
    (let* ((metadata (+noema-jupyter-notebook-metadata buffer-file-name))
           (source (plist-get metadata :source)))
      (plist-put metadata :source
                 (if (and source (not (string-empty-p source)))
                     source
                   buffer-file-name)))))

(defun +noema-jupyter/cell-sync-buffer ()
  "Notify Noema that the notebook source projection was saved."
  (interactive)
  (unless (and +noema-jupyter-cell-source-file
               (not (string-empty-p +noema-jupyter-cell-source-file)))
    (user-error "This notebook is not linked to a Noema source note"))
  (+noema/command
   "jupyter-cell-script-saved"
   (+noema-jupyter-cell--with-project-root
    `((file . ,+noema-jupyter-cell-source-file)
      (scriptFile . ,buffer-file-name)
      (kernel . ,(or +noema-jupyter-cell-kernel ""))
      (session . ,(or +noema-jupyter-cell-session ""))
      (storage . "ipynb"))))
  t)

(defun +noema-jupyter-cell-after-save-h ()
  "Notify Noema after saving a Noema notebook projection."
  (when +noema-jupyter-cell-mode
    (condition-case err
        (+noema-jupyter/cell-sync-buffer)
      (error
       (message "Noema notebook sync failed: %s" (error-message-string err))))))

(defcustom +noema-jupyter-cell-introspect-timeout 1.5
  "Seconds to wait for low-latency Noema kernel completion."
  :type 'number
  :group '+noema)

(defcustom +noema-jupyter-cell-inspect-timeout 7
  "Seconds to wait for an explicit Noema kernel inspection.
The server permits inspect requests up to six seconds by default, so the
interactive bridge must allow enough time for that bounded reply."
  :type 'number
  :group '+noema)

(defcustom +noema-jupyter-cell-introspect-backoff 20
  "Seconds to stop asking a kernel for completions after it misses the deadline.
A kernel busy in a long cell cannot answer `complete_request\=', and asking it
again on every keystroke only adds latency to typing."
  :type 'number
  :group '+noema)

(defvar-local +noema-jupyter-cell--introspect-backoff-until nil
  "Time before which implicit introspection is skipped in this buffer.")

(defun +noema-jupyter-cell--introspect-backoff-p ()
  "Return non-nil while this buffer's kernel is being left alone."
  (and +noema-jupyter-cell--introspect-backoff-until
       (time-less-p (current-time)
                    +noema-jupyter-cell--introspect-backoff-until)))

(defun +noema-jupyter-cell--introspect-failed ()
  "Back off implicit introspection after a failed or timed-out request."
  (setq-local +noema-jupyter-cell--introspect-backoff-until
              (time-add (current-time)
                        +noema-jupyter-cell-introspect-backoff)))

(defun +noema-jupyter-cell--introspect-succeeded ()
  "Clear any introspection backoff after the kernel answers again."
  (setq-local +noema-jupyter-cell--introspect-backoff-until nil))

(defun +noema-jupyter-cell--json-true-p (object key)
  "Return non-nil when OBJECT's JSON KEY is true."
  (let ((value (+noema-jupyter-notebook--get key object)))
    (and value (not (eq value :json-false)))))

(defun +noema-jupyter-cell--inspect-expression (body-beg body-end)
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

(defun +noema-jupyter-cell--expression-before (position body-beg)
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

(defun +noema-jupyter-cell--enclosing-call-expression (body-beg body-end)
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
                    (+noema-jupyter-cell--expression-before
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
                          (+noema-jupyter-cell--expression-before
                           (point) (point-min)))))
              (scan-error nil)))))
      target)))

(defun +noema-jupyter-cell--python-literal-inspect-target (expression)
  "Return the Python type target appropriate for literal EXPRESSION."
  (when (and expression
             (string-match-p
              "python"
              (downcase (or +noema-jupyter-cell-language ""))))
    (cond
     ((string-match-p "\\`[+-]?[0-9]+\\'" expression) "int")
     ((string-match-p
       "\\`[+-]?\\(?:[0-9]+\\.[0-9]*\\|[0-9]*\\.[0-9]+\\|[0-9]+[eE][+-]?[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?\\'"
       expression)
      "float")
     ((member expression '("True" "False")) "bool"))))

(defun +noema-jupyter-cell--inspect-target (body-beg body-end)
  "Return the deterministic inspect target at point in the current cell."
  (let* ((state (syntax-ppss))
         (in-string (nth 3 state))
         (expression (unless (or in-string (nth 4 state))
                       (+noema-jupyter-cell--inspect-expression
                        body-beg body-end))))
    (cond
     ;; Inside an argument string or another literal, documentation for the
     ;; enclosing callable is the useful and stable answer.
     (in-string
      (or (+noema-jupyter-cell--enclosing-call-expression body-beg body-end)
          (and (string-match-p
                "python"
                (downcase (or +noema-jupyter-cell-language "")))
               "str")))
     ((+noema-jupyter-cell--python-literal-inspect-target expression))
     (expression)
     ((+noema-jupyter-cell--enclosing-call-expression body-beg body-end)))))

(defun +noema-jupyter-cell--introspect (kind extra &optional explicit)
  "Ask Noema's live kernel for introspection KIND with EXTRA fields.
This never starts a kernel as a typing side effect.  When EXPLICIT is non-nil,
surface transport errors and use the explicit-inspection timeout."
  (when-let* (((bound-and-true-p +noema--ready))
              (bounds (+noema-jupyter-cell--bounds-at-point))
              (body-beg (plist-get bounds :body-beg))
              (body-end (plist-get bounds :body-end))
              (inspect-target
               (or (and explicit
                        (equal kind "inspect")
                        (+noema-jupyter-cell--inspect-target
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
                (+noema-jupyter-cell--document-detail)
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
              (channel (format "noema:api:jupyter-cell:%s" kind))
              (timeout (if explicit
                           +noema-jupyter-cell-inspect-timeout
                         +noema-jupyter-cell-introspect-timeout))
              ((or explicit
                   (not (+noema-jupyter-cell--introspect-backoff-p))))
              (call
               (lambda (body)
                 (condition-case err
                     (if explicit
                         (+noema-jupyter-cell--api-sync channel body timeout)
                       ;; Implicit introspection runs on a keystroke, and the
                       ;; bridge is synchronous: without this, a busy kernel
                       ;; freezes the editor for the whole timeout on every
                       ;; character.  `while-no-input' abandons the request the
                       ;; moment the user types again, and a kernel that keeps
                       ;; missing the deadline is left alone for a while
                       ;; instead of being asked once per keystroke.
                       (let ((reply
                              (while-no-input
                                (+noema-jupyter-cell--api-sync
                                 channel body timeout t))))
                         (if (eq reply t)
                             nil
                           (+noema-jupyter-cell--introspect-succeeded)
                           reply)))
                   (error
                    (if explicit
                        (user-error "Noema Jupyter %s failed: %s"
                                    kind (error-message-string err))
                      (+noema-jupyter-cell--introspect-failed)
                      nil)))))
              (reply (funcall call request)))
    reply))

(defun +noema-jupyter-cell-capf ()
  "Complete at point using Noema's live Jupyter kernel when available."
  (when +noema-jupyter-cell-mode
    (when-let* ((bounds (+noema-jupyter-cell--bounds-at-point))
                (body-beg (plist-get bounds :body-beg))
                (reply (+noema-jupyter-cell--introspect "complete" nil))
                (matches (+noema-jupyter-notebook--get 'matches reply))
                ((> (length matches) 0)))
      (let* ((items (append (or (+noema-jupyter-notebook--get
                                  'items reply) []) nil))
             (start (+ body-beg (or (+noema-jupyter-notebook--get
                                      'cursorStart reply) 0)))
             (end (+ body-beg (or (+noema-jupyter-notebook--get
                                    'cursorEnd reply) 0)))
             (annotations (make-hash-table :test #'equal)))
        (dolist (item items)
          (when-let* ((candidate
                       (+noema-jupyter-notebook--get 'text item)))
            (puthash
             candidate
             (string-trim
              (concat
               (format "%s" (or (+noema-jupyter-notebook--get
                                   'signature item) ""))
               (when-let* ((type (+noema-jupyter-notebook--get 'type item))
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

(defun +noema-jupyter/cell-inspect (&optional detailed)
  "Show Noema kernel documentation for the symbol at point.
With DETAILED non-nil, request source-level detail."
  (interactive "P")
  (let* ((bounds (+noema-jupyter-cell--bounds-at-point))
         (target (and bounds
                      (+noema-jupyter-cell--inspect-target
                       (plist-get bounds :body-beg)
                       (plist-get bounds :body-end))))
         (reply (+noema-jupyter-cell--introspect
                 "inspect" `((detailLevel . ,(if detailed 1 0))) t)))
    (cond
     ((null reply)
      (user-error "Noema Jupyter inspect did not receive a reply"))
     ((not (+noema-jupyter-cell--json-true-p reply 'supported))
      (message "Noema Jupyter: no running kernel for this notebook"))
     ((not (+noema-jupyter-cell--json-true-p reply 'found))
      (if target
          (message
           "Noema Jupyter: kernel has no runtime definition for `%s'; execute/import it first"
           target)
        (message "Noema Jupyter: no inspectable object at point")))
     (t
      (let* ((data (+noema-jupyter-notebook--get 'data reply))
             (plain (+noema-jupyter-notebook--get "text/plain" data))
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
(define-minor-mode +noema-jupyter-cell-mode
  "Minor mode that forwards notebook commands to Noema.
This mode never starts, connects to, or speaks the Jupyter protocol itself."
  :lighter " NCell"
  (if +noema-jupyter-cell-mode
      (progn
        ;; `+noema-jupyter/cell-activate-buffer' is called again on every
        ;; reload, so this branch runs while the mode is already on.  Only the
        ;; first pass may record the original header line; re-recording would
        ;; save this mode's own :eval form and leave it installed for good.
        (unless (+noema-jupyter-cell--header-line-installed-p)
          (setq-local +noema-jupyter-cell--saved-header-line-format
                      header-line-format))
        (setq-local header-line-format
                    '(:eval (+noema-jupyter-cell--header-line)))
        (add-hook 'after-save-hook #'+noema-jupyter-cell-after-save-h nil t)
        (add-hook 'post-command-hook #'+noema-jupyter-cell--post-command-h nil t)
        (add-hook 'completion-at-point-functions
                  #'+noema-jupyter-cell-capf -10 t)
        (add-hook '+snippet-action-functions
                  #'+noema-jupyter-cell-expand-snippet-action nil t)
        (+noema-jupyter-cell--update-highlight)
        (+noema-jupyter-cell--ensure-noema-host))
    (remove-hook 'after-save-hook #'+noema-jupyter-cell-after-save-h t)
    (remove-hook 'post-command-hook #'+noema-jupyter-cell--post-command-h t)
    (remove-hook 'completion-at-point-functions
                 #'+noema-jupyter-cell-capf t)
    (remove-hook '+snippet-action-functions
                 #'+noema-jupyter-cell-expand-snippet-action t)
    (setq-local +noema-jupyter-cell--session-refresh-pending nil)
    (setq-local header-line-format
                +noema-jupyter-cell--saved-header-line-format)
    (+noema-jupyter-cell--hide-overlay)))

(with-eval-after-load 'editor/snippets/config
  (keymap-set +snippet-map "j" #'+noema-jupyter/cell-snippet-menu))

(defun +noema-jupyter-cell--candidate-file-p ()
  "Return non-nil for any standard ipynb file."
  (when-let* ((file buffer-file-name))
    (string-match-p "\\.ipynb\\'" file)))

;;;###autoload
(defun +noema-jupyter/cell-activate-buffer (&optional payload)
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
    (when-let* (((+noema-jupyter-cell--candidate-file-p))
                (metadata (+noema-jupyter-cell--read-metadata)))
      (setq-local +noema-jupyter-cell-source-file (plist-get metadata :source))
      (setq-local +noema-jupyter-cell-kernel
                  (or (funcall get 'kernel) (plist-get metadata :kernel)))
      (setq-local +noema-jupyter-cell-session
                  (or (funcall get 'session) (plist-get metadata :session)))
      (setq-local +noema-jupyter-cell-language (plist-get metadata :language))
      (setq-local +noema-jupyter-cell-storage "ipynb")
      (setq-local +noema-jupyter-cell-kernel-spec (funcall get 'kernelSpec))
      (setq-local +noema-jupyter-cell-kernel-spec-error
                  (funcall get 'kernelSpecError))
      (+noema-jupyter-cell-mode 1))))

(provide 'noema/jupyter/cell)

;;; noema/jupyter/cell.el ends here
