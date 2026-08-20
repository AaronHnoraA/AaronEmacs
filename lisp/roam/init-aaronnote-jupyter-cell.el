;;; init-aaronnote-jupyter-cell.el --- Noema Jupyter notebook buffers -*- lexical-binding: t -*-

;;; Commentary:
;; Noema stores same-kernel cells in one standard ipynb beside the note.  This
;; minor mode adds execution controls to its native source projection.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'init-aaronnote-jupyter-manager)
(require 'init-aaronnote-jupyter-notebook)

(declare-function my/noema-command "init-aaronnote" (command &optional detail))
(declare-function my/noema--api-call-sync "init-aaronnote"
                  (channel args &optional timeout))
(declare-function my/noema-jupyter-output-open "init-aaronnote"
                  (&optional cell-id focus view))
(declare-function my/noema-jupyter-engine-execute
                  "init-aaronnote-jupyter-engine" (params callback))
(declare-function my/noema-jupyter-engine-document-mutate
                  "init-aaronnote-jupyter-engine" (params))
(declare-function my/noema-jupyter-engine--gateway-control
                  "init-aaronnote-jupyter-engine" (action params))
(declare-function my/noema-jupyter-engine--clear-output
                  "init-aaronnote-jupyter-engine" (params &optional all))
(declare-function my/noema-jupyter-engine-introspect
                  "init-aaronnote-jupyter-engine" (kind params &optional timeout))
(declare-function my/noema-jupyter-engine--language-for-kernel
                  "init-aaronnote-jupyter-engine" (kernel requested))
(declare-function my/noema-jupyter-engine--script-identity
                  "init-aaronnote-jupyter-engine" (script source))
(declare-function my/noema-jupyter-engine-variables
                  "init-aaronnote-jupyter-engine" (params))
(declare-function my/noema-jupyter-engine--gateway-tasks
                  "init-aaronnote-jupyter-engine" (params client))
(declare-function my/noema-jupyter-engine--get
                  "init-aaronnote-jupyter-engine" (key object))
(declare-function my/noema--gateway-hash-value "init-aaronnote" (value))

(defvar-local my/noema-jupyter-cell-source-file nil
  "Markdown note file that owns the current Noema notebook.")

(defvar-local my/noema-jupyter-cell-kernel nil
  "Jupyter kernel name for the current hidden cell script.")

(defvar-local my/noema-jupyter-cell-session nil
  "Jupyter session name for the current hidden cell script.")

(defvar-local my/noema-jupyter-cell-language nil
  "Jupyter language for the current hidden cell script.")

(defvar-local my/noema-jupyter-cell-storage nil
  "Storage mode for the current notebook; always ipynb.")

(defvar-local my/noema-jupyter-cell-kernel-spec nil
  "Resolved kernelspec sent by the owning Noema runtime.")

(defvar-local my/noema-jupyter-cell-kernel-spec-error nil
  "Kernelspec discovery error sent by Noema, when any.")

(defvar-local my/noema-jupyter-cell-current-id nil
  "Cell id at point in the current notebook source projection.")

(defvar-local my/noema-jupyter-cell--start-overlay nil
  "Overlay highlighting the current Jupyter cell start marker.")

(defvar-local my/noema-jupyter-cell--end-overlay nil
  "Overlay highlighting the current Jupyter cell end marker.")

(defvar-local my/noema-jupyter-cell--saved-header-line-format nil
  "Header line active before Jupyter Cell mode installed its toolbar.")

(defvar my/noema-jupyter-cell-mode)

(defface my/noema-jupyter-cell-marker-face
  '((t :background "#1f6f43" :foreground "#eafff1" :weight bold))
  "Face for active Noema Jupyter cell boundary markers."
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
  "C-c C-s" #'my/noema-jupyter-cell-sync-buffer
  "C-c C-i" #'my/noema-jupyter-cell-inspect
  "<backtab>" #'my/noema-jupyter-cell-inspect
  "C-c i p" #'my/noema-jupyter-output-page
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
  "C-c i v" #'my/noema-jupyter-cell-variables
  "C-c i t" #'my/noema-jupyter-cell-tasks
  "C-c i ?" #'my/noema-jupyter-cell-command-menu)

(defconst my/noema-jupyter-cell--comment-prefix
  "\\(?://\\|--\\|#\\|;\\)"
  "Line comment prefixes used in generated Jupyter cell script files.")

(defconst my/noema-jupyter-cell--start-re
  (concat "^[ \t]*" my/noema-jupyter-cell--comment-prefix
          "[ \t]*%%\\(?:[ \t]+\\[\\(?:markdown\\|raw\\)\\]\\)?"
          "[ \t]+id=\\([A-Za-z0-9_-]+\\)[ \t]*$"))

(defun my/noema-jupyter-cell--ensure-overlay (symbol face)
  "Return buffer-local overlay stored in SYMBOL, creating it with FACE."
  (or (symbol-value symbol)
      (set symbol
           (let ((overlay (make-overlay (point-min) (point-min) nil nil t)))
             (overlay-put overlay 'face face)
             (overlay-put overlay 'evaporate t)
             overlay))))

(defun my/noema-jupyter-cell--hide-overlays ()
  "Hide current cell highlight overlays."
  (dolist (overlay (list my/noema-jupyter-cell--start-overlay
                         my/noema-jupyter-cell--end-overlay))
    (when (overlayp overlay)
      (delete-overlay overlay))))

(defun my/noema-jupyter-cell--bounds-at-point ()
  "Return plist describing the projected Jupyter cell around point."
  (save-excursion
    (save-restriction
      (widen)
      (let ((origin (point))
            id block-beg start-end body-beg raw-end body-end)
        (goto-char (line-beginning-position))
        (when (or (looking-at my/noema-jupyter-cell--start-re)
                  (re-search-backward my/noema-jupyter-cell--start-re nil t))
          (setq id (match-string-no-properties 1)
                block-beg (line-beginning-position)
                start-end (line-end-position)
                body-beg (progn (forward-line 1) (point)))
          (setq raw-end
                (if (re-search-forward my/noema-jupyter-cell--start-re nil t)
                    (line-beginning-position)
                  (point-max))
                body-end
                (my/noema-jupyter-notebook--body-end body-beg raw-end))
          (when (and (<= block-beg origin)
                     (or (< origin raw-end)
                         (and (= origin (point-max)) (= raw-end (point-max)))))
            (list :id id :start-beg block-beg :start-end start-end
                  :body-beg body-beg :body-end body-end
                  :end-beg body-end :end-end raw-end
                  :block-beg block-beg :block-end raw-end)))))))

(defun my/noema-jupyter-cell--update-highlight ()
  "Update current cell overlays and `my/noema-jupyter-cell-current-id'."
  (if-let* ((bounds (my/noema-jupyter-cell--bounds-at-point)))
      (let ((start (my/noema-jupyter-cell--ensure-overlay
                    'my/noema-jupyter-cell--start-overlay
                    'my/noema-jupyter-cell-marker-face))
            (end (my/noema-jupyter-cell--ensure-overlay
                  'my/noema-jupyter-cell--end-overlay
                  'my/noema-jupyter-cell-marker-face)))
        (setq-local my/noema-jupyter-cell-current-id (plist-get bounds :id))
        (move-overlay start (plist-get bounds :start-beg) (plist-get bounds :start-end))
        (move-overlay end (plist-get bounds :end-beg) (plist-get bounds :end-end)))
    (setq-local my/noema-jupyter-cell-current-id nil)
    (my/noema-jupyter-cell--hide-overlays)))

(defun my/noema-jupyter-cell--document-detail ()
  "Return document-level command detail independent of point."
  `((file . ,my/noema-jupyter-cell-source-file)
    (kernel . ,(or my/noema-jupyter-cell-kernel ""))
    (kernelSpecName . ,(or my/noema-jupyter-cell-kernel ""))
    (session . ,(or my/noema-jupyter-cell-session ""))
    (sessionName . ,(or my/noema-jupyter-cell-session ""))
    (language . ,(or my/noema-jupyter-cell-language ""))
    (storage . ,(or my/noema-jupyter-cell-storage ""))))

(defun my/noema-jupyter-cell--command-detail (&optional cell-id)
  "Return Cell command detail for CELL-ID or the cell at point."
  (let ((cell-id (or cell-id
                     my/noema-jupyter-cell-current-id
                     (plist-get (my/noema-jupyter-cell--bounds-at-point) :id))))
    (unless (and cell-id (not (string-empty-p cell-id)))
      (user-error "Point is not inside an Noema Jupyter cell"))
    (append (my/noema-jupyter-cell--document-detail)
            `((cellId . ,cell-id)))))

(defun my/noema-jupyter-cell--post-command-h ()
  "Track current cell for local highlighting.
Cursor moves in the Emacs source buffer are intentionally local; use
`my/noema-jupyter-cell-sync-cursor' (`M-RET') to sync Noema."
  (when my/noema-jupyter-cell-mode
    (my/noema-jupyter-cell--update-highlight)
    (force-mode-line-update)))

(defun my/noema-jupyter-cell-sync-cursor ()
  "Sync the current Emacs Jupyter cell cursor back to Noema."
  (interactive)
  (my/noema-jupyter-cell--update-highlight)
  (my/noema-command
   "jupyter-select-cell"
   (my/noema-jupyter-cell--command-detail)))

(defun my/noema-jupyter-cell--engine-params (&optional cell-id require-cell)
  "Return Emacs Jupyter engine parameters.
Include CELL-ID or the cell at point when available.  REQUIRE-CELL makes a
missing Cell an error; document/kernel commands work from any point."
  (append
   `((scriptFile . ,buffer-file-name))
   (if (or require-cell cell-id my/noema-jupyter-cell-current-id
           (my/noema-jupyter-cell--bounds-at-point))
       (my/noema-jupyter-cell--command-detail cell-id)
     (my/noema-jupyter-cell--document-detail))))

(defun my/noema-jupyter-output-page ()
  "Open the singleton Noema Jupyter output page for this script."
  (interactive)
  (my/noema-jupyter-cell--update-highlight)
  (my/noema-jupyter-output-open my/noema-jupyter-cell-current-id t))

(defun my/noema-jupyter-cell-jump-output ()
  "Jump from the current source Cell to its output page block."
  (interactive)
  (my/noema-jupyter-output-page))

(defun my/noema-jupyter-cell--execute (mode &optional cell-ids callback)
  "Execute MODE through the Emacs coordinator.
CELL-IDS is used by the explicit selected mode.  CALLBACK receives the final
result and error."
  (when (buffer-modified-p) (save-buffer))
  (my/noema-jupyter-cell--update-highlight)
  (let* ((cell-id my/noema-jupyter-cell-current-id)
         (_ (when (and (not (equal mode "all")) (null cell-id))
              (user-error "Point is not inside a Noema Jupyter Cell")))
         (params (append
                  (my/noema-jupyter-cell--engine-params cell-id
                                                        (not (equal mode "all")))
                  `((mode . ,mode)
                    ,@(when cell-ids `((cellIds . ,(vconcat cell-ids))))))))
    ;; The output page is a projection of this Emacs-owned request.  Running a
    ;; Cell reveals it below without stealing the source editor's focus.
    (my/noema-jupyter-output-open cell-id nil)
    (my/noema-jupyter-engine-execute
     params
     (lambda (result error)
       (if error
           (message "Noema Jupyter: %s" error)
         (message "Noema Jupyter: %s"
                  (if (equal (alist-get 'status result) "error")
                      "execution stopped on error"
                    "execution complete")))
       (when callback (funcall callback result error))))))

(defun my/noema-jupyter-cell-run-current ()
  "Run only the current Cell, matching JupyterLab Run Cell semantics."
  (interactive)
  (my/noema-jupyter-cell--execute "current"))

(defun my/noema-jupyter-cell-run-current-next ()
  "Run the current Cell and select the following Cell."
  (interactive)
  (my/noema-jupyter-cell--execute "current")
  (my/noema-jupyter-cell-next))

(defun my/noema-jupyter-cell-run-all ()
  "Run every Cell in this script in document order."
  (interactive)
  (my/noema-jupyter-cell--execute "all"))

(defun my/noema-jupyter-cell-run-above ()
  "Run the current Cell and all Cells above it."
  (interactive)
  (my/noema-jupyter-cell--execute "above"))

(defun my/noema-jupyter-cell-run-below ()
  "Run the current Cell and all Cells below it."
  (interactive)
  (my/noema-jupyter-cell--execute "below"))

(defun my/noema-jupyter-cell-restart-run-all ()
  "Restart this script's Emacs-owned kernel and run all Cells."
  (interactive)
  (require 'init-aaronnote-jupyter-engine)
  (ignore-errors
    (my/noema-jupyter-engine--gateway-control
     'restart (my/noema-jupyter-cell--engine-params)))
  (my/noema-jupyter-cell-run-all))

(defun my/noema-jupyter-cell-interrupt ()
  "Interrupt this script buffer's Emacs-owned Jupyter kernel."
  (interactive)
  (require 'init-aaronnote-jupyter-engine)
  (my/noema-jupyter-engine--gateway-control
   'interrupt (my/noema-jupyter-cell--engine-params))
  (message "Noema Jupyter: interrupt requested"))

(defun my/noema-jupyter-cell-restart ()
  "Restart this script buffer's Emacs-owned Jupyter kernel."
  (interactive)
  (my/noema-jupyter-engine--gateway-control
   'restart (my/noema-jupyter-cell--engine-params))
  (message "Noema Jupyter: kernel restarted"))

(defun my/noema-jupyter-cell-shutdown ()
  "Shut down this script buffer's Emacs-owned Jupyter kernel."
  (interactive)
  (my/noema-jupyter-engine--gateway-control
   'shutdown (my/noema-jupyter-cell--engine-params))
  (message "Noema Jupyter: kernel shut down"))

(defun my/noema-jupyter-cell--goto-id (cell-id)
  "Move point to CELL-ID's body and return non-nil when found."
  (goto-char (point-min))
  (when (re-search-forward
         (format "^[ \t]*%s[ \t]*%%\\(?:[ \t]+\\[\\(?:markdown\\|raw\\)\\]\\)?[ \t]+id=%s[ \t]*$"
                 my/noema-jupyter-cell--comment-prefix
                 (regexp-quote cell-id)) nil t)
    (forward-line 1)
    (my/noema-jupyter-cell--update-highlight)
    t))

(defun my/noema-jupyter-cell-next ()
  "Select the next Cell in this script."
  (interactive)
  (let* ((bounds (or (my/noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Jupyter Cell")))
         (end (plist-get bounds :end-end)))
    (goto-char end)
    (when (re-search-forward my/noema-jupyter-cell--start-re nil t)
      (forward-line 1))
    (my/noema-jupyter-cell--update-highlight)))

(defun my/noema-jupyter-cell-previous ()
  "Select the previous Cell in this script."
  (interactive)
  (let* ((bounds (or (my/noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Jupyter Cell")))
         (start (plist-get bounds :start-beg)))
    (goto-char start)
    (when (re-search-backward my/noema-jupyter-cell--start-re nil t)
      (forward-line 1))
    (my/noema-jupyter-cell--update-highlight)))

(defun my/noema-jupyter-cell--mutate (operation &optional extra)
  "Apply structural Cell OPERATION with EXTRA parameters."
  (when (buffer-modified-p) (save-buffer))
  (my/noema-jupyter-cell--update-highlight)
  (let* ((result
          (my/noema-jupyter-engine-document-mutate
           (append (my/noema-jupyter-cell--engine-params)
                   `((op . ,operation)) extra)))
         (active (or (alist-get 'activeCellId result)
                     (alist-get "activeCellId" result nil nil #'string=))))
    (when (and active (not (string-empty-p active)))
      (my/noema-jupyter-cell--goto-id active))
    (message "Noema Jupyter: %s" operation)
    result))

(defun my/noema-jupyter-cell-insert-above ()
  "Insert an empty Cell above the current Cell."
  (interactive)
  (my/noema-jupyter-cell--mutate "insertAbove"))

(defun my/noema-jupyter-cell-insert-below ()
  "Insert an empty Cell below the current Cell."
  (interactive)
  (my/noema-jupyter-cell--mutate "insertBelow"))

(defun my/noema-jupyter-cell-duplicate ()
  "Duplicate the current Cell below it."
  (interactive)
  (my/noema-jupyter-cell--mutate "duplicate"))

(defun my/noema-jupyter-cell-delete ()
  "Delete the current Cell and its saved output."
  (interactive)
  (let* ((bounds (or (my/noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Jupyter Cell")))
         (code (buffer-substring-no-properties
                (plist-get bounds :body-beg) (plist-get bounds :body-end))))
    (when (or (string-empty-p (string-trim code))
              (yes-or-no-p "Delete this non-empty Jupyter Cell? "))
      (my/noema-jupyter-cell--mutate "delete"))))

(defun my/noema-jupyter-cell-move-up ()
  "Move the current Cell upward."
  (interactive)
  (my/noema-jupyter-cell--mutate "moveUp"))

(defun my/noema-jupyter-cell-move-down ()
  "Move the current Cell downward."
  (interactive)
  (my/noema-jupyter-cell--mutate "moveDown"))

(defun my/noema-jupyter-cell-split ()
  "Split the current Cell at point."
  (interactive)
  (let* ((bounds (or (my/noema-jupyter-cell--bounds-at-point)
                     (user-error "Point is not inside a Jupyter Cell")))
         (offset (- (max (plist-get bounds :body-beg)
                         (min (point) (plist-get bounds :body-end)))
                    (plist-get bounds :body-beg))))
    (my/noema-jupyter-cell--mutate "split" `((offset . ,offset)))))

(defun my/noema-jupyter-cell-merge-above ()
  "Merge the current Cell into the Cell above."
  (interactive)
  (my/noema-jupyter-cell--mutate "mergeAbove"))

(defun my/noema-jupyter-cell-merge-below ()
  "Merge the Cell below into the current Cell."
  (interactive)
  (my/noema-jupyter-cell--mutate "mergeBelow"))

(defun my/noema-jupyter-cell-clear-output ()
  "Clear the persisted output of the current Cell."
  (interactive)
  (my/noema-jupyter-cell--update-highlight)
  (my/noema-jupyter-engine--clear-output
   (my/noema-jupyter-cell--engine-params) nil)
  (message "Noema Jupyter: output cleared"))

(defun my/noema-jupyter-cell-clear-all-outputs ()
  "Clear persisted output for every Cell in the current script."
  (interactive)
  (my/noema-jupyter-engine--clear-output
   (my/noema-jupyter-cell--engine-params) t)
  (message "Noema Jupyter: all outputs cleared"))

(defun my/noema-jupyter-cell-variables ()
  "Open the unified workspace's live variable explorer."
  (interactive)
  (my/noema-jupyter-output-open my/noema-jupyter-cell-current-id t "variables"))

(defun my/noema-jupyter-cell-tasks ()
  "Open the unified Emacs-owned Jupyter management workspace."
  (interactive)
  (my/noema-jupyter-output-open my/noema-jupyter-cell-current-id t "manage"))

(defun my/noema-jupyter-cell-command-menu ()
  "Prompt for the complete Cell and kernel command set."
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
            ("Select / Connect Kernel" . my/noema-jupyter-cell-select-kernel)
            ("Variables" . my/noema-jupyter-cell-variables)
            ("Kernel Tasks" . my/noema-jupyter-cell-tasks)
            ("Interrupt Kernel" . my/noema-jupyter-cell-interrupt)
            ("Restart Kernel" . my/noema-jupyter-cell-restart)
            ("Restart and Run All" . my/noema-jupyter-cell-restart-run-all)
            ("Shut Down Kernel" . my/noema-jupyter-cell-shutdown)))
         (selection (completing-read "Jupyter: " choices nil t)))
    (call-interactively (alist-get selection choices nil nil #'equal))))

(defun my/noema-jupyter-cell--manager-metadata ()
  "Return manager metadata for the current script buffer."
  (list :script-file buffer-file-name
        :source-file my/noema-jupyter-cell-source-file
        :kernel my/noema-jupyter-cell-kernel
        :session my/noema-jupyter-cell-session
        :language my/noema-jupyter-cell-language))

(defun my/noema-jupyter-cell--manager-session (&optional create)
  "Return the current script's global Jupyter session.
CREATE registers the current buffer as its controller."
  (when buffer-file-name
    (if create
        (my/noema-jupyter-manager-register-controller
         buffer-file-name (current-buffer)
         (my/noema-jupyter-cell--manager-metadata))
      (my/noema-jupyter-manager-session buffer-file-name t))))

(defun my/noema-jupyter-cell--spec-field (entry key)
  "Return kernelspec ENTRY's KEY."
  (let ((spec (my/noema-jupyter--get 'spec entry)))
    (my/noema-jupyter--get key spec)))

(defun my/noema-jupyter-cell--set-kernelspec-header (name)
  "Persist kernelspec NAME in the current notebook metadata."
  (when (buffer-modified-p) (save-buffer))
  (my/noema-jupyter-notebook-set-kernel buffer-file-name name)
  (setq-local my/noema-jupyter-cell-kernel name)
  (when-let* ((session (my/noema-jupyter-cell--manager-session t)))
    (setf (my/noema-jupyter-session-kernelspec session) name))
  (set-visited-file-modtime))

(defun my/noema-jupyter-cell-select-kernel ()
  "Select this script's Jupyter kernel using JupyterLab-style choices.
The chooser works anywhere in the script.  It offers compatible kernelspecs,
compatible running kernels, and No Kernel; language-changing choices belong
to creation of a different sidecar and are intentionally excluded."
  (interactive)
  (let* ((session (or (my/noema-jupyter-cell--manager-session t)
                      (user-error "This buffer has no Jupyter session")))
         (language (downcase (or my/noema-jupyter-cell-language "")))
         (specs
          (condition-case error
              (my/noema-jupyter--kernelspecs
               (or my/noema-jupyter-cell-source-file buffer-file-name))
            (error
             (user-error "Kernelspec discovery failed: %s"
                         (error-message-string error)))))
         choices)
    (dolist (entry specs)
      (let* ((name (format "%s" (my/noema-jupyter--get 'name entry)))
             (spec-language
              (downcase (format "%s"
                                (or (my/noema-jupyter-cell--spec-field
                                     entry 'language) ""))))
             (display (or (my/noema-jupyter-cell--spec-field
                           entry 'display_name) name)))
        (when (or (string-empty-p spec-language)
                  (equal language spec-language)
                  (and (equal language "bash")
                       (member spec-language '("sh" "shell" "bash"))))
          (push (cons (format "Start %s  [%s]" display name)
                      (list :kind 'start :kernelspec name))
                choices))))
    (maphash
     (lambda (id kernel)
       (when (equal language
                    (downcase (or (my/noema-jupyter-kernel-language kernel) "")))
         (push (cons
                (format "Connect to %s  [%s]"
                        (my/noema-jupyter-kernel-kernelspec kernel) id)
                (list :kind 'connect :kernel-id id))
               choices)))
     my/noema-jupyter-manager-kernels)
    (setq choices
          (append (nreverse choices)
                  (list (cons "No Kernel" (list :kind 'none)))))
    (let* ((label (completing-read "Jupyter kernel: " choices nil t))
           (selection (cdr (assoc label choices))))
      (pcase (plist-get selection :kind)
        ('start
         (my/noema-jupyter-cell--set-kernelspec-header
          (plist-get selection :kernelspec)))
        ('connect
         (when-let* ((kernel (gethash (plist-get selection :kernel-id)
                                     my/noema-jupyter-manager-kernels)))
           (my/noema-jupyter-cell--set-kernelspec-header
            (my/noema-jupyter-kernel-kernelspec kernel)))))
      (my/noema-jupyter-manager-select session selection t)
      (force-mode-line-update t)
      (message "Noema Jupyter: %s" label))))

(defun my/noema-jupyter-cell--header-button (label command help &optional enabled)
  "Return clickable header LABEL invoking COMMAND with HELP.
ENABLED defaults to non-nil unless explicitly `disabled'."
  (let ((enabled (not (eq enabled 'disabled))))
    (if (not enabled)
        (propertize (format " %s " label) 'face 'shadow)
      (let ((map (make-sparse-keymap)))
        (define-key map [header-line mouse-1] command)
        (propertize (format " %s " label)
                    'face 'mode-line-highlight
                    'mouse-face 'highlight
                    'help-echo help
                    'local-map map)))))

(defun my/noema-jupyter-cell--header-line ()
  "Render the buffer-local Jupyter visual control line."
  (let* ((session (my/noema-jupyter-cell--manager-session))
         (kernel (my/noema-jupyter-manager-kernel-for-session session))
         (cell (or my/noema-jupyter-cell-current-id
                   (plist-get (my/noema-jupyter-cell--bounds-at-point) :id)))
         (status (if kernel
                     (symbol-name (or (my/noema-jupyter-kernel-status kernel)
                                      'idle))
                   "no kernel")))
    (list
     (propertize " Jupyter " 'face 'mode-line-buffer-id)
     (my/noema-jupyter-cell--header-button
      (format "%s · %s" (or my/noema-jupyter-cell-kernel "Select") status)
      #'my/noema-jupyter-cell-select-kernel
      "Start a compatible kernel, connect to an existing kernel, or detach")
     (my/noema-jupyter-cell--header-button
      "Run" #'my/noema-jupyter-cell-run-current "Run current Cell"
      (unless cell 'disabled))
     (my/noema-jupyter-cell--header-button
      "All" #'my/noema-jupyter-cell-run-all "Run all Cells")
     (my/noema-jupyter-cell--header-button
      "Stop" #'my/noema-jupyter-cell-interrupt "Interrupt current kernel"
      (unless kernel 'disabled))
     (my/noema-jupyter-cell--header-button
      "Restart" #'my/noema-jupyter-cell-restart "Restart current kernel"
      (unless kernel 'disabled))
     (my/noema-jupyter-cell--header-button
      "Cell" #'my/noema-jupyter-cell-command-menu "Cell structure commands")
     (my/noema-jupyter-cell--header-button
      "Outputs" #'my/noema-jupyter-output-page "Open Jupyter Workspace")
     (my/noema-jupyter-cell--header-button
      "Vars" #'my/noema-jupyter-cell-variables "Open Variables")
     (my/noema-jupyter-cell--header-button
      "Manage" #'my/noema-jupyter-cell-tasks "Open Jupyter management workspace"))))

(defun my/noema-jupyter-cell--release-controller-h ()
  "Release the current buffer from its global session without shutdown."
  (when buffer-file-name
    (my/noema-jupyter-manager-release-controller
     buffer-file-name (current-buffer))))

(defun my/noema-jupyter-cell--read-header ()
  "Read Noema metadata from the current notebook."
  (let* ((meta (and buffer-file-name
                    (my/noema-jupyter-notebook-metadata buffer-file-name)))
         (source (plist-get meta :source)))
    (when (and source (not (string-empty-p source)))
      (list :source source
            :kernel (or (plist-get meta :kernel) "")
            :session (or (plist-get meta :session) "default")
            :language (or (plist-get meta :language) "python")
            :storage "ipynb"))))

(defun my/noema-jupyter-cell-sync-buffer ()
  "Notify Noema that the generated Jupyter cell script was saved."
  (interactive)
  (unless (and my/noema-jupyter-cell-source-file
               (not (string-empty-p my/noema-jupyter-cell-source-file)))
    (user-error "This buffer is not linked to an Noema Jupyter cell source"))
  (my/noema-command
   "jupyter-cell-script-saved"
   `((file . ,my/noema-jupyter-cell-source-file)
     (kernel . ,(or my/noema-jupyter-cell-kernel ""))
     (session . ,(or my/noema-jupyter-cell-session ""))
     (storage . ,(or my/noema-jupyter-cell-storage ""))))
  t)

(defun my/noema-jupyter-cell-after-save-h ()
  "Handle generated Jupyter cell scripts after saving."
  (when my/noema-jupyter-cell-mode
    (condition-case err
        (my/noema-jupyter-cell-sync-buffer)
      (error
       (message "Noema Jupyter cell sync failed: %s" (error-message-string err))))))

(defcustom my/noema-jupyter-cell-introspect-timeout 1.5
  "Seconds to wait for a kernel completion or inspection reply.

The kernel answers shell-channel requests strictly in order, so a request
issued while a cell is running is not served until that cell finishes.
Completion runs on a keystroke, so it must give up quickly and let the
language server answer alone rather than block the editor."
  :type 'number
  :group 'my/noema)

(defun my/noema-jupyter-cell--true-p (table key)
  "Return non-nil when TABLE's KEY is JSON true.
Gateway JSON decodes `false' as `:json-false', which is truthy in Lisp, so
boolean replies must never be tested with `gethash' alone."
  (let ((value (and (hash-table-p table) (gethash key table))))
    (and value (not (eq value :json-false)))))

(defun my/noema-jupyter-cell--introspect (kind extra)
  "Call Emacs Jupyter introspection KIND at point with EXTRA parameters.
Return a decoded reply, or nil when the request times out or no kernel runs."
  (when-let* ((source my/noema-jupyter-cell-source-file)
              (bounds (my/noema-jupyter-cell--bounds-at-point))
              (body-beg (plist-get bounds :body-beg))
              (body-end (plist-get bounds :body-end))
              (code (buffer-substring-no-properties body-beg body-end))
              (result
               (ignore-errors
                 (my/noema--gateway-hash-value
                  (my/noema-jupyter-engine-introspect
                   kind
                   (append
                    `((file . ,source)
                      (scriptFile . ,buffer-file-name)
                      (kernel . ,(or my/noema-jupyter-cell-kernel ""))
                      (session . ,(or my/noema-jupyter-cell-session ""))
                      (code . ,code)
                      ;; Jupyter `cursor_pos' is a code-point count.
                      (cursorPos . ,(length
                                     (buffer-substring-no-properties
                                      body-beg (min (point) body-end)))))
                    extra)
                   my/noema-jupyter-cell-introspect-timeout)))))
    ;; `supported' is false when no kernel is running for this script — asking
    ;; must never launch one as a side effect of typing.
    (and (my/noema-jupyter-cell--true-p result "supported") result)))

(defun my/noema-jupyter-cell-capf ()
  "Kernel-native `completion-at-point-function' for Noema cell scripts.

Answers from the live kernel's `complete_request', which sees runtime state
a static analyzer cannot: names bound by earlier cells, DataFrame columns,
IPython magics, Sage's injected builtins.  Declared non-exclusive, so when
the kernel is idle, unreachable, or has nothing to say, completion falls
through to Eglot and the rest of `completion-at-point-functions'."
  (when my/noema-jupyter-cell-mode
    (when-let* ((bounds (my/noema-jupyter-cell--bounds-at-point))
                (body-beg (plist-get bounds :body-beg))
                (reply (my/noema-jupyter-cell--introspect
                        'complete nil))
                (matches (gethash "matches" reply))
                ((> (length matches) 0)))
      (let* ((items (or (gethash "items" reply) '()))
             ;; The kernel decides how much of the line its matches replace
             ;; (a quoted DataFrame key, not just a word), so the
             ;; span comes from the reply and is never guessed here.
             (start (+ body-beg (or (gethash "cursorStart" reply) 0)))
             (end (+ body-beg (or (gethash "cursorEnd" reply) 0)))
             (annotations (make-hash-table :test #'equal)))
        (dolist (item (append items nil))
          (when-let* ((text (gethash "text" item)))
            (puthash text
                     (string-trim
                      (concat (or (gethash "signature" item) "")
                              (when-let* ((type (gethash "type" item))
                                          ((not (string-empty-p type))))
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
  "Show the kernel's documentation for the symbol at point.

This is Jupyter's `inspect_request' — the same thing `Shift-TAB' shows in a
notebook and `?obj' prints in IPython.  With a prefix argument, request the
source instead (IPython's `??obj').  Falls back to a message when no kernel
is running, rather than silently doing nothing."
  (interactive "P")
  (let ((reply (my/noema-jupyter-cell--introspect
                'inspect
                `((detailLevel . ,(if detailed 1 0))))))
    (cond
     ((null reply)
      (message "Noema Jupyter: no running kernel for this script"))
     ((not (my/noema-jupyter-cell--true-p reply "found"))
      (message "Noema Jupyter: nothing known about the symbol at point"))
     (t
      (let* ((data (gethash "data" reply))
             (text (and data (gethash "text/plain" data))))
        (if (or (null text) (string-empty-p text))
            (message "Noema Jupyter: kernel returned no plain-text documentation")
          (with-current-buffer
              (get-buffer-create "*Noema Jupyter Inspect*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert text)
              (goto-char (point-min))
              ;; Kernels return ANSI-coloured text; render it rather than
              ;; showing the escape sequences.
              (ansi-color-apply-on-region (point-min) (point-max))
              (special-mode))
            (display-buffer (current-buffer)))))))))

;;;###autoload
(define-minor-mode my/noema-jupyter-cell-mode
  "Minor mode for Noema Jupyter notebook source projections."
  :lighter " JCell"
  (if my/noema-jupyter-cell-mode
      (progn
        (setq-local my/noema-jupyter-cell--saved-header-line-format
                    header-line-format)
        (setq-local header-line-format
                    '(:eval (my/noema-jupyter-cell--header-line)))
        (add-hook 'after-save-hook #'my/noema-jupyter-cell-after-save-h nil t)
        (add-hook 'post-command-hook #'my/noema-jupyter-cell--post-command-h nil t)
        (add-hook 'kill-buffer-hook
                  #'my/noema-jupyter-cell--release-controller-h nil t)
        ;; Depth -10 so the kernel is consulted before Eglot.  It declares
        ;; itself non-exclusive, so Eglot still answers whenever the kernel
        ;; has nothing (or no kernel is running at all).
        (add-hook 'completion-at-point-functions
                  #'my/noema-jupyter-cell-capf -10 t)
        (my/noema-jupyter-cell--update-highlight))
    (remove-hook 'after-save-hook #'my/noema-jupyter-cell-after-save-h t)
    (remove-hook 'post-command-hook #'my/noema-jupyter-cell--post-command-h t)
    (remove-hook 'kill-buffer-hook
                 #'my/noema-jupyter-cell--release-controller-h t)
    (remove-hook 'completion-at-point-functions
                 #'my/noema-jupyter-cell-capf t)
    (my/noema-jupyter-cell--release-controller-h)
    (setq-local header-line-format
                my/noema-jupyter-cell--saved-header-line-format)
    (my/noema-jupyter-cell--hide-overlays)))

(defun my/noema-jupyter-cell--candidate-file-p ()
  "Return non-nil when the current buffer might be a generated cell script.
Cheap gate for `find-file-hook' so the header regexp scan (and any TRAMP
round-trip it implies) only runs for files under a `.cell' store directory."
  (when-let* ((file (buffer-file-name)))
    (string-match-p "\\(?:\\`\\|/\\)\\.cell/[^/]+\\.ipynb\\'" file)))

;;;###autoload
(defun my/noema-jupyter-cell-activate-buffer (&optional payload)
  "Enable `my/noema-jupyter-cell-mode' in a valid Noema Cell script.
Ordinary `find-file' visits and Noema Edit events share this path.  PAYLOAD,
when present, is the open event containing the owning runtime's kernelspec."
  (interactive (list nil))
  (let ((get (lambda (key) (or (alist-get key payload)
                               (alist-get (symbol-name key) payload
                                          nil nil #'string=)))))
    ;; openScript may rewrite an already visited sidecar.  Keep unsaved user
    ;; edits, but otherwise make the buffer and its runtime header atomic with
    ;; the event before Eglot preparation runs.
    (when (and payload buffer-file-name
               (not (buffer-modified-p))
               (not (verify-visited-file-modtime (current-buffer))))
      (revert-buffer :ignore-auto :noconfirm)
      (goto-char (point-min))
      (forward-line (max 0 (1- (truncate (or (funcall get 'line) 1)))))
      (move-to-column (max 0 (truncate (or (funcall get 'col) 0)))))
    (when-let* (((my/noema-jupyter-cell--candidate-file-p))
                (meta (my/noema-jupyter-cell--read-header))
                (source (plist-get meta :source)))
      (let* ((old (list my/noema-jupyter-cell-kernel
                        my/noema-jupyter-cell-session
                        my/noema-jupyter-cell-language
                        my/noema-jupyter-cell-kernel-spec))
             (kernel (or (funcall get 'kernel) (plist-get meta :kernel)))
             (identity
              (and (fboundp 'my/noema-jupyter-engine--script-identity)
                   (my/noema-jupyter-engine--script-identity
                    buffer-file-name source)))
             (session (or (plist-get identity :session)
                          (funcall get 'session)
                          (plist-get meta :session)))
             (language
              (or (plist-get identity :language)
                  (funcall get 'language)
                  (let ((value (plist-get meta :language)))
                    (and value (not (string-empty-p value)) value))
                  (and (fboundp 'my/noema-jupyter-engine--language-for-kernel)
                       (my/noema-jupyter-engine--language-for-kernel kernel nil))
                  "python"))
             (kernel-spec (funcall get 'kernelSpec))
             (changed (and my/noema-jupyter-cell-mode
                           (not (equal old (list kernel session language
                                                 kernel-spec))))))
        (when (and changed
                   (fboundp 'my/noema-jupyter-cell-lsp-runtime-changing))
          (my/noema-jupyter-cell-lsp-runtime-changing))
        (setq-local my/noema-jupyter-cell-source-file source)
        (setq-local my/noema-jupyter-cell-kernel kernel)
        (setq-local my/noema-jupyter-cell-session session)
        (setq-local my/noema-jupyter-cell-language language)
        (setq-local my/noema-jupyter-cell-storage (plist-get meta :storage))
        (setq-local my/noema-jupyter-cell-kernel-spec kernel-spec)
        (setq-local my/noema-jupyter-cell-kernel-spec-error
                    (funcall get 'kernelSpecError))
        (my/noema-jupyter-manager-register-controller
         buffer-file-name (current-buffer)
         (my/noema-jupyter-cell--manager-metadata))
        (my/noema-jupyter-cell-mode 1)
        (when (and changed
                   (fboundp 'my/language-server-ensure-deferred))
          (my/language-server-ensure-deferred))))))

;; Only Noema-owned notebooks get the execution controller.  Generic ipynb
;; files still use the native projection installed by the notebook codec.
(add-hook 'find-file-hook #'my/noema-jupyter-cell-activate-buffer)

(provide 'init-aaronnote-jupyter-cell)

;;; init-aaronnote-jupyter-cell.el ends here
