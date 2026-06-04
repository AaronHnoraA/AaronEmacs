;;; ai-workbench-docs.el --- One-shot docs Q&A via Codex -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run a single ephemeral Codex request against this Emacs config's docs.

;;; Code:

(require 'cl-lib)
(require 'lv)
(require 'subr-x)

(declare-function evil-emacs-state "evil" ())
(declare-function turn-off-evil-mode "evil" ())
(defgroup ai-workbench-docs nil
  "One-shot docs Q&A helpers for ai-workbench."
  :group 'ai-workbench
  :prefix "ai-workbench-docs-")

(defconst ai-workbench-docs-root-directory
  (let* ((source (or load-file-name
                     (when-let* ((library (locate-library "ai-workbench-docs")))
                       library)
                     buffer-file-name
                     default-directory))
         (dir (file-name-directory (file-truename (expand-file-name source)))))
    (expand-file-name "../.." dir))
  "Root directory of this Emacs configuration.")

(defcustom ai-workbench-docs-directory
  (expand-file-name "docs" ai-workbench-docs-root-directory)
  "Directory containing local documentation used for one-shot Q&A."
  :type 'directory
  :group 'ai-workbench-docs)

(defcustom ai-workbench-docs-agent-file
  (expand-file-name "agent.md" ai-workbench-docs-directory)
  "Instruction file used by one-shot docs Q&A."
  :type 'file
  :group 'ai-workbench-docs)

(defcustom ai-workbench-docs-command-timeout 180
  "Maximum seconds allowed for a one-shot docs request."
  :type 'integer
  :group 'ai-workbench-docs)

(defvar ai-workbench-docs--process nil
  "Live process for the current one-shot docs request.")

(defvar ai-workbench-docs--timer nil
  "Timeout timer for the current one-shot docs request.")

(defvar ai-workbench-docs--spinner-timer nil
  "Spinner timer for the current one-shot docs request.")

(defconst ai-workbench-docs--spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Spinner frames used while a docs request is running.")

(defvar ai-workbench-docs--spinner-index 0
  "Current spinner frame index.")

(defun ai-workbench-docs--codex-executable ()
  "Return the Codex executable for one-shot docs requests."
  (cond
   ((and (boundp 'ai-workbench-codex-executable)
         (stringp ai-workbench-codex-executable)
         (not (string-empty-p ai-workbench-codex-executable)))
    ai-workbench-codex-executable)
   ((and (boundp 'codex-cli-executable)
         (stringp codex-cli-executable)
         (not (string-empty-p codex-cli-executable)))
    codex-cli-executable)
   (t
    "codex")))

(defun ai-workbench-docs--ensure-ready ()
  "Validate local prerequisites for one-shot docs Q&A."
  (unless (file-directory-p ai-workbench-docs-directory)
    (user-error "Docs directory not found: %s" ai-workbench-docs-directory))
  (unless (file-exists-p ai-workbench-docs-agent-file)
    (user-error "Docs agent file not found: %s" ai-workbench-docs-agent-file))
  (unless (executable-find (ai-workbench-docs--codex-executable))
    (user-error "Codex executable not found: %s" (ai-workbench-docs--codex-executable))))

(defun ai-workbench-docs--cleanup-process ()
  "Clear transient state for the current docs request."
  (when (timerp ai-workbench-docs--timer)
    (cancel-timer ai-workbench-docs--timer))
  (when (timerp ai-workbench-docs--spinner-timer)
    (cancel-timer ai-workbench-docs--spinner-timer))
  (setq ai-workbench-docs--timer nil)
  (setq ai-workbench-docs--spinner-timer nil)
  (setq ai-workbench-docs--process nil))

(defun ai-workbench-docs-hide ()
  "Hide the transient docs UI."
  (interactive)
  (lv-delete-window))

(defun ai-workbench-docs--dismiss-ui ()
  "Hide the transient docs UI."
  (ai-workbench-docs-hide))

(defun ai-workbench-docs--lv-setup ()
  "Configure the transient LV buffer used by docs Q&A."
  (use-local-map (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map special-mode-map)
                   (define-key map (kbd "q") #'ai-workbench-docs-hide)
                   map))
  (setq-local cursor-type nil)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (when (fboundp 'evil-emacs-state)
    (evil-emacs-state))
  (when (bound-and-true-p evil-local-mode)
    (turn-off-evil-mode)))

(add-hook 'lv-window-hook #'ai-workbench-docs--lv-setup)

(defun ai-workbench-docs--show (text)
  "Show TEXT in the transient docs UI."
  (lv-message "%s" text)
  (when-let* ((buffer (get-buffer " *LV*")))
    (with-current-buffer buffer
      (ai-workbench-docs--lv-setup))))

(defun ai-workbench-docs--spinner-tick ()
  "Refresh the loading spinner UI."
  (when (process-live-p ai-workbench-docs--process)
    (let ((frame (aref ai-workbench-docs--spinner-frames
                       (mod ai-workbench-docs--spinner-index
                            (length ai-workbench-docs--spinner-frames)))))
      (setq ai-workbench-docs--spinner-index (1+ ai-workbench-docs--spinner-index))
      (ai-workbench-docs--show (format "%s Docs ask loading..." frame)))))

(defun ai-workbench-docs--cleanup-artifacts (process)
  "Delete temp files and buffers associated with PROCESS."
  (let ((output-file (process-get process :ai-workbench-docs-output-file))
        (buffer (process-buffer process)))
    (when (and output-file (file-exists-p output-file))
      (ignore-errors (delete-file output-file)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun ai-workbench-docs--build-prompt (question)
  "Return the one-shot prompt for QUESTION."
  (string-join
   (list
    "Read docs/agent.md first, then read the relevant files under docs/ before answering."
    "Answer the user's question about using this Emacs configuration."
    "Do not modify files. Do not create or resume any long-lived session."
    "Keep the answer concise and practical. Use Chinese unless the user asks otherwise."
    "When relevant, mention the docs file path(s) you relied on."
    ""
    "User question:"
    question)
   "\n"))

(defun ai-workbench-docs--command (prompt output-file)
  "Return the Codex command list for PROMPT and OUTPUT-FILE."
  (list (ai-workbench-docs--codex-executable)
        "exec"
        "--skip-git-repo-check"
        "--ephemeral"
        "--color" "never"
        "-C" ai-workbench-docs-root-directory
        "-s" "workspace-write"
        "-o" output-file
        prompt))

(defun ai-workbench-docs--read-answer (process)
  "Return the answer text produced by PROCESS."
  (let ((output-file (process-get process :ai-workbench-docs-output-file)))
    (unless (and output-file (file-exists-p output-file))
      (error "Codex did not produce an output file"))
    (with-temp-buffer
      (insert-file-contents output-file)
      (string-trim (buffer-string)))))

(defun ai-workbench-docs--timeout (process)
  "Abort PROCESS after timeout."
  (when (process-live-p process)
    (delete-process process)
    (ai-workbench-docs--show
     (format "Docs ask timed out after %ss  [q to close]" ai-workbench-docs-command-timeout)))
  (ai-workbench-docs--cleanup-artifacts process)
  (ai-workbench-docs--cleanup-process))

(defun ai-workbench-docs--sentinel (process event)
  "Handle one-shot docs PROCESS EVENT."
  (when (memq (process-status process) '(exit signal))
    (let ((log-buffer (process-buffer process)))
      (unwind-protect
          (if (and (eq (process-status process) 'exit)
                   (zerop (process-exit-status process)))
              (let ((answer (ai-workbench-docs--read-answer process)))
                (ai-workbench-docs--show (format "%s\n\n[q to close]" answer)))
            (let ((details (string-trim
                            (if (buffer-live-p log-buffer)
                                (with-current-buffer log-buffer
                                  (buffer-string))
                              ""))))
              (let ((summary
                     (if (string-empty-p details)
                         (format "Docs ask failed: %s" (string-trim event))
                       (format "Docs ask failed: %s | %s"
                               (string-trim event)
                               (replace-regexp-in-string "[\n\r\t ]+" " "
                                                         details)))))
                (ai-workbench-docs--show (format "%s\n\n[q to close]" summary)))))
        (ai-workbench-docs--cleanup-artifacts process)
        (ai-workbench-docs--cleanup-process)))))

;;;###autoload
(defun ai-workbench-docs-ask (question)
  "Ask Codex a one-shot QUESTION about this Emacs config's docs."
  (interactive
   (list
    (read-from-minibuffer "Ask docs with Codex: " nil nil nil nil nil t)))
  (ai-workbench-docs--ensure-ready)
  (when (process-live-p ai-workbench-docs--process)
    (user-error "A docs ask request is already running"))
  (unless (and (stringp question)
               (not (string-empty-p (string-trim question))))
    (user-error "Question cannot be empty"))
  (let* ((project-root ai-workbench-docs-root-directory)
         (default-directory project-root)
         (prompt (ai-workbench-docs--build-prompt (string-trim question)))
         (output-file (make-temp-file "ai-workbench-docs-" nil ".txt"))
         (process-buffer (generate-new-buffer " *ai-workbench-docs-codex*"))
         (process
          (make-process
           :name "ai-workbench-docs-codex"
           :buffer process-buffer
           :command (ai-workbench-docs--command prompt output-file)
           :coding 'utf-8
           :noquery t
           :sentinel #'ai-workbench-docs--sentinel)))
    (setq ai-workbench-docs--process process)
    (setq ai-workbench-docs--spinner-index 0)
    (setq ai-workbench-docs--timer
          (run-at-time ai-workbench-docs-command-timeout nil
                       #'ai-workbench-docs--timeout process))
    (setq ai-workbench-docs--spinner-timer
          (run-at-time 0 0.12 #'ai-workbench-docs--spinner-tick))
    (process-put process :ai-workbench-docs-output-file output-file)
    (process-put process :ai-workbench-docs-project-root project-root)
    (ai-workbench-docs--show "⠋ Docs ask loading...  [q to close]")))

(provide 'ai-workbench-docs)
;;; ai-workbench-docs.el ends here
