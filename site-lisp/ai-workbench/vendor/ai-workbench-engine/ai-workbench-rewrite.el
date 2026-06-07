;;; ai-workbench-rewrite.el --- Refactoring functions for ai-workbench-engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: hypermedia, convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'ai-workbench-transient)
(require 'cl-lib)

(defvar eldoc-documentation-functions)
(defvar diff-entire-buffers)
(defvar ediff-window-setup-function)
(defvar ediff-split-window-function)

(declare-function diff-no-select "diff")
(declare-function rmc--add-key-description "rmc")
(declare-function ediff-setup-windows-plain "ediff-wind")

;; * User options

(defcustom ai-workbench-rewrite-directives-hook nil
  "Hook run to generate ai-workbench-engine's default rewrite directives.

Each function in this hook is called with no arguments until one
returns a non-nil value, the base string to use as the
rewrite instruction.

Use this hook to tailor context-specific rewrite directives.
For example, you can specialize the default rewrite directive
for a particular major-mode or project."
  :group 'ai-workbench-engine
  :type 'hook)

(defcustom ai-workbench-post-rewrite-functions nil
  "Abnormal hook run after a `ai-workbench-rewrite' action.

This hook is called after the LLM response for the rewrite action
has been fully received in a temporary buffer.  Each function is
called with two arguments: the response beginning and end
positions.

Note: this hook only runs if the rewrite request succeeds."
  :type 'hook
  :group 'ai-workbench-engine)

(defcustom ai-workbench-rewrite-default-action nil
  "Action to take when rewriting a text region using ai-workbench-engine.

When the LLM response with the rewritten text is received, you can
- merge it with the current region, possibly creating a merge conflict,
- diff or ediff against the original region,
- or accept it in place, replacing the original region.
- display a dispatch menu with the above choices.

If this option is nil (the default), ai-workbench-engine waits for an explicit
command.  Set it to the symbol `merge', `diff', `ediff', `accept'
or `dispatch' to automatically do one of these things instead.

You can also set it to a function of your choosing for a custom
action.  This function receives one argument, the rewrite
overlay."
  :group 'ai-workbench-engine
  :type '(choice
          (const :tag "Wait" nil)
          (const :tag "Merge with current region" merge)
          (const :tag  "Diff against current region" diff)
          (const :tag "Ediff against current region" ediff)
          (const :tag "Accept rewrite" accept)
          (const :tag "Dispatch" dispatch)
          (function :tag "Custom action")))

(defface ai-workbench-rewrite-highlight-face
  '((((class color) (min-colors 88) (background dark))
     :background "#041714" :extend t :inherit default)
    (((class color) (min-colors 88) (background light))
     :background "light goldenrod yellow" :extend t :inherit default)
    (t :inherit secondary-selection))
  "Face for highlighting regions with pending rewrites."
  :group 'ai-workbench-engine)

;; * Variables

(defvar-keymap ai-workbench-rewrite-actions-map
  :doc "Keymap for ai-workbench-engine rewrite actions at point."
  "RET" #'ai-workbench--rewrite-dispatch
  "<mouse-1>" #'ai-workbench--rewrite-dispatch
  "C-c C-a" #'ai-workbench--rewrite-accept
  "C-c C-r" #'ai-workbench--rewrite-iterate
  "C-c C-k" #'ai-workbench--rewrite-reject
  "C-c C-d" #'ai-workbench--rewrite-diff
  "C-c C-e" #'ai-workbench--rewrite-ediff
  "C-c C-n" #'ai-workbench--rewrite-next
  "C-c C-p" #'ai-workbench--rewrite-previous
  "C-c C-m" #'ai-workbench--rewrite-merge)

(defvar-local ai-workbench--rewrite-overlays nil
  "List of active rewrite overlays in the buffer.")

(defvar-local ai-workbench--rewrite-message nil
  "Request-specific instructions for a `ai-workbench-rewrite' action.")

;; Add the rewrite directive to `ai-workbench-directives'
(unless (alist-get 'rewrite ai-workbench-directives)
  (add-to-list 'ai-workbench-directives `(rewrite . ,#'ai-workbench--rewrite-directive-default)))

(defvar ai-workbench--rewrite-directive
  (or (alist-get 'rewrite ai-workbench-directives)
      #'ai-workbench--rewrite-directive-default)
  "Active system message for rewrite actions.

This variable is for internal use only.  To customize the rewrite
system message, set a system message (or function that generates
the system message) as the value of the `rewrite' key in
`ai-workbench-directives':

 (setf (alist-get \\='rewrite ai-workbench-directives)
       #\\='my-rewrite-message-generator)

You can also customize `ai-workbench-rewrite-directives-hook' to
dynamically inject a rewrite-specific system message.")

(defun ai-workbench--rewrite-directive-default ()
  "Generic directive for rewriting or refactoring.

These are instructions not specific to any particular required
change.

The returned string is interpreted as the system message for the
rewrite request.  To use your own, add a different directive to
`ai-workbench-directives', or add to `ai-workbench-rewrite-directives-hook',
which see."
  (or (save-mark-and-excursion
        (run-hook-with-args-until-success
         'ai-workbench-rewrite-directives-hook))
      (let* ((lang (downcase (ai-workbench--strip-mode-suffix major-mode)))
             (article (if (and lang (not (string-empty-p lang))
                               (memq (aref lang 0) '(?a ?e ?i ?o ?u)))
                          "an" "a")))
        (if (derived-mode-p 'prog-mode)
            (format (concat "You are %s %s programmer.  "
                            "Follow my instructions and refactor %s code I provide.\n"
                            "- Generate ONLY %s code as output, without "
                            "any explanation or markdown code fences.\n"
                            "- Generate code in full, do not abbreviate or omit code.\n"
                            "- Do not produce intermediate text or report on your progress.\n"
                            "- Do not ask for further clarification, and make "
                            "any assumptions you need to follow instructions.")
                    article lang lang lang)
          (concat
           (if (string-empty-p lang)
               "You are an editor."
             (format "You are %s %s editor." article lang))
           "  Follow my instructions and improve or rewrite the text I provide."
           "  Do not produce intermediate text or report on your progress."
           "  Generate ONLY the replacement text,"
           " without any explanation or markdown code fences.")))))

;; MAYBE: Save FSM to `ai-workbench--fsm-last' on request end?
(defvar ai-workbench--rewrite-handlers
  `((WAIT ,#'ai-workbench--handle-wait ,#'ai-workbench--rewrite-update-wait)
    (TPRE ,#'ai-workbench--handle-pre-tool ,#'ai-workbench--fsm-transition)
    (TOOL ,#'ai-workbench--rewrite-update-tool-call ,#'ai-workbench--handle-tool-use)
    (TRET ,#'ai-workbench--handle-post-tool ,#'ai-workbench--rewrite-update-tool-call
          ,#'ai-workbench--handle-tool-result))
  "Alist specifying FSM handlers for `ai-workbench-rewrite' state transitions.")

;; * Helper functions

;; ** UI Indicators
(defun ai-workbench--rewrite-update-tool-call (fsm)
  "Update the rewrite overlay to indicate tool call progress for FSM."
  (when-let* ((info (ai-workbench-fsm-info fsm))
              (ov (car-safe (plist-get info :context)))
              (names (cl-loop for call in (plist-get info :tool-use)
                              collect (plist-get call :name))))
    (with-current-buffer (plist-get info :buffer)
      (setq ai-workbench--fsm-last fsm)
      (ai-workbench--rewrite-update-status
       ov (concat
           (propertize
            (if (length> names 1) " Calling tools (" " Calling tool (")
            'face '(mode-line-emphasis default))
           (mapconcat (lambda (name) (propertize name 'face '(font-lock-keyword-face default)))
                      names (propertize ", " 'face '(mode-line-emphasis default)))
           (propertize ")" 'face '(mode-line-emphasis default)))))))

(defun ai-workbench--rewrite-update-wait (fsm)
  "Update the rewrite overlay status for FSM to indicate a waiting state."
  (when-let* ((info (ai-workbench-fsm-info fsm))
              (ov (car-safe (plist-get info :context)))
              (model (ai-workbench--model-name
                      (or (plist-get info :model) ai-workbench-model)))
              (hint-str (concat "[" model "]\n")))
    (overlay-put
     ov 'status
     (list (propertize "REWRITE" 'face '(warning default))     ;status element 0
           (propertize " Waiting..." 'face '(warning default)) ;status element 1
           (propertize                                         ;status element 2
            " " 'display
            (if (and (fboundp 'string-pixel-width)
                     (display-graphic-p))
                `(space :align-to (- right (,(string-pixel-width hint-str))))
              `(space :align-to (- right ,(+ 1 (string-width hint-str))))))
           (propertize hint-str 'face '(warning default)))) ;status element 3
    (overlay-put ov 'before-string (apply #'concat (overlay-get ov 'status)))))

(defun ai-workbench--rewrite-update-status (ov msg &optional face)
  "Update overlay OV's status with MSG and refresh its before-string.
If FACE is non-nil, apply that face to MSG when storing the status."
  (setq msg (or msg ""))
  (setf (cadr (overlay-get ov 'status))
        (if face (propertize msg 'face face) msg))
  (overlay-put ov 'before-string (apply #'concat (overlay-get ov 'status))))

(defun ai-workbench--rewrite-key-help (callback)
  "Eldoc documentation function for ai-workbench-engine rewrite actions.

CALLBACK is supplied by Eldoc, see
`eldoc-documentation-functions'."
  (when (and ai-workbench--rewrite-overlays
             (get-char-property (point) 'ai-workbench-rewrite))
      (funcall callback
               (format (substitute-command-keys "%s rewrite available: accept \\[ai-workbench--rewrite-accept], iterate \\[ai-workbench--rewrite-iterate], clear \\[ai-workbench--rewrite-reject], merge \\[ai-workbench--rewrite-merge], diff \\[ai-workbench--rewrite-diff] or ediff \\[ai-workbench--rewrite-ediff]")
                       (propertize (ai-workbench--model-name ai-workbench-model) 'face 'mode-line-emphasis)))))

;; ** Navigation across rewrite regions

(defun ai-workbench--rewrite-move (search-func)
  "Move directionally to a ai-workbench-engine rewrite location using SEARCH-FUNC."
  (let* ((ov (cdr (get-char-property-and-overlay (point) 'ai-workbench-rewrite)))
         (pt (save-excursion
               (if ov
                   (goto-char
                    (funcall search-func (overlay-start ov) 'ai-workbench-rewrite))
                 (goto-char
                  (max (1- (funcall search-func (point) 'ai-workbench-rewrite))
                       (point-min))))
               (funcall search-func (point) 'ai-workbench-rewrite))))
    (if (get-char-property pt 'ai-workbench-rewrite)
        (goto-char pt)
      (user-error "No further rewrite regions!"))))

(defun ai-workbench--rewrite-next ()
  "Go to next pending LLM rewrite in buffer, if one exists."
  (interactive)
  (ai-workbench--rewrite-move #'next-single-char-property-change))

(defun ai-workbench--rewrite-previous ()
  "Go to previous pending LLM rewrite in buffer, if one exists."
  (interactive)
  (ai-workbench--rewrite-move #'previous-single-char-property-change))

;; ** Rewrite actions helpers

(defun ai-workbench--rewrite-overlay-at (&optional pt)
  "Check for a ai-workbench-engine rewrite overlay at PT and return it.

If no suitable overlay is found, raise an error."
  (pcase-let ((`(,response . ,ov)
               (get-char-property-and-overlay (or pt (point)) 'ai-workbench-rewrite))
              (diff-entire-buffers nil))
    (unless ov (user-error "Could not find region being rewritten"))
    (unless response (user-error "No LLM output available for this rewrite"))
    ov))

(defun ai-workbench--rewrite-prepare-buffer (ovs &optional buf)
  "Prepare new buffer with LLM changes applied and return it.

This is used for (e)diff purposes.

RESPONSE is the LLM response.  OVS are the overlays specifying
the changed regions.  BUF is the (current) buffer."
  (setq buf (or buf (overlay-buffer (or (car-safe ovs) ovs))))
  (with-current-buffer buf
    (let ((pmin (point-min))
          (pmax (point-max))
          (pt   (point))
          ;; (mode major-mode)
          (newbuf (get-buffer-create "*ai-workbench-diff*"))
          (inhibit-read-only t)
          (inhibit-message t))
      (save-restriction
        (widen)
        (with-current-buffer newbuf
          (erase-buffer)
          (insert-buffer-substring buf)))
      (with-current-buffer newbuf
        (narrow-to-region pmin pmax)
        (goto-char pt)
        ;; We mostly just want font-locking
        ;; (delay-mode-hooks (funcall mode))
        ;; Apply the changes to the new buffer
        (save-excursion
          (ai-workbench--rewrite-accept ovs newbuf)))
      newbuf)))

(defun ai-workbench--rewrite-read-message (prompt &optional _ history)
  "Read a rewrite message from the minibuffer.

Provide custom keybindings for cycling, editing, and submitting the
`ai-workbench-rewrite' action directly from this prompt.

PROMPT is the prompt string to display.  HISTORY, if provided, is the
input history list."
  (let* ((rewrite-directive
          (car-safe (ai-workbench--parse-directive ai-workbench--rewrite-directive 'raw)))
         (cb (current-buffer))
         (cycle-prefix (lambda () (interactive)
                         (ai-workbench--read-with-prefix rewrite-directive)
                         (push-mark) (goto-char (point-max))
                         (activate-mark)))
         (set-rewrite-message
          (lambda ()
            (let ((message (buffer-substring-no-properties
                            (minibuffer-prompt-end) (point-max))))
              (with-current-buffer cb (setq ai-workbench--rewrite-message message))
              (setf (alist-get 'ai-workbench--infix-rewrite-extra transient-history)
                    (delete-dups (cons message transient--history))))))
         (start-rewrite-maybe
          (lambda () (interactive)
            (when (minibufferp) (funcall set-rewrite-message))
            (if transient--prefix    ;Called from transient? Don't start rewrite
                (run-at-time 0 nil #'transient-setup 'ai-workbench-rewrite)
              (with-current-buffer cb
                (ai-workbench--suffix-rewrite ai-workbench--rewrite-message)))
            (when (minibufferp) (exit-minibuffer))))
         (start-transient
          (lambda () (interactive)
            (run-at-time 0 nil #'transient-setup 'ai-workbench-rewrite)
            (when (minibufferp)
              (funcall set-rewrite-message)
              (exit-minibuffer))))
         (edit-in-buffer
          (lambda () (interactive)
            (let ((offset (- (point) (minibuffer-prompt-end))))
              (ai-workbench--edit-directive 'ai-workbench--rewrite-message
                :prompt rewrite-directive :initial (minibuffer-contents)
                :buffer cb :setup (lambda () (ignore-errors (forward-char offset)))
                :callback
                (lambda (msg)
                  (when msg
                    (push (buffer-local-value 'ai-workbench--rewrite-message cb)
                          (alist-get 'ai-workbench--infix-rewrite-extra transient-history))
                    (with-current-buffer cb (ai-workbench--suffix-rewrite)))
                  (when (minibufferp) (exit-minibuffer)))))))
         (minibuffer-local-map
          (make-composed-keymap (define-keymap
                                  "TAB" cycle-prefix "<tab>" cycle-prefix
                                  "C-c C-e" edit-in-buffer
                                  "<remap> <exit-minibuffer>" start-rewrite-maybe
                                  "M-RET" start-transient)
                                minibuffer-local-map)))
    (minibuffer-with-setup-hook cycle-prefix
      (read-string
       prompt (or ai-workbench--rewrite-message "Rewrite: ")
       history))))

;; * Rewrite action functions

(defun ai-workbench--rewrite-reject (&optional ovs)
  "Clear pending LLM responses in OVS or at point."
  (interactive (list (ai-workbench--rewrite-overlay-at)))
  (dolist (ov (ensure-list ovs))
    (setq ai-workbench--rewrite-overlays (delq ov ai-workbench--rewrite-overlays))
    (delete-overlay ov))
  (unless ai-workbench--rewrite-overlays
    (remove-hook 'eldoc-documentation-functions 'ai-workbench--rewrite-key-help 'local))
  (message "Cleared pending LLM response(s)."))

(defun ai-workbench--rewrite-accept (&optional ovs buf)
  "Apply pending LLM responses in OVS or at point.

BUF is the buffer to modify, defaults to the overlay buffer."
  (interactive (list (ai-workbench--rewrite-overlay-at)))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              (buf (or buf ov-buf))
              ((buffer-live-p buf)))
    (with-current-buffer ov-buf
      (cl-loop for ov in (ensure-list ovs)
               for ov-beg = (overlay-start ov)
               for ov-end = (overlay-end ov)
               for response = (overlay-get ov 'ai-workbench-rewrite)
               do (with-current-buffer buf
                    (goto-char ov-beg)
                    (delete-region ov-beg ov-end)
                    (insert response))))
    (message "Replaced region(s) with LLM output in buffer: %s."
             (buffer-name ov-buf))))

(defalias 'ai-workbench--rewrite-iterate 'ai-workbench-rewrite
  "Iterate on pending LLM response at point.")

(defun ai-workbench--rewrite-diff (&optional ovs switches)
  "Diff pending LLM responses in OVS or at point.

SWITCHES are diff arguments."
  (interactive (list (ai-workbench--rewrite-overlay-at)))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              ((buffer-live-p ov-buf)))
    (require 'diff)
    (let* ((newbuf (ai-workbench--rewrite-prepare-buffer ovs))
           (diff-buf (diff-no-select ov-buf newbuf switches)))
      (with-current-buffer diff-buf
        (setq-local diff-jump-to-old-file t))
      (display-buffer diff-buf))))

(defun ai-workbench--rewrite-ediff (&optional ovs)
  "Ediff pending LLM responses in OVS or at point."
  (interactive (list (ai-workbench--rewrite-overlay-at)))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              ((buffer-live-p ov-buf)))
    (letrec ((newbuf (ai-workbench--rewrite-prepare-buffer ovs))
             (cwc (current-window-configuration))
             (hideshow
              (lambda (&optional restore)
                (dolist (ov (ensure-list ovs))
                  (when-let* ((overlay-buffer ov))
                    (let ((disp (overlay-get ov 'display))
                          (stored (overlay-get ov 'ai-workbench--ediff)))
                      (overlay-put ov 'face (and restore 'ai-workbench-rewrite-highlight-face))
                      (overlay-put ov 'display (and restore stored))
                      (overlay-put ov 'ai-workbench--ediff (unless restore disp)))))))
             (ai-workbench--ediff-restore
              (lambda ()
                (when (window-configuration-p cwc)
                  (set-window-configuration cwc))
                (funcall hideshow 'restore)
                (remove-hook 'ediff-quit-hook ai-workbench--ediff-restore))))
      (funcall hideshow)
      (add-hook 'ediff-quit-hook ai-workbench--ediff-restore 50)
      (let ((ediff-window-setup-function #'ediff-setup-windows-plain)
            (ediff-split-window-function #'split-window-horizontally))
        (ediff-buffers ov-buf newbuf)))))

(defun ai-workbench--rewrite-merge-git (beg end new-str)
  "Produce a merge conflict region between BEG and END.

Merge the region with NEW-STR using git merge-file."
  (let ((original-temp-file (make-temp-file "ai-workbench-merge-"))
        (empty-temp-file (make-temp-file "ai-workbench-merge-")) ; use /dev/null? (windows?)
        (new-temp-file (make-temp-file "ai-workbench-merge-")))
    (unwind-protect
        (progn (write-region beg end original-temp-file)
               (with-temp-file empty-temp-file (insert ""))
               (with-temp-file new-temp-file (insert new-str))
               (goto-char beg)
               (delete-region beg end)
               (call-process
                "git" nil (list (current-buffer) nil) nil
                "merge-file" "--no-diff3" "-L" "original" "-L" "Empty" "-L"
                (ai-workbench-backend-name ai-workbench-backend) "-p"
                original-temp-file empty-temp-file new-temp-file)
               ;; Make merge marker active if required
               (goto-char beg) (unless (bolp) (insert "\n")))
      (delete-file original-temp-file)
      (delete-file empty-temp-file)
      (delete-file new-temp-file))))

(defun ai-workbench--rewrite-merge-simple (beg end new-str)
  "Produce a merge conflict region between BEG and END.

NEW-STR is the new string intended to replace the region."
  (goto-char end)                       ;End first to preserve ordering
  (unless (bolp) (insert "\n"))
  (insert "=======\n" new-str "\n>>>>>>> "
          (ai-workbench-backend-name ai-workbench-backend) "\n")
  (goto-char beg)
  (unless (bolp) (insert "\n"))
  (insert-before-markers "<<<<<<< original\n"))

(defun ai-workbench--rewrite-merge (&optional ovs)
  "Insert pending LLM responses in OVS as merge conflicts."
  (interactive (list (ai-workbench--rewrite-overlay-at)))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              ((buffer-live-p ov-buf)))
    (with-current-buffer ov-buf
      (let ((changed))
        (dolist (ov (ensure-list ovs))
          (save-excursion
            (when-let* ((new-str (overlay-get ov 'ai-workbench-rewrite)))
              (if (executable-find "git") ;Replace overlay content with merge result
                  (ai-workbench--rewrite-merge-git (overlay-start ov) (overlay-end ov) new-str)
                (ai-workbench--rewrite-merge-simple (overlay-start ov) (overlay-end ov) new-str))
              (setq changed t))))
        (when changed (smerge-mode 1)))
      (ai-workbench--rewrite-reject ovs))))

(defun ai-workbench--rewrite-dispatch (&optional ov ci)
  "Dispatch actions for ai-workbench-engine rewrites.

OV is the rewrite overlay, CI is true for interactive calls."
  (interactive (list (ai-workbench--rewrite-overlay-at) t))
  (let ((choice)
        (orig-status (copy-sequence (overlay-get ov 'status))))
    (unwind-protect
        (pcase-let ((choices '((?a "accept") (?k "reject") (?r "iterate")
                               (?m "merge") (?d "diff") (?e "ediff"))))
          (ai-workbench--rewrite-update-status
           ov (when (fboundp #'rmc--add-key-description) ; introduced in Emacs 29
                (concat " " (mapconcat (lambda (e) (cdr e))
                                       (mapcar #'rmc--add-key-description choices) ", "))))
          (setq choice (read-multiple-choice "Action: " choices)))
      (overlay-put ov 'status orig-status)
      (overlay-put ov 'before-string (apply #'concat orig-status)))
    (if ci
        (call-interactively (intern (concat "ai-workbench--rewrite-" (cadr choice))))
      (funcall (intern (concat "ai-workbench--rewrite-" (cadr choice))) ov))))

(defun ai-workbench--rewrite-callback (response info)
  "Callback for ai-workbench-engine rewrite actions.

Show the rewrite result in an overlay over the original text, and
set up dispatch actions.

RESPONSE is the response received.  It may also be t (to indicate
success) nil (to indicate failure), or the symbol `abort'.

INFO is the async communication channel for the rewrite request."
  (when-let* ((ov-and-buf (plist-get info :context))
              (ov (car ov-and-buf))
              (proc-buf (cdr ov-and-buf))
              (buf (overlay-buffer ov)))
    (cond
     ((stringp response)            ;partial or fully successful result
      (with-current-buffer proc-buf ;auxiliary buffer, insert text here and copy to overlay
        (let ((inhibit-modification-hooks nil)
              (inhibit-read-only t))
          (when (= (buffer-size) 0)
            (buffer-disable-undo)
            (overlay-put ov 'ai-workbench-rewrite nil)
            (ai-workbench--rewrite-update-status ov " Typing..." '(success default))
            (insert-buffer-substring buf (overlay-start ov) (overlay-end ov))
            (when (eq (char-before (point-max)) ?\n)
              (plist-put info :newline t))
            (setq major-mode (buffer-local-value 'major-mode buf)) ;Don't turn on major-mode (#730, #722)
            (add-text-properties (point-min) (point-max) '(face shadow font-lock-face shadow))
            (goto-char (point-min)))
          (insert response)
          (unless (eobp) (ignore-errors (delete-char (length response))))
          (font-lock-ensure)
          (overlay-put ov 'display (propertize (buffer-string) 'face 'default))))
      (unless (plist-get info :stream) (ai-workbench--rewrite-callback t info)))

     ((eq response 'abort)              ;request aborted
      (when-let* ((proc-buf (cdr-safe (plist-get info :context))))
        (kill-buffer proc-buf))
      (delete-overlay ov))

     ((eq (car-safe response) 'tool-call) ;tool call confirmation
      (ai-workbench--rewrite-update-status ov " Run tools?" '(mode-line-emphasis default))
      (ai-workbench--display-tool-calls   ;use minibuffer
       (cdr response) info         ;; (buffer-local-value 'buffer-read-only buf)
       t))

     ((null response)                   ;finished with error
      (message (concat "LLM response error: %s. Rewrite in buffer %s canceled.")
               (plist-get info :status) (plist-get info :buffer))
      (ai-workbench--rewrite-callback 'abort info))

     ((eq (car-safe response) 'reasoning) ;Reasoning redirection to other buffer
      (and-let* ((rbuf (plist-get info :include-reasoning))
                 ((stringp rbuf)))
        (ai-workbench--display-reasoning-stream (cdr response) info))
      t)

     ((consp response))             ;reasoning or tool call result -- don't care

     (t
      (if (plist-get info :tool-use)    ;stopped to use tools
          ;; Clear text inserted so far
          (with-current-buffer proc-buf (delete-region (point-min) (point)))
        (let ((mkb (propertize "<mouse-1>" 'face 'help-key-binding))) ;or finished successfully
          (with-current-buffer proc-buf
            (let ((inhibit-read-only t))
              (delete-region (point) (point-max))
              ;; Run post-rewrite-functions on rewritten text in its buffer
              (setq-local ai-workbench-post-rewrite-functions
                          (buffer-local-value 'ai-workbench-post-rewrite-functions buf))
              (with-demoted-errors "ai-workbench-post-rewrite-functions: %S"
                (run-hook-with-args 'ai-workbench-post-rewrite-functions (point-min) (point-max)))
              (when (and (plist-get info :newline)
                         (not (eq (char-before (point-max)) ?\n)))
                (insert "\n"))
              (font-lock-ensure))
            (overlay-put ov 'display (buffer-string))
            (overlay-put ov 'ai-workbench-rewrite (buffer-string))
            (kill-buffer proc-buf))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (pulse-momentary-highlight-region (overlay-start ov) (overlay-end ov))
              (add-hook 'eldoc-documentation-functions #'ai-workbench--rewrite-key-help nil 'local)
              ;; (overlay-put ov 'ai-workbench-rewrite response)
              (overlay-put ov 'face 'ai-workbench-rewrite-highlight-face)
	      (overlay-put ov 'priority 2000)
              (overlay-put ov 'keymap ai-workbench-rewrite-actions-map)
              (overlay-put ov 'mouse-face 'highlight)
              (let ((status (overlay-get ov 'status)))
                (dolist (idx '(0 1 3))
                  (setf (nth idx status)
                        (propertize (nth idx status) 'face '(success default))))
                (ai-workbench--rewrite-update-status ov " Ready" '(success default)))
              (overlay-put
               ov 'help-echo
               (format (concat "%s rewrite available: %s or \\[ai-workbench--rewrite-dispatch] for options")
                       (concat (ai-workbench-backend-name ai-workbench-backend) ":" (ai-workbench--model-name ai-workbench-model))
                       mkb))
              (push ov ai-workbench--rewrite-overlays))
            (if-let* ((sym ai-workbench-rewrite-default-action))
                (if-let* ((action (intern (concat "ai-workbench--rewrite-" (symbol-name sym))))
                          ((functionp action)))
                    (funcall action ov) (funcall sym ov))
              (message (concat
                        "LLM rewrite output"
                        (unless (eq (current-buffer) buf)
                          (format " in buffer %s " (buffer-name buf)))
                        (concat " ready: " mkb ", " (propertize "RET" 'face 'help-key-binding)
                                " or " (substitute-command-keys "\\[ai-workbench-rewrite] to continue."))))))))))))

;; * Transient Prefixes for rewriting

(transient-define-prefix ai-workbench--rewrite-directive-menu ()
  "Set the directive (system message) for rewrite actions.

By default, ai-workbench-engine uses the directive associated with the `rewrite'
 key in `ai-workbench-directives'.  You can add more rewrite-specific
 directives to `ai-workbench-directives' and pick one from here."
  [:description ai-workbench-system-prompt--format
   [(ai-workbench--suffix-rewrite-directive)]
   [(ai-workbench--infix-variable-scope)]]
   [:class transient-column
    :setup-children
    (lambda (_) (transient-parse-suffixes
            'ai-workbench--rewrite-directive-menu
            (ai-workbench--setup-directive-menu
             'ai-workbench--rewrite-directive "Rewrite directive")))
    :pad-keys t])

;;;###autoload (autoload 'ai-workbench-rewrite "ai-workbench-rewrite" nil t)
(transient-define-prefix ai-workbench-rewrite ()
  "Rewrite or refactor text region using an LLM."
  :environment #'ai-workbench--transient-fix-evil-visual
  [:description
   (lambda ()
     (ai-workbench--describe-directive
      ai-workbench--rewrite-directive (max (- (window-width) 14) 20) " "))
   [""
    (ai-workbench-preset
     :transient t
     :if (lambda () (or (get-char-property (point) 'ai-workbench-rewrite)
                   (use-region-p)))
     :key "@" :format "%d"
     :description
     (lambda ()
       (concat (propertize "Instructions" 'face 'transient-heading)
               (ai-workbench--format-preset-string))))
    ("s" "Set full directive" ai-workbench--rewrite-directive-menu)
    (ai-workbench--infix-rewrite-extra)]]
  ;; FIXME: We are requiring `ai-workbench-transient' because of this suffix, perhaps
  ;; we can get find some way around that?
  [:description "Context for rewrite"
   :if use-region-p
   (ai-workbench--infix-context-remove-all :key "-d")
   (ai-workbench--suffix-context-buffer :key "C" :format "  %k %d")]
  [[:description "Diff Options"
    :if (lambda () ai-workbench--rewrite-overlays)
    ("-b" "Ignore whitespace changes"      ("-b" "--ignore-space-change"))
    ("-w" "Ignore all whitespace"          ("-w" "--ignore-all-space"))
    ("-i" "Ignore case"                    ("-i" "--ignore-case"))
    (ai-workbench--infix-rewrite-diff:-U)]
   [:description "Accept all"
    :if (lambda () ai-workbench--rewrite-overlays)
    (ai-workbench--suffix-rewrite-merge)
    (ai-workbench--suffix-rewrite-accept)
    "Reject all"
    (ai-workbench--suffix-rewrite-reject)]]
  [[:description "Diff rewrite regions"
    :if (lambda () ai-workbench--rewrite-overlays)
    (ai-workbench--suffix-rewrite-diff)
    (ai-workbench--suffix-rewrite-ediff)]]
  [[:description "Rewrite"
    :if (lambda () (or (get-char-property (point) 'ai-workbench-rewrite)
                  (use-region-p)))
    (ai-workbench--suffix-rewrite)]
   ["Dry Run"
    :if (lambda () (and (or ai-workbench-log-level ai-workbench-expert-commands)
                   (or (get-char-property (point) 'ai-workbench-rewrite)
                       (use-region-p))))
    ("I" "Inspect query (Lisp)"
     (lambda ()
       "Inspect the query that will be sent as a lisp object."
       (interactive)
       (ai-workbench--sanitize-model)
       (ai-workbench--inspect-query
        (ai-workbench--suffix-rewrite ai-workbench--rewrite-message t))))
    ("J" "Inspect query (JSON)"
     (lambda ()
       "Inspect the query that will be sent as a JSON object."
       (interactive)
       (ai-workbench--sanitize-model)
       (ai-workbench--inspect-query
        (ai-workbench--suffix-rewrite ai-workbench--rewrite-message t)
        'json)))]]
  (interactive)
  (ai-workbench--rewrite-sanitize-overlays)
  (cond
   ((use-region-p)                      ;Start a/another rewrite
    (let ((transient--history ;No transient reader, so We manage history ourselves
           (alist-get 'ai-workbench--infix-rewrite-extra transient-history)))
      (ai-workbench--rewrite-read-message
       (concat "Instructions (" ai-workbench--read-with-prefix-help
               (format " %s%s) "
                       (propertize "M-RET" 'face 'help-key-binding)
                       (propertize ": More options" 'face 'default)))
       nil (cons 'transient--history 1))))
   (ai-workbench--rewrite-overlays             ;Rewrite actions pending, show options
    (transient-setup 'ai-workbench-rewrite))
   (t (user-error
       "`ai-workbench-rewrite' requires an active region or rewrite in progress"))))

;; * Transient infixes for rewriting

(transient-define-infix ai-workbench--infix-rewrite-extra ()
  "Chat directive (system message) to use for rewriting or refactoring."
  :description "Rewrite instruction"
  :class 'ai-workbench-lisp-variable
  :variable 'ai-workbench--rewrite-message
  :set-value #'ai-workbench--set-with-scope
  :display-nil "(None)"
  :key "d"
  :format " %k %d %v"
  :prompt (concat "Instructions (" ai-workbench--read-with-prefix-help ") ")
  :reader #'ai-workbench--rewrite-read-message)

(transient-define-argument ai-workbench--infix-rewrite-diff:-U ()
  :description "Context lines"
  :class 'transient-option
  :argument "-U"
  :reader #'transient-read-number-N0)

;; * Transient suffixes for rewriting

(transient-define-suffix ai-workbench--suffix-rewrite-directive (&optional cancel)
  "Edit Rewrite directive.

CANCEL is used to avoid touching dynamic rewrite directives,
generated from functions."
  :transient 'transient--do-exit
  :description "Edit full rewrite directive"
  :key "s"
  (interactive
   (list (and
          (functionp ai-workbench--rewrite-directive)
          (not (y-or-n-p
                "Rewrite directive is dynamically generated: Edit its current value instead?")))))
  (if cancel (progn (message "Edit canceled")
                    (call-interactively #'ai-workbench-rewrite))
    (ai-workbench--edit-directive 'ai-workbench--rewrite-directive
      :callback (lambda (_) (call-interactively #'ai-workbench-rewrite))
      :setup #'activate-mark)))

(transient-define-suffix ai-workbench--suffix-rewrite (&optional rewrite-message dry-run)
  "Rewrite or refactor region contents."
  :key "r"
  :description (lambda () (if (get-char-property (point) 'ai-workbench-rewrite) "Iterate" "Rewrite"))
  (interactive (list ai-workbench--rewrite-message))
  (let* ((nosystem (ai-workbench--model-capable-p 'nosystem))
         ;; Try to send context with system message
         (ai-workbench-use-context
          (and ai-workbench-use-context (if nosystem 'user 'system)))
         (prompt (list (or (get-char-property (point) 'ai-workbench-rewrite)
                           (buffer-substring-no-properties (region-beginning) (region-end)))
                       "What is the required change?  I will generate only the final replacement."
                       (or rewrite-message ai-workbench--rewrite-message))))
    (when nosystem
      (setcar prompt (concat (car-safe (ai-workbench--parse-directive
                                        ai-workbench--rewrite-directive 'raw))
                             "\n\n" (car prompt))))
    (prog1 (ai-workbench-request prompt
             :dry-run dry-run
             :system ai-workbench--rewrite-directive
             :stream ai-workbench-stream
             :context
             (let ((ov (or (cdr-safe (get-char-property-and-overlay (point) 'ai-workbench-rewrite))
                           (make-overlay (region-beginning) (region-end) nil t))))
               (overlay-put ov 'evaporate t)
               ;; NOTE: Switch to `generate-new-buffer' after we drop Emacs 27.1 (#724)
               (cons ov (ai-workbench--temp-buffer " *ai-workbench-rewrite*")))
             :transforms ai-workbench-prompt-transform-functions
             :fsm (ai-workbench-make-fsm :handlers ai-workbench--rewrite-handlers)
             :callback #'ai-workbench--rewrite-callback)
      ;; Move back so that the cursor is on the overlay when done.
      (unless (get-char-property (point) 'ai-workbench-rewrite)
        (when (= (point) (region-end)) (run-at-time 0 nil #'backward-char 1)))
      (setq deactivate-mark t))))

;; Allow this to be called non-interactively for dry runs
(put 'ai-workbench--suffix-rewrite 'interactive-only nil)

(transient-define-suffix ai-workbench--suffix-rewrite-diff (&optional switches)
  "Diff LLM output against buffer."
  :if (lambda () ai-workbench--rewrite-overlays)
  :key "D"
  :description "Diff  LLM rewrites"
  (interactive (list (transient-args transient-current-command)))
  (ai-workbench--rewrite-diff ai-workbench--rewrite-overlays switches))

(transient-define-suffix ai-workbench--suffix-rewrite-ediff ()
  "Ediff LLM output against buffer."
  :if (lambda () ai-workbench--rewrite-overlays)
  :key "E"
  :description "Ediff LLM rewrites"
  (interactive)
  (ai-workbench--rewrite-ediff ai-workbench--rewrite-overlays))

(transient-define-suffix ai-workbench--suffix-rewrite-merge ()
  "Insert LLM output as merge conflicts."
  :if (lambda () ai-workbench--rewrite-overlays)
  :key "M"
  :description "Merge with conflicts"
  (interactive)
  (ai-workbench--rewrite-merge ai-workbench--rewrite-overlays))

(transient-define-suffix ai-workbench--suffix-rewrite-accept ()
  "Accept pending LLM rewrites."
  :if (lambda () ai-workbench--rewrite-overlays)
  :key "A"
  :description "Accept and replace"
  (interactive)
  (ai-workbench--rewrite-accept ai-workbench--rewrite-overlays))

(transient-define-suffix ai-workbench--suffix-rewrite-reject ()
  "Clear pending LLM rewrites."
  :if (lambda () ai-workbench--rewrite-overlays)
  :key "K"
  :description "Clear pending rewrites"
  (interactive)
  (ai-workbench--rewrite-reject ai-workbench--rewrite-overlays))

(provide 'ai-workbench-rewrite)
;;; ai-workbench-rewrite.el ends here

;; Local Variables:
;; outline-regexp: "^;; \\*+"
;; End:
