;;; ai-workbench-context.el --- Context aggregator for ai-workbench-engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Karthik Chikmagalur

;; Author: daedsidog <contact@daedsidog.com>
;; Keywords: convenience, buffers

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; The context allows you to conveniently create contexts which can be fed
;; to ai-workbench-engine.

;;; Code:

(eval-and-compile (require 'ai-workbench-request))
(require 'cl-lib)
(require 'project)

(declare-function ai-workbench-menu "ai-workbench-transient")
(declare-function dired-get-marked-files "dired")
(declare-function ibuffer-get-marked-buffers "ibuffer")
(declare-function ibuffer-current-buffer "ibuffer")
(declare-function image-file-name-regexp "image-file")
(declare-function create-image "image")

(defface ai-workbench-context-highlight-face
  '((((background dark)  (min-colors 88)) :background "gray4" :extend t)
    (((background light) (min-colors 88)) :background "alice blue" :extend t)
    (t :inherit mode-line))
  "Face used to highlight ai-workbench-engine contexts in buffers."
  :group 'ai-workbench-engine)

(defface ai-workbench-context-deletion-face
  '((((class color) (min-colors 257) (background light))
     :background "#ffeeee" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffdddd" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#553333" :extend t)
    (((class color)) :foreground "red" :extend t))
  "Face used to highlight ai-workbench-engine contexts to be deleted.

This is used in ai-workbench-engine context buffers."
  :group 'ai-workbench-engine)

(defvar ai-workbench-context-wrap-function nil
  "Function to format the context string sent with the ai-workbench-engine request.")
(make-obsolete-variable
 'ai-workbench-context-wrap-function
 "Custom functions for wrapping context are no longer supported by ai-workbench-engine.\
  See `ai-workbench-context--wrap-in-buffer' for details."
 "0.9.9")

(defcustom ai-workbench-context-string-function #'ai-workbench-context--string
  "Function to prepare the context string sent with the ai-workbench-engine request.

This function can be synchronous or asynchronous, and receives one or
two arguments respectively.

Synchronous: An alist of contexts with buffers or files (the context
alist).
Asynchronous: A callback to call with the result, and the context alist.

Entries in the context alist can have one of these forms:

 (buffer1 overlay1 overlay2 ...)             ;text overlays in a buffer
 (buffer2)                                   ;a buffer object
 (\"/path/to/file\")                         ;a text file
 (\"/path/to/file\" :mime \"text/markdown\") ;with explicit mime type
 (\"/path/to/image\" :mime \"image/jpeg\")   ;media file

Each overlay covers a buffer region containing the
context chunk.  This is accessible as, for example:

 (with-current-buffer buffer1
   (buffer-substring (overlay-start overlay1)
                     (overlay-end   overlay1)))"
  :group 'ai-workbench-engine
  :type 'function)

(defcustom ai-workbench-context-restrict-to-project-files t
  "Restrict files eligible to be added to the context to project files.

When set to t, files in a VCS that are not project files (such as files
listed in `.gitignore' in a Git repository) will not be added to the
context."
  :group 'ai-workbench-engine
  :type 'boolean)

(defvar ai-workbench-context--project-files nil
  "Cached alist of project files per project.")

(defvar ai-workbench-context--reset-cache nil
  "Whether a project files cache-buster has been scheduled.")

;;; Commands

(defun ai-workbench-context-add-current-kill (&optional arg)
  "Add current kill to ai-workbench-engine, accumulating if ARG is non-nil."
  (interactive "P")
  (let ((kill (current-kill 0)))
    (with-current-buffer (get-buffer-create " *ai-workbench-kill-ring-context*")
      (if (not arg)
          (kill-region (point-min) (point-max))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n----\n")))
      (insert kill)
      (ai-workbench-context--add-region (current-buffer)
                                 (point-min) (point-max))
      (message "*current-kill* has been added as context."))))

(defun ai-workbench-context-add (&optional arg confirm)
  "Add context to ai-workbench-engine in a DWIM fashion.

- If a region is selected, add the selected region to the
  context.  If there is already a ai-workbench-engine context at point, remove it
  instead.

- If in Dired, add marked files or file at point to the context.  If
  the selection includes directories, add all their files recursively,
  prompting the user for confirmation if called interactively or
  CONFIRM is non-nil.  With negative prefix ARG, remove all files from
  the context instead.

- If in IBuffer, add marked buffers or buffer at point to the context.
  With negative prefix ARG, remove buffers from the context instead.

- Otherwise add the current buffer to the context.  With positive
  prefix ARG, prompt for a buffer name and add it to the context.

- With negative prefix ARG, remove all ai-workbench-engine contexts from the current
  buffer, prompting the user for confirmation if called interactively
  or CONFIRM is non-nil."
  (interactive "P\np")
  (cond
   ;; A region is selected.
   ((use-region-p)
    (ai-workbench-context--add-region (current-buffer)
                               (region-beginning)
                               (region-end))
    (deactivate-mark)
    (message "Current region added as context."))
   ;; If in dired
   ((derived-mode-p 'dired-mode)
    (let* ((files (dired-get-marked-files))
           (dirs (cl-remove-if-not #'file-directory-p files))
           (remove-p (< (prefix-numeric-value arg) 0))
	   (action-fn (if remove-p
			  #'ai-workbench-context-remove
			#'ai-workbench-context-add-file)))
      (when (or remove-p (null dirs) (null confirm)
		(y-or-n-p (format "Recursively add files from %d director%s? "
				  (length dirs)
				  (if (= (length dirs) 1) "y" "ies"))))
	(mapc action-fn files))))
   ;; If in ibuffer
   ((derived-mode-p 'ibuffer-mode)
    (let* ((buffers (or (ibuffer-get-marked-buffers)
                        (list (ibuffer-current-buffer))))
           (remove-p (< (prefix-numeric-value arg) 0))
	   (action-fn (if remove-p
			  #'ai-workbench-context-remove
			#'ai-workbench-context--add-buffer)))
      (mapc action-fn buffers)))
   ;; If in an image buffer
   ((and (derived-mode-p 'image-mode)
	 (ai-workbench--model-capable-p 'media)
	 (buffer-file-name)
	 (not (ai-workbench-context--skip-p (buffer-file-name))))
    (funcall (if (and arg (< (prefix-numeric-value arg) 0))
                 #'ai-workbench-context-remove
               #'ai-workbench-context-add-file)
             (buffer-file-name)))
   ;; No region is selected, and ARG is positive.
   ((and arg (> (prefix-numeric-value arg) 0))
    (let* ((buffer-name (read-buffer "Choose buffer to add as context: "
                                     (current-buffer) t))
           (start (with-current-buffer buffer-name (point-min)))
           (end (with-current-buffer buffer-name (point-max))))
      (ai-workbench-context--add-region
       (get-buffer buffer-name) start end t)
      (message "Buffer '%s' added as context." buffer-name)))
   ;; No region is selected, and ARG is negative.
   ((and arg (< (prefix-numeric-value arg) 0))
    (when (or (null confirm)
	      (y-or-n-p "Remove all contexts from this buffer? "))
      (let ((removed-contexts 0))
        (cl-loop for cov in
                 (ai-workbench-context--in-region (current-buffer) (point-min) (point-max))
                 do (progn
                      (cl-incf removed-contexts)
                      (ai-workbench-context-remove cov)))
        (message (format "%d context%s removed from current buffer."
                         removed-contexts
                         (if (= removed-contexts 1) "" "s"))))))
   (t ; Default behavior
    (if (ai-workbench-context--at-point)
        (progn
          (ai-workbench-context-remove
           (car (ai-workbench-context--in-region (current-buffer)
                                          (max (point-min) (1- (point)))
                                          (point))))
          (message "Context under point has been removed."))
      (ai-workbench-context--add-buffer (current-buffer))))))

;;;###autoload (autoload 'ai-workbench-add "ai-workbench-context" "Add/remove regions or buffers from ai-workbench-engine's context." t)
(defalias 'ai-workbench-add #'ai-workbench-context-add)

(defun ai-workbench-context--add-buffer (buffer)
  "Add BUFFER to context."
  (with-current-buffer buffer
    (ai-workbench-context--add-region (current-buffer) (point-min) (point-max) t))
  (message "Buffer \"%s\" added to context." (buffer-name buffer)))

(defun ai-workbench-context--add-text-file (path)
  "Add text file at PATH to context."
  (cl-pushnew (list path) ai-workbench-context :test #'equal)
  (message "File \"%s\" added to context." path)
  path)

(defun ai-workbench-context--add-binary-file (path)
  "Add binary file at PATH to context if supported.
Return PATH if added, nil if ignored."
  (if-let* (((ai-workbench--model-capable-p 'media))
            (mime (mailcap-file-name-to-mime-type path))
            ((ai-workbench--model-mime-capable-p mime)))
      (prog1 path
        (cl-pushnew (list path :mime mime)
                    ai-workbench-context :test #'equal)
        (message "File \"%s\" added to context." path))
    (message "Ignoring unsupported binary file \"%s\"." path)
    nil))

(defun ai-workbench-context--add-directory (path action)
  "Process all files in directory at PATH according to ACTION.
ACTION should be either `add' or `remove'."
  (dolist (file (directory-files-recursively path "."))
    (pcase-exhaustive action
      ('add
       (unless ai-workbench-context--reset-cache
         (setq ai-workbench-context--reset-cache t)
         (run-at-time
          0 nil
          (lambda () (setq ai-workbench-context--reset-cache nil
                      ai-workbench-context--project-files nil))))
       (if (ai-workbench-context--skip-p file)
           ;; Don't message about .git, as this creates thousands of messages
           (unless (string-match-p "\\.git/" file)
             (ai-workbench-context--message-skipped file))
         (ai-workbench-context-add-file file)))
      ('remove
       (setf (alist-get file ai-workbench-context nil 'remove #'equal) nil)))))

(defun ai-workbench-context-add-file (path)
  "Add the file at PATH to the ai-workbench-engine context.

If PATH is a directory, recursively add all files in it.  PATH should be
readable as text."
  (interactive "fChoose file to add to context: ")
  (cond ((file-directory-p path)
         (ai-workbench-context--add-directory path 'add))
	((ai-workbench--file-binary-p path)
         (ai-workbench-context--add-binary-file path))
	(t (ai-workbench-context--add-text-file path))))

;;;###autoload (autoload 'ai-workbench-add-file "ai-workbench-context" "Add files to ai-workbench-engine's context." t)
(defalias 'ai-workbench-add-file #'ai-workbench-context-add-file)

;;; project-related functions
(defun ai-workbench-context--get-project-files (dir)
  "Return a list of files in the project DIR, or nil if no project is found."
  (when-let* ((project (project-current nil dir)))
    (with-memoization (alist-get dir ai-workbench-context--project-files
                                 nil nil #'equal)
      (project-files project))))

(defun ai-workbench-context--skip-p (file)
  "Return non-nil if FILE should not be added to the context."
  (when (and ai-workbench-context-restrict-to-project-files
	     (not (file-remote-p file)))
    (and-let* ((file-dir (or (file-name-directory file) default-directory))
               (project (project-current nil file-dir)))
      (not (member (expand-file-name file)
                   (ai-workbench-context--get-project-files (project-root project)))))))

(defun ai-workbench-context--message-skipped (file)
  "Message that FILE is skipped because it is not a project file."
  (let* ((type (if (file-directory-p file) "directory" "file"))
	 (reminder (format "To include it, unset `%S'."
			   'ai-workbench-context-restrict-to-project-files)))
    (if-let* ((root (cl-some (lambda (dir) (and (file-in-directory-p file dir) dir))
                             (map-keys ai-workbench-context--project-files)))
	      (rel-file (file-relative-name file root)))
	(message "Skipping %s \"%s\" in project \"%s\".  %s"
		 type rel-file root reminder)
      (message "Skipping %s \"%s\". %s" type file reminder))))

;;; Remove context
(defun ai-workbench-context-remove (&optional context)
  "Remove the CONTEXT overlay from the contexts list.

If CONTEXT is nil, removes the context at point.
If selection is active, removes all contexts within selection.
If CONTEXT is a directory, recursively removes all files in it."
  (cond
   ((overlayp context)                  ;Overlay in buffer
    (when-let* ((buf (overlay-buffer context)))
      (delete-overlay context)
      ;; FIXME: Quadratic cost when clearing a bunch of contexts at once
      (unless
          (cl-loop
           for ov in
           (plist-get (alist-get buf ai-workbench-context) :overlays)
           thereis (overlay-start ov))
        (setf (alist-get buf ai-workbench-context nil 'remove) nil))))
   ((bufferp context)                   ;Full buffer
    (setf (alist-get context ai-workbench-context nil 'remove) nil)
    (when (buffer-live-p context)
      (with-current-buffer context
        (without-restriction
          (remove-overlays nil nil 'ai-workbench-context t)))))
   ((stringp context)                   ;file or directory
    (if (file-directory-p context)
        (ai-workbench-context--add-directory context 'remove)
      (setf (alist-get context ai-workbench-context nil 'remove #'equal) nil)
      (message "File \"%s\" removed from context." context)))
   ((region-active-p)                   ;Overlays in region
    (when-let* ((contexts (ai-workbench-context--in-region (current-buffer)
                                                    (region-beginning)
                                                    (region-end))))
      (cl-loop for ctx in contexts do (delete-overlay ctx))))
   (t                                   ;Anything at point
    (when-let* ((ctx (ai-workbench-context--at-point)))
      (delete-overlay ctx)))))

(defun ai-workbench-context-remove-all (&optional verbose)
  "Remove all ai-workbench-engine context.

If VERBOSE is non-nil, ask for confirmation and message
afterwards."
  (interactive (list t))
  (if (null ai-workbench-context)
      (when verbose (message "No ai-workbench-engine context sources to remove."))
    (when (or (not verbose) (y-or-n-p "Remove all context? "))
      (cl-loop
       for context in ai-workbench-context
       for (source . spec) = (ensure-list context)
       if (bufferp source) do           ;Buffers and buffer regions
       (mapc #'ai-workbench-context-remove (plist-get spec :overlays))
       else do (ai-workbench-context-remove source) ;files or other types
       finally do (setq ai-workbench-context nil))
      (when verbose (message "Removed all ai-workbench-engine context sources.")))))

;;; Context wrap
(defun ai-workbench-context--make-overlay (start end &optional advance)
  "Highlight the region from START to END.

ADVANCE controls the overlay boundary behavior."
  (let ((overlay (make-overlay start end nil (not advance) advance))
        (buf-entry (alist-get (current-buffer) ai-workbench-context)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'face 'ai-workbench-context-highlight-face)
    (overlay-put overlay 'ai-workbench-context t)
    (setf (alist-get (current-buffer) ai-workbench-context)
          (plist-put buf-entry :overlays
                     (cons overlay (plist-get buf-entry :overlays))))
    overlay))

;;;###autoload
(defun ai-workbench-context--wrap (callback data-buf)
  "Add request context to DATA-BUF and run CALLBACK.

DATA-BUF is the buffer where the request prompt is constructed."
  (if (= (car (func-arity ai-workbench-context-string-function)) 2)
      (funcall ai-workbench-context-string-function
               (lambda (c) (with-current-buffer data-buf
                             (ai-workbench-context--wrap-in-buffer c))
                 (funcall callback))
               (ai-workbench-context--collect))
    (with-current-buffer data-buf
      (thread-last (ai-workbench-context--collect)
                   (funcall ai-workbench-context-string-function)
                   (ai-workbench-context--wrap-in-buffer)))
    (funcall callback)))

(defun ai-workbench-context--wrap-in-buffer (context-string &optional method)
  "Inject CONTEXT-STRING to current buffer using METHOD.

METHOD is either system or user, and defaults to `ai-workbench-use-context'.
This modifies the buffer."
  (when (length> context-string 0)
    (pcase (or method ai-workbench-use-context)
      ('system
       (if (ai-workbench--model-capable-p 'nosystem)
           (ai-workbench-context--wrap-in-buffer context-string 'user)
         (if ai-workbench-system-prompt
             (cl-etypecase ai-workbench-system-prompt
               (string
                (setq ai-workbench-system-prompt
                      (concat context-string "\n\n" ai-workbench-system-prompt)))
               (function
                (setq ai-workbench-system-prompt
                      (ai-workbench--parse-directive ai-workbench-system-prompt 'raw))
                (ai-workbench-context--wrap-in-buffer context-string))
               (list
                (setq ai-workbench-system-prompt ;cons a new list to avoid mutation
                      (cons (concat context-string "\n\n" (car ai-workbench-system-prompt))
                            (cdr ai-workbench-system-prompt)))))
           (setq ai-workbench-system-prompt context-string))))
      ('user
       (goto-char (point-max))
       (text-property-search-backward 'ai-workbench-engine nil t)
       (and ai-workbench-mode
            (looking-at
             (concat "[\n[:blank:]]*"
                     (and-let* ((prefix (ai-workbench-prompt-prefix-string))
                                ((not (string-empty-p prefix))))
                       (concat "\\(?:" (regexp-quote prefix) "\\)?"))))
            (delete-region (match-beginning 0) (match-end 0)))
       (insert "\n" context-string "\n\n")))))

(defun ai-workbench-context--collect-media (&optional contexts)
  "Collect media CONTEXTS.

CONTEXTS, which are typically paths to binary files, are
base64-encoded and prepended to the first user prompt."
  (cl-loop for context in (or contexts ai-workbench-context)
           for (path . props) = (ensure-list context)
           when (and (stringp path) (plist-get props :mime))
           collect (cons :media context)))

(cl-defun ai-workbench-context--add-region (buffer region-beginning region-end &optional advance)
  "Add region delimited by REGION-BEGINNING, REGION-END in BUFFER as context.

If ADVANCE is non-nil, the context overlay envelopes changes at
the beginning and end."
  ;; Remove existing contexts in the same region, if any.
  (mapc #'ai-workbench-context-remove
        (ai-workbench-context--in-region buffer region-beginning region-end))
  (prog1 (with-current-buffer buffer
           (ai-workbench-context--make-overlay region-beginning region-end advance))
    (message "Region added to context buffer.")))

(defun ai-workbench-context--in-region (buffer start end)
  "Return the list of context overlays in the given region, if any, in BUFFER.
START and END signify the region delimiters."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov) (overlay-get ov 'ai-workbench-context))
                      (overlays-in start end))))

(defun ai-workbench-context--at-point ()
  "Return the context overlay at point, if any."
  (cl-find-if (lambda (ov) (overlay-get ov 'ai-workbench-context))
              (overlays-at (point))))

;;;###autoload
(defun ai-workbench-context--collect (&optional context-alist)
  "Get the list of all active context sources from CONTEXT-ALIST.

CONTEXT-ALIST defaults to the current value of `ai-workbench-context'.

Ignore overlays, buffers and files that are not live or readable."
  ;; Get only the non-degenerate overlays, collect them, and update the overlays variable.
  (let ((res))
    (dolist (entry (or context-alist ai-workbench-context))
      (pcase entry                      ;Context entry is:
        (`(,buf . ,data)
         (cond
          ((buffer-live-p buf)
           ;; (<buf> :overlays ... :lines ... :bounds ...)
           (when-let* ((ovs (plist-get data :overlays))) ;Clear dead overlays
             (plist-put data :overlays (cl-remove-if-not #'overlay-start ovs)))
           (push (cons buf data) res))
          ((and (stringp buf) (file-readable-p buf))
           ;; ("/file/path" :mime ... :bounds ... :line ...)
           (push (cons buf data) res))))

        ((and (pred stringp) (pred file-readable-p)) ;Just a file, figure out mimetype
         (if (file-directory-p entry)
             (progn
               (unless ai-workbench-context--reset-cache
                 (setq ai-workbench-context--reset-cache t)
                 (run-at-time
                  0 nil
                  (lambda () (setq ai-workbench-context--reset-cache nil
                              ai-workbench-context--project-files nil))))
               (dolist (f (directory-files-recursively entry "."))
                 (unless (ai-workbench-context--skip-p f)
                   (push `(,f ,@(and (ai-workbench--file-binary-p f)
                                     (list :mime (mailcap-file-name-to-mime-type entry))))
                         res))))
           (push `(,entry ,@(and (ai-workbench--file-binary-p entry)
                                 (list :mime (mailcap-file-name-to-mime-type entry))))
                 res)))
        ((pred buffer-live-p) (push (list entry) res)))) ;Just a buffer
    res))

(defun ai-workbench-context--collect-regions (buffer context-data)
  "Collect BUFFER regions from CONTEXT-DATA specification.

CONTEXT-DATA is a plist with keys :overlays, :lines and :bounds.
Returns a sorted list of (START . END) position pairs."
  (let ((regions                   ; Collect bounds (already in position format)
         (when-let* ((bounds (plist-get context-data :bounds)))
           (if (consp (car bounds)) bounds (list bounds)))))
    (with-current-buffer buffer
      (without-restriction
        ;; Collect overlays
        (dolist (ov (plist-get context-data :overlays))
          (when (overlay-start ov)
            (push (cons (overlay-start ov) (overlay-end ov))
                  regions)))                 ;(start1 . end1)
        ;; Collect lines (convert line numbers to positions)
        (when-let* ((line-bounds (plist-get context-data :lines)))
          (dolist (pair (if (consp (car line-bounds)) ;Handle single (BEG . END)
                            line-bounds (list line-bounds)))
            (push (cons (progn (goto-char (point-min))
                               (forward-line (1- (car pair)))
                               (point))
                        (progn (goto-char (point-min))
                               (forward-line (cdr pair))
                               (point)))
                  regions)))))
    ;; TODO: Update sort for Emacs 28+ calling convention
    ;; Sort by start position.
    ;; NOTE: This can modify `:bounds' of `context-data' by side-effect!
    (sort regions #'car-less-than-car)))

(defun ai-workbench-context--insert-buffer-string (buffer context-data &optional header)
  "Insert at point a context string from CONTEXT-DATA in BUFFER.

CONTEXT-DATA is a plist with keys :overlays, :lines and :bounds to
include specific overlays, line ranges or position bounds instead of the
entire buffer.  See `ai-workbench-context'.

HEADER is an optional header to insert before the contents."
  (let ((is-top-snippet t)
        (previous-line 1)
        (regions (ai-workbench-context--collect-regions buffer context-data)))

    ;; Insert header
    (insert (or header (format "In buffer `%s`:\n\n```"(buffer-name buffer)))
            (ai-workbench--strip-mode-suffix (buffer-local-value
                                       'major-mode buffer))
            "\n")
    (if (not regions)
        (insert-buffer-substring-no-properties buffer)
      (dolist (region regions)
        (let ((start (car region)) (end (cdr region)))
          (let (lineno column)
            (with-current-buffer buffer
              (without-restriction
                (setq lineno (line-number-at-pos start t)
                      column (save-excursion (goto-char start) (current-column)))))
            ;; We do not need to insert a line number indicator if we have two regions
            ;; on the same line, because the previous region should have already put the
            ;; indicator.
            (unless (= previous-line lineno)
              (unless (= lineno 1)
                (unless is-top-snippet (insert "\n"))
                (insert (format "... (Line %d)\n" lineno))))
            (setq previous-line lineno)
            (unless (zerop column) (insert " ..."))
            (if is-top-snippet
                (setq is-top-snippet nil)
              (unless (= previous-line lineno) (insert "\n"))))
          (insert-buffer-substring-no-properties buffer start end)))
      (unless (>= (cdr (car (last regions))) (point-max))
        (insert "\n...")))
    (insert "\n```")))

(defun ai-workbench-context--insert-file-string (path &optional spec)
  "Insert at point the contents of file at PATH as context.

SPEC is a plist specifying :lines or position :bounds to include instead
of the entire file.  See `ai-workbench-context' for details."
  (if (not (and spec (or (plist-member spec :lines)
                         (plist-member spec :bounds))))
      ;; Insert whole file
      (ai-workbench--insert-file-string path)
    ;; Insert only regions from lines and/or bounds
    (let* ((visiting-buf (find-buffer-visiting ;Reuse buffer
                          path (lambda (b) (not (buffer-modified-p b)))))
           (file-buf (or visiting-buf   ;temp buf to dump file contents
                         (ai-workbench--temp-buffer " *ai-workbench-file-context*"))))
      (unless visiting-buf
        (with-current-buffer file-buf (insert-file-contents path)))
      (ai-workbench-context--insert-buffer-string
       file-buf spec (format "In file `%s`:\n\n```\n"
                             (abbreviate-file-name path)))
      (unless visiting-buf (kill-buffer file-buf)))))

(defun ai-workbench-context--string (context-alist)
  "Format the aggregated ai-workbench-engine context as annotated markdown fragments.

Returns a string.  CONTEXT-ALIST is a structure containing
context overlays, see `ai-workbench-context'."
  (with-temp-buffer
    (cl-loop for entry in context-alist
             for (source . spec) = (ensure-list entry)
             if (bufferp source)
             do (ai-workbench-context--insert-buffer-string source spec)
             else if (or (not (plist-get spec :mime))
                         (string-match-p "^text/" (plist-get spec :mime)))
             do (ai-workbench-context--insert-file-string source spec) end
             do (insert "\n\n")
             finally do
             (skip-chars-backward "\n\t\r ")
             (delete-region (point) (point-max))
             (unless (bobp)
               (goto-char (point-min))
               (insert "Request context:\n\n"))
             finally return
             (and (> (buffer-size) 0)
                  (buffer-string)))))

;;; Major mode for context inspection buffers
(defvar-keymap ai-workbench-context-buffer-mode-map
  "C-c C-c" #'ai-workbench-context-confirm
  "C-c C-k" #'ai-workbench-context-quit
  "RET"     #'ai-workbench-context-visit
  "n"       #'ai-workbench-context-next
  "p"       #'ai-workbench-context-previous
  "d"       #'ai-workbench-context-flag-deletion)

(define-derived-mode ai-workbench-context-buffer-mode special-mode "ai-workbench-context"
  "Major-mode for inspecting context used by ai-workbench-engine."
  :group 'ai-workbench-engine
  (add-hook 'post-command-hook #'ai-workbench-context--post-command
            nil t)
  (setq-local revert-buffer-function #'ai-workbench-context--buffer-setup))

;; FIXME(targeted-context): This does not handle :bounds and :lines.  Reuse
;; `ai-workbench-context--insert-buffer-string'?
(defun ai-workbench-context--buffer-setup (&optional _ignore-auto _noconfirm context-alist)
  "Set up the ai-workbench-engine context buffer.

CONTEXT-ALIST is the alist of contexts to use to populate the buffer."
  (with-current-buffer (get-buffer-create "*ai-workbench-context*")
    (ai-workbench-context-buffer-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq header-line-format
            (substitute-command-keys
             (concat
              "\\[ai-workbench-context-flag-deletion]: Mark/unmark deletion, "
              "\\[ai-workbench-context-next]/\\[ai-workbench-context-previous]: next/previous, "
              "\\[ai-workbench-context-visit]: visit, "
              "\\[ai-workbench-context-confirm]: apply, "
              "\\[ai-workbench-context-quit]: cancel, "
              "\\[quit-window]: quit")))
      (save-excursion
        (let ((contexts (ai-workbench-context--collect context-alist)))
          (if (length= contexts 0)
              (insert "There are no active ai-workbench-engine contexts.")
            (let (beg ov l1 l2)
              (pcase-dolist (`(,buf . ,spec) contexts)
                (cond
                 ((bufferp buf)
                  (if (not spec)      ;BUF is a full buffer, not specific ranges
                      (progn
                        (insert (propertize (format "In buffer %s:\n\n"
                                                    (buffer-name buf))
                                            'face 'bold))
                        (setq beg (point))
                        (insert-buffer-substring buf)
                        (insert "\n")
                        (setq ov (make-overlay beg (point))))
                    (dolist (source-ov (plist-get spec :overlays)) ;BUF is a buffer with some overlay(s)
                      (with-current-buffer buf
                        (setq l1 (line-number-at-pos (overlay-start source-ov))
                              l2 (line-number-at-pos (overlay-end source-ov))))
                      (insert (propertize (format "In buffer %s (lines %d-%d):\n\n"
                                                  (buffer-name buf) l1 l2)
                                          'face 'bold))
                      (setq beg (point))
                      (insert-buffer-substring
                       buf (overlay-start source-ov) (overlay-end source-ov))
                      (insert "\n")
                      (setq ov (make-overlay beg (point)))
                      (overlay-put ov 'ai-workbench-context source-ov)
                      (overlay-put ov 'ai-workbench-overlay t)
                      (overlay-put ov 'evaporate t)))
                  (insert "\n" (make-separator-line) "\n"))
                 (t                     ;BUF is a file path, not a buffer
                  (insert (propertize (format "In file %s:\n\n" (file-name-nondirectory buf))
                                      'face 'bold))
                  (setq beg (point))
                  (if-let* ((mime (plist-get spec :mime))
                            ((not (string-match-p "^text/" mime)))) ;BUF is a binary file
                      (if-let* (((string-match-p (image-file-name-regexp) buf))
                                (img (create-image buf)))
                          (insert-image img "*") ; Can be displayed
                        (insert
                         buf " " (propertize "(No preview for binary file)"
                                             'face '(:inherit shadow :slant italic))))
                    (insert-file-contents buf))
                  (goto-char (point-max))
                  (insert "\n")
                  (setq ov (make-overlay beg (point)))
                  (overlay-put ov 'ai-workbench-context buf)
                  (overlay-put ov 'ai-workbench-overlay t)
                  (overlay-put ov 'evaporate t)
                  (insert "\n" (make-separator-line) "\n"))))
              (goto-char (point-min)))))))
    (display-buffer (current-buffer)
                    `((display-buffer-reuse-window
                       display-buffer-reuse-mode-window
                       display-buffer-below-selected)
                      (body-function . ,#'select-window)
                      (window-height . ,#'fit-window-to-buffer)))))

(defvar ai-workbench-context--buffer-reverse nil
  "Last direction of cursor movement in ai-workbench-engine context buffer.

If non-nil, indicates backward movement.")

(defalias 'ai-workbench-context--post-command
  (let ((highlight-overlay))
    (lambda ()
      ;; Only update if point moved outside the current region.
      (unless (memq highlight-overlay (overlays-at (point)))
        (let ((context-overlay
               (cl-loop for ov in (overlays-at (point))
                        thereis (and (overlay-get ov 'ai-workbench-overlay) ov))))
          (when highlight-overlay
            (overlay-put highlight-overlay 'face nil))
          (when context-overlay
            (overlay-put context-overlay 'face 'highlight))
          (setq highlight-overlay context-overlay))))))

(defun ai-workbench-context-visit ()
  "Display the location of this ai-workbench-engine context chunk in its original buffer."
  (interactive)
  (let ((ov-here (car (overlays-at (point)))))
    (if-let* ((source (overlay-get ov-here 'ai-workbench-context))
              (buf (if (overlayp source)
                       (overlay-buffer source)
                     (find-file-noselect source)))
              (offset (- (point) (overlay-start ov-here))))
        (with-selected-window (display-buffer buf)
          (goto-char (if (overlayp source)
                         (overlay-start source)
                       (point-min)))
          (forward-char offset)
          (recenter))
      (message "No source location for this ai-workbench-engine context chunk."))))

(defun ai-workbench-context-next ()
  "Move to next ai-workbench-engine context chunk."
  (interactive)
  (let ((ov-here (car (overlays-at (point))))
        (next-start (next-overlay-change (point))))
    (when (and (/= (point-max) next-start) ov-here)
      ;; We were inside the overlay, so we want the next overlay change, which
      ;; would be the start of the next overlay.
      (setq next-start (next-overlay-change next-start)))
    (when (/= next-start (point-max))
      (setq ai-workbench-context--buffer-reverse nil)
      (goto-char next-start)
      (recenter (floor (window-height) 4)))))

(defun ai-workbench-context-previous ()
  "Move to previous ai-workbench-engine context chunk."
  (interactive)
  (let ((ov-here (car (overlays-at (point)))))
    (when ov-here (goto-char (overlay-start ov-here)))
    (let ((previous-context-pos (previous-overlay-change
                                 (previous-overlay-change (point)))))
      ;; Prevent point from jumping to the start of the buffer.
      (unless (= previous-context-pos (point-min))
        (goto-char previous-context-pos)
        (recenter (floor (window-height) 4))
        (setq ai-workbench-context--buffer-reverse t)))))

(defun ai-workbench-context-flag-deletion ()
  "Mark ai-workbench-engine context chunk at point for removal."
  (interactive)
  (let* ((overlays (if (use-region-p)
                       (overlays-in (region-beginning) (region-end))
                     (overlays-at (point))))
         (deletion-ov)
         (marked-ovs (cl-remove-if-not (lambda (ov) (overlay-get ov 'ai-workbench-context-deletion-mark))
                                       overlays)))
    (if marked-ovs
        (mapc #'delete-overlay marked-ovs)
      (save-excursion
        (dolist (ov overlays)
          (when (overlay-get ov 'ai-workbench-context)
            (goto-char (overlay-start ov))
            (setq deletion-ov (make-overlay (overlay-start ov) (overlay-end ov)))
            (overlay-put deletion-ov 'ai-workbench-context (overlay-get ov 'ai-workbench-context))
            (overlay-put deletion-ov 'priority -80)
            (overlay-put deletion-ov 'face 'ai-workbench-context-deletion-face)
            (overlay-put deletion-ov 'ai-workbench-context-deletion-mark t)))))
    (if (use-region-p)
        (deactivate-mark)
      (if ai-workbench-context--buffer-reverse
          (ai-workbench-context-previous)
        (ai-workbench-context-next)))))

(defun ai-workbench-context-quit ()
  "Cancel pending operations and return to ai-workbench-engine's menu."
  (interactive)
  (quit-window)
  (call-interactively #'ai-workbench-menu))

(defun ai-workbench-context-confirm ()
  "Confirm pending operations and return to ai-workbench-engine's menu."
  (interactive)
  ;; Delete all the context overlays that have been marked for deletion.
  (when-let* ((deletion-marks
               (delq nil (mapcar
                          (lambda (ov)
                            (and
                             (overlay-get ov 'ai-workbench-context-deletion-mark)
                             (overlay-get ov 'ai-workbench-context)))
                          (overlays-in (point-min) (point-max))))))
    (mapc #'ai-workbench-context-remove deletion-marks)
    (revert-buffer))
  ;; FIXME(context): This should run in the buffer from which the context
  ;; inspection buffer was visited.
  ;; Update contexts and revert buffer (#482)
  (setq ai-workbench-context (nreverse (ai-workbench-context--collect)))
  (ai-workbench-context-quit))

(provide 'ai-workbench-context)
;;; ai-workbench-context.el ends here.
