;;; init-note-agenda.el --- Aggregate Typst note task chips -*- lexical-binding: t; -*-

;;; Commentary:
;; Scans every Typst note below `my/note-root' for inline task chips defined
;; in `lisp/note/typst/note.typ' (`#todo[...]', `#doing[...]', ...) and
;; presents them grouped by state in a `*Note Agenda*' buffer.  Lines act as
;; buttons that jump to the source file and line.

;;; Code:

(require 'init-note)
(require 'seq)
(require 'subr-x)

(defcustom my/note-task-states
  '("todo" "doing" "waiting" "done" "cancelled")
  "Typst task chip names recognised by `my/note-agenda'.
Order here determines the section order in the agenda buffer."
  :type '(repeat string)
  :group 'my/note)

(defcustom my/note-agenda-active-states '("todo" "doing" "waiting")
  "Subset of `my/note-task-states' shown by `my/note-agenda-active'."
  :type '(repeat string)
  :group 'my/note)

(defface my/note-agenda-state-todo
  '((t :inherit error :weight bold))
  "Face for TODO chips in the agenda buffer."
  :group 'my/note)

(defface my/note-agenda-state-doing
  '((t :inherit warning :weight bold))
  "Face for DOING chips in the agenda buffer."
  :group 'my/note)

(defface my/note-agenda-state-waiting
  '((t :inherit shadow :weight bold))
  "Face for WAITING chips in the agenda buffer."
  :group 'my/note)

(defface my/note-agenda-state-done
  '((t :inherit success :weight bold :strike-through t))
  "Face for DONE chips in the agenda buffer."
  :group 'my/note)

(defface my/note-agenda-state-cancelled
  '((t :inherit shadow :strike-through t))
  "Face for CANCELLED chips in the agenda buffer."
  :group 'my/note)

(defun my/note-agenda--state-face (state)
  "Return the face used for STATE."
  (intern (format "my/note-agenda-state-%s" state)))

(defun my/note-agenda--regexp ()
  "Return the regexp matching any configured task chip."
  (concat "#\\("
          (mapconcat #'regexp-quote my/note-task-states "\\|")
          "\\)\\[\\([^]\n]*\\)\\]"))

(defun my/note-agenda--scan-file (file)
  "Return a list of task plists found in FILE."
  (let (entries)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((regexp (my/note-agenda--regexp)))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (push (list :file file
                      :line (line-number-at-pos (match-beginning 0))
                      :state (match-string-no-properties 1)
                      :body (string-trim
                             (match-string-no-properties 2)))
                entries))))
    (nreverse entries)))

(defun my/note-agenda--collect (&optional states)
  "Return task plists across every Typst note.
When STATES is non-nil, restrict the result to those state names."
  (let (all)
    (dolist (file (my/note--typst-files))
      (setq all (nconc all (my/note-agenda--scan-file file))))
    (if states
        (seq-filter (lambda (entry)
                      (member (plist-get entry :state) states))
                    all)
      all)))

(defun my/note-agenda--jump (button)
  "Open the file/line referenced by BUTTON."
  (find-file (button-get button 'file))
  (goto-char (point-min))
  (forward-line (1- (button-get button 'line))))

(defun my/note-agenda--insert-entry (entry)
  "Insert ENTRY into the current buffer as a clickable line."
  (let* ((file (plist-get entry :file))
         (line (plist-get entry :line))
         (body (plist-get entry :body))
         (rel (file-relative-name file my/note-root)))
    (insert "  ")
    (insert-text-button
     (format "%s:%d" rel line)
     'follow-link t
     'file file
     'line line
     'face 'link
     'action #'my/note-agenda--jump)
    (insert "  " body "\n")))

(defun my/note-agenda--render (entries title)
  "Render ENTRIES into a `*Note Agenda*' buffer with TITLE."
  (let ((buffer (get-buffer-create "*Note Agenda*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize title 'face 'bold) "\n")
        (insert (make-string (length title) ?-) "\n\n")
        (dolist (state my/note-task-states)
          (let ((items (seq-filter
                        (lambda (e) (string= (plist-get e :state) state))
                        entries)))
            (when items
              (insert (propertize (format "%s (%d)"
                                          (upcase state)
                                          (length items))
                                  'face (my/note-agenda--state-face state))
                      "\n")
              (dolist (item items)
                (my/note-agenda--insert-entry item))
              (insert "\n"))))
        (when (zerop (length entries))
          (insert "No matching tasks.\n"))
        (goto-char (point-min)))
      (special-mode))
    (pop-to-buffer buffer)))

;;;###autoload
(defun my/note-agenda (&optional all)
  "Show active task chips across notes.  With prefix ALL, include DONE/CANCELLED."
  (interactive "P")
  (let* ((states (if all my/note-task-states my/note-agenda-active-states))
         (entries (my/note-agenda--collect states))
         (title (if all "Note Agenda (all)" "Note Agenda")))
    (my/note-agenda--render entries title)))

(provide 'init-note-agenda)
;;; init-note-agenda.el ends here
