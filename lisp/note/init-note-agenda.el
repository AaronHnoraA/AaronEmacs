;;; init-note-agenda.el --- Aggregate Typst note task chips -*- lexical-binding: t; -*-

;;; Commentary:
;; Scans Typst notes below `my/note-root' for inline task chips defined in
;; `lisp/note/typst/note.typ' (`#todo[...]', `#doing[...]', ...) and presents
;; them either as an agenda list or a compact board.  Entries are buttons that
;; jump to the source file and line.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'init-note)
(require 'seq)
(require 'subr-x)

(defcustom my/note-task-states
  '("todo" "doing" "waiting" "done" "cancelled")
  "Typst task chip names recognised by note agenda commands.
Order here determines the section order in agenda buffers."
  :type '(repeat string)
  :group 'my/note)

(defcustom my/note-agenda-active-states '("todo" "doing" "waiting")
  "Subset of `my/note-task-states' shown by `my/note-agenda'."
  :type '(repeat string)
  :group 'my/note)

(defcustom my/note-agenda-board-states '("todo" "doing" "waiting")
  "Task states shown as columns by `my/note-agenda-board'."
  :type '(repeat string)
  :group 'my/note)

(defcustom my/note-agenda-board-column-width 36
  "Preferred column width in `my/note-agenda-board'."
  :type 'integer
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

(defvar-local my/note-agenda--last-all nil
  "Non-nil when the current note agenda buffer includes closed tasks.")

(defvar-local my/note-agenda--last-view 'list
  "Current note agenda view type.")

(defvar my/note-agenda-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'my/note-agenda-refresh)
    (define-key map (kbd "b") #'my/note-agenda-board)
    (define-key map (kbd "B") (lambda ()
                                (interactive)
                                (my/note-agenda-board t)))
    (define-key map (kbd "l") #'my/note-agenda)
    (define-key map (kbd "L") (lambda ()
                                (interactive)
                                (my/note-agenda t)))
    map)
  "Keymap for `my/note-agenda-mode'.")

(define-derived-mode my/note-agenda-mode special-mode "Note Agenda"
  "Major mode for Typst note agenda buffers."
  (setq-local truncate-lines t))

(defun my/note-agenda--state-face (state)
  "Return the face used for STATE."
  (intern (format "my/note-agenda-state-%s" state)))

(defun my/note-agenda--regexp ()
  "Return the regexp matching any configured task chip."
  (concat "#\\("
          (mapconcat #'regexp-quote my/note-task-states "\\|")
          "\\)\\[\\([^]\n]*\\)\\]"))

(defun my/note-agenda--commented-match-p (position)
  "Return non-nil when POSITION is after a line comment marker."
  (save-excursion
    (let ((line-start (line-beginning-position)))
      (goto-char line-start)
      (search-forward "//" position t))))

(defun my/note-agenda--current-heading ()
  "Return the nearest preceding Typst heading text."
  (save-excursion
    (when (re-search-backward "^=+[ \t]+\\(.+\\)$" nil t)
      (string-trim (match-string-no-properties 1)))))

(defun my/note-agenda--scan-file (file)
  "Return a list of task plists found in FILE."
  (let (entries)
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((note (ignore-errors (my/note-parse-current-buffer file)))
             (title (or (plist-get note :title)
                        (file-name-base file)))
             (regexp (my/note-agenda--regexp)))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (unless (my/note-agenda--commented-match-p (match-beginning 0))
            (push (list :file file
                        :line (line-number-at-pos (match-beginning 0))
                        :state (match-string-no-properties 1)
                        :body (string-trim
                               (match-string-no-properties 2))
                        :title title
                        :heading (my/note-agenda--current-heading))
                  entries)))))
    (nreverse entries)))

(defun my/note-agenda--state-index (state states)
  "Return STATE position in STATES, or a large fallback index."
  (or (cl-position state states :test #'string=)
      most-positive-fixnum))

(defun my/note-agenda--sort (entries &optional states)
  "Return ENTRIES sorted by STATES, file, then line."
  (let ((states (or states my/note-task-states)))
    (sort entries
          (lambda (left right)
            (let ((left-state (my/note-agenda--state-index
                               (plist-get left :state) states))
                  (right-state (my/note-agenda--state-index
                                (plist-get right :state) states)))
              (cond
               ((/= left-state right-state) (< left-state right-state))
               ((not (string= (plist-get left :file)
                              (plist-get right :file)))
                (string< (plist-get left :file)
                         (plist-get right :file)))
               (t
                (< (plist-get left :line)
                   (plist-get right :line)))))))))

(defun my/note-agenda--collect (&optional states)
  "Return task plists across every Typst note.
When STATES is non-nil, restrict the result to those state names."
  (let (all)
    (dolist (file (my/note--typst-files))
      (setq all (nconc all (my/note-agenda--scan-file file))))
    (my/note-agenda--sort
     (if states
         (seq-filter (lambda (entry)
                       (member (plist-get entry :state) states))
                     all)
       all)
     states)))

(defun my/note-agenda--jump (button)
  "Open the file/line referenced by BUTTON."
  (find-file (button-get button 'file))
  (goto-char (point-min))
  (forward-line (1- (button-get button 'line))))

(defun my/note-agenda--location (entry)
  "Return a compact source location string for ENTRY."
  (format "%s:%d"
          (file-relative-name (plist-get entry :file)
                              (file-name-as-directory my/note-root))
          (plist-get entry :line)))

(defun my/note-agenda--entry-context (entry)
  "Return title and heading context for ENTRY."
  (let ((title (or (plist-get entry :title) "Untitled"))
        (heading (plist-get entry :heading)))
    (if (and heading (not (string-empty-p heading)))
        (format "%s > %s" title heading)
      title)))

(defun my/note-agenda--insert-button (label entry &optional face)
  "Insert LABEL as a button for ENTRY.
Optional FACE overrides the default link face."
  (insert-text-button
   label
   'follow-link t
   'file (plist-get entry :file)
   'line (plist-get entry :line)
   'face (or face 'link)
   'help-echo (format "%s  %s"
                      (my/note-agenda--location entry)
                      (plist-get entry :body))
   'action #'my/note-agenda--jump))

(defun my/note-agenda--insert-entry (entry)
  "Insert ENTRY into the current buffer as a clickable line."
  (insert "  ")
  (my/note-agenda--insert-button (my/note-agenda--location entry) entry)
  (insert "  "
          (propertize (my/note-agenda--entry-context entry) 'face 'bold)
          "  "
          (plist-get entry :body)
          "\n"))

(defun my/note-agenda--buffer ()
  "Return the agenda buffer."
  (get-buffer-create "*Note Agenda*"))

(defun my/note-agenda--render-list (entries title all)
  "Render ENTRIES into a list agenda buffer with TITLE.
ALL records whether closed tasks are included."
  (let ((buffer (my/note-agenda--buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize title 'face 'bold) "\n")
        (insert (make-string (length title) ?-) "\n\n")
        (dolist (state my/note-task-states)
          (let ((items (seq-filter
                        (lambda (entry)
                          (string= (plist-get entry :state) state))
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
        (when (null entries)
          (insert "No matching tasks.\n"))
        (goto-char (point-min)))
      (my/note-agenda-mode)
      (setq-local my/note-agenda--last-all all)
      (setq-local my/note-agenda--last-view 'list))
    (pop-to-buffer buffer)))

(defun my/note-agenda--pad (text width)
  "Return TEXT truncated and padded to WIDTH display columns."
  (let ((text (truncate-string-to-width text width 0 nil t)))
    (concat text (make-string (max 0 (- width (string-width text))) ? ))))

(defun my/note-agenda--board-column-width (states)
  "Return a useful board column width for STATES."
  (let* ((count (max 1 (length states)))
         (available (- (window-width) (* 2 (1- count))))
         (dynamic (max 24 (/ available count))))
    (min my/note-agenda-board-column-width dynamic)))

(defun my/note-agenda--board-label (entry)
  "Return the board cell label for ENTRY."
  (format "%s: %s"
          (my/note-agenda--entry-context entry)
          (plist-get entry :body)))

(defun my/note-agenda--insert-board-cell (entry width)
  "Insert one board cell for ENTRY padded to WIDTH."
  (if entry
      (let* ((label (my/note-agenda--board-label entry))
             (text (my/note-agenda--pad label width)))
        (my/note-agenda--insert-button text entry 'link))
    (insert (make-string width ? ))))

(defun my/note-agenda--group-by-state (entries states)
  "Return ENTRIES grouped by STATES."
  (mapcar (lambda (state)
            (cons state
                  (seq-filter (lambda (entry)
                                (string= (plist-get entry :state) state))
                              entries)))
          states))

(defun my/note-agenda--render-board (entries states title all)
  "Render ENTRIES as a state board for STATES with TITLE.
ALL records whether closed tasks are included."
  (let* ((buffer (my/note-agenda--buffer))
         (groups (my/note-agenda--group-by-state entries states))
         (width (my/note-agenda--board-column-width states))
         (max-rows (apply #'max 0 (mapcar (lambda (group)
                                            (length (cdr group)))
                                          groups))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize title 'face 'bold) "\n")
        (insert (make-string (length title) ?-) "\n\n")
        (cl-loop for group in groups
                 for state = (car group)
                 for last = (eq group (car (last groups)))
                 do (insert (propertize
                             (my/note-agenda--pad
                              (format "%s (%d)"
                                      (upcase state)
                                      (length (cdr group)))
                              width)
                             'face (my/note-agenda--state-face state)))
                 unless last do (insert "  "))
        (insert "\n")
        (cl-loop repeat (length groups)
                 for index from 0
                 do (insert (make-string width ?-))
                 unless (= index (1- (length groups))) do (insert "  "))
        (insert "\n")
        (dotimes (row max-rows)
          (cl-loop for group in groups
                   for last = (eq group (car (last groups)))
                   for entry = (nth row (cdr group))
                   do (my/note-agenda--insert-board-cell entry width)
                   unless last do (insert "  "))
          (insert "\n"))
        (when (zerop max-rows)
          (insert "No matching tasks.\n"))
        (goto-char (point-min)))
      (my/note-agenda-mode)
      (setq-local my/note-agenda--last-all all)
      (setq-local my/note-agenda--last-view 'board))
    (pop-to-buffer buffer)))

;;;###autoload
(defun my/note-agenda (&optional all)
  "Show active task chips across notes.
With prefix ALL, include DONE/CANCELLED."
  (interactive "P")
  (let* ((states (if all my/note-task-states my/note-agenda-active-states))
         (entries (my/note-agenda--collect states))
         (title (if all "Note Agenda (all)" "Note Agenda")))
    (my/note-agenda--render-list entries title all)))

;;;###autoload
(defun my/note-agenda-board (&optional all)
  "Show task chips as an Org-style agenda board.
With prefix ALL, include DONE/CANCELLED columns."
  (interactive "P")
  (let* ((states (if all my/note-task-states my/note-agenda-board-states))
         (entries (my/note-agenda--collect states))
         (title (if all "Note Agenda Board (all)" "Note Agenda Board")))
    (my/note-agenda--render-board entries states title all)))

(defun my/note-agenda-refresh ()
  "Refresh the current note agenda buffer."
  (interactive)
  (pcase my/note-agenda--last-view
    ('board (my/note-agenda-board my/note-agenda--last-all))
    (_ (my/note-agenda my/note-agenda--last-all))))

(provide 'init-note-agenda)
;;; init-note-agenda.el ends here
