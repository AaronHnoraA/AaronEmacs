;;; init-note-agenda.el --- Aggregate Typst note task chips -*- lexical-binding: t; -*-

;;; Commentary:
;; Scans Typst notes below `my/note-root' for inline task chips defined in
;; `lisp/note/typst/note.typ' (`#todo[...]', `#doing[...]', ...) and presents
;; them either as an agenda list or a compact board.  Entries are buttons that
;; jump to the source file and line.

;;; Code:

(require 'button)
(require 'calendar)
(require 'cl-lib)
(require 'init-note)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'transient)

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

(defcustom my/note-agenda-closed-states '("done" "cancelled")
  "Closed task states shown by `my/note-agenda-board-closed'."
  :type '(repeat string)
  :group 'my/note)

(defcustom my/note-agenda-board-column-width 36
  "Preferred column width in `my/note-agenda-board'."
  :type 'integer
  :group 'my/note)

(defcustom my/note-agenda-upcoming-days 14
  "Number of days considered upcoming for dated task chips."
  :type 'integer
  :group 'my/note)

(defcustom my/note-agenda-calendar-preview-limit 8
  "Maximum dated entries shown above the agenda board columns."
  :type 'integer
  :group 'my/note)

(defcustom my/note-agenda-priorities '("A" "B" "C")
  "Task priorities recognised in Typst task chip metadata."
  :type '(repeat string)
  :group 'my/note)

(defconst my/note-task-metadata-keys
  '("priority" "due" "scheduled" "repeat")
  "Canonical metadata keys for Typst task chips.")

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

(defface my/note-agenda-priority-a
  '((t :inherit error :weight bold))
  "Face for high-priority note tasks."
  :group 'my/note)

(defface my/note-agenda-priority-b
  '((t :inherit warning :weight bold))
  "Face for medium-priority note tasks."
  :group 'my/note)

(defface my/note-agenda-priority-c
  '((t :inherit shadow :weight bold))
  "Face for low-priority note tasks."
  :group 'my/note)

(defface my/note-agenda-date-overdue
  '((t :inherit error :weight bold))
  "Face for overdue note task dates."
  :group 'my/note)

(defface my/note-agenda-date-today
  '((t :inherit warning :weight bold))
  "Face for note task dates due today."
  :group 'my/note)

(defface my/note-agenda-date-upcoming
  '((t :inherit success))
  "Face for upcoming note task dates."
  :group 'my/note)

(defface my/note-agenda-date-future
  '((t :inherit shadow))
  "Face for later note task dates."
  :group 'my/note)

(defvar-local my/note-agenda--last-all nil
  "Non-nil when the current note agenda buffer includes closed tasks.")

(defvar-local my/note-agenda--last-view 'list
  "Current note agenda view type.")

(defvar-local my/note-agenda--last-states nil
  "Task states used to render the current note agenda buffer.")

(defvar-local my/note-agenda--last-scope 'active
  "Task scope used to render the current note agenda buffer.")

(defvar-local my/note-agenda--board-rendered-width nil
  "Column width used by the current agenda board rendering.")

(defvar my/note-agenda-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'my/note-agenda-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'my/note-task-dispatch)
    (define-key map (kbd "RET") #'my/note-agenda-open-at-point)
    (define-key map (kbd "o") #'my/note-agenda-open-at-point)
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    (define-key map (kbd "h") #'my/note-agenda-left)
    (define-key map (kbd "l") #'my/note-agenda-right)
    (define-key map (kbd "b") #'my/note-agenda-board)
    (define-key map (kbd "B") (lambda ()
                                (interactive)
                                (my/note-agenda-board t)))
    (define-key map (kbd "C") #'my/note-agenda-board-closed)
    (define-key map (kbd "v") #'my/note-agenda)
    (define-key map (kbd "V") (lambda ()
                                (interactive)
                                (my/note-agenda t)))
    (define-key map (kbd "c") #'my/note-task-cycle-state)
    (define-key map (kbd "t") #'my/note-task-set-state)
    (define-key map (kbd "s") #'my/note-task-set-scheduled)
    (define-key map (kbd "D") #'my/note-task-set-due)
    (define-key map (kbd "p") #'my/note-task-set-priority)
    (define-key map (kbd "r") #'my/note-task-set-repeat)
    (define-key map (kbd "0") #'my/note-task-clear-date-metadata)
    (define-key map (kbd ".") #'my/note-task-set-scheduled-today)
    (define-key map (kbd "!") #'my/note-task-set-due-today)
    (define-key map (kbd ">") #'my/note-task-shift-date-forward)
    (define-key map (kbd "<") #'my/note-task-shift-date-backward)
    (define-key map (kbd "K") #'calendar)
    map)
  "Keymap for `my/note-agenda-mode'.")

(define-derived-mode my/note-agenda-mode special-mode "Note Agenda"
  "Major mode for Typst note agenda buffers."
  (setq-local truncate-lines t))

(defun my/note-agenda--state-face (state)
  "Return the face used for STATE."
  (intern (format "my/note-agenda-state-%s" state)))

(defun my/note-task--canonical-metadata-key (key)
  "Return canonical task metadata KEY, or nil when KEY is unsupported."
  (pcase (downcase (or key ""))
    ((or "p" "prio" "priority") "priority")
    ((or "d" "deadline" "due") "due")
    ((or "s" "sched" "scheduled") "scheduled")
    ((or "r" "repeat" "recurrence") "repeat")
    (_ nil)))

(defun my/note-task--clean-metadata-value (value)
  "Return unquoted task metadata VALUE."
  (let ((value (string-trim (or value ""))))
    (when (and (>= (length value) 2)
               (string-prefix-p "\"" value)
               (string-suffix-p "\"" value))
      (setq value (substring value 1 -1))
      (setq value (replace-regexp-in-string "\\\\\"" "\"" value t t))
      (setq value (replace-regexp-in-string "\\\\\\\\" "\\\\" value t t)))
    value))

(defun my/note-task--parse-args (args)
  "Parse Typst task ARGS into an alist of canonical keys."
  (save-match-data
    (let ((pos 0)
          parsed)
      (while (and args
                  (string-match
                   "\\([[:alpha:]_-][[:alnum:]_-]*\\)[ \t\n]*:[ \t\n]*\\(\"[^\"]*\"\\|[^,)\n]+\\)"
                   args pos))
        (let* ((key (my/note-task--canonical-metadata-key
                     (match-string 1 args)))
               (value (my/note-task--clean-metadata-value
                       (match-string 2 args))))
          (setq pos (match-end 0))
          (when key
            (setq parsed (assq-delete-all (intern key) parsed))
            (push (cons (intern key) value) parsed))))
      (nreverse parsed))))

(defun my/note-task--arg (args key)
  "Return from ARGS the value for KEY."
  (cdr (assq (intern key) args)))

(defun my/note-task--typst-string (value)
  "Return VALUE encoded as a Typst string literal."
  (format "%S" (or value "")))

(defun my/note-task--format-args (args)
  "Return a Typst argument list for task metadata ARGS."
  (let (parts)
    (dolist (key my/note-task-metadata-keys)
      (when-let* ((value (my/note-task--arg args key))
                  ((not (string-empty-p value))))
        (push (format "%s: %s" key (my/note-task--typst-string value))
              parts)))
    (if parts
        (concat "(" (string-join (nreverse parts) ", ") ")")
      "")))

(defun my/note-task--source (state args body)
  "Return Typst source for task STATE ARGS and BODY."
  (format "#%s%s[%s]" state (my/note-task--format-args args) body))

(defun my/note-agenda--regexp (&optional states)
  "Return the regexp matching task chips in STATES.
When STATES is nil, match every configured task state."
  (concat "#\\("
          (mapconcat #'regexp-quote (or states my/note-task-states) "\\|")
          "\\)\\(?:[ \t\n]*(\\([^)\n]*\\))\\)?\\[\\([^]\n]*\\)\\]"))

(defun my/note-task--bounds-at-point ()
  "Return task chip bounds and metadata at point on the current line."
  (let ((regexp (my/note-agenda--regexp))
        (pos (point))
        found)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (and (not found)
                  (re-search-forward regexp (line-end-position) t))
        (when (and (<= (match-beginning 0) pos)
                   (<= pos (match-end 0)))
          (setq found
                (list :begin (match-beginning 0)
                      :end (match-end 0)
                      :state (match-string-no-properties 1)
                      :args-source (match-string-no-properties 2)
                      :args (my/note-task--parse-args
                             (match-string-no-properties 2))
                      :body (string-trim
                             (match-string-no-properties 3)))))))
    found))

(defun my/note-task--entry-bounds (entry)
  "Move to ENTRY and return its task bounds."
  (goto-char (point-min))
  (forward-line (1- (plist-get entry :line)))
  (forward-char (or (plist-get entry :column) 0))
  (or (my/note-task--bounds-at-point)
      (save-excursion
        (goto-char (line-beginning-position))
        (when (re-search-forward (my/note-agenda--regexp)
                                 (line-end-position) t)
          (list :begin (match-beginning 0)
                :end (match-end 0)
                :state (match-string-no-properties 1)
                :args-source (match-string-no-properties 2)
                :args (my/note-task--parse-args
                       (match-string-no-properties 2))
                :body (string-trim
                       (match-string-no-properties 3)))))))

(defun my/note-task--replace-bounds (bounds state args)
  "Replace task BOUNDS with STATE and ARGS, preserving its body."
  (let ((body (plist-get bounds :body))
        (start (plist-get bounds :begin))
        (end (plist-get bounds :end)))
    (delete-region start end)
    (goto-char start)
    (insert (my/note-task--source state args body))))

(defun my/note-task--alist-set (args key value)
  "Return ARGS with KEY set to VALUE, or removed when VALUE is empty."
  (let* ((symbol (intern key))
         (args (assq-delete-all symbol (copy-sequence args))))
    (if (and value (not (string-empty-p (string-trim value))))
        (append args (list (cons symbol (string-trim value))))
      args)))

(defun my/note-task--read-date (prompt &optional from-string)
  "Read an ISO date using Org's date prompt and calendar with PROMPT.
FROM-STRING is passed to `org-read-date' for noninteractive callers."
  (let ((value (org-read-date nil nil from-string prompt)))
    (unless (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" value)
      (user-error "Org date prompt did not return an ISO date"))
    (match-string 1 value)))

(defun my/note-task--today ()
  "Return today's date as an ISO string."
  (format-time-string "%Y-%m-%d"))

(defun my/note-task--date-add-days (date days)
  "Return DATE shifted by DAYS.
DATE must be an ISO date string."
  (format-time-string "%Y-%m-%d"
                      (time-add (date-to-time date)
                                (days-to-time days))))

(defun my/note-task--read-priority ()
  "Read a task priority.
An empty value clears the priority."
  (let ((value (completing-read "Priority: " my/note-agenda-priorities nil nil)))
    (if (string-empty-p value)
        ""
      (upcase value))))

(defun my/note-task--closed-state-p (state)
  "Return non-nil when STATE is a terminal task state."
  (member state '("done" "cancelled")))

(defun my/note-task--priority-rank (priority)
  "Return sort rank for PRIORITY."
  (or (cl-position (upcase (or priority "")) my/note-agenda-priorities
                   :test #'string=)
      (length my/note-agenda-priorities)))

(defun my/note-task--date-days (date)
  "Return absolute day number for ISO DATE, or nil."
  (when (and date
             (string-match-p
              "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" date))
    (ignore-errors (time-to-days (date-to-time date)))))

(defun my/note-task--date-sort-key (entry)
  "Return date sort key for ENTRY."
  (or (plist-get entry :due)
      (plist-get entry :scheduled)
      "9999-99-99"))

(defun my/note-task--date-delta (date)
  "Return DATE minus today in days, or nil."
  (when-let* ((days (my/note-task--date-days date)))
    (- days (time-to-days (current-time)))))

(defun my/note-agenda--date-face (date &optional due-p state)
  "Return face for DATE.
DUE-P means DATE is a due/deadline date.  STATE suppresses urgency for
closed task states."
  (cond
   ((or (not date) (my/note-task--closed-state-p state)) 'my/note-agenda-date-future)
   ((let ((delta (my/note-task--date-delta date)))
      (cond
       ((and due-p delta (< delta 0)) 'my/note-agenda-date-overdue)
       ((and delta (= delta 0)) 'my/note-agenda-date-today)
       ((and delta (<= delta my/note-agenda-upcoming-days))
        'my/note-agenda-date-upcoming)
       (t 'my/note-agenda-date-future))))))

(defun my/note-agenda--priority-face (priority)
  "Return face for PRIORITY."
  (pcase (upcase (or priority ""))
    ("A" 'my/note-agenda-priority-a)
    ("B" 'my/note-agenda-priority-b)
    ("C" 'my/note-agenda-priority-c)
    (_ 'shadow)))

(defun my/note-agenda--metadata-string (entry)
  "Return a compact metadata string for ENTRY."
  (let ((priority (plist-get entry :priority))
        (due (plist-get entry :due))
        (scheduled (plist-get entry :scheduled))
        (repeat (plist-get entry :repeat)))
    (string-join
     (delq nil
           (list
            (and priority (not (string-empty-p priority))
                 (format "[#%s]" (upcase priority)))
            (and due (not (string-empty-p due))
                 (format "due:%s" due))
            (and scheduled (not (string-empty-p scheduled))
                 (format "sched:%s" scheduled))
            (and repeat (not (string-empty-p repeat))
                 (format "rep:%s" repeat))))
     " ")))

(defun my/note-agenda--insert-metadata (entry)
  "Insert propertized metadata for ENTRY."
  (let ((priority (plist-get entry :priority))
        (due (plist-get entry :due))
        (scheduled (plist-get entry :scheduled))
        (repeat (plist-get entry :repeat))
        (state (plist-get entry :state))
        parts)
    (when (and priority (not (string-empty-p priority)))
      (push (propertize (format "[#%s]" (upcase priority))
                        'face (my/note-agenda--priority-face priority))
            parts))
    (when (and due (not (string-empty-p due)))
      (push (propertize (format "due:%s" due)
                        'face (my/note-agenda--date-face due t state))
            parts))
    (when (and scheduled (not (string-empty-p scheduled)))
      (push (propertize (format "sched:%s" scheduled)
                        'face (my/note-agenda--date-face scheduled nil state))
            parts))
    (when (and repeat (not (string-empty-p repeat)))
      (push (propertize (format "rep:%s" repeat) 'face 'shadow)
            parts))
    (when parts
      (insert "  " (string-join (nreverse parts) " ")))))

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

(defun my/note-agenda--scan-file (file &optional states)
  "Return a list of task plists found in FILE.
When STATES is non-nil, only those state names are matched at the regexp
level so closed tasks do not enter the default scan."
  (let (entries)
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((note (ignore-errors (my/note-parse-current-buffer file)))
             (title (or (plist-get note :title)
                        (file-name-base file)))
             (regexp (my/note-agenda--regexp states)))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (unless (my/note-agenda--commented-match-p (match-beginning 0))
            (let* ((args (my/note-task--parse-args
                          (match-string-no-properties 2)))
                   (state (match-string-no-properties 1))
                   (body (string-trim
                          (match-string-no-properties 3)))
                   (line-start (line-beginning-position))
                   (column (- (match-beginning 0) line-start)))
              (push (list :file file
                          :line (line-number-at-pos (match-beginning 0))
                          :column column
                          :state state
                          :args args
                          :priority (my/note-task--arg args "priority")
                          :due (my/note-task--arg args "due")
                          :scheduled (my/note-task--arg args "scheduled")
                          :repeat (my/note-task--arg args "repeat")
                          :body body
                          :title title
                          :heading (my/note-agenda--current-heading))
                    entries))))))
    (nreverse entries)))

(defun my/note-agenda--state-index (state states)
  "Return STATE position in STATES, or a large fallback index."
  (or (cl-position state states :test #'string=)
      most-positive-fixnum))

(defun my/note-agenda--sort (entries &optional states)
  "Return ENTRIES sorted by STATES, priority, dates, file, then line."
  (let ((states (or states my/note-task-states)))
    (sort entries
          (lambda (left right)
            (let ((left-state (my/note-agenda--state-index
                               (plist-get left :state) states))
                  (right-state (my/note-agenda--state-index
                                (plist-get right :state) states))
                  (left-priority (my/note-task--priority-rank
                                  (plist-get left :priority)))
                  (right-priority (my/note-task--priority-rank
                                   (plist-get right :priority)))
                  (left-date (my/note-task--date-sort-key left))
                  (right-date (my/note-task--date-sort-key right)))
              (cond
               ((/= left-state right-state) (< left-state right-state))
               ((/= left-priority right-priority)
                (< left-priority right-priority))
               ((not (string= left-date right-date))
                (string< left-date right-date))
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
      (setq all (nconc all (my/note-agenda--scan-file file states))))
    (my/note-agenda--sort all states)))

(defun my/note-agenda--jump (button)
  "Open the file/line referenced by BUTTON."
  (find-file (button-get button 'file))
  (goto-char (point-min))
  (forward-line (1- (button-get button 'line))))

(defun my/note-agenda-open-at-point ()
  "Open the task entry at point."
  (interactive)
  (if-let* ((entry (or (get-text-property (point) 'my/note-agenda-entry)
                       (and (button-at (point))
                            (button-get (button-at (point))
                                        'my/note-agenda-entry)))))
      (progn
        (find-file (plist-get entry :file))
        (goto-char (point-min))
        (forward-line (1- (plist-get entry :line)))
        (forward-char (or (plist-get entry :column) 0)))
    (user-error "No note task at point")))

(defun my/note-agenda--board-view-p ()
  "Return non-nil when the current agenda buffer is a board."
  (eq my/note-agenda--last-view 'board))

(defun my/note-agenda--move-board-column (delta)
  "Move horizontally by DELTA columns in a note agenda board."
  (unless (my/note-agenda--board-view-p)
    (user-error "Not in a note agenda board"))
  (let* ((states (or my/note-agenda--last-states my/note-agenda-board-states))
         (width (or my/note-agenda--board-rendered-width
                    my/note-agenda-board-column-width))
         (step (+ width 2))
         (index (max 0 (min (1- (max 1 (length states)))
                            (+ (floor (current-column) step) delta)))))
    (move-to-column (* index step))))

(defun my/note-agenda-left ()
  "Move left in board buffers, or to the beginning of the current line."
  (interactive)
  (if (my/note-agenda--board-view-p)
      (my/note-agenda--move-board-column -1)
    (beginning-of-line)))

(defun my/note-agenda-right ()
  "Move right in board buffers, or open the current list entry."
  (interactive)
  (if (my/note-agenda--board-view-p)
      (my/note-agenda--move-board-column 1)
    (my/note-agenda-open-at-point)))

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
  (let ((start (point)))
    (insert-text-button
     label
     'follow-link t
     'file (plist-get entry :file)
     'line (plist-get entry :line)
     'my/note-agenda-entry entry
     'face (or face 'link)
     'help-echo (format "%s  %s"
                        (my/note-agenda--location entry)
                        (plist-get entry :body))
     'action #'my/note-agenda--jump)
    (add-text-properties start (point)
                         `(my/note-agenda-entry ,entry))))

(defun my/note-agenda--insert-entry (entry)
  "Insert ENTRY into the current buffer as a clickable line."
  (let ((start (point)))
    (insert "  ")
    (my/note-agenda--insert-button (my/note-agenda--location entry) entry)
    (insert "  ")
    (insert (propertize (upcase (plist-get entry :state))
                        'face (my/note-agenda--state-face
                               (plist-get entry :state))))
    (my/note-agenda--insert-metadata entry)
    (insert "  "
            (propertize (my/note-agenda--entry-context entry) 'face 'bold)
            "  "
            (plist-get entry :body)
            "\n")
    (add-text-properties start (point)
                         `(my/note-agenda-entry ,entry))))

(defun my/note-agenda--dated-entries (entries)
  "Return dated ENTRIES sorted by their due or scheduled date."
  (sort (seq-filter
         (lambda (entry)
           (or (plist-get entry :due)
               (plist-get entry :scheduled)))
         (copy-sequence entries))
        (lambda (left right)
          (string< (my/note-task--date-sort-key left)
                   (my/note-task--date-sort-key right)))))

(defun my/note-agenda--insert-calendar-preview (entries &optional limit)
  "Insert a compact calendar preview for dated ENTRIES.
LIMIT caps the number of entries inserted; nil means show all."
  (let ((dated (my/note-agenda--dated-entries entries))
        (shown 0))
    (when dated
      (insert (propertize (format "CALENDAR (%d)" (length dated))
                          'face 'font-lock-keyword-face)
              "\n")
      (catch 'done
        (dolist (item dated)
          (when (and limit (>= shown limit))
            (throw 'done nil))
          (my/note-agenda--insert-entry item)
          (setq shown (1+ shown))))
      (when (and limit (> (length dated) shown))
        (insert (propertize
                 (format "  ... %d more dated tasks; use V/B for all closed states or v for full list\n"
                         (- (length dated) shown))
                 'face 'shadow)))
      (insert "\n"))))

(defun my/note-agenda--buffer ()
  "Return the agenda buffer."
  (get-buffer-create "*Note Agenda*"))

(defun my/note-task--agenda-entry-at-point ()
  "Return the note agenda entry at point, or nil."
  (or (get-text-property (point) 'my/note-agenda-entry)
      (and (button-at (point))
           (button-get (button-at (point)) 'my/note-agenda-entry))))

(defun my/note-task--edit-current-buffer (edit-fn)
  "Apply EDIT-FN to the Typst task chip at point."
  (unless (derived-mode-p 'typst-ts-mode 'typst-mode 'my/typst-mode)
    (user-error "Not in a Typst note buffer"))
  (let ((bounds (my/note-task--bounds-at-point)))
    (unless bounds
      (user-error "No note task chip at point"))
    (funcall edit-fn bounds)))

(defun my/note-task--edit-entry (entry edit-fn)
  "Apply EDIT-FN to agenda ENTRY's source task and refresh."
  (let ((file (plist-get entry :file)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (let ((bounds (my/note-task--entry-bounds entry)))
          (unless bounds
            (user-error "No note task chip found at %s"
                        (my/note-agenda--location entry)))
          (funcall edit-fn bounds)))
      (save-buffer)
      (my/note-db-sync-file file)))
  (when (derived-mode-p 'my/note-agenda-mode)
    (my/note-agenda-refresh)))

(defun my/note-task--edit-dwim (edit-fn)
  "Apply EDIT-FN either to an agenda entry or the current Typst task."
  (if (derived-mode-p 'my/note-agenda-mode)
      (if-let* ((entry (my/note-task--agenda-entry-at-point)))
          (my/note-task--edit-entry entry edit-fn)
        (user-error "No note task at point"))
    (my/note-task--edit-current-buffer edit-fn)))

;;;###autoload
(defun my/note-task-insert (state)
  "Insert a Typst task chip with STATE.
When the region is active, wrap it as the task body."
  (interactive
   (list (completing-read "Insert task state: " my/note-task-states nil t)))
  (when (derived-mode-p 'my/note-agenda-mode)
    (user-error "Insert tasks from a Typst note buffer"))
  (let ((body "")
        (start nil))
    (when (use-region-p)
      (setq body (buffer-substring-no-properties (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end)))
    (setq start (point))
    (insert (my/note-task--source state nil body))
    (when (string-empty-p body)
      (goto-char (- (point) 1)))
    (unless (string-empty-p body)
      (goto-char start))))

;;;###autoload
(defun my/note-task-insert-todo ()
  "Insert or wrap a note TODO task chip."
  (interactive)
  (my/note-task-insert "todo"))

;;;###autoload
(defun my/note-task-insert-doing ()
  "Insert or wrap a note DOING task chip."
  (interactive)
  (my/note-task-insert "doing"))

;;;###autoload
(defun my/note-task-insert-waiting ()
  "Insert or wrap a note WAITING task chip."
  (interactive)
  (my/note-task-insert "waiting"))

;;;###autoload
(defun my/note-task-set-state (state)
  "Set current note task chip to STATE."
  (interactive
   (list (completing-read "State: " my/note-task-states nil t)))
  (my/note-task--edit-dwim
   (lambda (bounds)
     (my/note-task--replace-bounds
      bounds
      state
      (plist-get bounds :args)))))

;;;###autoload
(defun my/note-task-set-todo ()
  "Set current note task chip to TODO."
  (interactive)
  (my/note-task-set-state "todo"))

;;;###autoload
(defun my/note-task-set-doing ()
  "Set current note task chip to DOING."
  (interactive)
  (my/note-task-set-state "doing"))

;;;###autoload
(defun my/note-task-set-waiting ()
  "Set current note task chip to WAITING."
  (interactive)
  (my/note-task-set-state "waiting"))

;;;###autoload
(defun my/note-task-set-done ()
  "Set current note task chip to DONE."
  (interactive)
  (my/note-task-set-state "done"))

;;;###autoload
(defun my/note-task-set-cancelled ()
  "Set current note task chip to CANCELLED."
  (interactive)
  (my/note-task-set-state "cancelled"))

;;;###autoload
(defun my/note-task-cycle-state ()
  "Cycle current note task chip through `my/note-task-states'."
  (interactive)
  (my/note-task--edit-dwim
   (lambda (bounds)
     (let* ((state (plist-get bounds :state))
            (index (or (cl-position state my/note-task-states :test #'string=)
                       -1))
            (next (nth (mod (1+ index) (length my/note-task-states))
                       my/note-task-states)))
       (my/note-task--replace-bounds
        bounds next (plist-get bounds :args))))))

(defun my/note-task--set-metadata (key value)
  "Set current note task metadata KEY to VALUE."
  (my/note-task--edit-dwim
   (lambda (bounds)
     (let ((args (my/note-task--alist-set
                  (plist-get bounds :args) key value)))
       (my/note-task--replace-bounds
        bounds (plist-get bounds :state) args)))))

;;;###autoload
(defun my/note-task-set-due (date)
  "Set current note task due/deadline DATE.
Use an empty value to clear it."
  (interactive (list (my/note-task--read-date "Due YYYY-MM-DD: ")))
  (my/note-task--set-metadata "due" date))

;;;###autoload
(defun my/note-task-set-scheduled (date)
  "Set current note task scheduled DATE.
Use an empty value to clear it."
  (interactive (list (my/note-task--read-date "Scheduled YYYY-MM-DD: ")))
  (my/note-task--set-metadata "scheduled" date))

;;;###autoload
(defun my/note-task-set-priority (priority)
  "Set current note task PRIORITY.
Use an empty value to clear it."
  (interactive (list (my/note-task--read-priority)))
  (my/note-task--set-metadata "priority" priority))

;;;###autoload
(defun my/note-task-set-repeat (repeat)
  "Set current note task repeat/recurrence string.
Use an empty value to clear it."
  (interactive (list (string-trim (read-string "Repeat: "))))
  (my/note-task--set-metadata "repeat" repeat))

;;;###autoload
(defun my/note-task-set-scheduled-today ()
  "Set current note task scheduled date to today."
  (interactive)
  (my/note-task--set-metadata "scheduled" (my/note-task--today)))

;;;###autoload
(defun my/note-task-set-due-today ()
  "Set current note task due date to today."
  (interactive)
  (my/note-task--set-metadata "due" (my/note-task--today)))

;;;###autoload
(defun my/note-task-shift-date (days)
  "Shift current task due and scheduled metadata by DAYS.
If the task has no date metadata, set scheduled to today plus DAYS."
  (interactive "nShift task date by days: ")
  (my/note-task--edit-dwim
   (lambda (bounds)
     (let* ((args (plist-get bounds :args))
            (due (my/note-task--arg args "due"))
            (scheduled (my/note-task--arg args "scheduled")))
       (cond
        ((or due scheduled)
         (when due
           (setq args (my/note-task--alist-set
                       args "due" (my/note-task--date-add-days due days))))
         (when scheduled
           (setq args (my/note-task--alist-set
                       args "scheduled"
                       (my/note-task--date-add-days scheduled days)))))
        (t
         (setq args (my/note-task--alist-set
                     args "scheduled"
                     (my/note-task--date-add-days (my/note-task--today)
                                                  days)))))
       (my/note-task--replace-bounds
        bounds (plist-get bounds :state) args)))))

;;;###autoload
(defun my/note-task-shift-date-forward ()
  "Shift current task date metadata one day forward."
  (interactive)
  (my/note-task-shift-date 1))

;;;###autoload
(defun my/note-task-shift-date-backward ()
  "Shift current task date metadata one day backward."
  (interactive)
  (my/note-task-shift-date -1))

;;;###autoload
(defun my/note-task-clear-date-metadata ()
  "Clear due, scheduled, and repeat metadata on the current note task."
  (interactive)
  (my/note-task--edit-dwim
   (lambda (bounds)
     (let* ((args (plist-get bounds :args))
            (args (my/note-task--alist-set args "due" ""))
            (args (my/note-task--alist-set args "scheduled" ""))
            (args (my/note-task--alist-set args "repeat" "")))
      (my/note-task--replace-bounds
       bounds (plist-get bounds :state) args)))))

;;;###autoload
(defun my/note-agenda-all ()
  "Show all note task chips, including closed tasks."
  (interactive)
  (my/note-agenda t))

;;;###autoload
(defun my/note-agenda-board-all ()
  "Show all note task chips as a board, including closed task columns."
  (interactive)
  (my/note-agenda-board t))

;;;###autoload
(defun my/note-agenda-board-closed ()
  "Show closed note task chips as a board.
This scans only `my/note-agenda-closed-states', instead of using the all-task
board."
  (interactive)
  (let* ((states my/note-agenda-closed-states)
         (entries (my/note-agenda--collect states)))
    (my/note-agenda--render-board
     entries states "Note Agenda Board (closed)" nil 'closed)))

;;;###autoload
(transient-define-prefix my/note-task-dispatch ()
  "Note task state, date, and view tools."
  [["State"
    ("t" "todo" my/note-task-set-todo)
    ("i" "doing" my/note-task-set-doing)
    ("w" "waiting" my/note-task-set-waiting)
    ("d" "done" my/note-task-set-done)
    ("c" "cancelled" my/note-task-set-cancelled)
    ("x" "cycle" my/note-task-cycle-state)]
   ["Insert / Wrap"
    ("1" "todo" my/note-task-insert-todo)
    ("2" "doing" my/note-task-insert-doing)
    ("3" "waiting" my/note-task-insert-waiting)
    ("I" "choose state" my/note-task-insert)]
   ["Plan"
    ("s" "schedule" my/note-task-set-scheduled)
    ("D" "due" my/note-task-set-due)
    ("." "schedule today" my/note-task-set-scheduled-today)
    ("!" "due today" my/note-task-set-due-today)
    (">" "+1 day" my/note-task-shift-date-forward)
    ("<" "-1 day" my/note-task-shift-date-backward)
    ("p" "priority" my/note-task-set-priority)
    ("r" "repeat" my/note-task-set-repeat)
    ("0" "clear dates" my/note-task-clear-date-metadata)
    ("K" "calendar" calendar)]
   ["Views / Maintenance"
    ("a" "agenda" my/note-agenda)
    ("A" "agenda all" my/note-agenda-all)
    ("b" "board" my/note-agenda-board)
    ("B" "board all" my/note-agenda-board-all)
    ("C" "closed board" my/note-agenda-board-closed)
    ("g" "rebuild index" my/note-db-sync)]])

(defun my/note-agenda--render-list (entries title all &optional states scope)
  "Render ENTRIES into a list agenda buffer with TITLE.
ALL records whether closed tasks are included.
STATES controls section order.  SCOPE is the refresh scope."
  (let ((buffer (my/note-agenda--buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize title 'face 'bold) "\n")
        (insert (make-string (length title) ?-) "\n\n")
        (insert (propertize
                 "Keys: RET/o/l open, j/k move, c cycle, t state, s schedule, D due, p priority, r repeat, . today, </> shift, b board, C closed board, ? menu, g refresh\n\n"
                 'face 'shadow))
        (my/note-agenda--insert-calendar-preview entries)
        (dolist (state (or states my/note-task-states))
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
      (setq-local my/note-agenda--last-states
                  (or states
                      (if all my/note-task-states
                        my/note-agenda-active-states)))
      (setq-local my/note-agenda--last-scope
                  (or scope (if all 'all 'active)))
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
  (let ((metadata (my/note-agenda--metadata-string entry)))
    (string-trim
     (format "%s%s%s"
             (if (string-empty-p metadata) "" (concat metadata " "))
             (plist-get entry :body)
             (let ((context (my/note-agenda--entry-context entry)))
               (if (string-empty-p context)
                   ""
                 (format " - %s" context)))))))

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

(defun my/note-agenda--render-board (entries states title all &optional scope)
  "Render ENTRIES as a state board for STATES with TITLE.
ALL records whether closed tasks are included.
SCOPE is the refresh scope."
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
        (insert (make-string (length title) ?-) "\n")
        (insert (propertize
                 "Keys: RET/o open, h/j/k/l move, c cycle, t state, s schedule, D due, p priority, r repeat, . today, </> shift, v list, C closed board, ? menu, g refresh\n\n"
                 'face 'shadow))
        (my/note-agenda--insert-calendar-preview
         entries my/note-agenda-calendar-preview-limit)
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
      (setq-local my/note-agenda--last-states states)
      (setq-local my/note-agenda--last-scope
                  (or scope (if all 'all 'active)))
      (setq-local my/note-agenda--board-rendered-width width)
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
    (my/note-agenda--render-list
     entries title all states (if all 'all 'active))))

;;;###autoload
(defun my/note-agenda-board (&optional all)
  "Show task chips as an Org-style agenda board.
With prefix ALL, include DONE/CANCELLED columns."
  (interactive "P")
  (let* ((states (if all my/note-task-states my/note-agenda-board-states))
         (entries (my/note-agenda--collect states))
         (title (if all "Note Agenda Board (all)" "Note Agenda Board")))
    (my/note-agenda--render-board
     entries states title all (if all 'all 'active))))

(defun my/note-agenda-refresh ()
  "Refresh the current note agenda buffer."
  (interactive)
  (pcase (cons my/note-agenda--last-view my/note-agenda--last-scope)
    (`(board . closed) (my/note-agenda-board-closed))
    (`(board . all) (my/note-agenda-board t))
    (`(board . ,_) (my/note-agenda-board))
    (`(,_ . all) (my/note-agenda t))
    (_ (my/note-agenda))))

(provide 'init-note-agenda)
;;; init-note-agenda.el ends here
