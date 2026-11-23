;;; lang/filetypes/csv-grid.el --- A grid view over a CSV buffer -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The CSV buffer is the table and stays the only copy of the data: it is
;; saved, undone and diffed as text.  A grid is a view of it, as a view is of a
;; table in a spreadsheet database: it sorts, filters, groups, hides columns
;; and totals without touching the file, and an edit made in the grid is a text
;; edit of the CSV buffer at that field (undo it there, or with `u' here).
;;
;; Keys follow the spreadsheet habit: arrows and TAB move by cell, RET edits
;; the cell, SPC opens the whole record, DEL clears the cell, `+' adds a row.
;; `?' is the menu.  The view redraws itself shortly after the CSV buffer
;; changes; it draws at most `+csv-grid-max-rows' rows.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'transient)

(defgroup +csv-grid nil
  "Grid views over CSV buffers."
  :group '+filetypes)

(defcustom +csv-grid-max-rows 2000
  "Most rows a grid draws; filter or group to see past them."
  :type 'integer :group '+csv-grid)

(defcustom +csv-grid-max-bytes (* 30 1024 1024)
  "Largest CSV buffer a grid reads."
  :type 'integer :group '+csv-grid)

(defcustom +csv-grid-max-column-width 40
  "Widest a column is drawn, in characters."
  :type 'integer :group '+csv-grid)

(defconst +csv-grid--gap 2 "Spaces between columns.")

(defconst +csv-grid--type-sample 500 "Values read to guess a column's type.")

(defconst +csv-grid--number-re
  "\\`[-+]?\\(?:[0-9]\\{1,3\\}\\(?:,[0-9]\\{3\\}\\)+\\|[0-9]*\\)\\(?:\\.[0-9]+\\)?\\(?:[eE][-+]?[0-9]+\\)?\\'"
  "A number, optionally with thousands separators.")

(defconst +csv-grid--date-re
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?:[T ][0-9:.]+Z?\\)?\\'"
  "An ISO date or date-time.")

(defconst +csv-grid--checkbox-pairs
  '(("true" . "false") ("yes" . "no") ("y" . "n") ("on" . "off")
    ("checked" . "unchecked") ("enabled" . "disabled") ("done" . "")
    ("x" . "") ("[x]" . "[ ]") ("✓" . "") ("✔" . "") ("☑" . "") ("✅" . ""))
  "Checked and unchecked spellings a checkbox column may use.
Taken from NocoDB's CSV import; 1/0 is left to number columns.")

(defconst +csv-grid--select-limit 64
  "Most distinct values a column may hold to be offered as a choice.")

(defconst +csv-grid--aggregates
  '((number . (count unique sum avg min max))
    (date . (count unique min max))
    (checkbox . (checked unchecked))
    (t . (count unique)))
  "Totals offered per column type, in the order `a' cycles them.")

(defface +csv-grid-header '((t :inherit (bold header-line)))
  "Column names.")

(defface +csv-grid-group '((t :inherit font-lock-keyword-face :weight bold))
  "Group headings.")

(defface +csv-grid-total '((t :inherit shadow :slant italic))
  "The totals row.")

(defface +csv-grid-cell '((t :inherit highlight))
  "The cell at point.")

;;;; Reading CSV

(defun +csv-grid--separator ()
  "Return the field separator of the current CSV buffer."
  (cond
   ((and (boundp 'csv-separator-chars) (local-variable-p 'csv-separator-chars)
         (consp csv-separator-chars) (null (cdr csv-separator-chars)))
    (car csv-separator-chars))
   ((and buffer-file-name (string-match-p "\\.\\(tsv\\|tab\\)\\'" buffer-file-name)) ?\t)
   ((and buffer-file-name (string-match-p "\\.psv\\'" buffer-file-name)) ?|)
   (t
    (let ((line (save-excursion
                  (goto-char (point-min))
                  (buffer-substring-no-properties (point) (line-end-position)))))
      (car (seq-reduce (lambda (best char)
                         (let ((count (cl-count char line)))
                           (if (> count (cdr best)) (cons char count) best)))
                       '(?\t ?\; ?|) (cons ?, (cl-count ?, line))))))))

(defun +csv-grid--read-record (separator &optional bounds)
  "Read the record at point and move past it.
Return its field strings, or with BOUNDS a list of (VALUE BEG END)
where BEG and END delimit the field's raw text, quotes included.
Quoted fields may hold separators, doubled quotes and newlines."
  (let ((stop (concat "^" (string separator) "\n"))
        fields done)
    (while (not done)
      (let ((beg (point)) value)
        (if (eq (char-after) ?\")
            (let ((parts nil) (closed nil))
              (forward-char 1)
              (while (not closed)
                (let ((start (point)))
                  (if (search-forward "\"" nil 'move)
                      (progn
                        (push (buffer-substring-no-properties start (1- (point))) parts)
                        (if (eq (char-after) ?\")
                            (progn (push "\"" parts) (forward-char 1))
                          (setq closed t)))
                    (push (buffer-substring-no-properties start (point)) parts)
                    (setq closed t))))
              (setq value (apply #'concat (nreverse parts)))
              ;; Text after a closing quote is kept, as most readers do.
              (let ((tail (point)))
                (skip-chars-forward stop)
                (setq value (concat value (buffer-substring-no-properties tail (point))))))
          (skip-chars-forward stop)
          (setq value (buffer-substring-no-properties beg (point))))
        (push (if bounds (list value beg (point)) value) fields)
        (cond ((eq (char-after) separator) (forward-char 1))
              (t (unless (eobp) (forward-char 1))
                 (setq done t)))))
    (nreverse fields)))

(defun +csv-grid--parse (separator)
  "Parse the current buffer with SEPARATOR.
Return a vector of (START . FIELDS) records, FIELDS a vector and START
the record's buffer position.  Blank lines are not records."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((records nil)
            (fast (string separator)))
        (while (not (eobp))
          (let ((start (point))
                (eol (line-end-position)))
            (cond
             ((= start eol) (forward-line 1))
             ;; Most lines hold no quote: split them without the character loop.
             ((not (search-forward "\"" eol t))
              (push (cons start (vconcat (split-string (buffer-substring-no-properties start eol) fast)))
                    records)
              (goto-char eol)
              (unless (eobp) (forward-char 1)))
             (t
              (goto-char start)
              (push (cons start (vconcat (+csv-grid--read-record separator))) records)))))
        (vconcat (nreverse records))))))

(defun +csv-grid--encode (value separator)
  "Return VALUE as CSV field text for SEPARATOR, quoted when needed."
  (if (or (string-match-p (concat "[\"\n" (regexp-quote (string separator)) "]") value)
          (string-match-p "\\`[ \t]\\|[ \t]\\'" value))
      (concat "\"" (string-replace "\"" "\"\"" value) "\"")
    value))

;;;; Grid state

(defvar-local +csv-grid--source nil "The CSV buffer this grid views.")
(defvar-local +csv-grid--separator nil "Field separator of the source.")
(defvar-local +csv-grid--tick nil "Source modification tick the records were read at.")
(defvar-local +csv-grid--header nil "Column names, a vector.")
(defvar-local +csv-grid--records nil "Data records, a vector of (START . FIELDS).")
(defvar-local +csv-grid--types nil
  "Column types: `number', `date', `checkbox', `select' or `text'.")
(defvar-local +csv-grid--checkboxes nil
  "Per column, the (CHECKED . UNCHECKED) pair of a checkbox column.")
(defvar-local +csv-grid--widths nil "Drawn column widths, a vector.")
(defvar-local +csv-grid--filters nil "List of (COLUMN OP VALUE), all must hold.")
(defvar-local +csv-grid--sort nil "(COLUMN . asc|desc) or nil.")
(defvar-local +csv-grid--group nil "Column the rows are grouped by, or nil.")
(defvar-local +csv-grid--hidden nil "Hidden column indices.")
(defvar-local +csv-grid--totals nil "Alist of COLUMN . AGGREGATE.")
(defvar-local +csv-grid--visible nil "Visible column indices, in order.")
(defvar-local +csv-grid--shown 0 "Rows drawn.")
(defvar-local +csv-grid--matched 0 "Rows passing the filters.")
(defvar-local +csv-grid--cell-overlay nil "Overlay on the cell at point.")
(defvar-local +csv-grid--order nil "Rows of the last draw, filtered and sorted.")

(defvar-local +csv-grid--views nil "In a CSV buffer: its live grid buffers.")
(defvar-local +csv-grid--timer nil "In a CSV buffer: pending grid redraw.")

(defun +csv-grid--field (record column)
  "Return COLUMN of RECORD, or the empty string."
  (let ((fields (cdr record)))
    (if (< column (length fields)) (aref fields column) "")))

(defun +csv-grid--number (value)
  "Return VALUE as a number, or nil."
  (and (not (string-empty-p value))
       (string-match-p +csv-grid--number-re value)
       (string-to-number (string-replace "," "" value))))

(defun +csv-grid--checkbox-pair (values)
  "Return the checkbox pair all non-empty VALUES spell, or nil."
  (let ((seen (delete-dups (mapcar #'downcase values))))
    (and seen
         (seq-find (lambda (pair)
                     (seq-every-p (lambda (value) (member value (list (car pair) (cdr pair)))) seen))
                   +csv-grid--checkbox-pairs))))

(defun +csv-grid--guess-types (records width)
  "Guess the type of each of WIDTH columns from RECORDS.
Return (TYPES . CHECKBOXES).  A text column with few distinct values
that repeat is a `select', as NocoDB's import decides: at most
`+csv-grid--select-limit' values and at most half as many as filled cells."
  (let ((types (make-vector width nil))
        (checkboxes (make-vector width nil))
        (limit (min (length records) +csv-grid--type-sample)))
    (dotimes (column width)
      (let ((number t) (date t) (values nil))
        (dotimes (index limit)
          (let ((value (+csv-grid--field (aref records index) column)))
            (unless (string-empty-p value)
              (push value values)
              (unless (string-match-p +csv-grid--number-re value) (setq number nil))
              (unless (string-match-p +csv-grid--date-re value) (setq date nil)))))
        (let ((pair (and values (not number) (+csv-grid--checkbox-pair values)))
              (distinct (length (delete-dups (copy-sequence values)))))
          (aset checkboxes column pair)
          (aset types column
                (cond ((null values) 'text)
                      (number 'number)
                      (date 'date)
                      (pair 'checkbox)
                      ((and (>= (length values) 4)
                            (<= distinct +csv-grid--select-limit)
                            (<= distinct (ceiling (length values) 2)))
                       'select)
                      (t 'text))))))
    (cons types checkboxes)))

(defun +csv-grid--checked-p (column value)
  "Non-nil when VALUE is the checked spelling of checkbox COLUMN."
  (let ((pair (aref +csv-grid--checkboxes column)))
    (and pair (not (string-empty-p value)) (equal (downcase value) (car pair)))))

(defun +csv-grid--toggled (column value)
  "Return VALUE of checkbox COLUMN flipped, keeping its letter case."
  (pcase-let* ((`(,on . ,off) (aref +csv-grid--checkboxes column))
               (next (if (+csv-grid--checked-p column value) off on))
               (style (if (string-empty-p value)
                          (seq-find (lambda (v) (not (string-empty-p v)))
                                    (mapcar (lambda (r) (+csv-grid--field r column)) +csv-grid--records)
                                    "")
                        value)))
    (cond ((and (not (string-empty-p style)) (string= style (upcase style)) (not (string= style (downcase style))))
           (upcase next))
          ((and (> (length style) 1) (string= style (capitalize style)) (not (string= style (downcase style))))
           (capitalize next))
          (t next))))

(defun +csv-grid--display (column value)
  "Return how VALUE of COLUMN is drawn."
  (if (eq (aref +csv-grid--types column) 'checkbox)
      (if (+csv-grid--checked-p column value) "☑" "☐")
    value))

(defun +csv-grid--choices (column)
  "Return the distinct non-empty values of COLUMN, sorted."
  (let ((seen (make-hash-table :test #'equal)) values)
    (seq-doseq (record +csv-grid--records)
      (let ((value (+csv-grid--field record column)))
        (unless (or (string-empty-p value) (gethash value seen))
          (puthash value t seen)
          (push value values))))
    (sort values #'string<)))

(defun +csv-grid--read-source ()
  "Read the source buffer into this grid's state."
  (let ((source +csv-grid--source) separator records tick)
    (with-current-buffer source
      (when (> (buffer-size) +csv-grid-max-bytes)
        (user-error "%s is larger than `+csv-grid-max-bytes'" (buffer-name)))
      (setq separator (+csv-grid--separator)
            records (+csv-grid--parse separator)
            tick (buffer-chars-modified-tick)))
    (let* ((width (seq-reduce (lambda (most record) (max most (length (cdr record)))) records 0))
           (header (if (> (length records) 0) (cdr (aref records 0)) [])))
      (setq +csv-grid--separator separator
            +csv-grid--tick tick
            +csv-grid--header (vconcat (cl-loop for column below width
                                                collect (let ((name (if (< column (length header)) (aref header column) "")))
                                                          (if (string-empty-p name) (format "Column %d" (1+ column)) name))))
            +csv-grid--records (if (> (length records) 0) (seq-subseq records 1) [])
            +csv-grid--types nil)
      (let ((guess (+csv-grid--guess-types +csv-grid--records width)))
        (setq +csv-grid--types (car guess)
              +csv-grid--checkboxes (cdr guess))))))

;;;; Filtering, sorting, totals

(defun +csv-grid--compare (a b type)
  "Compare values A and B of TYPE: negative, zero or positive."
  (let ((x (and (eq type 'number) (+csv-grid--number a)))
        (y (and (eq type 'number) (+csv-grid--number b))))
    (cond
     ((and x y) (cond ((< x y) -1) ((> x y) 1) (t 0)))
     ;; Empty values sort last whatever the direction asked.
     ((string-empty-p a) (if (string-empty-p b) 0 1))
     ((string-empty-p b) -1)
     (t (let ((r (compare-strings a nil nil b nil nil t)))
          (if (eq r t) 0 r))))))

(defun +csv-grid--matches-p (record filter)
  "Non-nil when RECORD satisfies FILTER (COLUMN OP VALUE)."
  (pcase-let* ((`(,column ,op ,expected) filter)
               (value (+csv-grid--field record column))
               (type (aref +csv-grid--types column)))
    (pcase op
      ("empty" (string-empty-p value))
      ("filled" (not (string-empty-p value)))
      ("~" (string-search (downcase expected) (downcase value)))
      ("!~" (not (string-search (downcase expected) (downcase value))))
      ("=" (= 0 (+csv-grid--compare value expected type)))
      ("!=" (/= 0 (+csv-grid--compare value expected type)))
      (_ (and (not (string-empty-p value))
              (let ((c (+csv-grid--compare value expected type)))
                (pcase op (">" (> c 0)) ("<" (< c 0)) (">=" (>= c 0)) ("<=" (<= c 0)))))))))

(defun +csv-grid--rows ()
  "Return the indices of records passing the filters, sorted."
  (let ((rows nil) (records +csv-grid--records))
    (dotimes (index (length records))
      (let ((record (aref records index)))
        (when (seq-every-p (lambda (filter) (+csv-grid--matches-p record filter)) +csv-grid--filters)
          (push index rows))))
    (setq rows (nreverse rows))
    (when +csv-grid--sort
      (let* ((column (car +csv-grid--sort))
             (number (eq (aref +csv-grid--types column) 'number))
             (checkbox (eq (aref +csv-grid--types column) 'checkbox))
             (desc (eq (cdr +csv-grid--sort) 'desc))
             ;; Each key is computed once; the comparison only compares keys.
             (keyed (mapcar (lambda (row)
                              (let ((value (+csv-grid--field (aref records row) column)))
                                (cons (cond (checkbox (if (+csv-grid--checked-p column value) 0 1))
                                            ((string-empty-p value) nil)
                                            (number (or (+csv-grid--number value) (downcase value)))
                                            (t (downcase value)))
                                      row)))
                            rows))
             (less (lambda (a b)
                     (if (and (numberp a) (numberp b)) (< a b)
                       (if (numberp a) t
                         (and (not (numberp b)) (string< a b)))))))
        (setq rows (mapcar #'cdr
                           (sort keyed
                                 (lambda (x y)
                                   (let ((a (car x)) (b (car y)))
                                     ;; Empty stays last in both directions.
                                     (cond ((null a) nil)
                                           ((null b) t)
                                           (desc (funcall less b a))
                                           (t (funcall less a b))))))))))
    rows))

(defun +csv-grid--aggregate (rows column op)
  "Return the OP total of COLUMN over ROWS as a string."
  (let* ((values (delq nil (mapcar (lambda (row)
                                     (let ((v (+csv-grid--field (aref +csv-grid--records row) column)))
                                       (unless (string-empty-p v) v)))
                                   rows)))
         (numbers (and (memq op '(sum avg min max))
                       (eq (aref +csv-grid--types column) 'number)
                       (delq nil (mapcar #'+csv-grid--number values)))))
    (pcase op
      ('checked (format "%d" (seq-count (lambda (row)
                                          (+csv-grid--checked-p column (+csv-grid--field (aref +csv-grid--records row) column)))
                                        rows)))
      ('unchecked (format "%d" (seq-count (lambda (row)
                                            (not (+csv-grid--checked-p column (+csv-grid--field (aref +csv-grid--records row) column))))
                                          rows)))
      ('count (format "%d" (length values)))
      ('unique (format "%d" (length (delete-dups (copy-sequence values)))))
      ('sum (+csv-grid--format-number (apply #'+ numbers)))
      ('avg (if numbers (+csv-grid--format-number (/ (apply #'+ numbers) (float (length numbers)))) ""))
      ('min (if numbers (+csv-grid--format-number (apply #'min numbers))
              (or (car (sort values #'string<)) "")))
      ('max (if numbers (+csv-grid--format-number (apply #'max numbers))
              (or (car (last (sort values #'string<))) "")))
      (_ ""))))

(defun +csv-grid--format-number (number)
  "Format NUMBER compactly."
  (if (and (floatp number) (/= number (ffloor number)))
      (format "%.4g" number)
    (format "%d" (truncate number))))

;;;; Drawing

(defun +csv-grid--totals-text (rows)
  "Return an alist of COLUMN . total text over ROWS for chosen totals."
  (mapcar (pcase-lambda (`(,column . ,op))
            (cons column (format "%s %s" op (+csv-grid--aggregate rows column op))))
          +csv-grid--totals))

(defun +csv-grid--compute-widths (rows totals)
  "Set column widths from the header, drawn ROWS and TOTALS text."
  (let ((widths (vconcat (mapcar #'string-width +csv-grid--header))))
    (pcase-dolist (`(,column . ,text) totals)
      (aset widths column (max (aref widths column) (string-width text))))
    (dolist (row rows)
      (let ((fields (cdr (aref +csv-grid--records row))))
        (dotimes (column (min (length fields) (length widths)))
          (let ((width (string-width (aref fields column))))
            (when (> width (aref widths column)) (aset widths column width))))))
    (dotimes (column (length widths))
      (aset widths column (min +csv-grid-max-column-width (max 3 (aref widths column)))))
    (setq +csv-grid--widths widths)))

(defun +csv-grid--pad (text width align)
  "Fit TEXT to WIDTH, padded on the left when ALIGN is `right'."
  (let* ((one-line (string-replace "\n" "⏎" text))
         (fitted (truncate-string-to-width one-line width nil nil "…"))
         (pad (make-string (max 0 (- width (string-width fitted))) ?\s)))
    (if (eq align 'right) (concat pad fitted) (concat fitted pad))))

(defun +csv-grid--line (cells &optional face)
  "Return a drawn line of CELLS, a list of (COLUMN TEXT), in FACE."
  (let ((gap (make-string +csv-grid--gap ?\s)))
    (mapconcat
     (lambda (cell)
       (let* ((column (car cell))
              (align (if (eq (aref +csv-grid--types column) 'number) 'right 'left))
              (text (+csv-grid--pad (cadr cell) (aref +csv-grid--widths column) align)))
         (propertize text 'csv-grid-column column 'face face)))
     cells gap)))

(defun +csv-grid--insert-row (row)
  "Insert the line of record index ROW."
  (let ((record (aref +csv-grid--records row))
        (start (point)))
    (insert (+csv-grid--line (mapcar (lambda (column)
                                       (list column (+csv-grid--display column (+csv-grid--field record column))))
                                     +csv-grid--visible))
            "\n")
    (put-text-property start (point) 'csv-grid-row row)))

(defun +csv-grid--insert-totals (totals)
  "Insert the totals row of TOTALS, an alist of COLUMN . text."
  (when totals
    (insert (+csv-grid--line
             (mapcar (lambda (column) (list column (or (alist-get column totals) "")))
                     +csv-grid--visible)
             '+csv-grid-total)
            "\n")))

(defun +csv-grid--group-rows (rows)
  "Split ROWS by the group column, keeping first-seen order."
  (let ((groups nil))
    (dolist (row rows)
      (let* ((key (+csv-grid--field (aref +csv-grid--records row) +csv-grid--group))
             (entry (assoc key groups)))
        (if entry (push row (cdr entry)) (push (list key row) groups))))
    (mapcar (lambda (group) (cons (car group) (nreverse (cdr group)))) (nreverse groups))))

(defun +csv-grid--header-line ()
  "Return the header line, scrolled with the window."
  (let ((line (+csv-grid--line (mapcar (lambda (column) (list column (aref +csv-grid--header column)))
                                       +csv-grid--visible)
                               '+csv-grid-header))
        (scroll (window-hscroll)))
    (concat (propertize " " 'display '(space :align-to 0))
            (if (> scroll 0) (substring line (min scroll (length line))) line))))

(defun +csv-grid--state-line ()
  "Return the one-line summary of this view."
  (let ((name (lambda (column) (aref +csv-grid--header column))))
    (string-join
     (delq nil
           (list (format "%s of %d rows" (if (< +csv-grid--shown +csv-grid--matched)
                                             (format "%d/%d" +csv-grid--shown +csv-grid--matched)
                                           +csv-grid--matched)
                         (length +csv-grid--records))
                 (when +csv-grid--filters
                   (concat "filter "
                           (mapconcat (pcase-lambda (`(,c ,op ,v)) (string-trim (format "%s %s %s" (funcall name c) op v)))
                                      +csv-grid--filters " & ")))
                 (when +csv-grid--sort
                   (format "sort %s %s" (funcall name (car +csv-grid--sort))
                           (if (eq (cdr +csv-grid--sort) 'desc) "↓" "↑")))
                 (when +csv-grid--group (format "group %s" (funcall name +csv-grid--group)))
                 (when +csv-grid--hidden (format "%d hidden" (length +csv-grid--hidden)))))
     " · ")))

(defun +csv-grid--render (&optional reuse-order)
  "Redraw the grid from the parsed records, keeping point's cell.
With REUSE-ORDER the rows keep the last draw's filtering and order,
which is right when only a column outside the filters and sort changed."
  (let* ((inhibit-read-only t)
         (row (get-text-property (point) 'csv-grid-row))
         (column (get-text-property (point) 'csv-grid-column))
         (line (line-number-at-pos))
         (rows (setq +csv-grid--order
                     (if (and reuse-order +csv-grid--order) +csv-grid--order (+csv-grid--rows))))
         (shown (seq-take rows +csv-grid-max-rows))
         (totals (+csv-grid--totals-text rows)))
    (setq +csv-grid--visible (cl-loop for c below (length +csv-grid--header)
                                      unless (memq c +csv-grid--hidden) collect c)
          +csv-grid--matched (length rows)
          +csv-grid--shown (length shown))
    (+csv-grid--compute-widths shown totals)
    (erase-buffer)
    (insert (propertize (+csv-grid--state-line) 'face 'shadow) "\n")
    (if (null +csv-grid--group)
        (mapc #'+csv-grid--insert-row shown)
      (dolist (group (+csv-grid--group-rows shown))
        (insert (propertize (format "%s: %s" (aref +csv-grid--header +csv-grid--group)
                                    (if (string-empty-p (car group)) "(empty)" (car group)))
                            'face '+csv-grid-group)
                (propertize (format "  %d" (length (cdr group))) 'face 'shadow)
                "\n")
        (mapc #'+csv-grid--insert-row (cdr group))
        (+csv-grid--insert-totals (+csv-grid--totals-text (cdr group)))))
    (unless +csv-grid--group (+csv-grid--insert-totals totals))
    (when (< +csv-grid--shown +csv-grid--matched)
      (insert (propertize (format "… %d more rows: filter or group to see them\n"
                                  (- +csv-grid--matched +csv-grid--shown))
                          'face 'shadow)))
    (unless (and row (+csv-grid--goto-row row column))
      (goto-char (point-min))
      (forward-line (1- line)))
    (+csv-grid--show-cell)))

;;;; Moving by cell

(defun +csv-grid--goto-row (row &optional column)
  "Move to the line of record ROW at COLUMN; nil when ROW is not drawn."
  (let ((found (text-property-any (point-min) (point-max) 'csv-grid-row row)))
    (when found
      (goto-char found)
      (+csv-grid--goto-column (or column (car +csv-grid--visible)))
      t)))

(defun +csv-grid--goto-column (column)
  "Move to the start of COLUMN on this line, or to the line start."
  (let ((end (line-end-position)) (pos (line-beginning-position)))
    (while (and (< pos end) (not (eq (get-text-property pos 'csv-grid-column) column)))
      (setq pos (or (next-single-property-change pos 'csv-grid-column nil end) end)))
    (goto-char (if (< pos end) pos (line-beginning-position)))))

(defun +csv-grid--show-cell ()
  "Highlight the cell at point."
  (let ((column (get-text-property (point) 'csv-grid-column)))
    (if (not (and column (get-text-property (point) 'csv-grid-row)))
        (when +csv-grid--cell-overlay (delete-overlay +csv-grid--cell-overlay))
      (let* ((beg (if (and (> (point) (line-beginning-position))
                           (eq (get-text-property (1- (point)) 'csv-grid-column) column))
                      (previous-single-property-change (point) 'csv-grid-column nil (line-beginning-position))
                    (point)))
             (end (next-single-property-change (point) 'csv-grid-column nil (line-end-position))))
        (unless +csv-grid--cell-overlay
          (setq +csv-grid--cell-overlay (make-overlay beg end))
          (overlay-put +csv-grid--cell-overlay 'face '+csv-grid-cell))
        (move-overlay +csv-grid--cell-overlay beg end)))))

(defun +csv-grid--column-at-point ()
  "Return the column at point, or the first visible one."
  (or (get-text-property (point) 'csv-grid-column) (car +csv-grid--visible)))

(defun +csv-grid--row-at-point ()
  "Return the record index at point or signal."
  (or (get-text-property (point) 'csv-grid-row) (user-error "No row here")))

(defun +csv-grid-next-cell (&optional backward)
  "Move to the next cell, or the previous one with BACKWARD."
  (interactive)
  (let* ((column (+csv-grid--column-at-point))
         (tail (memq column (if backward (reverse +csv-grid--visible) +csv-grid--visible))))
    (if (cadr tail)
        (+csv-grid--goto-column (cadr tail))
      (forward-line (if backward -1 1))
      (+csv-grid--goto-column (if backward (car (last +csv-grid--visible)) (car +csv-grid--visible))))))

(defun +csv-grid-previous-cell ()
  "Move to the previous cell."
  (interactive)
  (+csv-grid-next-cell t))

(defun +csv-grid-next-row (&optional n)
  "Move N rows down, staying in the same column."
  (interactive "p")
  (let ((column (+csv-grid--column-at-point)))
    (forward-line (or n 1))
    (+csv-grid--goto-column column)))

(defun +csv-grid-previous-row (&optional n)
  "Move N rows up, staying in the same column."
  (interactive "p")
  (+csv-grid-next-row (- (or n 1))))

;;;; Writing the source

(defun +csv-grid--ensure-current ()
  "Signal unless the records still match the source text."
  (unless (and (buffer-live-p +csv-grid--source)
               (eql +csv-grid--tick (buffer-chars-modified-tick +csv-grid--source)))
    (+csv-grid-refresh)
    (user-error "The CSV changed; the grid is redrawn, try again")))

(defun +csv-grid--write (row column value)
  "Set COLUMN of record index ROW to VALUE in the source buffer.
Only that record is read again: the records after it move by the
change in length, so an edit costs no reparse of the file."
  (+csv-grid--ensure-current)
  (let* ((start (car (aref +csv-grid--records row)))
         (separator +csv-grid--separator)
         (source +csv-grid--source)
         delta fields tick)
    (with-current-buffer source
      (save-excursion
        (let ((size (buffer-size)))
          (goto-char start)
          (let ((old (+csv-grid--read-record separator t))
                (text (+csv-grid--encode value separator)))
            (if (< column (length old))
                (pcase-let ((`(,_ ,beg ,end) (nth column old)))
                  (goto-char beg)
                  (delete-region beg end)
                  (insert text))
              ;; A short record grows to reach the column.
              (pcase-let ((`(,_ ,_ ,end) (car (last old))))
                (goto-char end)
                (insert (make-string (- column (length old) -1) separator) text))))
          (setq delta (- (buffer-size) size))
          (goto-char start)
          (setq fields (vconcat (+csv-grid--read-record separator))
                tick (buffer-chars-modified-tick)))))
    (setcdr (aref +csv-grid--records row) fields)
    (unless (zerop delta)
      (cl-loop for index from (1+ row) below (length +csv-grid--records)
               do (cl-incf (car (aref +csv-grid--records index)) delta)))
    (setq +csv-grid--tick tick)
    (+csv-grid--render (not (or (eql column (car +csv-grid--sort))
                                (assq column +csv-grid--filters))))))

(defun +csv-grid-edit ()
  "Edit the cell at point.
A checkbox flips without asking; a column of few repeated values offers
them for completion, though any text is accepted."
  (interactive)
  (let* ((row (+csv-grid--row-at-point))
         (column (+csv-grid--column-at-point))
         (old (+csv-grid--field (aref +csv-grid--records row) column))
         (name (aref +csv-grid--header column))
         (value (pcase (aref +csv-grid--types column)
                  ('checkbox (+csv-grid--toggled column old))
                  ('select (completing-read (format-prompt name old) (+csv-grid--choices column)
                                            nil nil nil nil old))
                  (_ (read-string (format "%s: " name) old)))))
    (unless (equal value old)
      (+csv-grid--write row column value))))

(defun +csv-grid-edit-text ()
  "Edit the cell at point as plain text, whatever its column type."
  (interactive)
  (let* ((row (+csv-grid--row-at-point))
         (column (+csv-grid--column-at-point))
         (old (+csv-grid--field (aref +csv-grid--records row) column))
         (value (read-string (format "%s: " (aref +csv-grid--header column)) old)))
    (unless (equal value old)
      (+csv-grid--write row column value))))

(defun +csv-grid-clear ()
  "Empty the cell at point (undo with `u')."
  (interactive)
  (+csv-grid--write (+csv-grid--row-at-point) (+csv-grid--column-at-point) ""))

(defun +csv-grid-copy ()
  "Copy the value of the cell at point."
  (interactive)
  (let ((value (+csv-grid--field (aref +csv-grid--records (+csv-grid--row-at-point))
                                 (+csv-grid--column-at-point))))
    (kill-new value)
    (message "Copied: %s" (truncate-string-to-width value 60 nil nil "…"))))

(defun +csv-grid-add-row ()
  "Append an empty record to the CSV and edit its first visible cell."
  (interactive)
  (+csv-grid--ensure-current)
  (let ((separator +csv-grid--separator)
        (width (length +csv-grid--header)))
    (with-current-buffer +csv-grid--source
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        ;; A blank line is not a record, so the new one holds its separators.
        (insert (make-string (max 1 (1- width)) separator) "\n"))))
  (+csv-grid-refresh)
  (let ((row (1- (length +csv-grid--records))))
    (if (+csv-grid--goto-row row (car +csv-grid--visible))
        (+csv-grid-edit)
      (message "Row added; it is outside the current filter"))))

(defun +csv-grid-delete-row ()
  "Delete the record at point from the CSV."
  (interactive)
  (let* ((row (+csv-grid--row-at-point))
         (fields (cdr (aref +csv-grid--records row))))
    (when (y-or-n-p (format "Delete row %s? " (truncate-string-to-width
                                               (string-join (append fields nil) ", ") 50 nil nil "…")))
      (+csv-grid--ensure-current)
      (let ((start (car (aref +csv-grid--records row)))
            (separator +csv-grid--separator))
        (with-current-buffer +csv-grid--source
          (save-excursion
            (goto-char start)
            (+csv-grid--read-record separator)
            (delete-region start (point)))))
      (+csv-grid-refresh))))

(defun +csv-grid-undo ()
  "Undo the last change in the CSV buffer."
  (interactive)
  (with-current-buffer +csv-grid--source
    (undo))
  (+csv-grid-refresh))

(defun +csv-grid-visit ()
  "Show the record at point in the CSV buffer."
  (interactive)
  (let* ((row (+csv-grid--row-at-point))
         (start (car (aref +csv-grid--records row))))
    (pop-to-buffer +csv-grid--source)
    (goto-char start)))

;;;; View settings

(defun +csv-grid--read-column (prompt)
  "Read a column name with PROMPT, defaulting to the one at point."
  (let* ((names (mapcar (lambda (c) (cons (aref +csv-grid--header c) c))
                        (number-sequence 0 (1- (length +csv-grid--header)))))
         (default (aref +csv-grid--header (+csv-grid--column-at-point))))
    (cdr (assoc (completing-read (format-prompt prompt default) names nil t nil nil default) names))))

(defun +csv-grid-filter (column op value)
  "Keep rows whose COLUMN satisfies OP VALUE; filters add up."
  (interactive
   (let* ((column (+csv-grid--read-column "Filter column"))
          (op (completing-read "Operator: " '("~" "!~" "=" "!=" ">" "<" ">=" "<=" "empty" "filled") nil t nil nil "~"))
          (value (if (member op '("empty" "filled")) ""
                   (read-string (format "%s %s " (aref +csv-grid--header column) op)
                                (+csv-grid--field (aref +csv-grid--records
                                                        (or (get-text-property (point) 'csv-grid-row) 0))
                                                  column)))))
     (list column op value)))
  (setq +csv-grid--filters (append +csv-grid--filters (list (list column op value))))
  (+csv-grid--render))

(defun +csv-grid-clear-filters ()
  "Remove every filter."
  (interactive)
  (setq +csv-grid--filters nil)
  (+csv-grid--render))

(defun +csv-grid-sort ()
  "Sort by the column at point: ascending, descending, then unsorted."
  (interactive)
  (let ((column (+csv-grid--column-at-point)))
    (setq +csv-grid--sort
          (cond ((not (eql (car +csv-grid--sort) column)) (cons column 'asc))
                ((eq (cdr +csv-grid--sort) 'asc) (cons column 'desc))
                (t nil)))
    (+csv-grid--render)))

(defun +csv-grid-group ()
  "Group rows by the column at point, or stop grouping by it."
  (interactive)
  (let ((column (+csv-grid--column-at-point)))
    (setq +csv-grid--group (unless (eql +csv-grid--group column) column))
    (+csv-grid--render)))

(defun +csv-grid-hide-column ()
  "Hide the column at point."
  (interactive)
  (when (<= (length +csv-grid--visible) 1) (user-error "The last column stays"))
  (let ((column (+csv-grid--column-at-point)))
    (push column +csv-grid--hidden)
    (+csv-grid--render)
    (message "Hid %s; H shows all columns" (aref +csv-grid--header column))))

(defun +csv-grid-show-columns ()
  "Show every hidden column."
  (interactive)
  (setq +csv-grid--hidden nil)
  (+csv-grid--render))

(defun +csv-grid-cycle-total ()
  "Cycle the total shown under the column at point."
  (interactive)
  (let* ((column (+csv-grid--column-at-point))
         (choices (alist-get (aref +csv-grid--types column) +csv-grid--aggregates
                             (alist-get t +csv-grid--aggregates)))
         (current (alist-get column +csv-grid--totals))
         (next (if current (cadr (memq current choices)) (car choices))))
    (setq +csv-grid--totals (if next
                                (cons (cons column next) (assq-delete-all column +csv-grid--totals))
                              (assq-delete-all column +csv-grid--totals)))
    (+csv-grid--render)))

(defun +csv-grid-refresh ()
  "Read the CSV buffer again and redraw."
  (interactive)
  (unless (buffer-live-p +csv-grid--source)
    (user-error "The CSV buffer is gone"))
  (+csv-grid--read-source)
  (setq +csv-grid--hidden (seq-filter (lambda (c) (< c (length +csv-grid--header))) +csv-grid--hidden))
  (+csv-grid--render))

;;;; The record form

(defvar-local +csv-grid-record--grid nil "Grid this record form belongs to.")
(defvar-local +csv-grid-record--row nil "Record index this form shows.")

(defun +csv-grid-expand ()
  "Show every field of the record at point, one per line."
  (interactive)
  (let ((grid (current-buffer))
        (row (+csv-grid--row-at-point)))
    (with-current-buffer (get-buffer-create (format "*CSV record: %s*" (buffer-name +csv-grid--source)))
      (+csv-grid-record-mode)
      (setq +csv-grid-record--grid grid +csv-grid-record--row row)
      (+csv-grid-record--render)
      (pop-to-buffer (current-buffer)))))

(defun +csv-grid-record--render ()
  "Draw this record form from its grid."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (row +csv-grid-record--row))
    (erase-buffer)
    (pcase-let ((`(,header ,record ,types)
                 (with-current-buffer +csv-grid-record--grid
                   (list +csv-grid--header (aref +csv-grid--records row) +csv-grid--types))))
      (let ((label (apply #'max (mapcar #'string-width header))))
        (insert (propertize (format "Row %d\n\n" (1+ row)) 'face 'bold))
        (dotimes (column (length header))
          (let ((start (point)))
            (insert (propertize (string-pad (aref header column) label) 'face 'bold)
                    "  "
                    (propertize (symbol-name (aref types column)) 'face 'shadow)
                    "\n  " (string-replace "\n" "\n  " (+csv-grid--field record column)) "\n\n")
            (put-text-property start (point) 'csv-grid-column column)))))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun +csv-grid-record-edit ()
  "Edit the field at point through the grid."
  (interactive)
  (let ((column (or (get-text-property (point) 'csv-grid-column) (user-error "No field here")))
        (row +csv-grid-record--row)
        (grid +csv-grid-record--grid))
    (unless (buffer-live-p grid) (user-error "The grid is gone"))
    (let* ((old (with-current-buffer grid (+csv-grid--field (aref +csv-grid--records row) column)))
           (name (with-current-buffer grid (aref +csv-grid--header column)))
           (value (read-string (format "%s: " name) old)))
      (unless (equal value old)
        (with-current-buffer grid (+csv-grid--write row column value))
        (+csv-grid-record--render)))))

(defun +csv-grid-record-step (n)
  "Show the record N rows away in the grid's order."
  (let ((grid +csv-grid-record--grid) next)
    (with-current-buffer grid
      (when (+csv-grid--goto-row +csv-grid-record--row)
        (forward-line n)
        (setq next (get-text-property (point) 'csv-grid-row))))
    (unless next (user-error "No more rows"))
    (setq +csv-grid-record--row next)
    (+csv-grid-record--render)))

(defun +csv-grid-record-next ()
  "Show the next record of the grid."
  (interactive)
  (+csv-grid-record-step 1))

(defun +csv-grid-record-previous ()
  "Show the previous record of the grid."
  (interactive)
  (+csv-grid-record-step -1))

(defvar +csv-grid-record-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'+csv-grid-record-edit)
    (define-key map (kbd "e") #'+csv-grid-record-edit)
    (define-key map (kbd "n") #'+csv-grid-record-next)
    (define-key map (kbd "p") #'+csv-grid-record-previous)
    map)
  "Keys of a CSV record form.")

(define-derived-mode +csv-grid-record-mode special-mode "CSV-Record"
  "One CSV record, a field per line.  RET edits a field; n and p step rows.")

;;;; Following the source

(defun +csv-grid--source-changed-h (&rest _)
  "Redraw this CSV buffer's grids shortly after it changes."
  (when (timerp +csv-grid--timer) (cancel-timer +csv-grid--timer))
  (setq +csv-grid--timer
        (run-with-idle-timer 0.3 nil #'+csv-grid--redraw-views (current-buffer))))

(defun +csv-grid--redraw-views (source)
  "Redraw the visible grids of SOURCE that are out of date."
  (when (buffer-live-p source)
    (with-current-buffer source
      (setq +csv-grid--timer nil)
      (setq +csv-grid--views (seq-filter #'buffer-live-p +csv-grid--views))
      (dolist (view +csv-grid--views)
        (with-current-buffer view
          (when (and (get-buffer-window view t)
                     (not (eql +csv-grid--tick (buffer-chars-modified-tick source))))
            (+csv-grid-refresh)))))))

(defun +csv-grid--source-killed-h ()
  "Stop following a CSV buffer that is killed."
  (when (timerp +csv-grid--timer) (cancel-timer +csv-grid--timer))
  (setq +csv-grid--timer nil))

(defun +csv-grid--view-killed-h ()
  "Stop the source following this grid when it is the last one."
  (let ((view (current-buffer)) (source +csv-grid--source))
    (when (buffer-live-p source)
      (with-current-buffer source
        (setq +csv-grid--views (delq view (seq-filter #'buffer-live-p +csv-grid--views)))
        (unless +csv-grid--views
          (remove-hook 'after-change-functions #'+csv-grid--source-changed-h t)
          (remove-hook 'kill-buffer-hook #'+csv-grid--source-killed-h t)
          (+csv-grid--source-killed-h))))))

;;;; Mode and entry

(defvar +csv-grid-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'+csv-grid-edit)
    (define-key map (kbd "e") #'+csv-grid-edit-text)
    (define-key map (kbd "SPC") #'+csv-grid-expand)
    (define-key map (kbd "DEL") #'+csv-grid-clear)
    (define-key map (kbd "<deletechar>") #'+csv-grid-clear)
    (define-key map (kbd "w") #'+csv-grid-copy)
    (define-key map (kbd "M-w") #'+csv-grid-copy)
    (define-key map (kbd "+") #'+csv-grid-add-row)
    (define-key map (kbd "D") #'+csv-grid-delete-row)
    (define-key map (kbd "u") #'+csv-grid-undo)
    (define-key map (kbd "o") #'+csv-grid-visit)
    (define-key map (kbd "/") #'+csv-grid-filter)
    (define-key map (kbd "\\") #'+csv-grid-clear-filters)
    (define-key map (kbd "s") #'+csv-grid-sort)
    (define-key map (kbd "G") #'+csv-grid-group)
    (define-key map (kbd "h") #'+csv-grid-hide-column)
    (define-key map (kbd "H") #'+csv-grid-show-columns)
    (define-key map (kbd "a") #'+csv-grid-cycle-total)
    (define-key map (kbd "g") #'+csv-grid-refresh)
    (define-key map (kbd "TAB") #'+csv-grid-next-cell)
    (define-key map (kbd "<backtab>") #'+csv-grid-previous-cell)
    (define-key map (kbd "<right>") #'+csv-grid-next-cell)
    (define-key map (kbd "<left>") #'+csv-grid-previous-cell)
    (define-key map (kbd "<down>") #'+csv-grid-next-row)
    (define-key map (kbd "<up>") #'+csv-grid-previous-row)
    (define-key map (kbd "n") #'+csv-grid-next-row)
    (define-key map (kbd "p") #'+csv-grid-previous-row)
    (define-key map (kbd "j") #'+csv-grid-next-row)
    (define-key map (kbd "k") #'+csv-grid-previous-row)
    (define-key map (kbd "?") #'+csv-grid-menu)
    map)
  "Keys of a CSV grid.")

(define-derived-mode +csv-grid-mode special-mode "CSV-Grid"
  "A view over a CSV buffer: sort, filter, group and total without editing it.
\\{+csv-grid-mode-map}"
  (setq truncate-lines t
        buffer-read-only t)
  (setq-local header-line-format '(:eval (+csv-grid--header-line)))
  (add-hook 'post-command-hook #'+csv-grid--show-cell nil t)
  (add-hook 'kill-buffer-hook #'+csv-grid--view-killed-h nil t))

;;;###autoload
(defun +csv/grid ()
  "Open a grid view of the current CSV buffer."
  (interactive)
  (let* ((source (if (derived-mode-p '+csv-grid-mode) +csv-grid--source (current-buffer)))
         (buffer (get-buffer-create (format "*CSV grid: %s*" (buffer-name source)))))
    (with-current-buffer buffer
      (unless (derived-mode-p '+csv-grid-mode) (+csv-grid-mode))
      (setq +csv-grid--source source)
      (+csv-grid-refresh))
    (with-current-buffer source
      (cl-pushnew buffer +csv-grid--views)
      (add-hook 'after-change-functions #'+csv-grid--source-changed-h nil t)
      (add-hook 'kill-buffer-hook #'+csv-grid--source-killed-h nil t))
    (pop-to-buffer buffer)))

(transient-define-prefix +csv-grid-menu ()
    "Act on a CSV grid."
    [["Cell"
      ("RET" "edit (flip a checkbox)" +csv-grid-edit)
      ("e" "edit as text" +csv-grid-edit-text)
      ("SPC" "whole record" +csv-grid-expand)
      ("DEL" "clear" +csv-grid-clear)
      ("w" "copy" +csv-grid-copy)]
     ["Row"
      ("+" "add" +csv-grid-add-row)
      ("D" "delete" +csv-grid-delete-row)
      ("o" "show in CSV" +csv-grid-visit)
      ("u" "undo in CSV" +csv-grid-undo)]
     ["View"
      ("/" "filter" +csv-grid-filter)
      ("\\" "clear filters" +csv-grid-clear-filters)
      ("s" "sort by column" +csv-grid-sort)
      ("G" "group by column" +csv-grid-group)]
     ["Columns"
      ("h" "hide" +csv-grid-hide-column)
      ("H" "show all" +csv-grid-show-columns)
      ("a" "cycle total" +csv-grid-cycle-total)
      ("g" "reread CSV" +csv-grid-refresh)]])

(provide 'lang/filetypes/csv-grid)
;;; lang/filetypes/csv-grid.el ends here
