;;; csv-grid-tests.el --- Grid views over CSV buffers -*- lexical-binding: t; -*-

;; emacs -Q --batch -L lisp -L lisp/core -l test/csv-grid-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'lang/filetypes/csv-grid)

(defconst csv-grid-tests--text
  "name,qty,when,note
apple,3,2026-01-02,fresh
pear,12,2026-01-01,\"ripe, soft\"
fig,,2026-03-01,\"says \"\"hi\"\"
twice\"

kiwi,1,2025-12-31,
")

(defmacro csv-grid-tests--with-grid (&rest body)
  "Run BODY in a grid over a buffer holding `csv-grid-tests--text'."
  (declare (indent 0))
  `(let ((source (generate-new-buffer "grid-source.csv")))
     (unwind-protect
         (progn
           (with-current-buffer source (insert csv-grid-tests--text))
           (with-current-buffer source
             (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
               (+csv/grid)))
           (with-current-buffer (format "*CSV grid: %s*" (buffer-name source))
             (unwind-protect (progn ,@body)
               (kill-buffer))))
       (kill-buffer source))))

(defun csv-grid-tests--column (name)
  "Index of column NAME in the current grid."
  (cl-position name +csv-grid--header :test #'equal))

(defun csv-grid-tests--drawn-names ()
  "First-column values of the drawn rows, top to bottom."
  (let (names)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((row (get-text-property (point) 'csv-grid-row)))
          (push (+csv-grid--field (aref +csv-grid--records row) 0) names))
        (forward-line 1)))
    (nreverse names)))

(ert-deftest csv-grid-parses-quotes-newlines-and-blank-lines ()
  (with-temp-buffer
    (insert csv-grid-tests--text)
    (let ((records (+csv-grid--parse ?,)))
      (should (= 5 (length records)))
      (should (equal ["pear" "12" "2026-01-01" "ripe, soft"] (cdr (aref records 2))))
      (should (equal "says \"hi\"\ntwice" (aref (cdr (aref records 3)) 3)))
      (should (equal ["kiwi" "1" "2025-12-31" ""] (cdr (aref records 4)))))))

(ert-deftest csv-grid-encodes-only-what-needs-quotes ()
  (should (equal "plain" (+csv-grid--encode "plain" ?,)))
  (should (equal "\"a,b\"" (+csv-grid--encode "a,b" ?,)))
  (should (equal "\"say \"\"x\"\"\"" (+csv-grid--encode "say \"x\"" ?,)))
  (should (equal "a,b" (+csv-grid--encode "a,b" ?\t)))
  (should (equal "\" lead\"" (+csv-grid--encode " lead" ?,))))

(ert-deftest csv-grid-guesses-column-types ()
  (csv-grid-tests--with-grid
    (should (eq 'text (aref +csv-grid--types (csv-grid-tests--column "name"))))
    (should (eq 'number (aref +csv-grid--types (csv-grid-tests--column "qty"))))
    (should (eq 'date (aref +csv-grid--types (csv-grid-tests--column "when"))))))

(ert-deftest csv-grid-sorts-numbers-numerically-with-empty-last ()
  (csv-grid-tests--with-grid
    (setq +csv-grid--sort (cons (csv-grid-tests--column "qty") 'asc))
    (+csv-grid--render)
    (should (equal '("kiwi" "apple" "pear" "fig") (csv-grid-tests--drawn-names)))
    (setq +csv-grid--sort (cons (csv-grid-tests--column "qty") 'desc))
    (+csv-grid--render)
    (should (equal '("pear" "apple" "kiwi" "fig") (csv-grid-tests--drawn-names)))))

(ert-deftest csv-grid-filters-add-up-and-never-touch-the-source ()
  (csv-grid-tests--with-grid
    (let ((before (with-current-buffer +csv-grid--source (buffer-string))))
      (+csv-grid-filter (csv-grid-tests--column "qty") ">" "2")
      (should (equal '("apple" "pear") (csv-grid-tests--drawn-names)))
      (+csv-grid-filter (csv-grid-tests--column "note") "~" "RIPE")
      (should (equal '("pear") (csv-grid-tests--drawn-names)))
      (should (string-match-p "filter qty > 2 & note ~ RIPE" (buffer-string)))
      (+csv-grid-clear-filters)
      (should (= 4 (length (csv-grid-tests--drawn-names))))
      (should (equal before (with-current-buffer +csv-grid--source (buffer-string)))))))

(ert-deftest csv-grid-groups-and-totals ()
  (csv-grid-tests--with-grid
    (let ((qty (csv-grid-tests--column "qty")))
      (setq +csv-grid--totals (list (cons qty 'sum)))
      (+csv-grid--render)
      (should (string-match-p "sum 16" (buffer-string)))
      (should (equal "16" (+csv-grid--aggregate '(0 1 2 3) qty 'sum)))
      (should (equal "5.333" (+csv-grid--aggregate '(0 1 2 3) qty 'avg)))
      (should (equal "3" (+csv-grid--aggregate '(0 1 2 3) qty 'count)))
      (setq +csv-grid--group (csv-grid-tests--column "qty"))
      (+csv-grid--render)
      (should (string-match-p "^qty: (empty)  1$" (buffer-string))))))

(ert-deftest csv-grid-edit-writes-the-field-in-the-source ()
  (csv-grid-tests--with-grid
    (+csv-grid--write 1 (csv-grid-tests--column "note") "ripe, \"very\"")
    (with-current-buffer +csv-grid--source
      (should (string-match-p "^pear,12,2026-01-01,\"ripe, \"\"very\"\"\"$" (buffer-string))))
    ;; The grid read the source again.
    (should (equal "ripe, \"very\"" (+csv-grid--field (aref +csv-grid--records 1) 3)))
    (+csv-grid--write 3 (csv-grid-tests--column "note") "")
    (with-current-buffer +csv-grid--source
      (should (string-match-p "^kiwi,1,2025-12-31,$" (buffer-string))))))

(ert-deftest csv-grid-refuses-to-write-through-stale-records ()
  (csv-grid-tests--with-grid
    (with-current-buffer +csv-grid--source
      (goto-char (point-min))
      (insert "x"))
    (should-error (+csv-grid--write 0 0 "plum") :type 'user-error)
    ;; The refusal reread the source, so the next write lands.
    (+csv-grid--write 0 0 "plum")
    (with-current-buffer +csv-grid--source
      (should (string-match-p "^plum,3," (buffer-string))))))

(ert-deftest csv-grid-adds-and-deletes-rows ()
  (csv-grid-tests--with-grid
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "plum")))
      (+csv-grid-add-row))
    (with-current-buffer +csv-grid--source
      (should (string-match-p "^plum,,,$" (buffer-string))))
    (should (= 5 (length +csv-grid--records)))
    (should (+csv-grid--goto-row 0))
    (cl-letf (((symbol-function 'y-or-n-p) #'always))
      (+csv-grid-delete-row))
    (with-current-buffer +csv-grid--source
      (should-not (string-match-p "^apple" (buffer-string))))))

(ert-deftest csv-grid-moves-by-cell ()
  (csv-grid-tests--with-grid
    (should (+csv-grid--goto-row 0 0))
    (+csv-grid-next-cell)
    (should (eql 1 (get-text-property (point) 'csv-grid-column)))
    (+csv-grid-next-row)
    (should (eql 1 (get-text-property (point) 'csv-grid-column)))
    (should (eql 1 (get-text-property (point) 'csv-grid-row)))
    (+csv-grid--show-cell)
    (should (overlay-buffer +csv-grid--cell-overlay))))

(ert-deftest csv-grid-hides-columns-and-keeps-one ()
  (csv-grid-tests--with-grid
    (+csv-grid--goto-row 0 0)
    (+csv-grid-hide-column)
    (should-not (memq 0 +csv-grid--visible))
    (+csv-grid-show-columns)
    (should (memq 0 +csv-grid--visible))))

(ert-deftest csv-grid-stops-following-a-source-without-views ()
  (let ((source (generate-new-buffer "grid-source.csv")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert csv-grid-tests--text)
            (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)) (+csv/grid))
            (should (memq #'+csv-grid--source-changed-h after-change-functions)))
          (kill-buffer (format "*CSV grid: %s*" (buffer-name source)))
          (with-current-buffer source
            (should-not (memq #'+csv-grid--source-changed-h after-change-functions))))
      (kill-buffer source))))

(defconst csv-grid-tests--typed
  "task,done,team
a,Yes,red
b,No,blue
c,Yes,red
d,,red
e,No,blue
")

(defmacro csv-grid-tests--with-typed-grid (&rest body)
  "Run BODY in a grid over `csv-grid-tests--typed'."
  (declare (indent 0))
  `(let ((csv-grid-tests--text csv-grid-tests--typed))
     (csv-grid-tests--with-grid ,@body)))

(ert-deftest csv-grid-detects-checkbox-and-select-columns ()
  (csv-grid-tests--with-typed-grid
    (should (eq 'checkbox (aref +csv-grid--types 1)))
    (should (equal '("yes" . "no") (aref +csv-grid--checkboxes 1)))
    (should (eq 'select (aref +csv-grid--types 2)))
    (should (eq 'text (aref +csv-grid--types 0)))
    (should (equal '("blue" "red") (+csv-grid--choices 2)))
    ;; Checkboxes draw as boxes, not their spelling.
    (should (string-match-p "☑" (buffer-string)))
    (should-not (string-match-p "Yes" (buffer-string)))))

(ert-deftest csv-grid-flips-a-checkbox-keeping-its-case ()
  (csv-grid-tests--with-typed-grid
    (+csv-grid--goto-row 0 1)
    (+csv-grid-edit)
    (with-current-buffer +csv-grid--source
      (should (string-match-p "^a,No,red$" (buffer-string))))
    ;; An empty cell takes the column's spelling style.
    (+csv-grid--goto-row 3 1)
    (+csv-grid-edit)
    (with-current-buffer +csv-grid--source
      (should (string-match-p "^d,Yes,red$" (buffer-string))))))

(ert-deftest csv-grid-offers-a-select-column-its-values ()
  (csv-grid-tests--with-typed-grid
    (+csv-grid--goto-row 0 2)
    (let (offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _) (setq offered collection) "green")))
        (+csv-grid-edit))
      (should (equal '("blue" "red") offered)))
    (with-current-buffer +csv-grid--source
      (should (string-match-p "^a,Yes,green$" (buffer-string))))))

(ert-deftest csv-grid-sorts-and-counts-checkboxes ()
  (csv-grid-tests--with-typed-grid
    (setq +csv-grid--sort (cons 1 'asc)
          +csv-grid--totals (list (cons 1 'checked)))
    (+csv-grid--render)
    (should (equal '("a" "c") (seq-take (csv-grid-tests--drawn-names) 2)))
    (should (string-match-p "checked 2" (buffer-string)))
    (should (equal "3" (+csv-grid--aggregate '(0 1 2 3 4) 1 'unchecked)))))

;;; csv-grid-tests.el ends here
