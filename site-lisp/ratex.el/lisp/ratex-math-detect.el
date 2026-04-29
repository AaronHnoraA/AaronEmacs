;;; ratex-math-detect.el --- Math fragment detection -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ratex-core)

(defconst ratex--delimiter-pairs
  '(("\\[" . "\\]")
    ("\\(" . "\\)"))
  "Lightweight Org math delimiters supported by RaTeX.")

(defconst ratex--active-fragment-preferred-opens
  '("\\[" "\\(")
  "Delimiters preferred for active edit previews.")

(defcustom ratex-region-scan-context-chars 600
  "Extra characters scanned around a requested region.

RaTeX refreshes visible areas incrementally.  A visible region can start or end
inside a math fragment, so the detector needs a bounded lookaround to recover
the full delimiter pair without falling back to a full-buffer parse."
  :type 'integer
  :group 'ratex)

(defun ratex-fragments-in-buffer ()
  "Return all math fragments in the current buffer."
  (let ((org-fragments (ratex--org-fragments-in-buffer))
        all)
    (dolist (pair ratex--delimiter-pairs)
      (setq all (nconc all (ratex--fragments-with-delimiters (car pair) (cdr pair)))))
    (setq all (nconc all (ratex--org-display-latex-blocks-in-buffer)))
    (ratex--select-non-overlapping-fragments (nconc org-fragments all))))

(defun ratex-fragments-in-region (beg end)
  "Return math fragments intersecting BEG..END in the current buffer.

This regional scan intentionally avoids Org's full-buffer parser so repeated
startup and refresh work stays local to what is on screen."
  (let* ((region-beg (max (point-min) (min beg end)))
         (region-end (min (point-max) (max beg end)))
         (scan-beg (max (point-min)
                        (- region-beg ratex-region-scan-context-chars)))
         (scan-end (min (point-max)
                        (+ region-end ratex-region-scan-context-chars))))
    (save-restriction
      (narrow-to-region scan-beg scan-end)
      (let (all)
        (dolist (pair ratex--delimiter-pairs)
          (setq all (nconc all (ratex--fragments-with-delimiters (car pair) (cdr pair)))))
        (setq all (nconc all (ratex--org-display-latex-blocks-in-buffer)))
        (cl-remove-if-not
         (lambda (fragment)
           (ratex--fragment-intersects-region-p fragment region-beg region-end))
         (ratex--select-non-overlapping-fragments all))))))

(defun ratex-fragment-at-point ()
  "Return the math fragment around point as a plist.

The plist contains `:begin', `:end' and `:content' when a fragment is found."
  (let ((fragment
         (unless (ratex--code-context-at-p (point))
           (let* ((candidates
                   (append
                    (when-let* ((org-fragment (ratex--org-fragment-at-point)))
                      (list org-fragment))
                    (cl-loop for (open . close) in ratex--delimiter-pairs
                             nconc (ratex--fragments-with-delimiters-at-point open close))
                    (ratex--org-display-latex-blocks-at-point)))
                  (preferred
                   (cl-remove-if-not #'ratex--preferred-active-fragment-p candidates)))
             (or (ratex--largest-fragment preferred)
                 (ratex--largest-fragment candidates))))))
    (ratex-debug-log "fragment-at-point pos=%s mode=%s fragment=%S"
                     (point) major-mode fragment)
    fragment))

(defun ratex--org-fragment-at-point ()
  "Return Org LaTeX fragment at point as a RaTeX plist, or nil."
  (when (derived-mode-p 'org-mode)
    (require 'org-element)
    (let* ((datum (org-element-context))
           (target (or (org-element-lineage datum '(latex-fragment) t)
                       datum)))
      (ratex-debug-log "org-element-context type=%S begin=%S end=%S value=%S"
                       (org-element-type target)
                       (ignore-errors (org-element-property :begin target))
                       (ignore-errors (org-element-property :end target))
                       (ignore-errors (org-element-property :value target)))
      (when (eq (org-element-type target) 'latex-fragment)
        (ratex--org-element-to-fragment target)))))

(defun ratex--org-fragments-in-buffer ()
  "Return all Org LaTeX fragments in the current buffer, or nil outside Org."
  (when (derived-mode-p 'org-mode)
    (require 'org-element)
    (let (fragments)
      (org-element-map (org-element-parse-buffer) '(latex-fragment)
        (lambda (datum)
          (when-let* ((fragment (ratex--org-element-to-fragment datum)))
            (push fragment fragments))))
      (nreverse fragments))))

(defun ratex--org-element-to-fragment (datum)
  "Convert Org LaTeX DATUM to the plist shape expected by RaTeX."
  (let* ((type (org-element-type datum))
         (begin (org-element-property :begin datum))
         (end (save-excursion
                (goto-char (org-element-property :end datum))
                (skip-chars-backward " \r\t\n")
                (point)))
         (value (buffer-substring-no-properties begin end)))
    (pcase type
      ('latex-fragment
       (pcase-let ((`(,open ,content ,close) (ratex--split-delimited-fragment value)))
         (when (member open '("\\[" "\\("))
           (list :begin begin
                 :end end
                 :content content
                 :open open
                 :close close))))
      (_ nil))))

(defun ratex--split-delimited-fragment (value)
  "Split VALUE into (OPEN CONTENT CLOSE) for common math delimiters."
  (cond
   ((and (string-prefix-p "\\[" value) (string-suffix-p "\\]" value))
    (list "\\[" (substring value 2 -2) "\\]"))
   ((and (string-prefix-p "\\(" value) (string-suffix-p "\\)" value))
    (list "\\(" (substring value 2 -2) "\\)"))
   (t
    (list "" (string-trim value) ""))))

(defun ratex--fragments-with-delimiters (open close)
  "Return all OPEN..CLOSE fragments in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((open-len (length open))
          (close-len (length close))
          fragments)
      (while (search-forward open nil t)
        (let ((begin (- (point) open-len)))
          (unless (or (ratex--escaped-at-p begin)
                      (ratex--code-context-at-p begin))
            (let ((content-begin (point))
                  found-end
                  content-end)
              (while (and (not found-end) (search-forward close nil t))
                (let ((end-start (- (point) close-len)))
                  (unless (or (ratex--escaped-at-p end-start)
                              (ratex--code-context-at-p end-start))
                    (setq found-end (point))
                    (setq content-end end-start))))
              (when (and found-end (<= content-begin content-end))
                (push (list :begin begin
                            :end found-end
                            :content (buffer-substring-no-properties content-begin content-end)
                            :open open
                            :close close)
                      fragments))))))
      (nreverse fragments))))

(defun ratex--fragments-with-delimiters-at-point (open close)
  "Return all OPEN..CLOSE fragments containing point."
  (save-excursion
    (let ((pos (point))
          (open-len (length open))
          (close-len (length close))
          fragments)
      (goto-char (min (point-max) (+ pos open-len)))
      (while (search-backward open nil t)
        (let ((begin (point)))
          (if (or (ratex--escaped-at-p begin)
                  (ratex--code-context-at-p begin))
              (goto-char (max (point-min) (1- begin)))
            (let ((content-begin (+ begin open-len))
                  found-end
                  content-end)
              (goto-char content-begin)
              (while (and (not found-end) (search-forward close nil t))
                (let ((end-start (- (point) close-len)))
                  (unless (or (ratex--escaped-at-p end-start)
                              (ratex--code-context-at-p end-start))
                    (setq found-end (point))
                    (setq content-end end-start))))
              (when (and found-end
                         (<= begin pos)
                         (< pos found-end))
                (push (list :begin begin
                            :end found-end
                            :content (buffer-substring-no-properties
                                      content-begin
                                      content-end)
                            :open open
                            :close close)
                      fragments))
              (goto-char (max (point-min) (1- begin)))))))
      fragments)))

(defun ratex--org-display-latex-blocks-in-buffer ()
  "Return all Org #+begin_display_latex blocks in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (fragments)
      (while (re-search-forward "^[ \t]*#\\+begin_display_latex\\b.*$" nil t)
        (when-let* ((fragment (ratex--org-display-latex-block-from (match-beginning 0))))
          (push fragment fragments)))
      (nreverse fragments))))

(defun ratex--org-display-latex-blocks-at-point ()
  "Return Org #+begin_display_latex blocks containing point."
  (save-excursion
    (let ((pos (point)))
      (when (re-search-backward "^[ \t]*#\\+begin_display_latex\\b.*$" nil t)
        (when-let* ((candidate (ratex--org-display-latex-block-from (match-beginning 0))))
          (when (and (<= (plist-get candidate :begin) pos)
                     (< pos (plist-get candidate :end)))
            (list candidate)))))))

(defun ratex--org-display-latex-block-from (begin)
  "Return an Org #+begin_display_latex block fragment starting at BEGIN."
  (save-excursion
    (goto-char begin)
    (let ((content-begin (line-beginning-position 2)))
      (when (re-search-forward "^[ \t]*#\\+end_display_latex[ \t]*$" nil t)
        (let ((content-end (match-beginning 0))
              (end (match-end 0)))
          (list :begin begin
                :end end
                :content (buffer-substring-no-properties content-begin content-end)
                :open "#+begin_display_latex"
                :close "#+end_display_latex"
                :block 'latex))))))

(defun ratex--preferred-active-fragment-p (fragment)
  "Return non-nil when FRAGMENT should win for active preview selection."
  (let ((open (plist-get fragment :open)))
    (or (member open ratex--active-fragment-preferred-opens)
        (plist-get fragment :block))))

(defun ratex--largest-fragment (fragments)
  "Return the widest fragment from FRAGMENTS, or nil."
  (car
   (sort (copy-sequence fragments)
         (lambda (a b)
           (let ((a-size (- (plist-get a :end) (plist-get a :begin)))
                 (b-size (- (plist-get b :end) (plist-get b :begin))))
             (if (= a-size b-size)
                 (< (plist-get a :begin) (plist-get b :begin))
               (> a-size b-size)))))))

(defun ratex--select-non-overlapping-fragments (fragments)
  "Return FRAGMENTS sorted and without overlaps."
  (let ((sorted
         (sort (copy-sequence fragments)
               (lambda (a b)
                 (let ((ab (plist-get a :begin))
                       (bb (plist-get b :begin))
                       (ae (plist-get a :end))
                       (be (plist-get b :end)))
                   (if (= ab bb) (> ae be) (< ab bb))))))
        accepted)
    (dolist (fragment sorted)
      (unless (cl-some (lambda (existing)
                         (ratex--fragments-overlap-p existing fragment))
                       accepted)
        (push fragment accepted)))
    (nreverse accepted)))

(defun ratex--fragments-overlap-p (a b)
  "Return non-nil if fragment A overlaps fragment B."
  (let ((ab (plist-get a :begin))
        (ae (plist-get a :end))
        (bb (plist-get b :begin))
        (be (plist-get b :end)))
    (and (< ab be) (< bb ae))))

(defun ratex--fragment-intersects-region-p (fragment beg end)
  "Return non-nil when FRAGMENT intersects BEG..END."
  (let ((fb (plist-get fragment :begin))
        (fe (plist-get fragment :end)))
    (and (< fb end) (< beg fe))))

(defun ratex--escaped-at-p (pos)
  "Return non-nil if the token at POS is escaped by backslashes."
  (let ((count 0)
        (i (1- pos)))
    (while (and (>= i (point-min))
                (eq (char-after i) ?\\))
      (setq count (1+ count))
      (setq i (1- i)))
    (= 1 (% count 2))))

(defun ratex--code-context-at-p (pos)
  "Return non-nil when POS is in a code-like context."
  (save-excursion
    (goto-char pos)
    (or (nth 3 (syntax-ppss))
        (nth 4 (syntax-ppss))
        (ratex--mode-code-context-p))))

(defun ratex--mode-code-context-p ()
  "Return non-nil when point is in a mode-specific code block."
  (cond
   ((derived-mode-p 'org-mode)
    (or (and (fboundp 'org-in-src-block-p)
             (org-in-src-block-p t))
        (and (fboundp 'org-in-block-p)
             (org-in-block-p '("example" "src" "verbatim")))))
   ((derived-mode-p 'markdown-mode 'gfm-mode)
    (and (fboundp 'markdown-code-block-at-point-p)
         (markdown-code-block-at-point-p)))
   (t nil)))

(provide 'ratex-math-detect)

;;; ratex-math-detect.el ends here
