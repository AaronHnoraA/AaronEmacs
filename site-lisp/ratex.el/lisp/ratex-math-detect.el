;;; ratex-math-detect.el --- Math fragment detection -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defconst ratex--delimiter-pairs
  '(("\\[" . "\\]")
    ("\\(" . "\\)")))

(defconst ratex--math-environments
  '("equation" "equation*"
    "displaymath"
    "align" "align*"
    "alignat" "alignat*"
    "gather" "gather*"
    "multline" "multline*"
    "flalign" "flalign*"
    "cases"
    "matrix" "pmatrix" "bmatrix" "Bmatrix" "vmatrix" "Vmatrix"
    "smallmatrix"
    "split" "aligned" "alignedat" "array")
  "Math environments that should be previewed by RaTeX.")

(defconst ratex--active-fragment-preferred-opens
  '("\\[" "\\(")
  "Delimiters preferred for active edit previews.")

(defun ratex-fragments-in-buffer ()
  "Return all math fragments in the current buffer."
  (let (all)
    (dolist (pair ratex--delimiter-pairs)
      (setq all (nconc all (ratex--fragments-with-delimiters (car pair) (cdr pair)))))
    (setq all (nconc all (ratex--environment-fragments-in-buffer)))
    (ratex--select-non-overlapping-fragments all)))

(defun ratex-fragment-at-point ()
  "Return the math fragment around point as a plist.

The plist contains `:begin', `:end' and `:content' when a fragment is found."
  (unless (ratex--code-context-at-p (point))
    (let* ((candidates
            (nconc
             (cl-loop for (open . close) in ratex--delimiter-pairs
                      nconc (ratex--fragments-with-delimiters-at-point open close))
             (ratex--environment-fragments-at-point)))
           (preferred
            (cl-remove-if-not #'ratex--preferred-active-fragment-p candidates)))
      (or (ratex--largest-fragment preferred)
          (ratex--largest-fragment candidates)))))

(defun ratex--fragment-with-delimiters (open close)
  "Return fragment bounded by OPEN and CLOSE around point."
  (save-excursion
    (let ((pos (point))
          (open-len (length open))
          (close-len (length close))
          fragment)
      (while (and (not fragment) (search-backward open nil t))
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
              (if (and found-end
                       (<= content-begin pos)
                       (<= pos content-end))
                  (setq fragment
                        (list :begin begin
                              :end found-end
                              :content (buffer-substring-no-properties
                                        content-begin
                                        content-end)
                              :open open
                              :close close))
                (goto-char (max (point-min) (1- begin))))))))
      fragment)))

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
                         (<= content-begin pos)
                         (<= pos content-end))
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

(defun ratex--math-environment-regexp ()
  "Return a regexp matching supported math environment openings."
  (concat "\\\\begin{\\("
          (regexp-opt ratex--math-environments t)
          "\\)}"))

(defun ratex--environment-fragments-in-buffer ()
  "Return all supported \\begin..\\end math environment fragments."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (ratex--math-environment-regexp))
          fragments)
      (while (re-search-forward regexp nil t)
        (let* ((begin (match-beginning 0))
               (env (match-string-no-properties 1)))
          (unless (or (ratex--escaped-at-p begin)
                      (ratex--code-context-at-p begin))
            (when-let* ((fragment (ratex--environment-fragment-from begin env)))
              (push fragment fragments)))))
      (nreverse fragments))))

(defun ratex--environment-fragment-at-point ()
  "Return the supported \\begin..\\end math environment around point."
  (save-excursion
    (let ((pos (point))
          (regexp (ratex--math-environment-regexp))
          fragment)
      (while (and (not fragment)
                  (re-search-backward regexp nil t))
        (let* ((begin (match-beginning 0))
               (env (match-string-no-properties 1)))
          (if (or (ratex--escaped-at-p begin)
                  (ratex--code-context-at-p begin))
              (goto-char (max (point-min) (1- begin)))
            (let ((candidate (ratex--environment-fragment-from begin env)))
              (if (and candidate
                       (<= (plist-get candidate :begin) pos)
                       (<= pos (plist-get candidate :end)))
                  (setq fragment candidate)
                (goto-char (max (point-min) (1- begin))))))))
      fragment)))

(defun ratex--environment-fragments-at-point ()
  "Return supported \\begin..\\end math environment fragments containing point."
  (save-excursion
    (let ((pos (point))
          (regexp (ratex--math-environment-regexp))
          fragments)
      (while (re-search-backward regexp nil t)
        (let* ((begin (match-beginning 0))
               (env (match-string-no-properties 1)))
          (if (or (ratex--escaped-at-p begin)
                  (ratex--code-context-at-p begin))
              (goto-char (max (point-min) (1- begin)))
            (let ((candidate (ratex--environment-fragment-from begin env)))
              (when (and candidate
                         (<= (plist-get candidate :begin) pos)
                         (<= pos (plist-get candidate :end)))
                (push candidate fragments))
              (goto-char (max (point-min) (1- begin)))))))
      fragments)))

(defun ratex--environment-fragment-from (begin env)
  "Return a math environment fragment starting at BEGIN for ENV."
  (save-excursion
    (goto-char begin)
    (let ((end-regexp (format "\\\\end{%s}" (regexp-quote env))))
      (when (re-search-forward end-regexp nil t)
        (let ((end (match-end 0)))
          (list :begin begin
                :end end
                :content (buffer-substring-no-properties begin end)
                :open (format "\\begin{%s}" env)
                :close (format "\\end{%s}" env)
                :environment env))))))

(defun ratex--preferred-active-fragment-p (fragment)
  "Return non-nil when FRAGMENT should win for active preview selection."
  (let ((open (plist-get fragment :open)))
    (or (member open ratex--active-fragment-preferred-opens)
        (plist-get fragment :environment))))

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
