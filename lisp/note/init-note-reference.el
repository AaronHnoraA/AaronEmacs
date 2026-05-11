;;; init-note-reference.el --- Typst label reference inserter -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight replacement for the Org reference picker.  Scans the current
;; Typst buffer for `<label>' targets and lets the user insert `@label' with
;; a surrounding-line preview.  Cross-file references go through the existing
;; `#note("id")[...]' link path.

;;; Code:

(require 'init-note)
(require 'subr-x)

(declare-function my/org-reference-insert-link "init-org-utility" ())

(defconst my/note-reference--label-regexp
  "<\\([a-zA-Z][a-zA-Z0-9_:.-]*\\)>"
  "Regexp matching a Typst label definition.")

(defcustom my/note-reference-excluded-labels '("note")
  "Label names that are infrastructure and should not appear as candidates."
  :type '(repeat string)
  :group 'my/note)

(defun my/note-reference--candidates ()
  "Return ((display . id) ...) for labels defined in the current buffer."
  (let (cands seen)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward my/note-reference--label-regexp nil t)
          (let* ((id (match-string-no-properties 1))
                 (line (line-number-at-pos (match-beginning 0)))
                 (context (string-trim
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
            (unless (or (member id my/note-reference-excluded-labels)
                        (member id seen))
              (push id seen)
              (push (cons (format "%-28s  L%-4d  %s"
                                  id line
                                  (if (> (length context) 72)
                                      (concat (substring context 0 72) "…")
                                    context))
                          id)
                    cands))))))
    (nreverse cands)))

;;;###autoload
(defun my/note-reference-insert ()
  "Insert `@label' for a Typst label defined in the current buffer."
  (interactive)
  (unless (derived-mode-p 'typst-ts-mode 'typst-mode 'my/typst-mode)
    (user-error "Not in a Typst note buffer"))
  (let ((cands (my/note-reference--candidates)))
    (unless cands
      (user-error "No Typst labels found in current buffer"))
    (let* ((choice (completing-read "Label: " cands nil t))
           (id (cdr (assoc choice cands))))
      (insert (concat "@" id)))))

;;;###autoload
(defun my/reference-insert-dispatch ()
  "Insert a reference link appropriate to the current major mode."
  (interactive)
  (cond
   ((derived-mode-p 'typst-ts-mode 'typst-mode 'my/typst-mode)
    (call-interactively #'my/note-reference-insert))
   ((derived-mode-p 'org-mode)
    (call-interactively #'my/org-reference-insert-link))
   (t
    (user-error "No reference-insert handler for %s" major-mode))))

(provide 'init-note-reference)
;;; init-note-reference.el ends here
