;;; init-pdf-save-place-tests.el --- PDFs reopen at their page -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-pdf-save-place-tests.el -f ert-run-tests-batch-and-exit
;;
;; Needs epdfinfo and `cupsfilter' (macOS) to make a multi-page PDF.

;;; Code:

(require 'ert)
(require 'saveplace)

(defun init-pdf-save-place-tests--make-pdf (file)
  "Write a PDF of several pages to FILE."
  (let ((text (make-temp-file "pdf-pages" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file text
            (dotimes (i 400) (insert (format "line %d\n" i))))
          (call-process "cupsfilter" nil (list :file file) nil text))
      (delete-file text))))

(ert-deftest +pdf-save-place-reopens-a-pdf-at-its-page ()
  "A PDF left on page 4 opens on page 4 again; plain files keep point."
  (skip-unless (executable-find "cupsfilter"))
  (let* ((dir (file-truename (make-temp-file "pdf-place" t)))
         (pdf (expand-file-name "paper.pdf" dir))
         (text (expand-file-name "note.txt" dir))
         (save-place-file (expand-file-name "places" dir))
         (save-place-alist nil)
         (save-place-loaded t))
    (unwind-protect
        (progn
          (init-pdf-save-place-tests--make-pdf pdf)
          (save-place-mode 1)
          (with-current-buffer (find-file-noselect pdf)
            (should (derived-mode-p 'pdf-view-mode))
            (skip-unless (> (pdf-cache-number-of-pages) 4))
            (pdf-view-goto-page 4)
            (kill-buffer))
          (should (equal (cdr (assoc (abbreviate-file-name pdf) save-place-alist))
                         '((pdf-page . 4))))
          (with-current-buffer (find-file-noselect pdf)
            (should (= (image-mode-window-get 'page t) 4))
            (kill-buffer))
          ;; Ordinary files are untouched: point is saved and restored.
          (with-temp-file text (insert "abcdef\n"))
          (with-current-buffer (find-file-noselect text)
            (goto-char 4)
            (kill-buffer))
          (with-current-buffer (find-file-noselect text)
            (should (= (point) 4))
            (kill-buffer)))
      (delete-directory dir t))))

(provide 'init-pdf-save-place-tests)
;;; init-pdf-save-place-tests.el ends here
