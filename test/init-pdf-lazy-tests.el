;;; init-pdf-lazy-tests.el --- PDF Tools waits for the first PDF -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-pdf-lazy-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(ert-deftest +pdf-tools-waits-for-the-first-pdf ()
  "Startup neither loads PDF Tools nor starts `epdfinfo'; a PDF still opens
in `pdf-view-mode'."
  (should-not (featurep 'pdf-tools))
  (should-not (seq-some (lambda (p) (string-match-p "epdfinfo" (process-name p)))
                        (process-list)))
  (should (eq (assoc-default "/x/Paper.PDF" auto-mode-alist #'string-match)
              'pdf-view-mode))
  (should (eq (assoc-default "%PDF-1.7" magic-mode-alist
                             (lambda (re s) (string-match-p re s)))
              'pdf-view-mode)))

(provide 'init-pdf-lazy-tests)
;;; init-pdf-lazy-tests.el ends here
