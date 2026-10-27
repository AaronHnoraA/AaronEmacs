;;; init-format-region-tests.el --- SPC c f formats the selection -*- lexical-binding: t -*-

;;; Commentary:
;; `+format/region-or-buffer' (SPC c f) takes Doom's behaviour: a selection
;; goes to the server's range formatting, otherwise the buffer is formatted.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'evil)

(defun init-format-region-tests--run (setup &optional range-p)
  "Run `+format/region-or-buffer' after SETUP.
RANGE-P is whether the fake server offers range formatting.  Return what
was formatted: `buffer', (BEG END), or the `user-error' message."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (transient-mark-mode 1)
    (goto-char (point-min))
    (funcall setup)
    (let (seen)
      (cl-letf (((symbol-function '+current-language-server-backend) (lambda () 'lsp))
                ((symbol-function 'lsp-feature?) (lambda (_method) range-p))
                ((symbol-function '+language-server/format-buffer)
                 (lambda () (setq seen 'buffer)))
                ((symbol-function 'lsp-format-region)
                 (lambda (beg end) (setq seen (list beg end)))))
        (condition-case err
            (+format/region-or-buffer)
          (user-error (setq seen (cadr err)))))
      seen)))

(ert-deftest +format-without-selection-formats-the-buffer ()
  (should (eq (init-format-region-tests--run #'ignore t) 'buffer)))

(ert-deftest +format-selection-formats-only-the-selection ()
  (should (equal (init-format-region-tests--run
                  (lambda () (set-mark 5) (goto-char 9) (activate-mark))
                  t)
                 '(5 9))))

(ert-deftest +format-evil-visual-line-selection-is-whole-lines ()
  (should (equal (init-format-region-tests--run
                  (lambda ()
                    (evil-local-mode 1)
                    (evil-normal-state)
                    (forward-line 1)
                    (evil-visual-line)
                    (evil-visual-refresh))
                  t)
                 '(5 9))))

(ert-deftest +format-selection-needs-range-formatting ()
  (should (string-match-p
           "cannot format a selection"
           (init-format-region-tests--run
            (lambda () (set-mark 5) (goto-char 9) (activate-mark))
            nil))))

(provide 'init-format-region-tests)
;;; init-format-region-tests.el ends here
