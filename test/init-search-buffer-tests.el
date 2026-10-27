;;; init-search-buffer-tests.el --- Buffer search follows the selection -*- lexical-binding: t -*-

;;; Commentary:
;; `+search/line-forward' (C-s, SPC s s) takes Doom's
;; `+default/search-buffer' behaviour: a selection on one line is the
;; initial input, a selection over several lines limits the search.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'evil)
(require 'consult)

(defun init-search-buffer-tests--run (setup)
  "Run `+search/line-forward' after SETUP; return (INITIAL BEG END MARK)."
  (with-temp-buffer
    (insert "alpha beta.gamma\nsecond line\nthird line\n")
    (transient-mark-mode 1)
    (goto-char (point-min))
    (funcall setup)
    (let (seen)
      (cl-letf (((symbol-function 'consult-line)
                 (lambda (&optional initial _start)
                   (interactive)
                   (setq seen (list initial (point-min) (point-max))))))
        (+search/line-forward))
      (append seen (list (region-active-p))))))

(ert-deftest +search-buffer-without-selection-searches-everything ()
  (pcase-let ((`(,initial ,beg ,end ,_) (init-search-buffer-tests--run #'ignore)))
    (should-not initial)
    (should (= beg 1))
    (should (= end 41))))

(ert-deftest +search-buffer-one-line-selection-is-literal-input ()
  (pcase-let ((`(,initial ,beg ,end ,mark)
               (init-search-buffer-tests--run
                (lambda ()
                  (set-mark (+ (point-min) 6))
                  (goto-char (+ (point-min) 16))
                  (activate-mark)))))
    (should (equal initial "beta\\.gamma"))
    (should (= beg 1))
    (should (= end 41))
    (should-not mark)))

(ert-deftest +search-buffer-escapes-spaces-for-orderless ()
  (pcase-let ((`(,initial . ,_)
               (init-search-buffer-tests--run
                (lambda ()
                  (set-mark (point-min))
                  (goto-char (+ (point-min) 10))
                  (activate-mark)))))
    (should (equal initial "alpha\\ beta"))))

(ert-deftest +search-buffer-multi-line-selection-limits-the-search ()
  (pcase-let ((`(,initial ,beg ,end ,_)
               (init-search-buffer-tests--run
                (lambda ()
                  (forward-line 1)
                  (set-mark (point))
                  (forward-line 2)
                  (activate-mark)))))
    (should-not initial)
    (should (= beg 18))
    (should (= end 41))))

(ert-deftest +search-buffer-evil-visual-word-is-the-input ()
  (pcase-let ((`(,initial . ,_)
               (init-search-buffer-tests--run
                (lambda ()
                  (evil-local-mode 1)
                  (evil-normal-state)
                  (evil-visual-char)
                  (evil-forward-word-end)
                  ;; What the command loop's `post-command-hook' does.
                  (evil-visual-refresh)))))
    (should (equal initial "alpha"))))

(ert-deftest +search-buffer-evil-visual-line-limits-the-search ()
  (pcase-let ((`(,initial ,beg ,end ,_)
               (init-search-buffer-tests--run
                (lambda ()
                  (evil-local-mode 1)
                  (evil-normal-state)
                  (forward-line 1)
                  (evil-visual-line)
                  (evil-visual-refresh)))))
    (should-not initial)
    (should (= beg 18))
    (should (= end 30))))

(provide 'init-search-buffer-tests)
;;; init-search-buffer-tests.el ends here
