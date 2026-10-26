;;; aaron-registration-tests.el --- Tests for Doom-style set-...! interfaces -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with `make core-test'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "lisp/ui/popup/autoload/popup.el" user-emacs-directory) nil t)
(load (expand-file-name "lisp/editor/evil/autoload/evil.el" user-emacs-directory) nil t)

(defvar shackle-rules)

(ert-deftest aaron-registration-popup-later-rule-takes-precedence ()
  (let ((+popup-rules nil) (shackle-rules nil))
    (set-popup-rule! "a" :select t)
    (set-popup-rule! "b" :align 'below)
    (should (equal (mapcar #'car shackle-rules) '("b" "a")))
    (set-popup-rule! "a" :select nil)
    (should (equal shackle-rules '(("a" :select nil) ("b" :align below))))))

(ert-deftest aaron-registration-popup-rules-keep-block-order ()
  (let ((+popup-rules nil) (shackle-rules nil))
    (set-popup-rule! "old" :select t)
    (set-popup-rules! '(("x" :size 1) ("y" :size 2)) '(("old" :size 3)))
    (should (equal (mapcar #'car shackle-rules) '("x" "y" "old")))
    (should (equal (cdr (assoc "old" shackle-rules)) '(:size 3)))))

(ert-deftest aaron-registration-evil-initial-state-now-or-later ()
  ;; `featurep' ignores a let-bound `features' in Emacs 31, so this goes
  ;; through a real `provide'; the batch running core-test never loads Evil.
  (skip-unless (not (featurep 'evil)))
  (let (calls)
    (cl-letf (((symbol-function 'evil-set-initial-state)
               (lambda (mode state) (push (cons mode state) calls))))
      (set-evil-initial-state! 'm1 'normal)
      (should (null calls))
      (provide 'evil)
      (should (equal calls '((m1 . normal))))
      (setq calls nil)
      (set-evil-initial-state! '(m2 m3) 'emacs)
      (should (equal (reverse calls) '((m2 . emacs) (m3 . emacs)))))))

(provide 'aaron-registration-tests)
;;; aaron-registration-tests.el ends here
