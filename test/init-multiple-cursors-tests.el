;;; init-multiple-cursors-tests.el --- multiple-cursors under Evil -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-multiple-cursors-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'evil)
(require 'multiple-cursors)

(defmacro init-multiple-cursors-tests--with-buffer (text &rest body)
  "Run BODY in a selected Evil buffer holding TEXT, point at its start."
  (declare (indent 1))
  `(let ((buffer (generate-new-buffer "*mc-test*"))
         (transient-mark-mode t)
         (mc/always-run-for-all t)
         (mc/list-file (make-temp-file "mc-lists")))
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer buffer)
           (text-mode)
           (insert ,text)
           (goto-char (point-min))
           (evil-local-mode 1)
           (evil-normal-state)
           ,@body)
       (with-current-buffer buffer
         (when (bound-and-true-p multiple-cursors-mode)
           (multiple-cursors-mode -1)))
       (kill-buffer buffer)
       (delete-file mc/list-file))))

(ert-deftest +multiple-cursors-mark-next-uses-the-whole-evil-selection ()
  "viw then gn marks the next copy of the whole word, not of all but its last char."
  (init-multiple-cursors-tests--with-buffer "foo fox foo\n"
    (execute-kbd-macro (kbd "viw"))
    (execute-kbd-macro (kbd "gn"))
    (should multiple-cursors-mode)
    (should (eq evil-state 'emacs))
    (should (equal (buffer-substring (region-beginning) (region-end)) "foo"))
    ;; The new cursor sits on the second "foo", not inside "fox".
    (should (= (mc/num-cursors) 2))
    (should (equal (sort (cons (point)
                               (mapcar #'overlay-start (mc/all-fake-cursors)))
                         #'<)
                   '(4 12)))))

(ert-deftest +multiple-cursors-edit-and-escape-back-to-normal ()
  "Typing edits every cursor; Escape ends the cursors and returns to Normal."
  (init-multiple-cursors-tests--with-buffer "foo fox foo\n"
    (execute-kbd-macro (kbd "viw"))
    (execute-kbd-macro (kbd "gn"))
    (execute-kbd-macro (kbd "DEL XY"))
    (should (equal (buffer-string) "XY fox XY\n"))
    (execute-kbd-macro (kbd "<escape>"))
    (should-not multiple-cursors-mode)
    (should (eq evil-state 'normal))))

(provide 'init-multiple-cursors-tests)
;;; init-multiple-cursors-tests.el ends here
