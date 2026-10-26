;;; init-minibuffer-orderless-tests.el --- Orderless anchors and editable export -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-minibuffer-orderless-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'consult)
(require 'orderless)
(require 'embark)

(ert-deftest +minibuffer-orderless-end-anchor-skips-consult-tofu ()
  "An orderless component ending in $ matches before Consult's tofu."
  (let ((cands (list (concat "(defun foo" (consult--tofu-encode 12))
                     (concat "foo bar" (consult--tofu-encode 13))
                     "plain foo")))
    (should (equal (mapcar #'substring-no-properties
                           (orderless-filter "foo$" cands))
                   (list (concat "(defun foo" (consult--tofu-encode 12))
                         "plain foo")))
    (should (= (length (orderless-filter "$" cands)) 3))))

(ert-deftest +minibuffer-embark-export-write-picks-the-edit-mode ()
  "Writable export turns on the edit mode that matches the candidates."
  (dolist (case '((consult-grep . wgrep-change-to-wgrep-mode)
                  (file . wdired-change-to-wdired-mode)
                  (consult-location . occur-edit-mode)))
    (let (seen)
      (cl-letf (((symbol-function 'run-hook-with-args-until-success)
                 (lambda (&rest _) (list (car case))))
                ((symbol-function 'embark-export)
                 (lambda () (setq seen embark-after-export-hook))))
        (+minibuffer/embark-export-write)
        (should (memq (cdr case) seen)))))
  (cl-letf (((symbol-function 'run-hook-with-args-until-success)
             (lambda (&rest _) (list 'bookmark))))
    (should-error (+minibuffer/embark-export-write) :type 'user-error)))

;;; init-minibuffer-orderless-tests.el ends here
