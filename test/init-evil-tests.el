;;; init-evil-tests.el --- Evil integration tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'init-evil)
(require 'avy)

(ert-deftest evil-avy-goto-char-in-line-reads-and-forwards-a-character ()
  (let (received)
    (cl-letf (((symbol-function 'read-char)
               (lambda (&rest _) ?x))
              ((symbol-function 'avy-goto-char-in-line)
               (lambda (char)
                 (setq received char))))
      (call-interactively #'my/evil-avy-goto-char-in-line))
    (should (eq received ?x))))

(provide 'init-evil-tests)
;;; init-evil-tests.el ends here
