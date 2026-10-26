;;; init-evil-tests.el --- Evil integration tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'editor/evil/config)
(require 'avy)

(ert-deftest evil-avy-goto-char-in-line-reads-and-forwards-a-character ()
  (let (received)
    (cl-letf (((symbol-function 'read-char)
               (lambda (&rest _) ?x))
              ((symbol-function 'avy-goto-char-in-line)
               (lambda (char)
                 (setq received char))))
      (call-interactively #'+evil/avy-goto-char-in-line))
    (should (eq received ?x))))

(ert-deftest evil-escape-has-a-global-non-evil-fallback ()
  (should (eq (lookup-key (current-global-map) (kbd "<escape>"))
              #'aaron/escape))
  (with-temp-buffer
    (evil-local-mode -1)
    (should (eq (key-binding (kbd "<escape>")) #'aaron/escape))))

(ert-deftest evil-escape-global-fallback-preserves-local-behavior ()
  (with-temp-buffer
    (evil-local-mode -1)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<escape>") #'ignore)
      (use-local-map map)
      (should (eq (key-binding (kbd "<escape>")) #'ignore)))))

(ert-deftest evil-escape-handles-state-and-highlight-in-one-pass ()
  (let (state-changed highlight-cleared)
    (cl-letf (((symbol-function '+evil-normal-state-maybe)
               (lambda () (setq state-changed t)))
              ((symbol-function '+evil-clear-ex-highlights-h)
               (lambda () (setq highlight-cleared t))))
      (should (+evil-escape-state-h)))
    (should state-changed)
    (should highlight-cleared)))

(provide 'init-evil-tests)
;;; init-evil-tests.el ends here
