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

;; An Agent chat is named `*Agent ...*' yet is edited: Escape leaves insert.
(define-derived-mode evil-tests-chat-mode text-mode "Chat")

(ert-deftest evil-escape-returns-declared-editing-modes-to-normal ()
  (let ((+evil-editing-modes nil))
    (set-evil-initial-state! 'evil-tests-chat-mode 'insert)
    (should (memq 'evil-tests-chat-mode +evil-editing-modes))
    (with-temp-buffer
      (rename-buffer "*Agent · test*" t)
      (evil-tests-chat-mode)
      (should (+evil-normal-editing-buffer-p))
      (evil-local-mode 1)
      (evil-insert-state)
      (should (+evil-escape-state-h))
      (should (eq evil-state 'normal)))
    (with-temp-buffer
      (rename-buffer "*Messages-like*" t)
      (text-mode)
      (should-not (+evil-normal-editing-buffer-p)))
    (set-evil-initial-state! 'evil-tests-chat-mode 'emacs)
    (should-not (memq 'evil-tests-chat-mode +evil-editing-modes))))

(provide 'init-evil-tests)
;;; init-evil-tests.el ends here
