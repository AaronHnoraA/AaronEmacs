;;; init-smerge-tests.el --- Merge-conflict entry and key lifecycle -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs/smerge/config)

(ert-deftest merge-conflict-menu-is-searchable-and-uses-native-smerge ()
  (should (member "merge-conflict-menu"
                  (all-completions "merge-conflict-menu" obarray #'commandp)))
  (dolist (entry '(("n" . smerge-next)
                   ("p" . smerge-prev)
                   ("u" . smerge-keep-upper)
                   ("l" . smerge-keep-lower)
                   ("b" . smerge-keep-base)
                   ("a" . smerge-keep-all)
                   ("e" . smerge-ediff)))
    (should (eq (plist-get (cdr (transient-get-suffix
                                'merge-conflict-menu (car entry))) :command)
                (cdr entry)))))

(ert-deftest smerge-quick-keys-exist-only-in-evil-normal-with-mode-active ()
  (with-temp-buffer
    (insert "<<<<<<< ours\na\n=======\nb\n>>>>>>> theirs\n")
    (+smerge-auto-enable)
    (should smerge-mode)
    (should-not (local-key-binding (kbd "q")))
    (should (eq (key-binding (kbd "C-c ^ n")) #'smerge-next))
    (when (featurep 'evil)
      (evil-local-mode 1)
      (evil-normal-state)
      (should (eq (key-binding (kbd "q")) #'merge-conflict-menu))
      (should (eq (key-binding (kbd "o"))
                  #'+smerge/keep-upper-and-advance))
      (evil-insert-state)
      (should-not (eq (key-binding (kbd "q")) #'merge-conflict-menu))
      (evil-normal-state))
    (smerge-mode -1)
    (should-not smerge-mode)
    (should-not (local-key-binding (kbd "q")))
    (should-not (eq (key-binding (kbd "q")) #'merge-conflict-menu))
    (should-not (eq (key-binding (kbd "C-c ^ n")) #'smerge-next))))

(provide 'init-smerge-tests)
;;; init-smerge-tests.el ends here
