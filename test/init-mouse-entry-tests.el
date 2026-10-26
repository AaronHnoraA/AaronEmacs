;;; init-mouse-entry-tests.el --- Mouse entry regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-mouse-entry-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'config/mouse/config)

(ert-deftest mouse-actions-menu-is-searchable-and-routed ()
  (should (commandp 'mouse-actions-menu))
  (should (member "mouse-actions-menu"
                  (all-completions "mouse-actions-menu" obarray #'commandp)))
  (should (eq (lookup-key global-map (kbd "H-C-M-m")) 'mouse-actions-menu))
  (should (eq (plist-get (cdr (transient-get-suffix
                              'mouse-actions-menu "b")) :command)
              'ibuffer)))

(ert-deftest mouse-git-mode-line-clicks-do-not-share-a-timeout ()
  (let ((calls 0))
    (cl-letf (((symbol-function '+mouse--select-event-window)
               (lambda (&rest _) (selected-window)))
              ((symbol-function '+mouse--git-menu-items)
               (lambda () nil))
              ((symbol-function 'easy-menu-create-menu)
               (lambda (&rest _) 'git-popup))
              ((symbol-function 'popup-menu)
               (lambda (menu &rest _)
                 (should (eq menu 'git-popup))
                 (cl-incf calls))))
      (+mouse/git-mode-line-menu nil)
      (+mouse/git-mode-line-menu nil))
    (should (= calls 2))
    (should (eq (lookup-key +mouse-git-mode-line-map
                            [mode-line down-mouse-1])
                '+mouse/git-mode-line-menu))
    (should (eq (lookup-key +mouse-git-mode-line-map
                            [mode-line mouse-1])
                'ignore))
    (should (eq (lookup-key +mouse-git-mode-line-map
                            [mode-line down-mouse-3])
                '+mouse/git-mode-line-menu))
    (should (eq (lookup-key +mouse-git-mode-line-map
                            [mode-line mouse-3])
                'ignore))))

(provide 'init-mouse-entry-tests)
;;; init-mouse-entry-tests.el ends here
