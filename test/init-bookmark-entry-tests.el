;;; init-bookmark-entry-tests.el --- Native bookmark entry and line lookup -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bookmark)
(require 'ui/windows/config)

(ert-deftest bookmark-actions-menu-searches-and-delegates-ordinary-actions ()
  (should (member "bookmark-actions-menu"
                  (all-completions "bookmark-actions-menu" obarray #'commandp)))
  (dolist (entry '(("j" . consult-bookmark)
                   ("o" . bookmark-jump-other-window)
                   ("l" . list-bookmarks)
                   ("m" . bookmark-set)
                   ("M" . bookmark-set-no-overwrite)
                   ("r" . bookmark-rename)
                   ("d" . bookmark-delete)))
    (should (eq (plist-get (cdr (transient-get-suffix
                                'bookmark-actions-menu (car entry))) :command)
                (cdr entry))))
  (with-temp-buffer
    (evil-local-mode 1)
    (evil-normal-state)
    (should (eq (key-binding (kbd "C-x r .")) #'bookmark-actions-menu))
    (should (eq (key-binding (kbd "C-x r m")) #'bookmark-set))
    (should (eq (key-binding (kbd "SPC b .")) #'bookmark-actions-menu))
    (should (eq (key-binding (kbd "SPC b m")) #'bookmark-set))
    (should (eq (key-binding (kbd "SPC b M")) #'bookmark-set-no-overwrite))
    (should (eq (key-binding (kbd "SPC b r")) #'bookmark-rename))))

(ert-deftest line-bookmark-lookup-uses-the-open-file-buffer ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/bookmark-line-lookup.md")
    (insert "first\nsecond\nthird\n")
    (goto-char (point-min))
    (forward-line 1)
    (let* ((position (point))
           (record (list (cons 'filename buffer-file-name)
                         (cons 'position position)
                         (cons 'my-kind 'line)))
           (entry (cons "line second" record))
           (bookmark-alist (list entry)))
      (cl-letf (((symbol-function 'bookmark-maybe-load-default-file)
                 (lambda () nil))
                ((symbol-function 'find-file-noselect)
                 (lambda (&rest _) (ert-fail "Line lookup reopened the file"))))
        (should (eq (+bookmark--current-line-bookmark) entry))
        (should (equal (+bookmark--line-bookmarks-in-current-file)
                       (list entry)))))))

(ert-deftest bookmark-save-keeps-native-file-on-client-from-remote-context ()
  (let* ((client-directory default-directory)
         (directory (make-temp-file "bookmark-client-save-" t))
         (file (expand-file-name "bookmarks" directory))
         (bookmark-default-file file)
         (bookmark-alist '(("probe" (filename . "/tmp/probe.md")
                                  (position . 1))))
         (bookmark-alist-modification-count 1)
         (bookmark-bookmarks-timestamp nil)
         (bookmark-watch-bookmark-file nil))
    (unwind-protect
        (let ((default-directory "/fs:bookmark-audit-unregistered:/tmp/"))
          (bookmark-save)
          (should (file-readable-p file))
          (should (= bookmark-alist-modification-count 0))
          (setq bookmark-alist nil
                bookmark-bookmarks-timestamp nil)
          (bookmark-maybe-load-default-file)
          (should (assoc "probe" bookmark-alist)))
      (let ((default-directory client-directory))
        (when-let* ((buffer (get-buffer " *Bookmarks*")))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest bookmark-writer-preserves-explicit-remote-and-relative-placements ()
  (let ((default-directory "/fs:bookmark-audit-unregistered:/tmp/"))
    (dolist (file '("/fs:bookmark-audit-unregistered:/tmp/bookmarks"
                    "bookmarks"))
      (should
       (equal
        (+bookmark-write-file-on-client-a
         (lambda (_file) default-directory) file)
        default-directory)))))

(provide 'init-bookmark-entry-tests)
;;; init-bookmark-entry-tests.el ends here
