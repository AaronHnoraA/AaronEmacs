;;; init-wgrep-tests.el --- Writable grep saves what it changes -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-wgrep-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'grep)
(require 'wgrep)

(ert-deftest +wgrep-applying-edits-saves-the-files ()
  "Applying a wgrep edit writes the changed file to disk."
  (let* ((dir (file-name-as-directory (make-temp-file "wgrep-" t)))
         (file (expand-file-name "a.txt" dir))
         (default-directory dir)
         grep-buffer)
    (unwind-protect
        (progn
          (write-region "alpha foo\n" nil file nil 'silent)
          (setq grep-buffer (grep (format "grep -nH foo %s" (shell-quote-argument file))))
          (with-timeout (20 (ert-fail "grep did not finish"))
            (while (process-live-p (get-buffer-process grep-buffer))
              (accept-process-output nil 0.1)))
          (with-current-buffer grep-buffer
            (wgrep-change-to-wgrep-mode)
            (goto-char (point-min))
            (re-search-forward "alpha foo")
            (replace-match "alpha bar")
            (wgrep-finish-edit))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "alpha bar\n")))
      (when (buffer-live-p grep-buffer) (kill-buffer grep-buffer))
      (when-let* ((buffer (get-file-buffer file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory dir t))))

;;; init-wgrep-tests.el ends here
