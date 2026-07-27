;;; init-git-core-tests.el --- Git integration regressions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-git-core-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'init-git-core)
(require 'diff-hl-dired)
(require 'vc-dir)

(ert-deftest my/diff-hl-dired-disables-only-its-vc-dir-output-limit ()
  "Large Diff-HL status output must not use VC-Dir's parent-buffer UI."
  (let ((vc-dir-process-output-limit 16)
        (vc-parent-buffer nil)
        observed-limit)
    (with-temp-buffer
      (insert (make-string 128 ?x))
      (my/diff-hl-dired-status-files-with-full-output-a
       (lambda (_backend _dir _files _update-function)
         (setq observed-limit vc-dir-process-output-limit)
         ;; Emacs 32 calls this from Git's async sentinel.  It would try
         ;; `(with-current-buffer nil ...)' when the limit remained active.
         (vc-dir-maybe-narrow-and-show-more-button
          "(reported states may be incorrect)"))
       'Git "/tmp/" nil #'ignore))
    (should-not observed-limit)
    (should (= vc-dir-process-output-limit 16))))

(ert-deftest my/diff-hl-dired-installs-large-status-compatibility-advice ()
  (should
   (advice-member-p
    #'my/diff-hl-dired-status-files-with-full-output-a
    'diff-hl-dired-status-files)))

(provide 'init-git-core-tests)

;;; init-git-core-tests.el ends here
