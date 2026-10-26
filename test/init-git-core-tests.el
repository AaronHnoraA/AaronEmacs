;;; init-git-core-tests.el --- Git integration regressions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-git-core-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'tools/git/core)
(require 'diff-hl-dired)
(require 'vc-dir)

(ert-deftest +diff-hl-dired-disables-only-its-vc-dir-output-limit ()
  "Large Diff-HL status output must not use VC-Dir's parent-buffer UI."
  (let ((vc-dir-process-output-limit 16)
        (vc-parent-buffer nil)
        observed-limit)
    (with-temp-buffer
      (insert (make-string 128 ?x))
      (+diff-hl-dired-status-files-with-full-output-a
       (lambda (_backend _dir _files _update-function)
         (setq observed-limit vc-dir-process-output-limit)
         ;; Emacs 32 calls this from Git's async sentinel.  It would try
         ;; `(with-current-buffer nil ...)' when the limit remained active.
         (vc-dir-maybe-narrow-and-show-more-button
          "(reported states may be incorrect)"))
       'Git "/tmp/" nil #'ignore))
    (should-not observed-limit)
    (should (= vc-dir-process-output-limit 16))))

(ert-deftest +diff-hl-dired-installs-large-status-compatibility-advice ()
  (should
   (advice-member-p
    #'+diff-hl-dired-status-files-with-full-output-a
    'diff-hl-dired-status-files)))

(ert-deftest +git-process-buffer-links-are-buttons ()
  "A URL printed by git in the Magit process buffer can be followed."
  (require 'magit-process)
  (with-temp-buffer
    (magit-process-mode)
    (should goto-address-mode)))

(ert-deftest +git-vc-diffs-use-the-histogram-algorithm ()
  "VC diffs, which feed the diff-hl gutter, use git's histogram algorithm."
  (require 'vc-git)
  (should (member "--histogram" (ensure-list vc-git-diff-switches))))

(provide 'init-git-core-tests)

(ert-deftest +magit-status-turns-off-long-line-shortcuts ()
  "A large Magit status buffer keeps its section motion (magit#5320)."
  (require 'magit)
  (with-temp-buffer
    (magit-status-mode)
    (should (local-variable-p 'long-line-threshold))
    (should-not long-line-threshold)))

;;; init-git-core-tests.el ends here
