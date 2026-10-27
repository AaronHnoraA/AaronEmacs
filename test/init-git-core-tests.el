;;; init-git-core-tests.el --- Git integration regressions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-git-core-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'tools/git/core)
(require 'tools/git/board)
(require 'diff-hl-dired)
(require 'vc-dir)

;; Emacs 32 defines this; Emacs 31 still needs the dynamic binding in the test.
(defvar vc-dir-process-output-limit)

(ert-deftest +diff-hl-dired-disables-only-its-vc-dir-output-limit ()
  "Large Diff-HL status output must not use VC-Dir's parent-buffer UI."
  (let (observed-limit)
    (with-temp-buffer
      (let ((vc-dir-process-output-limit 16)
            (vc-parent-buffer nil))
        (insert (make-string 128 ?x))
        (+diff-hl-dired-status-files-with-full-output-a
         (lambda (_backend _dir _files _update-function)
           (setq observed-limit vc-dir-process-output-limit)
           ;; Emacs 32 calls this from Git's async sentinel.  It would try
           ;; `(with-current-buffer nil ...)' when the limit remained active.
           (when (fboundp 'vc-dir-maybe-narrow-and-show-more-button)
             (vc-dir-maybe-narrow-and-show-more-button
              "(reported states may be incorrect)")))
         'Git "/tmp/" nil #'ignore)
        (should-not observed-limit)
        (should (= vc-dir-process-output-limit 16))))))

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

(ert-deftest +git-board-uses-nul-delimited-raw-paths ()
  "Git board row IDs remain usable for names Git quotes in text status."
  (let* ((repo (make-temp-file "git-board-paths-" t))
         (old "旧 文件.txt")
         (renamed "新 文件.txt")
         (spaced " 空 格.txt")
         (arrow "name -> target.txt")
         (newline "含\n换行.txt"))
    (unwind-protect
        (let ((default-directory repo))
          (should (zerop (process-file "git" nil nil nil "init" "-q")))
          (with-temp-file old (insert "rename me\n"))
          (should (zerop (process-file "git" nil nil nil "add" "--" old)))
          (should (zerop (process-file
                          "git" nil nil nil
                          "-c" "user.name=Test" "-c" "user.email=test@example.invalid"
                          "commit" "-qm" "base")))
          (should (zerop (process-file "git" nil nil nil "mv" "--" old renamed)))
          (with-temp-file spaced (insert "space\n"))
          (with-temp-file arrow (insert "arrow\n"))
          (with-temp-file newline (insert "newline\n"))
          (with-temp-buffer
            (let* ((entries (+git-board--entries repo))
                   (ids (mapcar #'car entries)))
              (should (equal (sort ids #'string<)
                             (sort (list renamed spaced arrow newline) #'string<)))
              (should (equal (substring-no-properties
                              (aref (cadr (assoc renamed entries)) 2))
                             (format "%s -> %s" old renamed)))
              (should (equal (substring-no-properties
                              (aref (cadr (assoc newline entries)) 2))
                             "含\\n换行.txt"))
              (should (stringp +git-board-branch-summary))
              (dolist (entry entries)
                (should (file-exists-p (expand-file-name (car entry) repo))))))
          (with-temp-buffer
            (+git-board-mode)
            (setq-local +git-board-repo-root repo)
            (+git-board/refresh)
            (goto-char (point-min))
            (search-forward "含\\n换行.txt")
            (should (equal (tabulated-list-get-id) newline)))
          (with-temp-buffer
            (should (equal (sort (mapcar #'car
                                         (+git-board--entries
                                          (concat "/fs:local:" repo)))
                                 #'string<)
                           (sort (list renamed spaced arrow newline)
                                 #'string<)))))
      (delete-directory repo t))))

(provide 'init-git-core-tests)

(ert-deftest +magit-status-turns-off-long-line-shortcuts ()
  "A large Magit status buffer keeps its section motion (magit#5320)."
  (require 'magit)
  (with-temp-buffer
    (magit-status-mode)
    (should (local-variable-p 'long-line-threshold))
    (should-not long-line-threshold)))

;;; init-git-core-tests.el ends here
