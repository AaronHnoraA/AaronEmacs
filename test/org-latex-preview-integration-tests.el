;;; org-latex-preview-integration-tests.el --- Integration tests for Org LaTeX preview -*- lexical-binding: t; -*-

;;; Code:

(require 'org-latex-preview-test-harness)

(ert-deftest my/org-latex-preview-integration-renders-shared-fragments-through-single-job ()
  (skip-unless (and (executable-find "xelatex")
                    (executable-find "dvisvgm")
                    (file-exists-p (expand-file-name "tools/org-xdvisvgm-hires"
                                                     my/org-latex-test-root))))
  (my/org-latex-test-with-org-buffer "\\(x+1\\) and \\(x+1\\)"
    (let ((specs (my/org-latex--collect-fragments (point-min) (point-max))))
      (should (= (length specs) 2))
      (should (equal (plist-get (nth 0 specs) :file)
                     (plist-get (nth 1 specs) :file)))
      (when-let* ((target (plist-get (car specs) :file))
                  ((file-exists-p target)))
        (delete-file target))
      (dolist (spec specs)
        (my/org-latex--enqueue-fragment spec nil t))
      (should (= (hash-table-count my/org-latex--pending-renders) 1))
      (my/org-latex--pump-render-queue)
      (my/org-latex-test--wait-for-renders 60.0)
      (should (= my/org-latex--render-running 0))
      (should (= (hash-table-count my/org-latex--pending-renders) 0))
      (let* ((overlays (my/org-latex-test--preview-overlays (point-min) (point-max)))
             (files (delete-dups
                     (mapcar (lambda (overlay)
                               (overlay-get overlay 'my/org-latex-file))
                             overlays))))
        (should (= (length overlays) 2))
        (should (= (length files) 1))
        (should (string-suffix-p ".svg" (car files)))
        (should (file-exists-p (car files)))
        (with-temp-buffer
          (insert-file-contents (car files))
          (goto-char (point-min))
          (should (search-forward "<svg" nil t)))))))

(provide 'org-latex-preview-integration-tests)

;;; org-latex-preview-integration-tests.el ends here
