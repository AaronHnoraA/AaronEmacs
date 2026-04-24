;;; org-latex-preview-tests.el --- Unit tests for Org LaTeX preview -*- lexical-binding: t; -*-

;;; Code:

(require 'org-latex-preview-test-harness)

(ert-deftest my/org-latex-preview-clear-range-removes-indexed-and-stray-overlays ()
  (my/org-latex-test-with-org-buffer "\\(x\\)"
    (let ((file-a (expand-file-name "a.svg" default-directory))
          (file-b (expand-file-name "b.svg" default-directory)))
      (my/org-latex-test--write-file file-a "<svg xmlns='http://www.w3.org/2000/svg'/>")
      (my/org-latex-test--write-file file-b "<svg xmlns='http://www.w3.org/2000/svg'/>")
      (my/org-latex--make-preview-overlay 1 6 file-a "svg")
      (org--make-preview-overlay 1 6 file-b "svg")
      (should (= (length (my/org-latex-test--preview-overlays 1 6)) 2))
      (should (= (hash-table-count my/org-latex--overlay-table) 1))
      (let ((removed (my/org-latex--clear-preview-range 1 6)))
        (should (= (length removed) 2))
        (should-not (my/org-latex--lookup-overlay 1 6))
        (should-not (my/org-latex-test--preview-overlays 1 6))
        (should (= (hash-table-count my/org-latex--overlay-table) 0))))))

(ert-deftest my/org-latex-preview-place-preview-reuses-overlay ()
  (my/org-latex-test-with-org-buffer "\\(x\\)"
    (let ((file (expand-file-name "cached.svg" default-directory)))
      (my/org-latex-test--write-file file "<svg xmlns='http://www.w3.org/2000/svg'/>")
      (goto-char (point-max))
      (my/org-latex--place-preview 1 6 "\\(x\\)" file "svg")
      (let ((first (my/org-latex--lookup-overlay 1 6)))
        (should (overlayp first))
        (should (= (length (my/org-latex-test--preview-overlays 1 6)) 1))
        (my/org-latex--place-preview 1 6 "\\(x\\)" file "svg")
        (should (eq first (my/org-latex--lookup-overlay 1 6)))
        (should (= (length (my/org-latex-test--preview-overlays 1 6)) 1))
        (should (equal (overlay-get first 'my/org-latex-file) file))))))

(ert-deftest my/org-latex-preview-enqueue-fragment-deduplicates-waiters ()
  (my/org-latex-test-with-org-buffer "\\(x\\)"
    (let* ((target (expand-file-name "ltximg/shared.svg" default-directory))
           (spec (list :beg 1
                       :end 6
                       :value "\\(x\\)"
                       :render-value "\\(x\\)"
                       :dir default-directory
                       :file target
                       :imagetype "svg"
                       :options '(:scale 1.0)
                       :background nil
                       :processing-type 'xdvisvgm-hires-script)))
      (cl-letf (((symbol-function 'my/org-latex--pump-render-queue) #'ignore))
        (my/org-latex--enqueue-fragment spec)
        (my/org-latex--enqueue-fragment spec))
      (should (= (hash-table-count my/org-latex--pending-renders) 1))
      (let ((job (gethash target my/org-latex--pending-renders)))
        (should job)
        (should (= (length (plist-get job :waiters)) 1))
        (should (= (hash-table-count (plist-get job :waiter-index)) 1))
        (should (equal my/org-latex--render-queue (list job)))
        (should (eq (car my/org-latex--render-queue-tail) job))))))

(ert-deftest my/org-latex-preview-render-sentinel-ignores-stale-generation ()
  (my/org-latex-test-with-org-buffer ""
    (let* ((log-buffer (generate-new-buffer " *org-latex-stale-sentinel*"))
           (process (make-process :name "org-latex-stale-sentinel"
                                  :buffer log-buffer
                                  :command (list shell-file-name shell-command-switch "exit 0")
                                  :noquery t))
           (target (expand-file-name "ltximg/shared.svg" default-directory))
           (old-job (list :file target
                          :image-output-file (expand-file-name "tmp/stale-output.svg"
                                                               default-directory)
                          :generation 1
                          :waiters (list (list :beg (copy-marker (point-min))
                                               :end (copy-marker (point-min))
                                               :value "\\(x\\)"))
                          :post-clean nil)))
      (while (eq (process-status process) 'run)
        (accept-process-output process 0.05))
      (setq-local my/org-latex--render-generation 2)
      (setq-local my/org-latex--render-running 7)
      (setq-local my/org-latex--render-processes nil)
      (puthash target :new-job my/org-latex--pending-renders)
      (process-put process 'my/org-latex-buffer (current-buffer))
      (process-put process 'my/org-latex-job old-job)
      (my/org-latex--render-sentinel process "finished\n")
      (should (= my/org-latex--render-running 7))
      (should (eq (gethash target my/org-latex--pending-renders) :new-job))
      (kill-buffer log-buffer))))

(provide 'org-latex-preview-tests)

;;; org-latex-preview-tests.el ends here
