;;; init-undo-tree-history-tests.el --- Saved undo history stays lean -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-undo-tree-history-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'undo-tree)

(ert-deftest aaron-undo-tree-history-drops-text-properties ()
  "Deleted text reaches the saved history without its faces."
  (let* ((dir (make-temp-file "undo-hist" t))
         (file (expand-file-name "a.el" dir))
         (history (expand-file-name "a.el.undo" dir)))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (undo-tree-mode 1)
          (insert (propertize "hello world" 'face 'bold 'secret-prop 42))
          (undo-boundary)
          (delete-region (point-min) (point-max))
          (undo-boundary)
          (undo-list-transfer-to-tree)
          (let ((undo-tree-auto-save-history t))
            (save-buffer)
            (undo-tree-save-history history t))
          (should (file-exists-p history))
          (with-temp-buffer
            (insert-file-contents history)
            (should-not (search-forward "secret-prop" nil t)))
          (set-buffer-modified-p nil)
          (kill-buffer))
      (delete-directory dir t))))

(provide 'init-undo-tree-history-tests)
;;; init-undo-tree-history-tests.el ends here
