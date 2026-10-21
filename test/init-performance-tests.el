;;; init-performance-tests.el --- Performance Watch ETAF regression -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'init-performance)

(ert-deftest my/performance-etaf-sections-and-report-stay-available ()
  "The dedicated page uses ETAF tabs and keeps the full export."
  (let ((source (generate-new-buffer " *performance source*"))
        (my/performance-record-directory temporary-file-directory)
        board)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (cl-letf (((symbol-function 'my/performance--ps-lines)
                     (lambda (&rest _) nil))
                    ((symbol-function 'my/performance-watch-display-buffer)
                     (lambda (buffer) (switch-to-buffer buffer))))
            (my/performance-watch)
            (setq board (current-buffer))
            (should (etaf-runtime-for-buffer board))
            (should (eq my/performance--inspected-buffer source))
            (should (string-match-p "Emacs CPU" (buffer-string)))
            (call-interactively (key-binding "2"))
            (should (string-match-p "Emacs Process List" (buffer-string)))
            (my/performance-watch-select-tab 'buffers)
            (should (string-match-p "Largest Buffers" (buffer-string)))
            (my/performance-copy-page)
            (should (string-match-p "Emacs Process List" (current-kill 0)))))
      (when (buffer-live-p board)
        (kill-buffer board))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(provide 'init-performance-tests)
;;; init-performance-tests.el ends here
