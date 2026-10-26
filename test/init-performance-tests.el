;;; init-performance-tests.el --- Performance Watch ETAF regression -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'config/performance/config)

(ert-deftest +performance-etaf-sections-and-report-stay-available ()
  "The dedicated page uses ETAF tabs and keeps the full export."
  (let ((source (generate-new-buffer " *performance source*"))
        (+performance-record-directory temporary-file-directory)
        board)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (cl-letf (((symbol-function '+performance--ps-lines)
                     (lambda (&rest _) nil))
                    ((symbol-function '+performance-watch-display-buffer)
                     (lambda (buffer) (switch-to-buffer buffer))))
            (+performance/watch)
            (setq board (current-buffer))
            (should (etaf-runtime-for-buffer board))
            (should (eq +performance--inspected-buffer source))
            (should (string-match-p "Emacs CPU" (buffer-string)))
            (call-interactively (key-binding "2"))
            (should (string-match-p "Emacs Process List" (buffer-string)))
            (+performance/watch-select-tab 'buffers)
            (should (string-match-p "Largest Buffers" (buffer-string)))
            (+performance/copy-page)
            (should (string-match-p "Emacs Process List" (current-kill 0)))))
      (when (buffer-live-p board)
        (kill-buffer board))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(provide 'init-performance-tests)
;;; init-performance-tests.el ends here
