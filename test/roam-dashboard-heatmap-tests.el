;;; roam-dashboard-heatmap-tests.el --- Deferred dashboard heatmap -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'noema/roam/config)

(defmacro roam-dashboard-heatmap-tests--with-pending (&rest body)
  "Run BODY after rendering a heatmap whose index is pending as `fill'."
  (declare (indent 0))
  `(with-temp-buffer
     (let (fill)
       (cl-letf (((symbol-function '+noema-roam-root) (lambda () "/roam/"))
                 ((symbol-function '+noema-roam--activity-date-counts)
                  (lambda (&rest _) '(("2026-10-31" . 3))))
                 ((symbol-function '+noema-roam--dashboard-insert-heatmap-counts)
                  (lambda (counts) (insert (format "HEATMAP %d\n" (cdar counts)))))
                 ((symbol-function '+noema-roam--runtime-index-async)
                  (lambda (callback) (setq fill callback))))
         (let ((+noema--ready nil)
               (+noema-roam--records-cache nil)
               (+noema-roam--runtime-index-cache nil))
           (insert "banner\n")
           (+noema-roam-dashboard-insert-heatmap)
           (insert "items\n")
           ,@body)))))

(ert-deftest roam-dashboard-heatmap-fills-in-place ()
  (roam-dashboard-heatmap-tests--with-pending
    (funcall fill)
    (should (equal (buffer-string) "banner\nHEATMAP 3\nitems\n"))))

(ert-deftest roam-dashboard-heatmap-survives-a-redraw-while-pending ()
  "A redraw erases the buffer: the old reply must not land at its top."
  (roam-dashboard-heatmap-tests--with-pending
    (erase-buffer)
    (insert "banner\nnew render\n")
    (funcall fill)
    (should (equal (buffer-string) "banner\nnew render\n"))))

(ert-deftest roam-dashboard-heatmap-ignores-a-killed-buffer ()
  (roam-dashboard-heatmap-tests--with-pending
    (let ((start (copy-marker 1)) (end (copy-marker 1)))
      (set-marker start nil)
      (should-not (+noema-roam--dashboard-fill-heatmap start end 70)))))

(provide 'roam-dashboard-heatmap-tests)
