;;; lean-tests.el --- Lean integration tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'flymake)
(require 'eglot)

(load-file
 (expand-file-name "../lisp/lang/lean/init-lean-eglot.el"
                   (file-name-directory (or load-file-name buffer-file-name))))
(load-file
 (expand-file-name "../lisp/init-diagnostics-extra.el"
                   (file-name-directory (or load-file-name buffer-file-name))))

(defconst lean-test--range
  '(:start (:line 0 :character 0) :end (:line 0 :character 1)))

(ert-deftest lean-diagnostics-replace-append-and-filter-silent ()
  (with-temp-buffer
    (let ((first `(:range ,lean-test--range :message "first" :severity 2))
          (silent `(:range ,lean-test--range :message "silent"
                            :severity 3 :isSilent t))
          (last `(:range ,lean-test--range :message "last" :severity 1)))
      (cl-letf (((symbol-function 'lean--schedule-notification-flush) #'ignore))
        (lean--record-diagnostics (vector first silent) 1 nil)
        (should (= (length lean--raw-diagnostics) 2))
        (should (equal (lean--visible-diagnostics) (list first)))
        (lean--record-diagnostics (vector last) 2 t)
        (should (= (length lean--raw-diagnostics) 3))
        (lean--record-diagnostics (vector last) 3 nil)
        (should (equal lean--raw-diagnostics (list last)))))))

(ert-deftest lean-task-tags-create-distinct-markers ()
  (with-temp-buffer
    (insert "theorem x := by\n  trivial\n")
    (cl-letf (((symbol-function 'eglot--lsp-position-to-point)
               (lambda (_pos &optional _marker) (point-min))))
      (lean--update-task-overlays
       (list `(:range ,lean-test--range :leanTags [2] :isSilent t)
             `(:range ,lean-test--range :leanTags [1] :isSilent t)))
      (should (= (length lean--task-overlays) 2))
      (should (seq-every-p
               (lambda (ov) (overlay-get ov 'lean-task-fringe))
               lean--task-overlays)))))

(ert-deftest lean-progress-missing-kind-is-processing-and-covers-range ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (let ((item '(:range (:start (:line 0 :character 0)
                          :end (:line 2 :character 0)))))
      (cl-letf (((symbol-function 'eglot--lsp-position-to-point)
                 (lambda (pos &optional _marker)
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line (plist-get pos :line))
                     (point)))))
        (should (string-match-p "processing" (lean--progress-help item)))
        (lean--update-fringe-overlays (list item))
        (should (= (length lean--fringe-overlays) 3))))))

(ert-deftest lean-flymake-publish-replaces-full-buffer-region ()
  (with-temp-buffer
    (insert "x")
    (let (reported)
      (setq lean--flymake-report-fn
            (lambda (&rest args) (setq reported args)))
      (cl-letf (((symbol-function 'lean--diagnostic-to-flymake)
                 (lambda (_diagnostic _version) 'converted)))
        (lean--publish-flymake-diagnostics
         (list `(:range ,lean-test--range :message "x" :severity 1)) 1)
        (should (equal (car reported) '(converted)))
        (should (eq (plist-get (cdr reported) :force) t))
        (should (equal (plist-get (cdr reported) :region)
                       (cons (point-min) (point-max))))))))

(ert-deftest diagnostics-counts-recognize-eglot-categories ()
  (with-temp-buffer
    (insert "abc")
    (let ((diags
           (list (flymake-make-diagnostic (current-buffer) 1 2 'eglot-error "e")
                 (flymake-make-diagnostic (current-buffer) 1 2 'eglot-warning "w")
                 (flymake-make-diagnostic (current-buffer) 1 2 'eglot-note "n"))))
      (should (equal (my/diagnostics--counts diags)
                     '(:error 1 :warning 1 :note 1))))))

;;; lean-tests.el ends here
