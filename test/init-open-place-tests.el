;;; init-open-place-tests.el --- Placed visits for bin/emacsopen -*- lexical-binding: t; -*-

(require 'ert)
(require 'aaron-open)

(defmacro init-open-place-tests--with-file (var &rest body)
  "Bind VAR to a fresh three-line file, run BODY in one window, clean up."
  (declare (indent 1))
  `(let ((,var (make-temp-file "open-place" nil ".txt" "one\ntwo words\nthree\n")))
     (save-window-excursion
       (delete-other-windows)
       (unwind-protect (progn ,@body)
         (when-let* ((buffer (find-buffer-visiting ,var)))
           (kill-buffer buffer))
         (delete-file ,var)))))

(ert-deftest open-place-splits-on-the-requested-side ()
  (dolist (case '((left . left) (right . right) (above . above) (below . below)))
    (init-open-place-tests--with-file file
      (let* ((origin (selected-window))
             (result (aaron-open-place file (car case)))
             (window (selected-window)))
        (should (equal (alist-get 'placement result) (symbol-name (car case))))
        (should-not (eq window origin))
        (should (eq (window-buffer window) (find-buffer-visiting file)))
        (should (eq (window-in-direction (pcase (cdr case)
                                           ('left 'right) ('right 'left)
                                           ('above 'below) ('below 'above))
                                         window)
                    origin))))))

(ert-deftest open-place-reuses-a-window-that-shows-the-file ()
  (init-open-place-tests--with-file file
    (aaron-open-place file 'right)
    (let ((shown (selected-window)))
      (select-window (window-in-direction 'left shown))
      (let ((result (aaron-open-place file 'here)))
        (should (eq (alist-get 'reused result) t))
        (should (eq (selected-window) shown))
        (should (= (length (window-list)) 2))))))

(ert-deftest open-place-positions-and-keeps-focus-on-request ()
  (init-open-place-tests--with-file file
    (let* ((origin (selected-window))
           (result (aaron-open-place file 'below 2 5 t))
           (window (get-buffer-window (find-buffer-visiting file))))
      (should (eq (selected-window) origin))
      (should (eq (alist-get 'positioned result) t))
      (with-current-buffer (window-buffer window)
        (goto-char (window-point window))
        (should (= (line-number-at-pos) 2))
        (should (= (current-column) 4))))))

(ert-deftest open-place-other-splits-only-when-alone ()
  (init-open-place-tests--with-file file
    (aaron-open-place file 'other)
    (should (= (length (window-list)) 2))
    (kill-buffer (find-buffer-visiting file))
    (aaron-open-place file 'other)
    (should (= (length (window-list)) 2))))

(ert-deftest open-place-client-reports-errors-as-json ()
  (let* ((reply (aaron-open-place-from-client
                 (base64-encode-string "/tmp/x") "sideways" nil nil t))
         (json (json-parse-string (base64-decode-string reply)
                                  :object-type 'alist)))
    (should (string-match-p "Unknown placement" (alist-get 'error json)))))

;;; init-open-place-tests.el ends here
