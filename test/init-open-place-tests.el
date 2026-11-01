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

(ert-deftest c37-open-other-never-replaces-the-originating-side-pane ()
  (init-open-place-tests--with-file file
    (let* ((main (selected-window))
           (source (generate-new-buffer "*Agent side fixture*"))
           (side (display-buffer-in-side-window source '((side . right)))))
      (unwind-protect
          (progn
            (select-window side)
            (aaron-open-place file 'other nil nil t)
            (should (eq (selected-window) side))
            (should (eq (window-buffer side) source))
            (should (eq (window-buffer main) (find-buffer-visiting file))))
        (kill-buffer source)))))

(ert-deftest open-place-client-reports-errors-as-json ()
  (let* ((reply (aaron-open-place-from-client
                 (base64-encode-string "/tmp/x") "sideways" nil nil t))
         (json (json-parse-string (base64-decode-string reply)
                                  :object-type 'alist)))
    (should (string-match-p "Unknown placement" (alist-get 'error json)))))

(ert-deftest c37-agent-open-preserves-chat-and-returns-the-actual-window ()
  (require 'noema/bridge/config)
  (init-open-place-tests--with-file file
    (let ((chat (generate-new-buffer "*Agent open fixture*"))
          (origin (selected-window)))
      (unwind-protect
          (progn
            (with-current-buffer chat (insert "Draft stays here"))
            (set-window-buffer origin chat)
            (let* ((params `((file . ,file) (placement . "other") (line . 2) (col . 4) (noFocus . t)))
                   (reply (+noema--api-open-file params nil))
                   (target (get-buffer-window (find-buffer-visiting file))))
              (should (alist-get 'ok reply))
              (should (eq (window-buffer origin) chat))
              (should (eq (selected-window) origin))
              (should-not (eq target origin))
              (should (equal (alist-get 'window reply) (format "%s" target)))
              (with-current-buffer (window-buffer target)
                (goto-char (window-point target))
                (should (= (line-number-at-pos) 2))
                (should (= (current-column) 4)))
              (should (eq (alist-get 'reused (+noema--api-open-file params nil)) t))
              (should (= (length (window-list)) 2))
              (should (equal (with-current-buffer chat (buffer-string)) "Draft stays here"))))
        (with-current-buffer chat (set-buffer-modified-p nil))
        (kill-buffer chat)))))

;;; init-open-place-tests.el ends here
