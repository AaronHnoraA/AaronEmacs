;;; agent-edit-tests.el --- Agents edit text files through Emacs (C74) -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/acp)
(require 'tools/agent/edit)

(defmacro agent-edit-tests--file (var text &rest body)
  "Bind VAR to a temporary file holding TEXT and run BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file "agent-edit" nil ".py" ,text)))
     (unwind-protect (progn ,@body)
       (when-let* ((buffer (find-buffer-visiting ,var)))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-file ,var))))

(defun agent-edit-tests--read (file)
  (+agent-edit-file-api `((action . "read") (file . ,file))))

(defun agent-edit-tests--disk (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest agent-edit-c74-an-unvisited-file-is-changed-on-disk ()
  (agent-edit-tests--file file "x = 1\ny = 2\n"
    (let* ((read (agent-edit-tests--read file))
           (reply (+agent-edit-file-api `((action . "apply") (file . ,file) (version . ,(alist-get 'version read))
                                          (edits . [((from . 6) (to . 11) (oldText . "y = 2") (newText . "y = 3"))])))))
      (should (equal (alist-get 'content read) "x = 1\ny = 2\n"))
      (should (eq (alist-get 'ok reply) t))
      (should (equal (agent-edit-tests--disk file) "x = 1\ny = 3\n"))
      (should-not (find-buffer-visiting file)))))

(ert-deftest agent-edit-c74-an-open-buffer-with-unsaved-edits-takes-the-change-unsaved ()
  (agent-edit-tests--file file "x = 1\ny = 2\n"
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer (goto-char (point-min)) (insert "# mine\n"))
      (let* ((read (agent-edit-tests--read file))
             (from (length "# mine\nx = 1\n")))
        (should (string-prefix-p "# mine\n" (alist-get 'content read)))
        (should (eq (alist-get 'ok (+agent-edit-file-api
                                    `((action . "apply") (file . ,file) (version . ,(alist-get 'version read))
                                      (edits . ,(vector `((from . ,from) (to . ,(+ from 5))
                                                          (oldText . "y = 2") (newText . "y = 3")))))))
                    t))
        (should (equal (with-current-buffer buffer (buffer-string)) "# mine\nx = 1\ny = 3\n"))
        (should (buffer-modified-p buffer))
        ;; The person's unsaved edit is not saved for them.
        (should (equal (agent-edit-tests--disk file) "x = 1\ny = 2\n"))))))

(ert-deftest agent-edit-c74-a-clean-open-buffer-is-saved ()
  (agent-edit-tests--file file "x = 1\ny = 2\n"
    (let* ((buffer (find-file-noselect file))
           (read (agent-edit-tests--read file)))
      (+agent-edit-file-api `((action . "apply") (file . ,file) (version . ,(alist-get 'version read))
                              (edits . [((from . 0) (to . 5) (oldText . "x = 1") (newText . "x = 0"))])))
      (should-not (buffer-modified-p buffer))
      (should (equal (agent-edit-tests--disk file) "x = 0\ny = 2\n")))))

(ert-deftest agent-edit-c74-a-changed-text-is-a-conflict-not-a-write ()
  (agent-edit-tests--file file "x = 1\ny = 2\n"
    (let ((read (agent-edit-tests--read file)))
      (with-temp-file file (insert "x = 1\ny = 9\n"))
      (should (eq (alist-get 'conflict (+agent-edit-file-api
                                        `((action . "apply") (file . ,file) (version . ,(alist-get 'version read))
                                          (edits . [((from . 6) (to . 11) (oldText . "y = 2") (newText . "y = 3"))]))))
                  t))
      (should (equal (agent-edit-tests--disk file) "x = 1\ny = 9\n"))
      (let ((fresh (agent-edit-tests--read file)))
        (should (eq (alist-get 'conflict (+agent-edit-file-api
                                          `((action . "apply") (file . ,file) (version . ,(alist-get 'version fresh))
                                            (edits . [((from . 6) (to . 11) (oldText . "y = 2") (newText . "y = 3"))]))))
                    t))))))

(ert-deftest agent-edit-c74-a-session-that-needs-the-person-says-so ()
  (let ((buffer (generate-new-buffer "*agent edit fixture*")) noted told)
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-session-buffer-for-token) (lambda (token) (and (equal token "tok") buffer)))
                  ((symbol-function '+agent-acp--note-attention) (lambda (b reason) (setq noted (list b reason))))
                  ((symbol-function 'aaron-notify) (lambda (title &rest args) (setq told (cons title args)))))
          (with-current-buffer buffer (setq-local +agent-acp-session-name "claude·SAI"))
          (+agent-edit-needs-person '((session . "tok") (file . "/n/SAI.md") (reason . "This edit rewrites most of SAI.md.")))
          (should (equal noted (list buffer 'input)))
          (should (string-match-p "claude·SAI" (car told)))
          (should (equal (plist-get (cdr told) :body) "This edit rewrites most of SAI.md.")))
      (kill-buffer buffer))))

(provide 'agent-edit-tests)
;;; agent-edit-tests.el ends here
