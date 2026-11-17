;;; agent-binding-tests.el --- Document collaborator bindings (C65) -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/binding)

(defmacro agent-binding-tests--with-store (&rest body)
  "Run BODY with an empty, private binding store."
  (declare (indent 0))
  `(let* ((store (make-temp-file "agent-bindings" nil ".eld"))
          (+agent-binding-store-file store)
          (+agent-binding--cache nil))
     (delete-file store)
     (unwind-protect (progn ,@body)
       (when (file-exists-p store) (delete-file store)))))

(defmacro agent-binding-tests--session (spec &rest body)
  "Bind SPEC, a list of (VAR NATIVE-ID), to isolated fake agent buffers and run BODY.
NATIVE-ID may be nil, for a session whose provider has not answered yet."
  (declare (indent 1))
  `(let* ((ids (make-hash-table :test #'eq))
          ,@(mapcar (lambda (entry)
                      `(,(car entry) (let ((buffer (generate-new-buffer "*agent binding fixture*")))
                                       (with-current-buffer buffer
                                         (setq-local default-directory "/tmp/"
                                                     +agent-acp-session-root "/tmp/"
                                                     +agent-acp-session-agent "claude"))
                                       (puthash buffer ,(cadr entry) ids)
                                       buffer)))
                    spec))
     (unwind-protect
         (cl-letf (((symbol-function '+agent-acp-agent-buffer-p)
                    (lambda (buffer) (and (buffer-live-p buffer)
                                          (not (eq (gethash buffer ids 'absent) 'absent)))))
                   ((symbol-function '+agent-binding--native-id)
                    (lambda (buffer) (gethash buffer ids))))
           (cl-flet ((set-native-id (buffer id) (puthash buffer id ids)))
             ,@body))
       ,@(mapcar (lambda (entry) `(kill-buffer ,(car entry))) spec))))

(ert-deftest agent-binding-set-records-canonical-file-and-persists ()
  (agent-binding-tests--with-store
    (agent-binding-tests--session ((buffer "native-one"))
      (let ((file (+agent-binding-canonical "/tmp/x/SAI.md")))
        (should (stringp file))
        (should (equal (+agent-binding-set buffer "/tmp/x/SAI.md") file))
        (should (equal (+agent-binding-file buffer) file))
        (let ((+agent-binding--cache nil))
          (should (equal (plist-get (+agent-binding-lookup "native-one") :file) file))
          (should (equal (plist-get (+agent-binding-lookup "native-one") :root) "/tmp/")))))))

(ert-deftest agent-binding-set-nil-moves-to-project-level ()
  (agent-binding-tests--with-store
    (agent-binding-tests--session ((buffer "native-one"))
      (+agent-binding-set buffer "/tmp/x/SAI.md")
      (should-not (+agent-binding-set buffer nil))
      (should-not (+agent-binding-file buffer))
      (should-not (plist-get (+agent-binding-lookup "native-one") :file)))))

(ert-deftest agent-binding-set-without-native-id-defers-store ()
  (agent-binding-tests--with-store
    (agent-binding-tests--session ((buffer nil))
      (+agent-binding-set buffer "/tmp/x/SAI.md")
      (should (+agent-binding-file buffer))
      (should-not (file-exists-p +agent-binding-store-file))
      (set-native-id buffer "native-late")
      (+agent-binding--restore-h buffer)
      (should (equal (plist-get (+agent-binding-lookup "native-late") :file)
                     (+agent-binding-canonical "/tmp/x/SAI.md"))))))

(ert-deftest agent-binding-restore-on-resume-brings-back-file-and-name ()
  (agent-binding-tests--with-store
    (agent-binding-tests--session ((old "native-one") (resumed nil))
      (with-current-buffer old (setq-local +agent-acp-session-name "claude·SAI"))
      (+agent-binding-set old "/tmp/x/SAI.md")
      (kill-buffer old)
      (with-current-buffer resumed (setq-local +agent-acp-session-name "manual-claude"))
      (+agent-binding--restore-h resumed)
      (should-not (+agent-binding-file resumed))
      (set-native-id resumed "native-one")
      (+agent-binding--restore-h resumed)
      (should (equal (+agent-binding-file resumed) (+agent-binding-canonical "/tmp/x/SAI.md")))
      (should (equal (buffer-local-value '+agent-acp-session-name resumed) "claude·SAI")))))

(ert-deftest agent-binding-restore-keeps-an-explicit-binding ()
  (agent-binding-tests--with-store
    (agent-binding-tests--session ((buffer "native-one"))
      (+agent-binding-set buffer "/tmp/x/SAI.md")
      (with-current-buffer buffer (setq-local +agent-acp-session-file nil))
      (+agent-binding-set buffer "/tmp/x/other.md")
      (+agent-binding--restore-h buffer)
      (should (equal (+agent-binding-file buffer) (+agent-binding-canonical "/tmp/x/other.md"))))))

(ert-deftest agent-binding-sessions-lists-collaborators-of-file-newest-first ()
  (let* ((file (+agent-binding-canonical "/tmp/x/SAI.md"))
         (rows (list (list :buffer 'a :file file :last-used 1)
                     (list :buffer 'b :file (+agent-binding-canonical "/tmp/x/other.md") :last-used 9)
                     (list :buffer 'c :file file :last-used 5)
                     (list :buffer 'd :file file :last-used 7 :background t))))
    (cl-letf (((symbol-function '+agent-pool-sessions) (lambda (&rest _) rows)))
      (should (equal (mapcar (lambda (row) (plist-get row :buffer))
                             (+agent-binding-sessions "/tmp/x/SAI.md"))
                     '(c a))))))

(ert-deftest agent-binding-touched-excludes-primary-file ()
  (agent-binding-tests--with-store
    (agent-binding-tests--session ((buffer "native-one"))
      (+agent-binding-set buffer "/tmp/x/SAI.md")
      (with-current-buffer buffer
        (setq-local +agent-acp--changes
                    (list (cons "/tmp/x/notes/a.md" '(:count 1 :at 2))
                          (cons "/tmp/x/SAI.md" '(:count 3 :at 1)))))
      (should (equal (+agent-binding-touched buffer)
                     (list (+agent-binding-canonical "/tmp/x/notes/a.md")))))))

(ert-deftest agent-binding-renamed-primary-keeps-old-path ()
  (agent-binding-tests--with-store
    (agent-binding-tests--session ((buffer "native-one"))
      (+agent-binding-set buffer "/tmp/x/SAI.md")
      (cl-letf (((symbol-function '+agent-pool-sessions)
                 (lambda (&rest _) (list (list :buffer buffer :file (+agent-binding-file buffer))))))
        (should (+agent-binding-sessions "/tmp/x/SAI.md"))
        (should-not (+agent-binding-sessions "/tmp/x/SAI-renamed.md"))))))

(ert-deftest agent-binding-store-is-bounded ()
  (agent-binding-tests--with-store
    (let ((+agent-binding-store-limit 3))
      (agent-binding-tests--session ((buffer "n0"))
        (dotimes (index 5)
          (set-native-id buffer (format "n%d" index))
          (+agent-binding-set buffer (format "/tmp/x/f%d.md" index)))
        (let ((+agent-binding--cache nil))
          (should-not (+agent-binding-lookup "n0"))
          (should-not (+agent-binding-lookup "n1"))
          (should (+agent-binding-lookup "n4"))
          (should (+agent-binding-lookup "n2")))))))

(ert-deftest agent-binding-unreadable-store-reads-as-empty ()
  (agent-binding-tests--with-store
    (with-temp-file +agent-binding-store-file (insert "((unbalanced"))
    (should-not (+agent-binding-lookup "native-one"))))

(provide 'agent-binding-tests)
;;; agent-binding-tests.el ends here
