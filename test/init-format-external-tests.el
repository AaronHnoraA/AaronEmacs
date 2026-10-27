;;; init-format-external-tests.el --- Target-side external formatting -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'remote-core)
(require 'remote-fs)

(defun +format-test--run (change-after-start &optional formatter)
  "Run FORMATTER and optionally CHANGE-AFTER-START in its buffer."
  (let ((file (make-temp-file "format-external-" nil ".sh")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (remote-make-file-name "local" file)
                default-directory
                (remote-make-file-name "local" (file-name-directory file)))
          (sh-mode)
          (insert "abc\n")
          (goto-char 2)
          (let* ((+format-external-rules
                  (list (cons 'sh-mode
                              (or formatter '("tr" "a-z" "A-Z")))))
                 (process
                  (cl-letf (((symbol-function '+current-language-server-backend)
                             (lambda () nil)))
                    (+format/buffer-dwim)))
                 (route (process-get process 'remote-route)))
            (should (equal (remote-route-target-id route) "local"))
            (pcase change-after-start
              ('edit (goto-char (point-max)) (insert "!"))
              ('rename (setq buffer-file-name "/fs:local:/tmp/renamed.sh")))
            (with-timeout (5 (ert-fail "External formatter did not finish"))
              (while +format--external-process
                (accept-process-output nil 0.05)))
            (list (buffer-string) (point))))
      (delete-file file))))

(ert-deftest +format-external-applies-target-process-output ()
  (should (equal (+format-test--run nil) '("ABC\n" 2))))

(ert-deftest +format-external-keeps-later-user-edits ()
  (should (equal (car (+format-test--run 'edit)) "abc\n!")))

(ert-deftest +format-external-keeps-renamed-buffer ()
  (should (equal (car (+format-test--run 'rename)) "abc\n")))

(ert-deftest +format-external-keeps-buffer-on-cli-error ()
  (should (equal (car (+format-test--run
                       nil '("sh" "-c" "printf bad >&2; exit 7")))
                 "abc\n")))

(ert-deftest +format-external-keeps-buffer-on-empty-cli-output ()
  (should (equal (car (+format-test--run
                       nil '("sh" "-c" "cat >/dev/null")))
                 "abc\n")))

(ert-deftest +format-external-new-request-cancels-old-process ()
  (let ((file (make-temp-file "format-external-cancel-" nil ".sh")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (remote-make-file-name "local" file)
                default-directory
                (remote-make-file-name "local" (file-name-directory file)))
          (sh-mode)
          (insert "abc\n")
          (let* ((+format-external-rules
                  '((sh-mode . ("sh" "-c" "sleep 3; tr a-z A-Z"))))
                 (old (+format/external-buffer)))
            (setq +format-external-rules
                  '((sh-mode . ("tr" "a-z" "A-Z"))))
            (+format/external-buffer)
            (should-not (process-live-p old))
            (with-timeout (5 (ert-fail "Replacement formatter did not finish"))
              (while +format--external-process
                (accept-process-output nil 0.05)))
            (should (equal (buffer-string) "ABC\n"))
            (should-not (buffer-live-p (process-buffer old)))))
      (delete-file file))))

(ert-deftest +format-external-passes-target-native-file-name ()
  (with-temp-buffer
    (setq buffer-file-name "/fs:lab:/workspace/app.ts"
          major-mode 'typescript-ts-mode)
    (cl-letf (((symbol-function 'remote-executable-find)
               (lambda (_program &optional _context) "/usr/bin/prettier")))
      (should (equal (+format--external-command)
                     '("/usr/bin/prettier" "--stdin-filepath"
                       "/workspace/app.ts"))))))

(ert-deftest +format-external-prefers-project-prettier-without-lsp ()
  (let* ((project (make-temp-file "format-prettier-project-" t))
         (bin (expand-file-name "node_modules/.bin" project))
         (prettier (expand-file-name "prettier" bin)))
    (unwind-protect
        (progn
          (make-directory bin t)
          (with-temp-file prettier
            (insert "#!/bin/sh\ntr a-z A-Z\n"))
          (set-file-modes prettier #o755)
          (with-temp-buffer
            (setq buffer-file-name
                  (remote-make-file-name
                   "local" (expand-file-name "app.ts" project))
                  default-directory
                  (remote-make-file-name
                   "local" (file-name-as-directory project))
                  major-mode 'typescript-ts-mode)
            (cl-letf (((symbol-function 'remote-executable-find)
                       (lambda (&rest _)
                         (ert-fail "Project-local Prettier was ignored"))))
              (should (equal (+format--external-command)
                             (list prettier "--stdin-filepath"
                                   (expand-file-name "app.ts" project))))
              (insert "abc\n")
              (+format/external-buffer)
              (with-timeout (5 (ert-fail "Project formatter did not finish"))
                (while +format--external-process
                  (accept-process-output nil 0.05)))
              (should (equal (buffer-string) "ABC\n")))))
      (delete-directory project t))))

(provide 'init-format-external-tests)
;;; init-format-external-tests.el ends here
