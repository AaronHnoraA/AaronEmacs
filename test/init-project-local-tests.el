;;; init-project-local-tests.el --- Project-local setting tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tools/project-local/config)
(require 'emacs/dir-locals/config)

(ert-deftest local-trust-c1-default-roots-are-trusted ()
  "Noema documents and the Emacs configuration are trusted by default."
  (let ((safe-local-variable-directories nil)
        (trusted-content nil))
    (aaron-trust-default-local-variable-directories)
    (dolist (root (list (expand-file-name "~/Documents/Noema/")
                        (file-truename user-emacs-directory)))
      (let ((root (file-name-as-directory root)))
        (should (member root safe-local-variable-directories))
        (should (member (abbreviate-file-name root) trusted-content))))))

(ert-deftest local-trust-c1-trusted-directory-enables-local-variables ()
  "Trusted content gets the full local-variable policy."
  (let* ((root (make-temp-file "local-trust-root-" t))
         (trusted-content (list (abbreviate-file-name
                                 (file-name-as-directory root))))
         (enable-local-variables t)
         (enable-local-eval 'maybe)
         observed)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "note.md" root)
                buffer-file-truename buffer-file-name)
          (aaron-hack-local-variables-trust-policy-a
           (lambda ()
             (setq observed (list enable-local-variables enable-local-eval))))
          (should (equal observed '(:all t)))
          (let ((enable-local-variables nil)
                (enable-local-eval nil))
            (aaron-hack-local-variables-trust-policy-a
             (lambda ()
               (setq observed (list enable-local-variables enable-local-eval))))
            (should (equal observed '(nil nil)))))
      (delete-directory root))))

(ert-deftest local-trust-c2-directory-approval-is-reused ()
  "An unknown value prompts once, then an approved source directory applies."
  (let* ((root (file-name-as-directory (make-temp-file "local-trust-dir-" t)))
         (trusted-content nil)
         (safe-local-variable-directories nil)
         (enable-local-variables t)
         (enable-local-eval 'maybe)
         (variable 'local-trust-tests--unsafe-value)
         (value '(:program "unreviewed-command"))
         (asked 0))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory root)
          (cl-letf (((symbol-function 'hack-local-variables-confirm)
                     (lambda (&rest _)
                       (cl-incf asked)
                       t)))
            (aaron-hack-local-variables-trust-policy-a
             (lambda ()
               (hack-local-variables-filter (list (cons variable value)) root)))
            (should (= asked 1))
            (should (equal (cdr (assq variable file-local-variables-alist)) value))
            (setq file-local-variables-alist nil
                  safe-local-variable-directories (list root))
            (aaron-hack-local-variables-trust-policy-a
             (lambda ()
               (hack-local-variables-filter (list (cons variable value)) root)))
            (should (= asked 1))
            (should (equal (cdr (assq variable file-local-variables-alist)) value))))
      (delete-directory root))))

(ert-deftest +project-local-toolchain-defaults-are-safe-but-commands-are-not ()
  (should
   (+project-local-settings-safe-p
    '(:toolchain ((python . sage))
      :noema-jupyter (:language sage :kernel sagemath :session default))))
  (should-not
   (+project-local-settings-safe-p
    '(:toolchain-profiles
      ((local . (:server-program ("custom-language-server")))))))
  (should-not (+project-local-settings-safe-p '(:toolchain))))

(ert-deftest +project-local-dir-locals-opens-on-its-own-machine ()
  (let ((default-directory "/fs:box:/work/")
        visited context)
    (cl-letf (((symbol-function '+project-local-root)
               (lambda () "/Users/test/project/"))
              ((symbol-function 'find-file)
               (lambda (file &rest _)
                 (setq visited file context default-directory))))
      (+project-local/open-dir-locals))
    (should (equal visited "/Users/test/project/.dir-locals.el"))
    (should-not (file-remote-p context))))

(ert-deftest +dir-locals-edit-uses-local-project-root-from-remote-buffer ()
  (let ((default-directory "/fs:box:/work/")
        visited context)
    (cl-letf (((symbol-function '+project-local-root)
               (lambda () "/Users/test/project/"))
              ((symbol-function 'find-file)
               (lambda (file &rest _)
                 (setq visited file context default-directory))))
      (+dir-locals/edit))
    (should (equal visited "/Users/test/project/.dir-locals.el"))
    (should-not (file-remote-p context))))

(provide 'init-project-local-tests)
;;; init-project-local-tests.el ends here
