;;; package-patches-tests.el --- VC patch lifecycle tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-package-utils)

(ert-deftest my/package-vc-patch-applies-once-and-suspends-for-update ()
  "The updater sees a clean checkout and restores its patch afterwards."
  (let* ((directory (make-temp-file "package-patch-" t))
         (source (expand-file-name "message.txt" directory))
         (patch (expand-file-name "change.patch" directory))
         (my/package-vc-patches `((fixture . ,patch))))
    (unwind-protect
        (progn
          (with-temp-file source (insert "original\n"))
          (with-temp-file patch
            (insert "diff --git a/message.txt b/message.txt\n"
                    "--- a/message.txt\n+++ b/message.txt\n"
                    "@@ -1 +1 @@\n-original\n+patched\n"))
          (should (zerop (call-process "git" nil nil nil "-C" directory "init" "-q")))
          (should (zerop (call-process "git" nil nil nil "-C" directory "add" "message.txt")))
          (should (zerop (call-process "git" nil nil nil "-C" directory
                                       "-c" "user.name=Test" "-c" "user.email=test@example.invalid"
                                       "commit" "-qm" "base")))
          (cl-letf (((symbol-function 'my/package-vc-source-directory)
                     (lambda (_package) directory))
                    ((symbol-function 'package-installed-p)
                     (lambda (package &rest _) (eq package 'fixture))))
            (should (eq (my/package-apply-vc-patch 'fixture) 'applied))
            (should (eq (my/package-apply-vc-patch 'fixture) 'already))
            (my/package-with-vc-patches-suspended
             (lambda ()
               (should (equal (with-temp-buffer
                                (insert-file-contents source) (buffer-string))
                              "original\n"))
               (should (zerop (car (my/package--git-result
                                    directory "diff" "--quiet" "HEAD" "--"))))))
            (should (equal (with-temp-buffer
                             (insert-file-contents source) (buffer-string))
                           "patched\n"))
            (with-temp-file source (insert "conflict\n"))
            (should-error (my/package-apply-vc-patch 'fixture))))
      (delete-directory directory t))))

(provide 'package-patches-tests)
;;; package-patches-tests.el ends here
