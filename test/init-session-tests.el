;;; init-session-tests.el --- Perspective session boundaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-session)

(ert-deftest workspace-session-save-preserves-old-file-on-serializer-error ()
  "A partial Perspective write must not destroy the previous session."
  (let* ((root (make-temp-file "workspace-session-failure-" t))
         (persp-state-default-file (expand-file-name "session/state.el" root))
         (persp-mode t)
         (target persp-state-default-file))
    (unwind-protect
        (progn
          (make-directory (file-name-directory target) t)
          (with-temp-file target (insert "previous session"))
          (cl-letf (((symbol-function 'persp-state-save)
                     (lambda (file &optional _interactive)
                       (with-temp-file file (insert "partial session"))
                       (error "serializer interrupted"))))
            (should-error (my/session-save)))
          (should (equal (with-temp-buffer
                           (insert-file-contents target)
                           (buffer-string))
                         "previous session"))
          (should (equal (directory-files (file-name-directory target) nil
                                          directory-files-no-dot-files-regexp)
                         '("state.el"))))
      (delete-directory root t))))

(ert-deftest workspace-session-state-stays-local-from-remote-buffer ()
  "Save and load target the client even when invoked from a remote buffer."
  (let* ((root (make-temp-file "workspace-session-local-" t))
         (persp-state-default-file (expand-file-name "session/state.el" root))
         (persp-mode t)
         saved-path loaded-path)
    (unwind-protect
        (with-temp-buffer
          (setq default-directory "/fs:missing-session-target:/work/")
          (cl-letf (((symbol-function 'persp-state-save)
                     (lambda (file &optional _interactive)
                       (setq saved-path file)
                       (with-temp-file file (insert "saved state"))))
                    ((symbol-function 'persp-state-load)
                     (lambda (file)
                       (setq loaded-path file))))
            (my/session-save)
            (my/session-load))
          (should (equal loaded-path persp-state-default-file))
          (should (string-prefix-p root saved-path))
          (should-not (file-remote-p saved-path))
          (should (equal default-directory "/fs:missing-session-target:/work/")))
      (delete-directory root t))))

(ert-deftest workspace-session-missing-load-does-not-create-state-directory ()
  "Load reports a missing state file without changing the filesystem."
  (let* ((root (make-temp-file "workspace-session-missing-" t))
         (persp-state-default-file (expand-file-name "session/state.el" root)))
    (unwind-protect
        (progn
          (should-error (my/session-load) :type 'user-error)
          (should-not (file-exists-p (file-name-directory
                                      persp-state-default-file))))
      (delete-directory root t))))

(provide 'init-session-tests)
;;; init-session-tests.el ends here
