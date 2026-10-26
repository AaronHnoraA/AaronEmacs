;;; init-scratch-tests.el --- Persistent scratch boundaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs/scratch/config)

(ert-deftest scratch-projects-with-the-same-name-keep-separate-state ()
  "A shared display name must not redirect one project's saved scratch."
  (let* ((temporary (make-temp-file "scratch-projects-" t))
         (first-root (expand-file-name "first/shared" temporary))
         (second-root (expand-file-name "second/shared" temporary))
         (+scratch-directory (expand-file-name "state" temporary))
         (+scratch-buffers nil)
         first second)
    (make-directory first-root t)
    (make-directory second-root t)
    (unwind-protect
        (progn
          (setq first (+scratch-buffer nil first-root)
                second (+scratch-buffer nil second-root))
          (should-not (eq first second))
          (should-not (equal (buffer-local-value '+scratch-storage-id first)
                             (buffer-local-value '+scratch-storage-id second)))
          (with-current-buffer first (insert "first project"))
          (with-current-buffer second (insert "second project"))
          (should (memq #'+scratch-save-buffer-h
                        (buffer-local-value 'kill-buffer-hook first)))
          (kill-buffer first)
          (kill-buffer second)
          (should (file-exists-p (+scratch--state-file
                                  (+scratch--storage-id first-root))))
          (setq first (+scratch-buffer nil first-root)
                second (+scratch-buffer nil second-root))
          (should (equal (with-current-buffer first (buffer-string)) "first project"))
          (should (equal (with-current-buffer second (buffer-string)) "second project")))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory temporary t))))

(ert-deftest scratch-state-stays-on-client-with-a-remote-working-directory ()
  "The remembered project directory is data, not the state file's target."
  (let* ((temporary (make-temp-file "scratch-remote-state-" t))
         (+scratch-directory temporary)
         (remote-directory "/fs:missing-scratch-target:/work/"))
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq default-directory remote-directory
                  +scratch-storage-id "remote-project")
            (insert "remote scratch text")
            (+scratch-save-buffer-h))
          (should (file-exists-p (+scratch--state-file "remote-project")))
          (with-temp-buffer
            (setq default-directory remote-directory)
            (+scratch--restore-buffer "remote-project" nil)
            (should (equal (buffer-string) "remote scratch text"))
            (should (equal default-directory remote-directory))))
      (delete-directory temporary t))))

(ert-deftest scratch-invalid-state-does-not-clear-an-open-buffer ()
  "Reject a malformed state before touching an existing scratch draft."
  (let* ((temporary (make-temp-file "scratch-invalid-state-" t))
         (+scratch-directory temporary)
         (state-file (+scratch--state-file "invalid")))
    (unwind-protect
        (progn
          (with-temp-file state-file
            (insert "(:content 42 :point 1 :mode fundamental-mode :directory nil)"))
          (with-temp-buffer
            (insert "unsaved draft")
            (should-error (+scratch--restore-buffer "invalid" nil))
            (should (equal (buffer-string) "unsaved draft"))))
      (delete-directory temporary t))))

(provide 'init-scratch-tests)
;;; init-scratch-tests.el ends here
