;;; neomacs-compat-tests.el --- Runtime compatibility regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'os/neomacs/config)

(ert-deftest neomacs-c1-native-modifiers-avoid-double-translation ()
  (skip-unless (featurep 'os/neomacs/trial))
  (let ((+neomacs-p t)
        (system-type 'darwin)
        (+neomacs-modifier-remap nil))
    (cl-letf (((symbol-function '+neomacs-native-modifiers-p) (lambda () t)))
      (should-not (+neomacs--effective-modifier-remap)))
    (cl-letf (((symbol-function '+neomacs-native-modifiers-p) (lambda () nil)))
      (should (equal (+neomacs--effective-modifier-remap)
                     +neomacs-default-modifier-remap)))))

(ert-deftest neomacs-c2-font-cache-refresh-preserves-custom-faces ()
  (skip-unless (featurep 'os/neomacs/trial))
  (make-face 'neomacs-compat-test-face)
  (set-face-attribute 'neomacs-compat-test-face nil :height 120)
  (clear-font-cache)
  (set-face-attribute 'neomacs-compat-test-face nil :height 130)
  (should (= (face-attribute 'neomacs-compat-test-face :height) 130)))

(ert-deftest neomacs-c3-reveal-respects-background-frames ()
  (skip-unless (featurep 'os/neomacs/trial))
  (let ((+background--frames '(hidden))
        (window-setup-hook nil))
    (should (+neomacs--background-frame-p 'hidden))
    (should-not (+neomacs--background-frame-p 'startup))
    (push '+background--hide-all-h window-setup-hook)
    (should (+neomacs--background-frame-p 'startup))))

(ert-deftest neomacs-c5-save-place-loads-numeric-page-metadata ()
  "Loading saved PDF pages preserves numbers alongside Dired file names."
  (skip-unless (featurep 'os/neomacs/trial))
  (require 'saveplace)
  (let* ((directory (make-temp-file "neomacs-place-metadata-" t))
         (pdf (expand-file-name "paper.pdf" directory))
         (text (expand-file-name "note.txt" directory))
         (child (expand-file-name "child" directory))
         (save-place-file (expand-file-name "places" directory))
         (save-place-alist nil)
         (save-place-loaded nil)
         (save-place-abbreviate-file-names nil))
    (unwind-protect
        (progn
          (with-temp-file save-place-file
            (prin1 `((,pdf (pdf-page . 15))
                     (,text . 4)
                     (,directory (dired-filename . ,child)))
                   (current-buffer)))
          (save-place-load-alist-from-file)
          (should (equal (cdr (assoc pdf save-place-alist)) '((pdf-page . 15))))
          (should (= (cdr (assoc text save-place-alist)) 4))
          (should (equal (cdr (assoc directory save-place-alist))
                         `((dired-filename . ,child)))))
      (delete-directory directory t))))

(provide 'neomacs-compat-tests)
;;; neomacs-compat-tests.el ends here
