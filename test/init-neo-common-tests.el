;;; init-neo-common-tests.el --- Tests for the pieces taken from neo-emacs common -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `+files/copy-file-line', `+macos/copy-file-to-clipboard' and the keyfreq
;; module.  Run with the full configuration loaded (scripts/test-all).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defmacro init-neo-common-tests--in-file (contents &rest body)
  "Run BODY in a buffer visiting a temporary file holding CONTENTS."
  (declare (indent 1))
  `(let ((file (make-temp-file "neo-common-" nil ".txt" ,contents)))
     (unwind-protect
         (with-current-buffer (find-file-noselect file)
           (unwind-protect (progn ,@body)
             (set-buffer-modified-p nil)
             (kill-buffer)))
       (delete-file file))))

(ert-deftest files-copy-file-line-copies-name-and-line ()
  (init-neo-common-tests--in-file "a\nb\nc\n"
    (goto-char (point-min))
    (forward-line 1)
    (deactivate-mark)
    (+files/copy-file-line)
    (should (equal (current-kill 0)
                   (format "%s:2" (file-name-nondirectory buffer-file-name))))))

(ert-deftest files-copy-file-line-copies-the-region-lines ()
  (init-neo-common-tests--in-file "a\nb\nc\nd\n"
    (let ((transient-mark-mode t))
      (goto-char (point-min))
      (forward-line 1)
      (push-mark (point) t t)
      (forward-line 2)                  ; region ends at the start of line 4
      (+files/copy-file-line)
      (should (equal (current-kill 0)
                     (format "%s:2,3" (file-name-nondirectory buffer-file-name)))))))

(ert-deftest macos-copy-file-to-clipboard-hands-osascript-a-client-path ()
  (skip-unless (fboundp '+macos/copy-file-to-clipboard))
  (init-neo-common-tests--in-file "x"
    (let (args)
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest a) (setq args a) 0)))
        (+macos/copy-file-to-clipboard))
      (should (equal (car args) "osascript"))
      (should (string-match-p (regexp-quote (file-truename buffer-file-name))
                              (file-truename (car (last args)))))
      (should (string-prefix-p "set the clipboard to (POSIX file " (car (last args)))))))

(ert-deftest macos-copy-file-to-clipboard-refuses-files-of-other-targets ()
  (skip-unless (fboundp '+macos/copy-file-to-clipboard))
  (init-neo-common-tests--in-file "x"
    (cl-letf (((symbol-function 'remote-client-file-name) (lambda (&rest _) nil))
              ((symbol-function 'call-process)
               (lambda (&rest _) (error "Must not run"))))
      (should-error (+macos/copy-file-to-clipboard) :type 'user-error))))

(ert-deftest keyfreq-starts-counting-on-first-input ()
  (should (memq '+keyfreq-enable-h aaron-first-input-hook))
  (should (fboundp 'keyfreq-show)))

(provide 'init-neo-common-tests)
;;; init-neo-common-tests.el ends here
