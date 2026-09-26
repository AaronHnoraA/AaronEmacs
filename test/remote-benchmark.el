;;; remote-benchmark.el --- Repeatable local file-handler cost probe -*- lexical-binding: t; -*-

;; Run from the repository root:
;; emacs --batch -Q --eval '(setq user-emacs-directory (file-name-as-directory default-directory))' \
;;   -L lisp -L lisp/remote -L lisp/remote/backend -l test/remote-benchmark.el

(require 'remote-framework)
(require 'remote-fs)
(remote-fs-install)

(defun remote-benchmark--seconds (function file count)
  "Return elapsed seconds for COUNT calls of FUNCTION on FILE."
  (funcall function file)
  (garbage-collect)
  (let ((started (float-time)))
    (dotimes (_ count)
      (funcall function file))
    (- (float-time) started)))

(let* ((count 3000)
       (file (make-temp-file "remote-benchmark-"))
       (logical (remote-make-file-name "local" file)))
  (unwind-protect
      (dolist (operation '(file-exists-p file-attributes file-truename))
        (let* ((native (remote-benchmark--seconds operation file count))
               (remote (remote-benchmark--seconds operation logical count)))
          (princ (format "%s: native %.2f us/op; /fs:local %.2f us/op; %.1fx\n"
                         operation (* 1e6 (/ native count))
                         (* 1e6 (/ remote count)) (/ remote native)))))
    (delete-file file)))

;;; remote-benchmark.el ends here
