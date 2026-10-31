;;; os/macos/autoload/drop.el --- Declaring what a file drop means -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `set-file-drop-handler!' lets a module say what a file dropped on its major
;; mode means.  See docs/design/macos-boundary.md.

;;; Code:

;;;###autoload
(defvar +macos-file-drop-handlers nil
  "Alist of (MAJOR-MODE . FUNCTION) for files dropped on a buffer.")

;;;###autoload
(defun set-file-drop-handler! (modes function)
  "Call FUNCTION for files dropped on a buffer in one of MODES.
MODES is a major mode or a list of them; derived modes count.  FUNCTION runs
in the buffer that was dropped on and gets a list of absolute native paths."
  (dolist (mode (ensure-list modes))
    (setf (alist-get mode +macos-file-drop-handlers) function)))

(provide 'os/macos/autoload/drop)
;;; os/macos/autoload/drop.el ends here
