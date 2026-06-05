;;; note-tests.el --- Note workflow test entrypoint -*- lexical-binding: t; -*-

;;; Code:

(load-file
 (expand-file-name "md-roam-tests.el"
                   (file-name-directory (or load-file-name buffer-file-name))))
(load-file
 (expand-file-name "note-code-tests.el"
                   (file-name-directory (or load-file-name buffer-file-name))))

;;; note-tests.el ends here
