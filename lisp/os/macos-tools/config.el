;;; os/macos-tools/config.el --- Lazy macOS utility commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Small user-invoked macOS integrations which do not belong on the early
;; platform initialization path.

;;; Code:

(require 'transient)

(declare-function macos-open-at-point "os/macos/config" ())

(defun +macos--ns-color-to-hex (result)
  "Convert AppleScript choose-color RESULT into #RRGGBB."
  (let ((components
         (mapcar (lambda (value)
                   (max 0 (min 255 (ash (string-to-number value) -8))))
                 (split-string
                  (replace-regexp-in-string "[{}\"]" "" result)
                  "[[:space:]]*,[[:space:]]*" t))))
    (unless (= (length components) 3)
      (error "Unexpected macOS color result: %S" result))
    (apply #'format "#%02x%02x%02x" components)))

(defun macos-pick-color (insert-p)
  "Choose a macOS color and copy its hexadecimal value.
With INSERT-P, insert the value at point as well."
  (interactive "P")
  (unless (and (eq system-type 'darwin) (fboundp 'do-applescript))
    (user-error "The macOS color picker is unavailable in this build"))
  (let* ((result
          (condition-case err
              (do-applescript
               "set chosenColor to choose color\nreturn (item 1 of chosenColor as text) & \",\" & (item 2 of chosenColor as text) & \",\" & (item 3 of chosenColor as text)")
            (error
             (if (string-match-p "\\(?:-128\\|[Cc]ancel\\)"
                                 (error-message-string err))
                 (user-error "Color selection cancelled")
               (signal (car err) (cdr err))))))
         (hex (+macos--ns-color-to-hex result)))
    (kill-new hex)
    (when insert-p
      (insert hex))
    (message "%s %s" (if insert-p "Inserted" "Copied") hex)
    hex))

(declare-function remote-client-file-name "remote-fs" (file-name))
(declare-function +project-current-root "tools/project/config" ())
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))

;; From Doom's os/macos: show a file or the project in Finder.  Only a path
;; this machine can read qualifies; a file on another target is refused.
(defun +macos--reveal (file)
  "Select FILE in a Finder window."
  (let* ((path (or (remote-client-file-name (expand-file-name file))
                   (user-error "Not a file on this machine: %s" file)))
         ;; `open' is a process of this machine, whatever the buffer's target.
         (default-directory temporary-file-directory))
    (unless (eql 0 (call-process "open" nil nil nil "-R" path))
      (user-error "Finder could not show %s" path))
    path))

(defun macos-reveal-in-finder ()
  "Show the current file (or the Dired entry at point) in Finder."
  (interactive)
  (+macos--reveal (or (and (derived-mode-p 'dired-mode)
                           (dired-get-filename nil t))
                      buffer-file-name
                      default-directory)))

(defun macos-reveal-project-in-finder ()
  "Show the current project's root in Finder."
  (interactive)
  (+macos--reveal (directory-file-name
                   (or (and (fboundp '+project-current-root)
                            (+project-current-root))
                       default-directory))))

(transient-define-prefix macos-tools-menu ()
  "macOS utility commands."
  [["macOS"
    ("c" "pick color" macos-pick-color)
    ("o" "open at point" macos-open-at-point)]
   ["Finder"
    ("r" "reveal file" macos-reveal-in-finder)
    ("R" "reveal project" macos-reveal-project-in-finder)]])

(provide 'os/macos-tools/config)
;;; os/macos-tools/config.el ends here
