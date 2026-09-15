;;; init-macos-tools.el --- Lazy macOS utility commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Small user-invoked macOS integrations which do not belong on the early
;; platform initialization path.

;;; Code:

(require 'transient)

(defun my/macos--ns-color-to-hex (result)
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

(defun my/macos-pick-color (insert-p)
  "Choose a macOS color and copy its hexadecimal value.
With INSERT-P, insert the value at point as well."
  (interactive "P")
  (unless (and (eq system-type 'darwin) (fboundp 'do-applescript))
    (user-error "The macOS color picker is unavailable in this build"))
  (condition-case _err
      (let* ((result
              (do-applescript
               "set chosenColor to choose color\nreturn (item 1 of chosenColor as text) & \",\" & (item 2 of chosenColor as text) & \",\" & (item 3 of chosenColor as text)"))
             (hex (my/macos--ns-color-to-hex result)))
        (kill-new hex)
        (when insert-p
          (insert hex))
        (message "%s %s" (if insert-p "Inserted" "Copied") hex)
        hex)
    (error
     (user-error "Color selection cancelled"))))

(transient-define-prefix my/macos-dispatch ()
  "macOS utility commands."
  [["macOS"
    ("c" "pick color" my/macos-pick-color)
    ("o" "open at point" my/macos-open-at-point)]])

(provide 'init-macos-tools)
;;; init-macos-tools.el ends here
