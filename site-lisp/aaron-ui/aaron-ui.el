;;; aaron-ui.el --- Local vendored UI theme entrypoint -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Keep the Kanagawa base theme in-repo so bootstrapping a fresh Emacs does not
;; need to fetch it from ELPA.  This file is the single local entrypoint for
;; loading the vendored theme files under `site-lisp/aaron-ui/vendor/`.

;;; Code:

(defconst aaron-ui--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of the local `aaron-ui' package.")

(defconst aaron-ui-theme-directory
  (expand-file-name "vendor/kanagawa-themes" aaron-ui--root)
  "Directory containing vendored Kanagawa theme files.")

(add-to-list 'load-path aaron-ui-theme-directory)
(add-to-list 'custom-theme-load-path aaron-ui-theme-directory)

(require 'kanagawa-themes)

(defcustom aaron-ui-default-variant 'wave
  "Default Kanagawa variant loaded by `aaron-ui-load-theme'."
  :type '(choice (const :tag "Wave" wave)
                 (const :tag "Dragon" dragon)
                 (const :tag "Lotus" lotus))
  :group 'faces)

(defun aaron-ui-load-theme (&optional variant)
  "Load local Kanagawa VARIANT from `site-lisp/aaron-ui'."
  (interactive)
  (let ((theme (intern (format "kanagawa-%s" (or variant aaron-ui-default-variant)))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(provide 'aaron-ui)

;;; aaron-ui.el ends here
