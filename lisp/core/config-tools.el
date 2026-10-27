;;; config-tools.el --- Compatibility entry points for config UI -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The config registry now uses native Customize for its UI.  This module keeps
;; the old command names stable; the leader route lives in `init-evil'.

;;; Code:

(require 'config)
(require 'config-custom)
(require 'aaron-open)
(require 'transient)

(autoload 'config-manual-panel "aaron-ui-docs" nil t)
(autoload 'aaron-ui-docs-home "aaron-ui-docs" nil t)

(defun config-filter-group ()
  "Open a generated native Customize group selected from the registry."
  (interactive)
  (config-custom-sync)
  (let* ((groups (delete-dups
                  (delq nil (mapcar (lambda (entry) (plist-get entry :group))
                                    (config-list)))))
         (selection (completing-read "Config group: "
                                     (mapcar #'symbol-name groups) nil t)))
    (customize-group (config-custom--group-symbol (intern selection)))))

(defun config-save ()
  "Force-write the config stores to disk."
  (interactive)
  (config--persist)
  (message "config: stores saved"))

(defun config-reset-all ()
  "Drop every stored override and rewrite the config stores."
  (interactive)
  (when (yes-or-no-p "Remove ALL stored config overrides? ")
    (config--clear-overrides)
    (config--persist)
    (config-custom-sync)
    (message "config: all overrides cleared")))

(defun config-open-store ()
  "Open the primary config store on the Emacs client."
  (interactive)
  (aaron-open-client-file config-store-file))

(transient-define-prefix config-dispatch ()
  "Configuration management workflow."
  [["Customize"
    ("c" "open config group" config-board)
    ("g" "open generated group" config-filter-group)]
   ["Documentation"
    ("h" "Emacs and Noema docs" aaron-ui-docs-home)
    ("m" "my Config documentation" config-manual-panel)
    ("i" "official Emacs manual" info-emacs-manual)]
   ["Store"
    ("s" "save stores" config-save)
    ("R" "refresh stores" config-refresh-store-files)
    ("!" "check integrity" config-check)
    ("o" "open primary store" config-open-store)
    ("D" "clear all overrides" config-reset-all)]
   ["Registered files"
    ("f" "edit config file" config-file-open)
    ("L" "reload config file" config-file-reload)]])

(provide 'config-tools)
;;; config-tools.el ends here
