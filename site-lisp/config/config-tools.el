;;; config-tools.el --- Compatibility entry points for config UI -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The config registry now uses native Customize for its UI.  This module keeps
;; the old command names and leader route stable for muscle memory.

;;; Code:

(require 'config)
(require 'config-custom)
(require 'transient)

;;;###autoload
(defun config-board ()
  "Open the native Customize group for the config registry."
  (interactive)
  (config-custom-open))

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
  "Open the primary config store file."
  (interactive)
  (find-file config-store-file))

(transient-define-prefix config-dispatch ()
  "Configuration management workflow."
  [["Customize"
    ("c" "open config group" config-board)
    ("g" "open generated group" config-filter-group)]
   ["Store"
    ("s" "save stores" config-save)
    ("R" "refresh stores" config-refresh-store-files)
    ("!" "check integrity" config-check)
    ("o" "open primary store" config-open-store)
    ("D" "clear all overrides" config-reset-all)]])

(when (fboundp 'my/leader!)
  (my/leader!
    "h c" '(:def config-dispatch :which-key "config")))

(provide 'config-tools)
;;; config-tools.el ends here
