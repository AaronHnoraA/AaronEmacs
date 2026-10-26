;;; tools/clutch/config.el --- Clutch database client integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Keep the package declaration in the main config, but defer installation and
;; loading until the first time the database client is used. Local connection
;; definitions live in `etc/clutch-config.el`.

;;; Code:

(require 'config)

(require 'aaron-packages)

(defgroup +clutch nil
  "Local integration for the clutch database client."
  :group 'tools)

(defconst +clutch-package-url
  "https://github.com/LuciusChen/clutch.git"
  "Git URL used to install clutch via package-vc.")

(config-defvar +clutch-config-file nil
  "Local clutch configuration file."
  :type 'file
  :group '+clutch)

(defvar +clutch--config-loaded nil
  "Whether `+clutch-config-file` has already been loaded.")

(aaron-package-register-vc 'clutch `(:url ,+clutch-package-url :rev :last-release))

(defun +clutch--load-config ()
  "Load `+clutch-config-file` once when it exists."
  (unless +clutch--config-loaded
    (setq +clutch--config-loaded t)
    (when (file-readable-p +clutch-config-file)
      (load +clutch-config-file nil 'nomessage))))

(defun +clutch--ensure-loaded ()
  "Ensure clutch is installed, loaded, and configured."
  (condition-case err
      (progn
        (aaron-package-ensure-vc 'clutch +clutch-package-url)
        (require 'clutch)
        (+clutch--load-config))
    (error
     (user-error "Failed to load clutch: %s"
                 (error-message-string err)))))

(defun +clutch/query-console ()
  "Install/load clutch on demand, then open a query console."
  (interactive)
  (+clutch--ensure-loaded)
  (call-interactively #'clutch-query-console))

(defun +clutch/switch-console ()
  "Install/load clutch on demand, then switch between query consoles."
  (interactive)
  (+clutch--ensure-loaded)
  (call-interactively #'clutch-switch-console))

(defun +clutch-mode ()
  "Install/load clutch on demand, then enable `clutch-mode`."
  (interactive)
  (+clutch--ensure-loaded)
  (call-interactively #'clutch-mode))

(defun +clutch-debug-mode ()
  "Install/load clutch on demand, then toggle `clutch-debug-mode`."
  (interactive)
  (+clutch--ensure-loaded)
  (call-interactively #'clutch-debug-mode))

(defun +clutch/query-sqlite-file (file)
  "Install/load clutch on demand, then open FILE as a SQLite database."
  (interactive "fSQLite database: ")
  (+clutch--ensure-loaded)
  (clutch-query-sqlite-file (expand-file-name file)))

(defalias 'clutch-query-console #'+clutch/query-console)
(defalias 'clutch-switch-console #'+clutch/switch-console)
(defalias 'clutch-mode #'+clutch-mode)
(defalias 'clutch-debug-mode #'+clutch-debug-mode)

(use-package clutch
  :ensure nil
  :defer t
  :config
  (+clutch--load-config))

(provide 'tools/clutch/config)
;;; tools/clutch/config.el ends here
