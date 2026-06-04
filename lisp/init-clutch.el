;;; init-clutch.el --- Clutch database client integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Keep the package declaration in the main config, but defer installation and
;; loading until the first time the database client is used. Local connection
;; definitions live in `etc/clutch-config.el`.

;;; Code:

(require 'init-package-utils)

(defgroup my/clutch nil
  "Local integration for the clutch database client."
  :group 'tools)

(defconst my/clutch-package-url
  "https://github.com/LuciusChen/clutch.git"
  "Git URL used to install clutch via package-vc.")

(defcustom my/clutch-config-file
  (expand-file-name "etc/clutch-config.el" user-emacs-directory)
  "Local clutch configuration file."
  :type 'file
  :group 'my/clutch)

(defvar my/clutch--config-loaded nil
  "Whether `my/clutch-config-file` has already been loaded.")

(my/package-register-vc 'clutch `(:url ,my/clutch-package-url :rev :last-release))

(defun my/clutch--load-config ()
  "Load `my/clutch-config-file` once when it exists."
  (unless my/clutch--config-loaded
    (setq my/clutch--config-loaded t)
    (when (file-readable-p my/clutch-config-file)
      (load my/clutch-config-file nil 'nomessage))))

(defun my/clutch--ensure-loaded ()
  "Ensure clutch is installed, loaded, and configured."
  (condition-case err
      (progn
        (my/package-ensure-vc 'clutch my/clutch-package-url)
        (require 'clutch)
        (my/clutch--load-config))
    (error
     (user-error "Failed to load clutch: %s"
                 (error-message-string err)))))

(defun my/clutch-query-console ()
  "Install/load clutch on demand, then open a query console."
  (interactive)
  (my/clutch--ensure-loaded)
  (call-interactively #'clutch-query-console))

(defun my/clutch-switch-console ()
  "Install/load clutch on demand, then switch between query consoles."
  (interactive)
  (my/clutch--ensure-loaded)
  (call-interactively #'clutch-switch-console))

(defun my/clutch-mode ()
  "Install/load clutch on demand, then enable `clutch-mode`."
  (interactive)
  (my/clutch--ensure-loaded)
  (call-interactively #'clutch-mode))

(defun my/clutch-debug-mode ()
  "Install/load clutch on demand, then toggle `clutch-debug-mode`."
  (interactive)
  (my/clutch--ensure-loaded)
  (call-interactively #'clutch-debug-mode))

(defalias 'clutch-query-console #'my/clutch-query-console)
(defalias 'clutch-switch-console #'my/clutch-switch-console)
(defalias 'clutch-mode #'my/clutch-mode)
(defalias 'clutch-debug-mode #'my/clutch-debug-mode)

(use-package clutch
  :ensure nil
  :defer t
  :config
  (my/clutch--load-config))

(provide 'init-clutch)
;;; init-clutch.el ends here
