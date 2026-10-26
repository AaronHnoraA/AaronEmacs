;;; tools/clutch/config.el --- Clutch database client integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Keep the package declaration in the main config, but defer installation and
;; loading until the first time the database client is used. Local connection
;; definitions live in `etc/clutch-config.el`.

;;; Code:

(require 'config)

(require 'aaron-packages)
(require 'aaron-lib)

(declare-function clutch-redis-mode "clutch-redis" ())
(declare-function sql-mode "sql" ())

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

;; Limits of clutch's connections and results, tunable from config-board.
;; Registered before clutch loads, so its defcustoms keep the stored values.
(dolist (setting '((clutch-connect-timeout-seconds . "Seconds to open a database connection")
                   (clutch-read-idle-timeout-seconds . "Seconds of silence tolerated while reading a result")
                   (clutch-query-timeout-seconds . "Seconds a query may run on the database")
                   (clutch-jdbc-rpc-timeout-seconds . "Seconds for a round trip to the JDBC agent")
                   (clutch-result-max-rows . "Rows shown in a result table")))
  (config-register (car setting) :type 'integer :group '+clutch :doc (cdr setting)))

;; SQL files open in clutch-mode, which is `sql-mode' plus running the SQL
;; against a connection; Redis command files in clutch-redis-mode.  A file
;; visit never installs clutch: without it the plain modes are used.
(defun +clutch-sql-mode ()
  "Use `clutch-mode' for this SQL file when clutch is installed, else `sql-mode'."
  (if (locate-library "clutch")
      (+clutch-mode)
    (sql-mode)))

(defun +clutch-redis-mode ()
  "Use `clutch-redis-mode' when clutch is installed, else `prog-mode'."
  (if (locate-library "clutch")
      (progn (+clutch--ensure-loaded) (clutch-redis-mode))
    (prog-mode)))

;; The file types themselves are mapped in lang/filetypes.

(setq-hook! 'sql-mode-hook indent-tabs-mode nil)

(provide 'tools/clutch/config)
;;; tools/clutch/config.el ends here
