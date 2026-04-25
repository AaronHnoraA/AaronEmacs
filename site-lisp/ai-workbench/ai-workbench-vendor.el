;;; ai-workbench-vendor.el --- Vendored package helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module centralizes load-path handling for packages vendored under
;; ai-workbench.

;;; Code:

(require 'cl-lib)

(defconst ai-workbench--root-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of the ai-workbench package.")

(defconst ai-workbench--vendor-directory
  (expand-file-name "vendor/" ai-workbench--root-directory)
  "Directory holding vendored upstream packages.")

(defconst ai-workbench--vendored-packages
  '(claude-code-ide codex-cli)
  "Packages currently vendored under ai-workbench.")

(defun ai-workbench-vendor-package-directory (package)
  "Return the vendored directory for PACKAGE."
  (expand-file-name (format "%s/" package) ai-workbench--vendor-directory))

(defun ai-workbench-vendor-package-present-p (package)
  "Return non-nil if PACKAGE exists in the vendored package tree."
  (file-directory-p (ai-workbench-vendor-package-directory package)))

(defun ai-workbench-add-vendor-to-load-path (package)
  "Add vendored PACKAGE to `load-path' when it exists.
Return non-nil when the directory was added or already present."
  (let ((dir (ai-workbench-vendor-package-directory package)))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)
      t)))

(defun ai-workbench-setup-vendor-load-paths ()
  "Add all known vendored package directories to `load-path'."
  (cl-loop for package in ai-workbench--vendored-packages
           do (ai-workbench-add-vendor-to-load-path package)))

(provide 'ai-workbench-vendor)
;;; ai-workbench-vendor.el ends here
