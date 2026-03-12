;;; init-package-utils.el --- Package helper utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'package)

(defgroup my/package-utils nil
  "Helpers for installing and loading packages."
  :group 'convenience)

(defvar my/package-vc-recipes nil
  "Registered VC package recipes declared by this config.
Each element is a cons cell of the form (PACKAGE . SPEC).")

(defun my/package-ensure-installed-list (packages)
  "Ensure every package in PACKAGES is installed."
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(defun my/package-register-vc (package spec)
  "Register PACKAGE with VC SPEC for bootstrap export."
  (setq my/package-vc-recipes (assq-delete-all package my/package-vc-recipes))
  (push (cons package (copy-tree spec)) my/package-vc-recipes)
  (when (boundp 'package-vc-selected-packages)
    (setq package-vc-selected-packages
          (assq-delete-all package package-vc-selected-packages))
    (push (cons package (copy-tree spec)) package-vc-selected-packages))
  (cons package spec))

(defun my/package-vc-recipes-sorted ()
  "Return a stable copy of `my/package-vc-recipes'."
  (sort (copy-tree my/package-vc-recipes)
        (lambda (a b)
          (string< (symbol-name (car a))
                   (symbol-name (car b))))))

(defun my/package-ensure-vc (package url &optional rev)
  "Install PACKAGE from URL via package-vc when it is missing.
REV defaults to `:last-release'."
  (let ((spec `(:url ,url :rev ,(or rev :last-release))))
    (my/package-register-vc package spec)
    (unless (package-installed-p package)
      (require 'package-vc)
      (package-vc-install (cons package spec)))))

(provide 'init-package-utils)
;;; init-package-utils.el ends here
