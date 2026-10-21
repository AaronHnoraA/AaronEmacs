;;; package-patches.el --- Local patches for package-vc checkouts -*- lexical-binding: t; -*-

;; Paths are relative to `user-emacs-directory'.  Keep package-vc recipes in
;; package-lock.el; only source differences belong here.
(setq my/package-vc-patches
      '((portal . "config/portal-local.patch")))

;;; package-patches.el ends here
