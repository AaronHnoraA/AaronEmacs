;;; config/keyfreq/config.el --- Command frequency statistics -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Counts which commands run and through which keys, so decisions about key
;; bindings can rest on actual use.  `M-x keyfreq-show' lists the counts,
;; which no-littering keeps in var/keyfreq.el.  Counting starts with the
;; first input, not at startup.

;;; Code:

(require 'aaron-lib)
(require 'aaron-emacs)

(use-package keyfreq
  :ensure t
  :commands (keyfreq-show keyfreq-mode keyfreq-autosave-mode)
  :init
  (add-hook! 'aaron-first-input-hook
    (defun +keyfreq-enable-h ()
      "Start counting commands and saving the counts."
      (keyfreq-mode 1)
      (keyfreq-autosave-mode 1))))

(provide 'config/keyfreq/config)

;;; config/keyfreq/config.el ends here
