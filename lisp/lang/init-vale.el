;;; init-elisp.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; asdadasdasd asdas
;;

;;; Code:
(require 'config)

(config-defvar flymake-vale-program nil
  "Vale executable used by flymake-vale."
  :type 'string
  :group 'languages)

(config-defvar flymake-vale-modes nil
  "Major modes where flymake-vale should run."
  :type '(repeat symbol)
  :group 'languages)

(config-defvar flymake-fringe-indicator-position nil
  "Fringe side used for Flymake diagnostic indicators."
  :type 'symbol
  :group 'languages)

;; ---------------------------
;; Vale + Flymake (Spell Check)
;; ---------------------------
(my/package-ensure-vc 'flymake-vale "https://github.com/tpeacock19/flymake-vale.git")

(defun my/flymake-vale-setup ()
  "Enable Flymake and the Vale backend for eligible local buffers."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name))
             (executable-find "vale")
             (require 'flymake-vale nil t))
    (flymake-mode 1)
    (flymake-vale-load)))

;; flymake-vale
(use-package flymake-vale
  :ensure t
  :after flymake
  :hook
  ((text-mode . my/flymake-vale-setup)
   (org-mode . my/flymake-vale-setup)
   (latex-mode . my/flymake-vale-setup)
   (LaTeX-mode . my/flymake-vale-setup)
   (tex-mode . my/flymake-vale-setup)
   (TeX-mode . my/flymake-vale-setup)
   (plain-tex-mode . my/flymake-vale-setup)
   (plain-TeX-mode . my/flymake-vale-setup)))


(provide 'init-vale)

;;; init-elisp.el ends here
