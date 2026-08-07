;;; init-ocaml.el --- ocaml -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function merlin-mode "merlin" (&optional arg))
(declare-function ocp-setup-indent "ocp-indent" ())
(defvar merlin-command)

;; Ocaml mode
(use-package tuareg
  :ensure t
  :mode ("\\.ml\\'" . tuareg-mode)
  :custom
  (tuareg-match-patterns-aligned t)
  (tuareg-indent-align-with-first-arg t))

;; Context sensitive completion
;; Bundled with the aur package `merlin'
(when (locate-library "merlin")
  ;; This library is supplied by some system OCaml installations, not by the
  ;; package lock.  Keep it optional without making byte compilation require
  ;; a machine-local library.
  (autoload 'merlin-mode "merlin" nil t)
  (setq merlin-command "ocamlmerlin")
  (add-hook 'tuareg-mode-hook #'merlin-mode))

;; Indentation tool for OCaml
;; Bundled with the system package `ocaml-ocp-indent'
(when (and (executable-find "ocp-indent")
           (locate-library "ocp-indent"))
  (autoload 'ocp-indent-region "ocp-indent" nil t)
  (autoload 'ocp-indent-buffer "ocp-indent" nil t)
  (autoload 'ocp-setup-indent "ocp-indent" nil t)
  (add-hook 'tuareg-mode-hook #'ocp-setup-indent))

;; The dune build system
;; Bundled with system package `dune'
(when (and (executable-find "dune")
           (locate-library "dune"))
  (autoload 'dune-mode "dune" nil t)
  (add-to-list 'auto-mode-alist '("dune\\(?:\\.inc\\)?\\'" . dune-mode)))

(provide 'init-ocaml)

;;; init-ocaml.el ends here
