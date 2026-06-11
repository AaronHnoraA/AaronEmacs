;;; init-neopyter.el --- Load the aaron-neopyter package -*- lexical-binding: t -*-

;;; Commentary:
;; Thin loader: adds site-lisp/aaron-neopyter to load-path and loads the
;; package.
;;
;; Auto-enable: `my/jupytext-auto-enable-mode' (in init-jupyter-core.el)
;; now calls `aaron-neopyter-mode' for *.ju.* files when this package is
;; loaded, falling back to plain `jupytext-mode' otherwise.
;;
;; `aaron-neopyter-mode' co-enables `jupytext-mode' as a sub-mode so the
;; save-to-disk fallback (jupytext --update) still runs when Neopyter is
;; disconnected.
;;
;; Keybindings beyond the minor-mode default map: init-evil.el.
;; Protocol details: docs/neopyter-protocol-notes.md

;;; Code:

(let ((pkg-dir (expand-file-name "site-lisp/aaron-neopyter"
                                 user-emacs-directory)))
  (when (file-directory-p pkg-dir)
    (add-to-list 'load-path pkg-dir)))

(use-package aaron-neopyter
  :commands (aaron-neopyter-mode
             aaron-neopyter-connect
             aaron-neopyter-disconnect
             aaron-neopyter-status
             aaron-neopyter-health-check)
  :init
  ;; JupyterLab's content API resolves all RPC paths relative to the
  ;; directory `jupyter lab' was launched from.  Must match that directory,
  ;; or fullSync/createNew produce doubled paths like
  ;; /root/Users/hc/.../foo.ipynb.  Auto-detect at runtime with
  ;; M-x aaron-neopyter-detect-jupyter-root if you launch Lab elsewhere.
  (setq aaron-neopyter-jupyter-root
        (expand-file-name "~/Documents/AaronNote")))

(provide 'init-neopyter)
;;; init-neopyter.el ends here
