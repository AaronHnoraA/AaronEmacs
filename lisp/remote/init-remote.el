;;; init-remote.el --- Unified logical file-system entry point -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Load before project and language-server modules.  Existing TRAMP tuning is
;; retained as a compatibility layer while all new callers use the target/link
;; registries and `/fs:' canonical file names.

;;; Code:

(require 'init-tramp)
(require 'remote-framework)
(require 'remote-config)
(require 'remote-board)

(define-minor-mode remote-mode
  "Route canonical `/fs:' files through registered target links."
  :global t
  :group 'remote
  (if remote-mode
      (remote-fs-install)
    (remote-fs-uninstall)))

(remote-mode 1)
(remote-config-load)

(provide 'init-remote)
;;; init-remote.el ends here
