;;; init-server.el --- Emacs server & browser editing -*- lexical-binding: t -*-

;;; Commentary:
;; Enable Emacs server and edit-server for browser integration.
;; Works with browser extensions like:
;;  - Edit with Emacs
;;  - Atomic Chrome
;;  - GhostText

;;; Code:

;; ------------------------------------------------------------
;; Start Emacs server
;; ------------------------------------------------------------

(require 'server)

(unless (server-running-p)
  (server-start))


(provide 'init-server)

;;; init-server.el ends here
