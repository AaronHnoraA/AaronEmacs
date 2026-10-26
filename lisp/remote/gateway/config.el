;;; remote/gateway/config.el --- Unified Emacs external gateway -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Keep the listener outside `remote-framework.el' so isolated framework tests
;; do not require the optional HTTP/WebSocket package.

;;; Code:

(require 'remote-gateway)

(unless noninteractive
  (remote-gateway-start))

(provide 'remote/gateway/config)
;;; remote/gateway/config.el ends here
