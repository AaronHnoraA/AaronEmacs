;;; remote-backend.el --- Backend loader for logical remote I/O -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Backends are kept below `lisp/remote/backend/' so the logical file-name
;; handler, route resolver, and consumers do not accumulate transport-specific
;; code.  This loader is the only module which adds that implementation
;; directory to `load-path'.

;;; Code:

(require 'remote-core)

(declare-function remote-backend-native-register "remote-backend-native" ())
(declare-function remote-backend-tramp-register "remote-backend-tramp" ())
(declare-function remote-backend-tramp-rpc-register
                  "remote-backend-tramp-rpc" ())

(defconst remote-backend-directory
  (file-name-as-directory
   (expand-file-name
    "backend"
    (file-name-directory (or load-file-name buffer-file-name))))
  "Directory containing built-in Remote backend implementations.")

(add-to-list 'load-path remote-backend-directory)

(require 'remote-backend-core)

(defun remote-backend-register-builtins ()
  "Load and register the native, TRAMP, and tramp-rpc backends."
  (require 'remote-backend-native)
  (require 'remote-backend-tramp)
  (require 'remote-backend-tramp-rpc)
  (remote-backend-native-register)
  (remote-backend-tramp-register)
  (remote-backend-tramp-rpc-register))

(provide 'remote-backend)
;;; remote-backend.el ends here
