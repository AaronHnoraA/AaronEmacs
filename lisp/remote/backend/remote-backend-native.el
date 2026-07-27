;;; remote-backend-native.el --- Native target backend -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'network-stream)
(require 'remote-backend-core)
(require 'remote-fs)

(defun remote-backend-native-project (file-name _link _route)
  "Project logical FILE-NAME onto the native file system."
  (let ((target (remote-fs-target-id file-name)))
    (unless (equal target "local")
      (error "Native backend cannot access target %s" target))
    (remote-fs-localname file-name)))

(defun remote-backend-native-expand-localname
    (name directory _link _route)
  "Resolve NAME against native DIRECTORY on the local target."
  (let ((inhibit-file-name-handlers
         (cons #'tramp-file-name-handler inhibit-file-name-handlers))
        (inhibit-file-name-operation 'expand-file-name))
    (expand-file-name name (or directory "/"))))

(defun remote-backend-native-connect (route _context)
  "Return the native root represented by ROUTE."
  (remote-backend-native-project
   (remote-make-file-name (remote-route-target-id route) "/")
   (remote-route-link route) route))

(defun remote-backend-native-live-p
    (_connection _route _context)
  "Return non-nil because the native backend has no remote session."
  t)

(defun remote-backend-native-network (_route _context arguments)
  "Call `make-network-process' with native ARGUMENTS."
  (apply #'make-network-process arguments))

(defun remote-backend-native-stream
    (_route _context name buffer host service parameters)
  "Call `open-network-stream' with native arguments."
  (apply #'open-network-stream
         name buffer host service parameters))

(defun remote-backend-native-register ()
  "Register the built-in native backend."
  (remote-register-backend
   "native"
   :capabilities remote-native-capabilities
   :project #'remote-backend-native-project
   :expand-localname #'remote-backend-native-expand-localname
   :connect #'remote-backend-native-connect
   :live #'remote-backend-native-live-p
   :make-network-process #'remote-backend-native-network
   :open-network-stream #'remote-backend-native-stream
   :program-form 'search
   :describe
   (lambda ()
     '(:kind native :session-owner emacs))))

(provide 'remote-backend-native)
;;; remote-backend-native.el ends here
