;;; init-aaronnote-agenda-source.el --- Agenda source gateway -*- lexical-binding: t; -*-
(require 'remote-source)
(require 'remote-gateway)

(defvar my/noema--agenda-source-handles (make-hash-table :test #'equal))
(declare-function my/noema--agenda-protected-sources "init-aaronnote" (params client))

(defun my/noema--agenda-source-release-client (client)
  (let (leases)
    (maphash (lambda (lease entry) (when (eq client (car entry)) (push lease leases)))
             my/noema--agenda-source-handles)
    (dolist (lease leases)
      (let ((entry (gethash lease my/noema--agenda-source-handles)))
        (remhash lease my/noema--agenda-source-handles)
        (when (cdr entry) (remote-source-close (cdr entry)))))))

(add-hook 'remote-gateway-client-disconnected-hook #'my/noema--agenda-source-release-client)

(defun my/noema--agenda-source-request (params client)
  "Forward PARAMS to a workspace-owned source process, preserving its lease."
  (unless client (error "Agenda source IO requires a connected gateway client"))
  (let* ((lease (alist-get 'lease params))
         (op (alist-get 'op params))
         (entry (gethash lease my/noema--agenda-source-handles)))
    (unless (and (stringp lease) (< (length lease) 160)) (error "Invalid source lease"))
    (when (and entry (not (eq (car entry) client))) (error "Source lease belongs to another client"))
    (cond
     ((equal op "close")
      (remhash lease my/noema--agenda-source-handles)
      (when (cdr entry) (remote-source-close (cdr entry)))
      '((closed . t)))
     ((equal op "open")
      (when entry (error "Source lease already exists"))
      (let* ((root (remote-canonicalize-file-name (alist-get 'root params)))
             (workspace (remote-workspace-open root :connect nil))
             (deferred (remote-gateway-defer 30))
             (entry (cons client nil)))
        (puthash lease entry my/noema--agenda-source-handles)
        (condition-case error-object
            (setcdr entry
                    (remote-source-open
                     workspace root :options (alist-get 'options params)
                     :callback (lambda (_result error-object)
                                 (if error-object
                                     (progn (remhash lease my/noema--agenda-source-handles)
                                            (remote-gateway-resolve deferred `((error . ,error-object))))
                                   (remote-gateway-resolve deferred '((protocol . 1)))))
                     :event (lambda (event)
                              (when (eq entry (gethash lease my/noema--agenda-source-handles))
                                (remote-gateway-notify client "aaronnote.agenda.source-event"
                                                       (cons (cons 'lease lease) event))))))
          (error
           (remhash lease my/noema--agenda-source-handles)
           (remote-gateway-resolve deferred `((error . ((code . "EIO") (message . ,(error-message-string error-object))))))))
        deferred))
     ((not entry) (error "Source lease is inactive"))
     (t
      (let ((source (cdr entry)) (deferred (remote-gateway-defer 30)))
        (if (and (equal op "write")
                 (> (length (alist-get 'files
                                       (my/noema--agenda-protected-sources
                                        `((files . [,(expand-file-name (alist-get 'path params) (remote-source-root source))])) client))) 0))
            (remote-gateway-resolve deferred '((error . ((code . "EBUSY") (message . "Source has unsaved Emacs edits")))))
          (remote-source-request source params
                                 (lambda (result error-object)
                                   (remote-gateway-resolve deferred (if error-object `((error . ,error-object)) result)))))
        deferred)))))

(provide 'init-aaronnote-agenda-source)
