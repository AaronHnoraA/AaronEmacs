;;; remote-backend-jupyter.el --- Jupyter Contents filesystem -*- lexical-binding: t; -*-
;;; Commentary:
;; A file-only Remote backend. No local project mirror and no SSH requirement.
;; Noema owns authenticated HTTP; this adapter implements Emacs file semantics.
;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'remote-backend-core)
(require 'remote-fs)
(require 'remote-pipeline)

(declare-function my/noema--api-call-sync "init-aaronnote" (channel args &optional timeout))
(declare-function ls-lisp-insert-directory "ls-lisp" (file switches time-index wildcard full))
(defvar my/noema-jupyter-servers nil)

(defun remote-jupyter-file-name-p (file)
  "Return non-nil when FILE belongs to a Jupyter Contents namespace."
  (and (stringp file) (string-match-p "\\`/fs:jupyter\\.[a-f0-9]+:/" file)))

(defun remote-jupyter-target-id (server-id)
  "Return SERVER-ID's reversible, collision-free Remote target id."
  (concat "jupyter." (mapconcat (lambda (c) (format "%02x" c))
                                (string-to-list (encode-coding-string server-id 'utf-8)) "")))

(defun remote-jupyter--get (key object)
  "Read KEY from JSON OBJECT."
  (if (hash-table-p object) (gethash (symbol-name key) object) (alist-get key object)))

(defun remote-jupyter--server (route)
  "Return the configured server id for ROUTE."
  (or (plist-get (remote-link-config (remote-route-link route)) :server-id)
      (error "Jupyter Contents route has no server id")))

(defun remote-jupyter--path (file route)
  "Resolve logical FILE within ROUTE's Contents namespace."
  (unless (and (remote-fs-file-name-p file)
               (equal (remote-fs-target-id file) (remote-route-target-id route)))
    (signal 'file-error (list "Not a file on this Jupyter server" file)))
  (string-remove-prefix "/" (remote-fs-localname file)))

(defun remote-jupyter--request (route operation file &rest fields)
  "Call the Contents OPERATION on FILE through ROUTE, with extra FIELDS."
  (let* ((body `((serverId . ,(remote-jupyter--server route))
                 (operation . ,operation)
                 (path . ,(remote-jupyter--path file route))
                 ,@fields))
         (reply (my/noema--api-call-sync "aaronnote:api:jupyter-cell:server-file"
                                         (vector body) 60)))
    (unless reply (signal 'file-error (list "Noema host is offline" file)))
    (unless (eq t (remote-jupyter--get 'ok reply))
      (let* ((failure (remote-jupyter--get 'error reply))
             (code (remote-jupyter--get 'code failure)))
        (signal (cond ((equal code "ENOENT") 'file-missing)
                      ((equal code "EEXIST") 'file-already-exists)
                      (t 'file-error))
                (list "Jupyter Contents" (remote-jupyter--get 'message failure) file))))
    reply))

(defun remote-jupyter--stat (route file)
  "Read FILE's metadata, returning nil only for a confirmed missing file."
  (condition-case nil
      (remote-jupyter--get 'model (remote-jupyter--request route "stat" file))
    (file-missing nil)))

(defun remote-jupyter--attributes (model &optional id-format)
  "Return Emacs file attributes for MODEL using ID-FORMAT."
  (when model
    (let* ((directory (equal (remote-jupyter--get 'type model) "directory"))
           (writable (eq t (remote-jupyter--get 'writable model)))
           (modified (date-to-time (remote-jupyter--get 'lastModified model)))
           (created (date-to-time (remote-jupyter--get 'created model)))
           (owner (if (eq id-format 'string) "jupyter" -1)))
      (list directory 1 owner owner modified modified created
            (remote-jupyter--get 'size model)
            (if directory (if writable "drwxr-xr-x" "dr-xr-xr-x")
              (if writable "-rw-r--r--" "-r--r--r--"))
            nil (sxhash-equal (remote-jupyter--get 'path model)) 0))))

(defun remote-jupyter--bytes (route file)
  "Read FILE as bytes."
  (base64-decode-string (remote-jupyter--get 'content (remote-jupyter--request route "read" file))))

(defun remote-jupyter--local-temp (bytes)
  "Write BYTES to a private local temporary file."
  (let ((default-directory temporary-file-directory)
        (file (make-temp-file "jupyter-contents-")))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert bytes)
      (let ((coding-system-for-write 'no-conversion)) (write-region nil nil file nil 'silent)))
    file))

(defun remote-jupyter--directory (route file full match nosort count attributes id-format)
  "List FILE on ROUTE with the standard directory operation arguments."
  (let* ((reply (remote-jupyter--request route "list" file))
         (model (remote-jupyter--get 'model reply))
         (entries (append (remote-jupyter--get 'entries reply) nil))
         (items (append (list (cons "." model)
                              (cons ".." (or (remote-jupyter--stat route
                                                       (file-name-directory (directory-file-name file))) model)))
                        (mapcar (lambda (entry) (cons (remote-jupyter--get 'name entry) entry)) entries))))
    (when match (setq items (seq-filter (lambda (item) (string-match-p match (car item))) items)))
    (unless nosort (setq items (sort items (lambda (a b) (string-lessp (car a) (car b))))))
    (when count (setq items (seq-take items count)))
    (mapcar (lambda (item)
              (let ((name (if full (concat (file-name-as-directory file) (car item)) (car item))))
                (if attributes (cons name (remote-jupyter--attributes (cdr item) id-format)) name)))
            items)))

(defun remote-jupyter-file-operation (operation args route _context)
  "Implement file OPERATION with logical ARGS for ROUTE.
Never fall through to the client's filesystem for unsupported operations."
  (let ((file (car args)))
    (pcase operation
      ((or 'file-name-sans-versions 'byte-compiler-base-file-name)
       ;; These are lexical transforms, with no filesystem access.
       (apply operation (remote-fs-localname file) (cdr args)))
      ('locate-dominating-file
       (tramp-run-real-handler operation args))
      ('temporary-file-directory default-directory)
      ('file-attributes (remote-jupyter--attributes (remote-jupyter--stat route file) (cadr args)))
      ((or 'file-exists-p 'file-readable-p) (and (remote-jupyter--stat route file) t))
      ((or 'file-directory-p 'file-accessible-directory-p)
       (equal (remote-jupyter--get 'type (remote-jupyter--stat route file)) "directory"))
      ('file-regular-p (when-let* ((model (remote-jupyter--stat route file)))
                         (not (equal (remote-jupyter--get 'type model) "directory"))))
      ('file-writable-p
       (eq t (remote-jupyter--get 'writable
                                  (or (remote-jupyter--stat route file)
                                      (remote-jupyter--stat route (file-name-directory file))))))
      ('file-modes (when-let* ((model (remote-jupyter--stat route file)))
                    (let ((writable (eq t (remote-jupyter--get 'writable model))))
                      (if (equal (remote-jupyter--get 'type model) "directory")
                          (if writable #o755 #o555)
                        (if writable #o644 #o444)))))
      ((or 'file-executable-p 'file-symlink-p 'file-locked-p 'file-name-case-insensitive-p
           'file-ownership-preserved-p 'vc-registered 'file-acl 'file-selinux-context
           'file-system-info 'lock-file 'unlock-file 'dired-uncache) nil)
      ('vc-call-backend
       (unless (memq (cadr args) '(registered responsible-p root state working-revision))
         (signal 'file-error (list "Contents has no version-control process" (cadr args)))))
      ('file-truename file)
      ('get-file-buffer (seq-find (lambda (buf) (equal (buffer-local-value 'buffer-file-name buf) file)) (buffer-list)))
      ((or 'file-user-uid 'file-group-gid) -1)
      ('access-file (unless (remote-jupyter--stat route file) (signal 'file-missing (list (cadr args) file))))
      ('file-local-copy (remote-jupyter--local-temp (remote-jupyter--bytes route file)))
      ('insert-file-contents
       (let ((temp (remote-jupyter--local-temp (remote-jupyter--bytes route file))))
         (unwind-protect
             (let ((result (insert-file-contents temp nil (nth 2 args) (nth 3 args) (nth 4 args))))
               (when (cadr args)
                 (setq buffer-file-name file)
                 (set-visited-file-modtime
                  (nth 5 (remote-jupyter--attributes (remote-jupyter--stat route file)))))
               (list file (cadr result)))
           (delete-file temp))))
      ('write-region
       (let* ((destination (nth 2 args))
              (append (nth 3 args))
              (visit (nth 4 args))
              (temp (remote-jupyter--local-temp
                     (if (and append (remote-jupyter--stat route destination))
                         (remote-jupyter--bytes route destination) ""))))
         (unwind-protect
             (progn
               (write-region (nth 0 args) (nth 1 args) temp append 'silent)
               (let ((bytes (with-temp-buffer
                              (set-buffer-multibyte nil) (insert-file-contents-literally temp) (buffer-string))))
                 (remote-jupyter--request route "write" destination
                                          (cons 'content (base64-encode-string bytes t))
                                          (cons 'exclusive (if (nth 6 args) t :json-false))))
               (when (or (eq visit t) (stringp visit))
                 (setq buffer-file-name (if (stringp visit) visit destination))
                 (set-visited-file-modtime)
                 (set-buffer-modified-p nil)))
           (delete-file temp))))
      ('directory-files (remote-jupyter--directory route file (nth 1 args) (nth 2 args)
                                                   (nth 3 args) (nth 4 args) nil nil))
      ('directory-files-and-attributes
       (remote-jupyter--directory route file (nth 1 args) (nth 2 args)
                                  (nth 3 args) (nth 5 args) t (nth 4 args)))
      ((or 'file-name-all-completions 'file-name-completion)
       (let* ((directory (cadr args))
              (entries (remote-jupyter--get 'entries (remote-jupyter--request route "list" directory)))
              (names (mapcar (lambda (entry) (concat (remote-jupyter--get 'name entry)
                                                       (when (equal (remote-jupyter--get 'type entry) "directory") "/")))
                              (append entries nil))))
         (if (eq operation 'file-name-all-completions) (all-completions file names)
           (try-completion file names (nth 2 args)))))
      ('insert-directory
       (require 'ls-lisp)
       (ls-lisp-insert-directory file (string-to-list (replace-regexp-in-string "[ -]" "" (cadr args)))
                                5 (and (nth 2 args) (wildcard-to-regexp (file-name-nondirectory file))) (nth 3 args)))
      ((or 'make-directory 'make-directory-internal)
       (remote-jupyter--request route "mkdir" file (cons 'recursive (if (cadr args) t :json-false))) nil)
      ((or 'delete-file 'delete-directory)
       (remote-jupyter--request route "delete" file
                                (cons 'recursive (if (and (eq operation 'delete-directory) (cadr args)) t :json-false))) nil)
      ('rename-file
       (remote-jupyter--request route "rename" file
                                (cons 'to (remote-jupyter--path (cadr args) route))
                                (cons 'overwrite (if (nth 2 args) t :json-false))) nil)
      ('make-nearby-temp-file
       (let ((path (concat (make-temp-name file) (or (nth 2 args) ""))))
         (if (cadr args) (remote-jupyter--request route "mkdir" path)
           (remote-jupyter--request route "write" path '(exclusive . t)
                                    (cons 'content (base64-encode-string (encode-coding-string (or (nth 3 args) "") 'utf-8) t))))
         path))
      ('copy-file
       (let* ((source file) (destination (cadr args))
              (bytes (if (remote-fs-file-name-p source) (remote-jupyter--bytes route source)
                       (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally source) (buffer-string)))))
         (if (remote-fs-file-name-p destination)
             (remote-jupyter--request route "write" destination
                                      (cons 'content (base64-encode-string bytes t))
                                      (cons 'exclusive (if (nth 2 args) :json-false t)))
           (when (and (file-exists-p destination) (not (nth 2 args)))
             (signal 'file-already-exists (list "Destination exists" destination)))
           (with-temp-buffer (set-buffer-multibyte nil) (insert bytes)
                             (let ((coding-system-for-write 'no-conversion))
                               (write-region nil nil destination nil 'silent))))))
      (_ (signal 'file-error (list "Jupyter Contents does not support this operation" operation file))))))

(defun remote-jupyter-register ()
  "Register the Contents backend and configured server targets without IO."
  (let ((configured
         (mapcar (lambda (entry) (remote-jupyter-target-id (plist-get entry :id)))
                 (seq-remove (lambda (entry) (eq (plist-get entry :kind) 'gateway))
                             my/noema-jupyter-servers)))
        stale)
    (maphash (lambda (id target)
               (when (and (eq (remote-target-source target) 'jupyter-contents)
                          (not (member id configured)))
                 (push id stale)))
             remote-targets)
    (dolist (id stale)
      (dolist (link (remote-links-for-target id))
        (remote-connection-invalidate-link (remote-link-id link))
        (remhash (remote-link-id link) remote-links))
      (remhash id remote-targets)))
  (unless (remote-get-backend "jupyter-contents")
    (remote-register-backend
     "jupyter-contents" :capabilities '(file-read file-write directory metadata)
     :project (lambda (file _link _route) (remote-fs-localname file))
     :file-operation #'remote-jupyter-file-operation
     :expand-localname (lambda (name directory _link _route)
                         (when (string-prefix-p "~" name) (user-error "Contents has no OS home; use /"))
                         (let ((default-directory temporary-file-directory))
                           (expand-file-name name (or directory "/"))))
     :describe (lambda () '(:kind jupyter-contents :file-operation-cost remote :processes nil))))
  (dolist (entry my/noema-jupyter-servers)
    (unless (eq (plist-get entry :kind) 'gateway)
      (let* ((server (plist-get entry :id)) (target (remote-jupyter-target-id server)))
        (unless (remote-get-target target)
          (remote-register-target target :label (format "Jupyter · %s" (or (plist-get entry :name) server))
                                  :workspaces '(((id . "contents") (path . "/")))
                                  :source 'jupyter-contents :transient t))
        (unless (remote-get-pipeline "contents" target)
          (remote-register-pipeline target "contents" '("jupyter-contents")
                                    :config (list :server-id server) :source 'jupyter-contents))))))

(provide 'remote-backend-jupyter)
;;; remote-backend-jupyter.el ends here
