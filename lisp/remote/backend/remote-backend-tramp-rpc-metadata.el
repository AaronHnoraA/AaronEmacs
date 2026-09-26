;;; remote-backend-tramp-rpc-metadata.el --- Round-trip-bounded RPC file metadata -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs asks for file metadata one predicate at a time, and TRAMP forces a
;; fresh stat wherever an answer guards data (modtime checks).  Over tramp-rpc
;; every such question used to be its own sequential round trip:
;;
;;   visit  stat, truename, lstat, read, stat, VC marker search      6 RTT
;;   save   stat x4, truename, stat, write, stat                     8 RTT
;;
;; VS Code Remote answers the same requests with stat + read and stat + write.
;; This adapter bounds the RPC backend the same way without changing Emacs's
;; visit or save semantics:
;;
;; - A file operation scope surrounds `find-file-noselect' and
;;   `basic-save-buffer'.  Its first metadata miss sends stat, lstat and
;;   truename for that file as one `batch' request and seeds tramp-rpc's own
;;   caches.
;; - Inside the scope, a forced-fresh stat of that file (TRAMP binds
;;   `remote-file-name-inhibit-cache' to t) reuses the scope's stat until the
;;   file is written.  The stat is fresh as of this command, and a write
;;   invalidates it, so the post-write modtime stat is still a real round trip.
;; - A visit's modtime comes from the scope's stat, taken strictly before the
;;   read: a concurrent writer can only produce a false "changed on disk", it
;;   can never hide newer disk content.  The server runs batch entries
;;   concurrently, which is why reads and writes are never folded into a batch.
;; - `locate-dominating-file' answers are reused under tramp-rpc's metadata TTL
;;   and dropped by any invalidation that can create or remove a marker.
;;
;; Everything here is installed only for the verified tramp-rpc release and
;; private call shapes; see `remote-backend-tramp-rpc-install'.

;;; Code:

(require 'cl-lib)

(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-file-name-localname "tramp" (vec))
(declare-function tramp-make-tramp-file-name "tramp" (vec &optional localname))
(declare-function tramp-file-local-name "tramp" (name))
(declare-function tramp-tramp-file-p "tramp" (name))
(declare-function tramp-rpc--call-batch "tramp-rpc" (vec requests))
(declare-function tramp-rpc-file-name-p "tramp-rpc" (filename))
(declare-function tramp-rpc--cache-file-stat-result "tramp-rpc-magit"
                  (vec localname stat &optional lstat))
(declare-function tramp-rpc--cache-put "tramp-rpc-magit" (cache key value))
(declare-function tramp-rpc--cache-entry-valid-p "tramp-rpc-magit"
                  (timestamp))
(declare-function tramp-rpc--convert-file-attributes "tramp-rpc"
                  (stat id-format))
(declare-function tramp-rpc--decode-string "tramp-rpc" (value))
(declare-function tramp-rpc--encode-path "tramp-rpc" (path))
(declare-function tramp-rpc--file-stat-cache-key "tramp-rpc-magit"
                  (vec localname lstat))
(defvar tramp-rpc--file-stat-cache)
(defvar tramp-rpc--file-truename-cache)
(defvar tramp-rpc-protocol-error-file-not-found)
(defvar tramp-time-dont-know)

(defcustom remote-backend-tramp-rpc-metadata-scope t
  "Bound RPC visit and save metadata to one batched round trip.
Nil restores tramp-rpc's one-RPC-per-query path."
  :type 'boolean
  :group 'remote)

(defcustom remote-backend-tramp-rpc-exact-mtime t
  "Compare RPC modtimes exactly instead of within TRAMP's two seconds.
Both the visited modtime and the check come from the server's stat of the
same file, so equality is exact; the window only hides a change made right
after a save, such as a formatter or `git checkout' on the target."
  :type 'boolean
  :group 'remote)

(defcustom remote-backend-tramp-rpc-locate-cache t
  "Reuse `locate-dominating-file' answers under tramp-rpc's metadata TTL."
  :type 'boolean
  :group 'remote)

(defconst remote-backend-tramp-rpc-metadata--private-contracts
  '((tramp-rpc--call-batch . (2 . 2))
    (tramp-rpc--call-file-stat . (2 . 3))
    (tramp-rpc--cache-file-stat-result . (3 . 4))
    (tramp-rpc--cache-put . (3 . 3))
    (tramp-rpc--cache-entry-valid-p . (1 . 1))
    (tramp-rpc--convert-file-attributes . (2 . 2))
    (tramp-rpc--decode-string . (1 . 1))
    (tramp-rpc--encode-path . (1 . 1))
    (tramp-rpc--file-stat-cache-key . (3 . 3))
    (tramp-rpc--invalidate-cache-for-path . (1 . 1))
    (tramp-rpc--invalidate-cache-for-subtree . (1 . 1))
    (tramp-rpc-handle-locate-dominating-file . (2 . 2))
    (tramp-handle-set-visited-file-modtime . (0 . 1))
    (tramp-handle-verify-visited-file-modtime . (0 . 1))
    (tramp-rpc-file-name-p . (1 . 1))
    (tramp-flush-file-properties . (2 . 2))
    (tramp-flush-directory-properties . (2 . 2))
    (tramp-flush-connection-properties . (1 . 1)))
  "Private tramp-rpc and TRAMP seams used by the metadata adapter.")

(cl-defstruct (remote-backend-tramp-rpc-metadata--scope
               (:constructor remote-backend-tramp-rpc-metadata--make-scope)
               (:copier nil))
  "Metadata owned by one visit or save."
  (prefetched nil :documentation "Non-nil once the scope sent its batch.")
  (stats nil :documentation "Alist of (LOCALNAME . (STAT . LSTAT)).")
  (host nil :documentation "Connection key of the prefetched file."))

(defvar remote-backend-tramp-rpc-metadata--scope nil
  "The innermost file operation scope, or nil.")

(defvar remote-backend-tramp-rpc-metadata--locate-cache
  (make-hash-table :test #'equal)
  "Map (SEARCH-START . NAMES) to (TIMESTAMP . RESULT).")

(defvar remote-backend-tramp-rpc-metadata--counters
  (list :batches 0 :fresh-reuse 0 :modtime-reuse 0
        :locate-hits 0 :locate-misses 0)
  "Counters for Remote Doctor and benchmarks.")

(defun remote-backend-tramp-rpc-metadata--count (key)
  "Increment KEY in `remote-backend-tramp-rpc-metadata--counters'."
  (cl-incf (plist-get remote-backend-tramp-rpc-metadata--counters key)))

(defun remote-backend-tramp-rpc-metadata-report ()
  "Return a plist describing the metadata adapter and its counters."
  (append
   (list :scope remote-backend-tramp-rpc-metadata-scope
         :locate-cache remote-backend-tramp-rpc-locate-cache
         :locate-entries
         (hash-table-count remote-backend-tramp-rpc-metadata--locate-cache)
         :installed
         (and (fboundp 'tramp-rpc--call-file-stat)
              (advice-member-p #'remote-backend-tramp-rpc-metadata--stat-a
                               'tramp-rpc--call-file-stat)
              t))
   (copy-sequence remote-backend-tramp-rpc-metadata--counters)))

;;;; Operation scope

(defun remote-backend-tramp-rpc-metadata--scope-a (function &rest arguments)
  "Run FUNCTION with ARGUMENTS inside a fresh file operation scope.
Nested operations (a mode hook visiting a sibling, a save hook writing
another file) get their own scope, so one file's metadata never answers for
another operation."
  (let ((remote-backend-tramp-rpc-metadata--scope
         (remote-backend-tramp-rpc-metadata--make-scope)))
    (apply function arguments)))

(defun remote-backend-tramp-rpc-metadata--host (vec)
  "Return a connection identity for VEC."
  (tramp-make-tramp-file-name vec ""))

(defun remote-backend-tramp-rpc-metadata--scope-stat (vec localname lstat)
  "Return the current scope's (STAT) cell for LOCALNAME on VEC, or nil.
The cell's car is the stat when LSTAT is nil, else the lstat.  A cell whose
answer is a missing file holds nil."
  (when-let* ((scope remote-backend-tramp-rpc-metadata--scope)
              ((equal (remote-backend-tramp-rpc-metadata--scope-host scope)
                      (remote-backend-tramp-rpc-metadata--host vec)))
              (entry (assoc localname
                            (remote-backend-tramp-rpc-metadata--scope-stats
                             scope))))
    (list (if lstat (cddr entry) (cadr entry)))))

(defun remote-backend-tramp-rpc-metadata--batch-error-p (entry)
  "Return non-nil when batch ENTRY is an error plist."
  (and (consp entry) (eq (car entry) :error)))

(defun remote-backend-tramp-rpc-metadata--missing-p (entry)
  "Return non-nil when batch ENTRY reports a missing file."
  (and (remote-backend-tramp-rpc-metadata--batch-error-p entry)
       (eql (plist-get entry :error)
            tramp-rpc-protocol-error-file-not-found)))

(defun remote-backend-tramp-rpc-metadata--seed-truename (vec localname lstat
                                                             result)
  "Seed tramp-rpc's truename cache for LOCALNAME on VEC from RESULT.
Only a plain regular file is seeded: the truename skeleton's quoting,
trailing-slash and symlink-cycle rules then reduce to identity, so the
cached value is exactly what the handler would have computed."
  (when-let* (((equal (alist-get 'type lstat) "file"))
              ((not (string-prefix-p "/:" localname)))
              ((not (string-suffix-p "/" localname)))
              (path (tramp-rpc--decode-string
                     (if (and (consp result)
                              (not (and (fboundp 'msgpack-bin-p)
                                        (msgpack-bin-p result))))
                         (alist-get 'path result)
                       result)))
              ((stringp path))
              ((file-name-absolute-p path))
              ((not (tramp-tramp-file-p path))))
    (tramp-rpc--cache-put
     tramp-rpc--file-truename-cache
     (expand-file-name (tramp-make-tramp-file-name vec localname))
     (tramp-make-tramp-file-name vec (directory-file-name path)))))

(defun remote-backend-tramp-rpc-metadata--answer (value)
  "Return (STAT) for batch VALUE, (nil) for a missing file, else nil."
  (cond
   ((remote-backend-tramp-rpc-metadata--missing-p value) '(nil))
   ((remote-backend-tramp-rpc-metadata--batch-error-p value) nil)
   (t (list value))))

(defun remote-backend-tramp-rpc-metadata--seed-stat (vec localname answer
                                                         lstat)
  "Seed tramp-rpc's stat cache for LOCALNAME on VEC with ANSWER.
ANSWER comes from `remote-backend-tramp-rpc-metadata--answer'; LSTAT says
which spelling it answers."
  (when answer
    (tramp-rpc--cache-file-stat-result vec localname (car answer) lstat)))

(defun remote-backend-tramp-rpc-metadata--prefetch (vec localname)
  "Fetch metadata for LOCALNAME on VEC and its directory in one round trip.
The batch holds the file's stat, lstat and truename and its directory's stat
and lstat (saves check that the directory is writable, and every write
invalidates it).  Record the file's answers in the current scope and seed
tramp-rpc's caches.  Any failure leaves the ordinary per-query path in
charge."
  (let* ((scope remote-backend-tramp-rpc-metadata--scope)
         (path (tramp-rpc--encode-path localname))
         (directory (file-name-directory (directory-file-name localname)))
         (directory-path (and directory
                              (tramp-rpc--encode-path directory)))
         (requests
          (append
           (list (cons "file.stat" path)
                 (cons "file.stat" (append path '((lstat . t))))
                 (cons "file.truename" path))
           (and directory-path
                (list (cons "file.stat" directory-path)
                      (cons "file.stat"
                            (append directory-path '((lstat . t))))))))
         (results
          (condition-case nil
              (tramp-rpc--call-batch vec requests)
            (error nil))))
    (setf (remote-backend-tramp-rpc-metadata--scope-prefetched scope) t)
    (when (= (length results) (length requests))
      (remote-backend-tramp-rpc-metadata--count :batches)
      (pcase-let ((`(,stat ,lstat ,truename ,dir-stat ,dir-lstat) results))
        (when directory-path
          (remote-backend-tramp-rpc-metadata--seed-stat
           vec directory (remote-backend-tramp-rpc-metadata--answer dir-stat)
           nil)
          (remote-backend-tramp-rpc-metadata--seed-stat
           vec directory (remote-backend-tramp-rpc-metadata--answer dir-lstat)
           t))
        (let ((answers
               (mapcar #'remote-backend-tramp-rpc-metadata--answer
                       (list stat lstat))))
          (remote-backend-tramp-rpc-metadata--seed-stat
           vec localname (car answers) nil)
          (remote-backend-tramp-rpc-metadata--seed-stat
           vec localname (cadr answers) t)
          ;; The scope only speaks for a file whose two answers both arrived.
          (when (and (car answers) (cadr answers))
            (setf (remote-backend-tramp-rpc-metadata--scope-host scope)
                  (remote-backend-tramp-rpc-metadata--host vec))
            (push (cons localname (cons (caar answers) (car (cadr answers))))
                  (remote-backend-tramp-rpc-metadata--scope-stats scope))))
        (unless (or (remote-backend-tramp-rpc-metadata--batch-error-p lstat)
                    (remote-backend-tramp-rpc-metadata--batch-error-p
                     truename))
          (remote-backend-tramp-rpc-metadata--seed-truename
           vec localname lstat truename))))))

(defun remote-backend-tramp-rpc-metadata--cached-p (vec localname lstat)
  "Return non-nil when tramp-rpc holds a live stat for LOCALNAME on VEC."
  (when-let* ((entry (gethash (tramp-rpc--file-stat-cache-key
                               vec localname lstat)
                              tramp-rpc--file-stat-cache)))
    (tramp-rpc--cache-entry-valid-p (car entry))))

(defun remote-backend-tramp-rpc-metadata--stat-a
    (function vec localname &optional lstat)
  "Answer FUNCTION's stat of LOCALNAME on VEC from the operation scope.
FUNCTION is `tramp-rpc--call-file-stat'; LSTAT selects the lstat answer."
  (let ((scope remote-backend-tramp-rpc-metadata--scope)
        (forced (eq remote-file-name-inhibit-cache t)))
    (when (and scope
               remote-backend-tramp-rpc-metadata-scope
               (not (remote-backend-tramp-rpc-metadata--scope-prefetched scope))
               (or forced
                   (not (remote-backend-tramp-rpc-metadata--cached-p
                         vec localname lstat))))
      (remote-backend-tramp-rpc-metadata--prefetch vec localname))
    (if-let* (((and forced scope remote-backend-tramp-rpc-metadata-scope))
              (cell (remote-backend-tramp-rpc-metadata--scope-stat
                     vec localname lstat)))
        (progn
          (remote-backend-tramp-rpc-metadata--count :fresh-reuse)
          (car cell))
      (funcall function vec localname lstat))))

(defun remote-backend-tramp-rpc-metadata--forget (filename)
  "Drop the current scope's answers for FILENAME after it changed."
  (when-let* ((scope remote-backend-tramp-rpc-metadata--scope)
              ((stringp filename))
              ((tramp-tramp-file-p filename))
              (localname (tramp-file-name-localname
                          (tramp-dissect-file-name filename))))
    (setf (remote-backend-tramp-rpc-metadata--scope-stats scope)
          (assoc-delete-all
           localname
           (remote-backend-tramp-rpc-metadata--scope-stats scope)))))

(defun remote-backend-tramp-rpc-metadata--localname (file)
  "Return FILE's target-native name for a TRAMP or logical `/fs:' FILE."
  (if (tramp-tramp-file-p file)
      (tramp-file-name-localname (tramp-dissect-file-name file))
    (file-local-name file)))

(defun remote-backend-tramp-rpc-metadata--modtime-a
    (function &optional time-list)
  "Call FUNCTION with the scope's pre-read stat when TIME-LIST is nil.
TRAMP otherwise binds `remote-file-name-inhibit-cache' to t and spends one
more round trip right after the read.  After a write the scope has already
forgotten the file, so the post-write modtime is measured fresh."
  (let* ((scope remote-backend-tramp-rpc-metadata--scope)
         (entry
          (and (null time-list)
               scope
               remote-backend-tramp-rpc-metadata-scope
               buffer-file-name
               (assoc (remote-backend-tramp-rpc-metadata--localname
                       buffer-file-name)
                      (remote-backend-tramp-rpc-metadata--scope-stats scope))))
         (lstat (cddr entry))
         (modtime (and lstat
                       (file-attribute-modification-time
                        (tramp-rpc--convert-file-attributes lstat 'integer)))))
    (if modtime
        (progn
          (remote-backend-tramp-rpc-metadata--count :modtime-reuse)
          (funcall function modtime))
      (funcall function time-list))))

(defun remote-backend-tramp-rpc-metadata--verify-a (function &optional buffer)
  "Verify BUFFER's RPC modtime exactly, else defer to FUNCTION.
Only the comparison of a known modtime changes; a missing file, an unknown
modtime and a disconnected buffer keep TRAMP's own answers."
  (with-current-buffer (or buffer (current-buffer))
    (let ((file buffer-file-name))
      (if (not (and remote-backend-tramp-rpc-exact-mtime
                    file
                    (tramp-rpc-file-name-p file)
                    (not (eq (visited-file-modtime) 0))
                    (file-remote-p file nil 'connected)))
          (funcall function buffer)
        (let* ((remote-file-name-inhibit-cache t)
               (attributes (file-attributes file))
               (modtime (file-attribute-modification-time attributes)))
          (if (and attributes
                   (not (time-equal-p modtime tramp-time-dont-know)))
              (time-equal-p modtime (visited-file-modtime))
            (funcall function buffer)))))))

;;;; locate-dominating-file

(defun remote-backend-tramp-rpc-metadata--locate-start (file)
  "Return the directory where the marker search for FILE begins, or FILE.
A name known from the stat cache to be a non-directory searches from its
parent, so sibling files share one cache entry.  Unknown names stay exact."
  (let* ((vec (tramp-dissect-file-name file))
         (localname (tramp-file-name-localname vec))
         (entry (and (not (directory-name-p file))
                     (gethash (tramp-rpc--file-stat-cache-key
                               vec localname nil)
                              tramp-rpc--file-stat-cache))))
    (if (and entry
             (tramp-rpc--cache-entry-valid-p (car entry))
             (cdr entry)
             (not (equal (alist-get 'type (cdr entry)) "directory")))
        (file-name-directory file)
      file)))

(defun remote-backend-tramp-rpc-metadata--locate-a (function file name)
  "Reuse FUNCTION's marker search for FILE and NAME within the metadata TTL."
  (if (or (not remote-backend-tramp-rpc-locate-cache)
          (functionp name)
          (eq remote-file-name-inhibit-cache t))
      (funcall function file name)
    (let* ((key (cons (remote-backend-tramp-rpc-metadata--locate-start
                       (expand-file-name file))
                      (ensure-list name)))
           (entry (gethash key
                           remote-backend-tramp-rpc-metadata--locate-cache)))
      (if (and entry (tramp-rpc--cache-entry-valid-p (car entry)))
          (progn
            (remote-backend-tramp-rpc-metadata--count :locate-hits)
            (cdr entry))
        (remote-backend-tramp-rpc-metadata--count :locate-misses)
        (let ((result (funcall function file name)))
          (puthash key (cons (float-time) result)
                   remote-backend-tramp-rpc-metadata--locate-cache)
          result)))))

(defun remote-backend-tramp-rpc-metadata-flush (&rest _)
  "Drop every cached marker search.
Directory, subtree and connection invalidations can create or remove a
marker anywhere below them, so the whole table goes."
  (clrhash remote-backend-tramp-rpc-metadata--locate-cache))

(defun remote-backend-tramp-rpc-metadata--flush-name (name)
  "Drop cached marker searches when NAME's last component is a marker.
Single-file invalidations are routine (every save, and TRAMP's flush when a
buffer is killed); only one that can create or remove a searched marker can
change an answer."
  (when (and (stringp name)
             (> (hash-table-count
                 remote-backend-tramp-rpc-metadata--locate-cache)
                0))
    (let ((base (file-name-nondirectory (directory-file-name name))))
      (when (catch 'marker
              (maphash (lambda (key _entry)
                         (when (member base (cdr key))
                           (throw 'marker t)))
                       remote-backend-tramp-rpc-metadata--locate-cache)
              nil)
        (remote-backend-tramp-rpc-metadata-flush)))))

(defun remote-backend-tramp-rpc-metadata--invalidate-path-a (filename)
  "Forget FILENAME in the operation scope and in marker searches."
  (remote-backend-tramp-rpc-metadata--forget filename)
  (remote-backend-tramp-rpc-metadata--flush-name filename))

(defun remote-backend-tramp-rpc-metadata--invalidate-subtree-a (directory)
  "Forget everything the operation scope and marker cache hold.
DIRECTORY is the invalidated subtree; any scope file may lie below it."
  (when-let* ((scope remote-backend-tramp-rpc-metadata--scope))
    (ignore directory)
    (setf (remote-backend-tramp-rpc-metadata--scope-stats scope) nil))
  (remote-backend-tramp-rpc-metadata-flush))

(defun remote-backend-tramp-rpc-metadata--flush-file-a (vec localname)
  "Forget LOCALNAME on VEC after TRAMP flushes its properties."
  (when-let* ((scope remote-backend-tramp-rpc-metadata--scope)
              ((stringp localname)))
    (ignore vec)
    (setf (remote-backend-tramp-rpc-metadata--scope-stats scope)
          (assoc-delete-all
           (tramp-file-local-name localname)
           (remote-backend-tramp-rpc-metadata--scope-stats scope))))
  (remote-backend-tramp-rpc-metadata--flush-name localname))

;;;; Installation

(defconst remote-backend-tramp-rpc-metadata--advice
  '((find-file-noselect :around remote-backend-tramp-rpc-metadata--scope-a)
    (basic-save-buffer :around remote-backend-tramp-rpc-metadata--scope-a)
    (tramp-rpc--call-file-stat
     :around remote-backend-tramp-rpc-metadata--stat-a)
    (tramp-handle-set-visited-file-modtime
     :around remote-backend-tramp-rpc-metadata--modtime-a)
    (tramp-handle-verify-visited-file-modtime
     :around remote-backend-tramp-rpc-metadata--verify-a)
    (tramp-rpc-handle-locate-dominating-file
     :around remote-backend-tramp-rpc-metadata--locate-a)
    (tramp-rpc--invalidate-cache-for-path
     :before remote-backend-tramp-rpc-metadata--invalidate-path-a)
    (tramp-rpc--invalidate-cache-for-subtree
     :before remote-backend-tramp-rpc-metadata--invalidate-subtree-a)
    (tramp-flush-file-properties
     :after remote-backend-tramp-rpc-metadata--flush-file-a)
    (tramp-flush-directory-properties
     :after remote-backend-tramp-rpc-metadata-flush)
    (tramp-flush-connection-properties
     :after remote-backend-tramp-rpc-metadata-flush))
  "Advice installed by `remote-backend-tramp-rpc-metadata-install'.")

(defun remote-backend-tramp-rpc-metadata-uninstall ()
  "Remove the metadata adapter and drop its cache."
  (pcase-dolist (`(,symbol ,_how ,function)
                 remote-backend-tramp-rpc-metadata--advice)
    (when (fboundp symbol)
      (advice-remove symbol function)))
  (remote-backend-tramp-rpc-metadata-flush))

(defun remote-backend-tramp-rpc-metadata-install (verified compatible-p)
  "Install the metadata adapter when VERIFIED and every seam is COMPATIBLE-P.
COMPATIBLE-P is called with (SYMBOL . EXPECTED-ARITY) and must look beneath
unrelated advice.  Anything else leaves the adapter uninstalled."
  (remote-backend-tramp-rpc-metadata-uninstall)
  (when (and verified
             (boundp 'tramp-rpc--file-stat-cache)
             (boundp 'tramp-rpc--file-truename-cache)
             (boundp 'tramp-rpc-protocol-error-file-not-found)
             (cl-every compatible-p
                       remote-backend-tramp-rpc-metadata--private-contracts))
    (pcase-dolist (`(,symbol ,how ,function)
                   remote-backend-tramp-rpc-metadata--advice)
      (advice-add symbol how function))
    t))

(provide 'remote-backend-tramp-rpc-metadata)
;;; remote-backend-tramp-rpc-metadata.el ends here
