;;; remote-search.el --- Target-side search tools for Remote -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep project search on the target.  A trusted target without ripgrep can
;; receive a verified, versioned binary in its user cache; other targets keep
;; the ordinary grep fallback.  No server or Node runtime is needed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'remote-config)
(require 'remote-fs)
(require 'remote-process)
(require 'remote-service)

(defgroup remote-search nil
  "Search tools on Remote targets."
  :group 'remote)

(defcustom remote-search-auto-provision-ripgrep t
  "Prepare pinned ripgrep in a trusted target's user cache after first search.
Unsupported platforms and failed provisioning continue with the target's
ordinary grep.  Set this to nil to use only tools already in the target PATH."
  :type 'boolean
  :group 'remote-search)

(defconst remote-search-ripgrep-version "15.2.0")
(defconst remote-search--positive-cache-seconds 1800
  "Seconds before a verified search tool is refreshed in the background.")

(defconst remote-search--archives
  '((x86_64
     :name "ripgrep_15.2.0-1_amd64.deb"
     :sha256 "5af93eebe4c352474632cf1d28523b0e98bcd4e2f115a249a713b8d8c7d1d01c"
     :binary-sha256 "c2e9783ac04b502122a78ad2154c992719d4a73b5001b19202853a4106738b03"
     :format deb)
    (aarch64
     :name "ripgrep-15.2.0-aarch64-unknown-linux-gnu.tar.gz"
     :sha256 "a740b91c82eaf9914cfedd353572f2791cbe0162c84101ee0951058f4dcbc90d"
     :binary-sha256 "e36d0eb52e70696bdf1781392722e05a21bb91d3b7b762ef5ec20e5df2ec687b"
     :format tar))
  "Verified upstream release archives supported by the managed search tool.")

(defvar remote-search--cache (make-hash-table :test #'equal)
  "Target ID to (target object config generation expiry path release) entry.
The path is :pending or :failed while provisioning is incomplete.")

(defvar remote-search--workers (make-hash-table :test #'equal)
  "Target ID to a live client-local ripgrep provisioning process.")

(defun remote-search--platform (context)
  "Return the supported release platform for CONTEXT, or nil."
  (let ((uname
         (string-trim
          (remote-exec-output
           "uname" :args '("-s" "-m")
           :context context :adapter "exec" :check t))))
    (cond
     ((equal uname "Linux aarch64") 'aarch64)
     ((and (equal uname "Linux x86_64")
           (string-prefix-p
            "glibc "
            (or (ignore-errors
                  (string-trim
                   (remote-exec-output
                    "getconf" :args '("GNU_LIBC_VERSION")
                    :context context :adapter "exec" :check t)))
                "")))
      'x86_64))))

(defun remote-search--archive-cache-directory ()
  "Return the client-local archive cache directory."
  (let* ((default-directory temporary-file-directory)
         (process-environment (default-value 'process-environment))
         (xdg (getenv "XDG_CACHE_HOME"))
         (base
          (if (and xdg (file-name-absolute-p xdg)
                   (not (file-remote-p xdg)))
              xdg
            (expand-file-name
             ".cache/" (or (getenv "HOME") temporary-file-directory)))))
    (expand-file-name "emacs-remote/search-archives/" base)))

(defun remote-search--sha256-file (file)
  "Return the SHA-256 digest of client-local FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun remote-search--client-command (directory program &rest args)
  "Run client-local PROGRAM with ARGS in DIRECTORY, or signal an error."
  (let ((default-directory (file-name-as-directory directory))
        (exec-path (default-value 'exec-path))
        (process-environment (default-value 'process-environment)))
    (unless (equal (apply #'call-process program nil nil nil args) 0)
      (error "Client-side %s failed while preparing ripgrep" program))))

(defun remote-search--verified-archive (release)
  "Return a client-local, digest-verified archive for RELEASE."
  (let* ((name (plist-get release :name))
         (expected (plist-get release :sha256))
         (directory (remote-search--archive-cache-directory))
         (archive (expand-file-name name directory))
         (url (format
               "https://github.com/BurntSushi/ripgrep/releases/download/%s/%s"
               remote-search-ripgrep-version name)))
    (make-directory directory t)
    (unless (and (file-regular-p archive)
                 (equal (remote-search--sha256-file archive) expected))
      (let ((temporary (make-temp-file (concat archive "."))))
        (unwind-protect
            (progn
              (remote-search--client-command
               directory "curl" "-fsSL" "--connect-timeout" "5"
               "--max-time" "60" "--output" temporary url)
              (unless (equal (remote-search--sha256-file temporary) expected)
                (error "Ripgrep archive checksum does not match %s" name))
              (rename-file temporary archive t))
          (when (file-exists-p temporary)
            (delete-file temporary)))))
    archive))

(defun remote-search--prepare-source (release archive)
  "Extract the verified ARCHIVE for RELEASE into a client-local source tree.
The caller owns the returned directory and must delete it afterward."
  (let* ((temporary (make-temp-file "emacs-remote-rg-" t))
         (source (expand-file-name "payload/" temporary))
         (binary (expand-file-name "bin/rg" source)))
    (condition-case error
        (progn
          (make-directory (file-name-directory binary) t)
          (pcase (plist-get release :format)
            ('deb
             (remote-search--client-command
              temporary "ar" "x" archive "data.tar.xz")
             (remote-search--client-command
              temporary "tar" "-xJf" "data.tar.xz" "-C" temporary
              "./usr/bin/rg"
              "./usr/share/doc/ripgrep/LICENSE-MIT"
              "./usr/share/doc/ripgrep/UNLICENSE")
             (copy-file (expand-file-name "usr/bin/rg" temporary) binary)
             (dolist (notice '("LICENSE-MIT" "UNLICENSE"))
               (copy-file
                (expand-file-name
                 (concat "usr/share/doc/ripgrep/" notice) temporary)
                (expand-file-name notice source))))
            ('tar
             (let ((root
                    (file-name-sans-extension
                     (file-name-sans-extension
                      (plist-get release :name)))))
               (remote-search--client-command
                temporary "tar" "-xzf" archive "-C" temporary
                (concat root "/rg")
                (concat root "/LICENSE-MIT")
                (concat root "/UNLICENSE"))
               (copy-file (expand-file-name (concat root "/rg") temporary)
                          binary)
               (dolist (notice '("LICENSE-MIT" "UNLICENSE"))
                 (copy-file
                  (expand-file-name (concat root "/" notice) temporary)
                  (expand-file-name notice source)))))
            (_ (error "Unsupported ripgrep release format")))
          (set-file-modes binary #o755)
          temporary)
      (error
       (delete-directory temporary t)
       (signal (car error) (cdr error))))))

(defun remote-search--version-valid-p (binary context)
  "Return whether target-native BINARY is the pinned version on CONTEXT."
  (condition-case nil
      (string-prefix-p
       (format "ripgrep %s" remote-search-ripgrep-version)
       (remote-exec-output
        binary :args '("--version")
        :context context :adapter "exec" :check t))
    (error nil)))

(defun remote-search--binary-hash-valid-p (binary expected context)
  "Return whether target-native BINARY hashes to EXPECTED on CONTEXT."
  (condition-case nil
      (let ((output
             (remote-exec-output
              "sha256sum" :args (list "--" binary)
              :context context :adapter "exec" :check t)))
        (and (>= (length output) 64)
             (equal (substring output 0 64) expected)))
    (error nil)))

(defun remote-search--candidate (context)
  "Return (RELEASE DIRECTORY BINARY) for CONTEXT, or nil if unsupported."
  (when-let* ((platform (remote-search--platform context))
              (release (cdr (assq platform remote-search--archives))))
    (let ((directory
           (remote-expand-file-name
            (format "~/.cache/emacs-remote/tools/ripgrep/%s-%s/"
                    remote-search-ripgrep-version platform)
            nil context)))
      (list release directory
            (concat (remote-file-local-name directory) "bin/rg")))))

(defun remote-search--installed (candidate context)
  "Return CANDIDATE's existing verified binary on CONTEXT, or nil."
  (let ((release (nth 0 candidate))
        (binary (nth 2 candidate)))
    (and (remote-executable-find binary context)
         (remote-search--binary-hash-valid-p
          binary (plist-get release :binary-sha256) context)
         binary)))

(defun remote-search--provision (context)
  "Return a verified managed ripgrep path for CONTEXT, or nil."
  (when-let* ((candidate (remote-search--candidate context)))
    (let ((release (nth 0 candidate))
          (directory (nth 1 candidate))
          (binary (nth 2 candidate))
          installed)
      (unless (remote-search--installed candidate context)
        (message "Installing ripgrep %s on %s..."
                 remote-search-ripgrep-version
                 (remote-context-target-id context))
        (let* ((archive (remote-search--verified-archive release))
               (temporary (remote-search--prepare-source release archive)))
          (unwind-protect
              (remote-service-provision-directory
               "ripgrep" (expand-file-name "payload/" temporary) directory
               :context context :adapter "exec"
               :ready-file "bin/rg" :ready-kind 'executable
               :validate
               (lambda (candidate-context candidate-directory)
                 (remote-search--binary-hash-valid-p
                  (concat (remote-file-local-name candidate-directory)
                          "bin/rg")
                  (plist-get release :binary-sha256)
                  candidate-context)))
            (delete-directory temporary t)))
        (setq installed t))
      (unless (remote-search--binary-hash-valid-p
               binary (plist-get release :binary-sha256) context)
        (error "Managed ripgrep at %s failed binary checksum" binary))
      (unless (remote-search--version-valid-p binary context)
        (error "Managed ripgrep at %s is not version %s"
               binary remote-search-ripgrep-version))
      (when installed
        (message "Ripgrep %s ready on %s"
                 remote-search-ripgrep-version
                 (remote-context-target-id context)))
      binary)))

(defun remote-search--cache-put (target value seconds)
  "Cache VALUE for TARGET for SECONDS under the current configuration."
  (puthash (remote-target-id target)
           (list target remote-config-generation
                 (+ (float-time) seconds) value
                 remote-search-ripgrep-version)
           remote-search--cache))

(defun remote-search--worker-command ()
  "Return a client-local batch Emacs command for search provisioning."
  (let* ((root (file-name-as-directory user-emacs-directory))
         (lisp (expand-file-name "lisp/" root))
         (remote (expand-file-name "lisp/remote/" root))
         (backend (expand-file-name "lisp/remote/backend/" root))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unless (and (file-executable-p emacs)
                 (file-readable-p (expand-file-name "remote-search.el" remote)))
      (error "Cannot start client-local search provisioning worker"))
    (list emacs "--batch" "-Q"
          "--eval"
          (format "(setq user-emacs-directory %S load-prefer-newer t)" root)
          "-L" lisp "-L" remote "-L" backend
          "-l" (expand-file-name "remote-framework.el" remote)
          "-l" (expand-file-name "remote-search.el" remote)
          "-f" "remote-search-worker-main")))

(defun remote-search-worker-main ()
  "Provision search in a separate client Emacs process.
The parent supplies the target ID and optional config path through its local
environment.  The only stdout protocol line contains a base64 target-native
path, so package and SSH diagnostic output cannot be mistaken for the result."
  (let ((target-id (getenv "REMOTE_SEARCH_TARGET"))
        (config (getenv "REMOTE_SEARCH_CONFIG")))
    (unless (and target-id (not (string-empty-p target-id)))
      (error "Search worker has no target ID"))
    (when (and config (file-name-absolute-p config))
      (setq remote-config-file config))
    (remote-config-load)
    (remote-fs-install)
    (let ((target (remote-get-target target-id)))
      (unless (and target (remote-target-trusted target))
        (error "Search worker target is not trusted: %s" target-id)))
    (let* ((context (remote-context
                     (remote-make-file-name target-id "/tmp/")))
           (program
            (or (remote-executable-find "rg" context)
                (remote-search--provision context))))
      (unless (and (stringp program) (file-name-absolute-p program))
        (error "Search worker could not prepare ripgrep on %s" target-id))
      (princ
       (format "REMOTE_SEARCH_READY %s\n"
               (base64-encode-string
                (encode-coding-string program 'utf-8) t))))))

(defun remote-search--worker-result (buffer)
  "Return the target-native path reported in worker BUFFER, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-match-data
        (goto-char (point-min))
        (when (re-search-forward
               "^REMOTE_SEARCH_READY \\([[:alnum:]+/=]+\\)$" nil t)
          (condition-case nil
              (decode-coding-string
               (base64-decode-string (match-string 1)) 'utf-8)
            (error nil)))))))

(defun remote-search--worker-finished
    (process _event target target-generation worker-version)
  "Commit PROCESS result for TARGET if its config and version still match."
  (when (memq (process-status process) '(exit signal))
    (let* ((target-id (remote-target-id target))
           (buffer (process-buffer process))
           (timer (process-get process 'remote-search-timeout))
           (current (and (eq (gethash target-id remote-search--workers)
                             process)
                         (eq (remote-get-target target-id) target)
                         (eql remote-config-generation target-generation)
                         (equal remote-search-ripgrep-version worker-version)))
           (program
            (and (eq (process-status process) 'exit)
                 (zerop (process-exit-status process))
                 (remote-search--worker-result buffer))))
      (when (timerp timer)
        (cancel-timer timer))
      (when (eq (gethash target-id remote-search--workers) process)
        (remhash target-id remote-search--workers))
      (when current
        (if (and (stringp program)
                 (file-name-absolute-p program)
                 (not (file-remote-p program)))
            (progn
              (remote-search--cache-put
               target program remote-search--positive-cache-seconds)
              (message "Ripgrep ready on %s; next search uses it" target-id))
          (remote-search--cache-put target :failed 60)
          (message "Ripgrep preparation failed on %s; using grep" target-id)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun remote-search--start-worker (target)
  "Start one client-local provisioning worker for TARGET, or reuse it."
  (let* ((target-id (remote-target-id target))
         (existing (gethash target-id remote-search--workers)))
    (if (and existing (process-live-p existing))
        existing
      (let* ((buffer (generate-new-buffer
                      (format " *remote-search:%s*" target-id)))
             (generation remote-config-generation)
             (version remote-search-ripgrep-version)
             (default-directory temporary-file-directory)
             (exec-path (default-value 'exec-path))
             (process-environment
              (cons (concat "REMOTE_SEARCH_TARGET=" target-id)
                    (cons (concat "REMOTE_SEARCH_CONFIG="
                                  remote-config-file)
                          (default-value 'process-environment))))
             process)
        (condition-case error
            (progn
              (setq process
                    (make-process
                     :name (format "remote-search:%s" target-id)
                     :buffer buffer :noquery t :coding 'utf-8-unix
                     :command (remote-search--worker-command)
                     :sentinel
                     (lambda (worker event)
                       (remote-search--worker-finished
                        worker event target generation version))))
              (puthash target-id process remote-search--workers)
              (process-put
               process 'remote-search-timeout
               (run-at-time
                180 nil
                (lambda ()
                  (when (process-live-p process)
                    (delete-process process)))))
              process)
          (error
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (signal (car error) (cdr error))))))))

(defun remote-search-ripgrep (context &optional system-rg-unavailable)
  "Find ripgrep on CONTEXT, preparing a missing binary without blocking search.
SYSTEM-RG-UNAVAILABLE means the caller already probed for `rg' in target PATH.
Return a target-native executable path, or nil for the grep fallback."
  (let* ((context (if (remote-context-p context)
                      context
                    (remote-context context)))
         (target-id (remote-context-target-id context))
         (target (remote-get-target target-id))
         (now (float-time))
         (cached (gethash target-id remote-search--cache))
         (cache-matches
          (and cached (eq (nth 0 cached) target)
               (eql (nth 1 cached) remote-config-generation)
               (equal (nth 4 cached) remote-search-ripgrep-version)))
         (cache-valid
          (and cache-matches (> (nth 2 cached) now))))
    (or (and remote-search-auto-provision-ripgrep
             target (remote-target-trusted target)
             cache-matches (stringp (nth 3 cached))
             (prog1 (nth 3 cached)
               (when (and (not cache-valid) target
                          (remote-target-trusted target))
                 ;; A previously verified path stays usable while a worker
                 ;; refreshes its hash and repairs it if necessary.
                 (condition-case error
                     (remote-search--start-worker target)
                   (error
                    (remote-search--cache-put target :failed 60)
                    (message "Ripgrep refresh unavailable on %s: %s"
                             target-id (error-message-string error)))))))
        (and (not system-rg-unavailable)
             (not (and cache-valid (eq (nth 3 cached) :pending)))
             (remote-executable-find "rg" context))
        (when (and remote-search-auto-provision-ripgrep
                   target (remote-target-trusted target)
                   (not (and cache-valid
                             (memq (nth 3 cached) '(:pending :failed)))))
          (condition-case error
              (progn
                ;; Architecture, cache hash and version checks can each cause
                ;; target round trips.  Keep them in the worker so the first
                ;; interactive search starts with target grep immediately.
                (remote-search--cache-put target :pending 180)
                (remote-search--start-worker target)
                nil)
            (error
             (remote-search--cache-put target :failed 60)
             (message "Managed ripgrep unavailable on %s: %s"
                      target-id (error-message-string error))
             nil))))))

(defun remote-search-consult-ripgrep-args (args program)
  "Return Consult ARGS using target-native PROGRAM, or nil if incompatible.
Only a leading literal `rg' is replaced, so user-customized search options
are preserved.  Callers can retain other custom Consult command forms."
  (cl-labels
      ((replace-head
        (string)
        (when (and (stringp string)
                   (string-match "\\`[[:space:]]*rg" string)
                   (let ((end (match-end 0)))
                     (or (= end (length string))
                         (memq (aref string end) '(?\s ?\t ?\n)))))
          ;; Consult parses this setting with `split-string-and-unquote',
          ;; which accepts double quotes but not shell-style escaped spaces.
          (concat (if (string-match-p "[[:space:]\"\\\\]" program)
                      (prin1-to-string program)
                    program)
                  (substring string (match-end 0))))))
    (cond
     ((stringp args) (replace-head args))
     ((consp args)
      (when-let* ((head (replace-head (car args))))
        (cons head (cdr args)))))))

(provide 'remote-search)
;;; remote-search.el ends here
