;;; init-tramp.el --- TRAMP remote access: performance, stability, LSP -*- lexical-binding: t -*-

;;; Commentary:
;; Centralises every TRAMP-related setting: core options, VC/diff-hl
;; suppression, LSP stdio transport management (including the lean4 CRLF fix),
;; memoization of expensive remote lookups, and Magit/compile integration.
;;
;; Two TRAMP methods are supported:
;;   /ssh:host:/path  — standard SSH, managed here (ControlMaster, pipe-mode LSP)
;;   /rpc:host:/path  — tramp-rpc MessagePack-RPC backend, ~38x faster file ops
;;
;; Several subsystems (VC, memoization, Eglot direct-async) are intentionally
;; skipped for /rpc: paths because tramp-rpc provides its own superior
;; implementations of those concerns.
;;
;; Load order: after init-base (needs my/tramp-state-dir), before init-lsp.

;;; Code:

(require 'cl-lib)


;;; ── tramp-rpc: high-performance MessagePack-RPC TRAMP backend ───────────────
;; /rpc:host:/path uses a binary RPC server on the remote for ~38x faster
;; connection setup and ~27x faster directory listing vs traditional SSH.
;;
;; The guards below (my/tramp-rpc-path-p) are safe regardless of whether
;; tramp-rpc is installed — they are pure string checks on the file path.
(my/package-ensure-vc 'tramp-rpc "https://github.com/ArthurHeymans/emacs-tramp-rpc")
;;;; tramp / tramp-rpc safe setup
(let ((tramp-rpc-dir
       (or (ignore-errors
             (when-let* ((library (locate-library "tramp-rpc")))
               (file-name-directory library)))
           (expand-file-name "tramp-rpc"
                             (or (and (boundp 'package-user-dir) package-user-dir)
                                 (expand-file-name "elpa" user-emacs-directory))))))
  (when (file-directory-p tramp-rpc-dir)
    (add-to-list 'load-path tramp-rpc-dir)
    (require 'tramp-rpc)))

(defun my/tramp-rpc-path-p (&optional path)
  "Return non-nil if PATH (or current buffer path) uses Tramp method \"rpc\"."
  (when (featurep 'tramp)
    (let* ((path (or path buffer-file-name default-directory))
           (vec  (and (stringp path)
                      (tramp-tramp-file-p path)
                      (ignore-errors
                        (tramp-dissect-file-name path nil)))))
      (and vec
           (string= (tramp-file-name-method vec) "rpc")))))

;;; ── Pre-load settings ───────────────────────────────────────────────────────
;; These must be set before TRAMP is first loaded.

;; Persist the TRAMP connection cache between sessions.
(setq tramp-persistency-file-name
      (expand-file-name "persistency.el" my/tramp-state-dir))

;; Store TRAMP auto-saves beside other auto-saves, not in /tmp on the remote.
(setq tramp-auto-save-directory my/tramp-auto-save-dir)

;; Cache remote stat results for 60 s — round-trips are expensive.
(setq remote-file-name-inhibit-cache 60)

;; Never auto-save-visited over TRAMP (causes extra round-trips).
(setq remote-file-name-inhibit-auto-save-visited t)

;; Don't create .#lockfile symlinks on remote hosts.
(setq remote-file-name-inhibit-locks t)

;; Don't auto-revert remote files — polling over SSH is wasteful.
(setq auto-revert-remote-files nil)

;;; ── Core TRAMP package ──────────────────────────────────────────────────────

(use-package tramp
  :ensure nil
  :defer t
  :config
  ;; Verbosity: 0 = silent, 3 = warnings/errors only, 6 = full debug.
  (setq tramp-verbose 3)

  ;; SSH as the default transport.
  (setq tramp-default-method "ssh")

  ;; Use bash for the remote login shell so TRAMP's prompt detection works.
  (setq tramp-login-shell "bash")
  (setq tramp-login-args '(("-l")))

  ;; Connection and session timeouts.
  (setq tramp-connection-timeout 10
        tramp-session-timeout 300)

  ;; Remote PATH: prefer the user's own installed tools first.
  (setq tramp-remote-path
        '(tramp-own-remote-path
          tramp-default-remote-path
          "/usr/local/bin"
          "/opt/homebrew/bin"
          "/opt/local/bin"
          "/usr/sbin"
          "/sbin"
          "~/.elan/bin"
          "~/.local/bin"
          "~/.pyenv/shims"
          "~/.asdf/shims"
          "~/.cargo/bin"
          "/snap/bin"))

  ;; SSH ControlMaster: multiplex all sessions over one TCP connection.
  ;; This is the single biggest speedup for remote file access.
  (setq tramp-use-ssh-controlmaster-options t)

  ;; SCP direct remote copying: avoid local staging for large transfers.
  (setq tramp-use-scp-direct-remote-copying t)
  (setq tramp-copy-size-limit (* 1024 1024)) ; 1 MiB

  ;; Only re-read remote directory listings at most once per minute.
  (setq tramp-completion-reread-directory-timeout 60)

  ;; Mirror local backup policy on the remote.
  (setq tramp-backup-directory-alist backup-directory-alist)

  ;; Don't write a history file to the remote home directory.
  (setq tramp-histfile-override "/dev/null")

  ;; Allow unsafe temporary files (needed on systems with nosuid /tmp,
  ;; e.g. some NixOS configurations).
  (setq tramp-allow-unsafe-temporary-files t))

;;; ── Direct-async profile for scp ────────────────────────────────────────────
;; Enable TRAMP's direct-async path for scp bulk copies so they get a fresh
;; subprocess instead of sharing the ControlMaster channel.  LSP stdio
;; connections manage their own direct-async policy per server (see below).

(with-eval-after-load 'files-x
  (when (and (fboundp 'connection-local-set-profile-variables)
             (fboundp 'connection-local-set-profiles))
    (connection-local-set-profile-variables
     'remote-direct-async-process
     '((tramp-direct-async-process . t)))
    (connection-local-set-profiles
     '(:application tramp :protocol "scp")
     'remote-direct-async-process)))

;;; ── VC / diff-hl suppression ────────────────────────────────────────────────
;; VC backends make synchronous subprocess calls that are prohibitively slow
;; over SSH.  Disable them for /ssh: buffers.
;;
;; Exception: /rpc: buffers.  tramp-rpc-magit.el provides its own parallel git
;; prefetch (60+ commands in a single RPC round-trip) and TTL-based caching
;; with filesystem-watch invalidation.  Suppressing VC there would disable
;; that fast path.

(defun my/remote-file-buffer-p ()
  "Return non-nil when the current buffer visits a remote path."
  (file-remote-p (or buffer-file-name default-directory)))

(defun my/disable-remote-vc-h ()
  "Disable VC for remote SSH buffers; leave /rpc: buffers alone."
  (when (and (my/remote-file-buffer-p)
             (not (my/tramp-rpc-path-p)))
    (setq-local vc-handled-backends nil
                vc-mode nil)
    (when (bound-and-true-p diff-hl-mode)
      (diff-hl-mode -1))))

(add-hook 'find-file-hook #'my/disable-remote-vc-h)

(with-eval-after-load 'vc-hooks
  (define-advice vc-refresh-state (:around (fn) my/skip-remote-vc-refresh)
    "Skip VC state refresh for remote SSH buffers; let tramp-rpc handle /rpc:."
    (if (and (my/remote-file-buffer-p) (not (my/tramp-rpc-path-p)))
        (progn (my/disable-remote-vc-h) nil)
      (funcall fn))))

(with-eval-after-load 'vc
  (setq-default vc-ignore-dir-regexp
                (format "%s\\|%s"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp)))

;;; ── LSP stdio: per-server direct-async management ──────────────────────────
;;
;; Two TRAMP paths exist for remote LSP processes:
;;
;; Traditional (ControlMaster shared)
;;   Reuses the existing SSH connection.  Fast to start.  BUT applies
;;   unix-EOL encoding to outgoing bytes, silently stripping every \r.
;;   Fine for servers that accept bare \n in JSON-RPC headers.
;;
;; Pipe-mode direct-async (fresh SSH subprocess, no PTY)
;;   Creates a new SSH process with :coding 'no-conversion → binary-clean
;;   channel.  \r\n is preserved end-to-end.  Required for servers whose
;;   parsers enforce CRLF headers (e.g. Lean 4: takeEnd 2 == "\r\n").
;;   We suppress PTY allocation (-t -t) by returning nil from
;;   tramp-get-method-parameter for the tramp-direct-async key, so
;;   (consp nil) = nil → SSH omits -t -t → plain pipe.

(defcustom my/tramp-lsp-pipe-mode-servers
  '("lean4")
  "LSP server name patterns that require pipe-mode direct-async over TRAMP.
Each element is a regexp matched against the server name string passed to
the lsp-stdio-connection :connect lambda (e.g. \"lean4-lsp\",
\"remote-lean4-lsp\").  Add a server here when it enforces strict CRLF
headers or is otherwise corrupted by TRAMP's unix-EOL encoding.

Note: this list only applies to /ssh: connections.  /rpc: connections use
tramp-rpc's own binary-safe MessagePack relay and are never routed through
the pipe-mode or ControlMaster paths below.

Examples:
  (\"lean4\")                   ; Lean 4 only (default)
  (\"lean4\" \"some-server\")     ; Lean 4 + some-server"
  :type '(repeat string)
  :group 'my)

(defun my/tramp-lsp-pipe-mode-p (server-name)
  "Return non-nil if SERVER-NAME should use pipe-mode direct-async over TRAMP."
  (seq-some (lambda (pat) (string-match-p pat server-name))
            my/tramp-lsp-pipe-mode-servers))

(defun my/tramp-lsp--pipe-connect (connect-fn filter sentinel name env-fn workspace)
  "Call CONNECT-FN through direct-async pipe mode (no PTY).
Overrides tramp-direct-async-process-p → t and suppresses -t -t by
returning nil for the tramp-direct-async method parameter."
  (let ((orig-gmp (symbol-function 'tramp-get-method-parameter)))
    (cl-letf (((symbol-function 'tramp-direct-async-process-p)
               (lambda (&rest _) t))
              ((symbol-function 'tramp-get-method-parameter)
               (lambda (vec param &optional default)
                 (if (eq param 'tramp-direct-async)
                     nil                ; (consp nil)=nil → no -t -t → no PTY
                   (funcall orig-gmp vec param default)))))
      (funcall connect-fn filter sentinel name env-fn workspace))))

(defun my/tramp-lsp--controlmaster-connect (connect-fn filter sentinel name env-fn workspace)
  "Call CONNECT-FN through the traditional ControlMaster path (no direct-async)."
  (cl-letf (((symbol-function 'tramp-direct-async-process-p) #'ignore))
    (funcall connect-fn filter sentinel name env-fn workspace)))

(with-eval-after-load 'lsp-mode
  (define-advice lsp-stdio-connection (:filter-return (plist) my/tramp-lsp-transport)
    "Route each LSP server through the appropriate TRAMP transport.

Servers listed in `my/tramp-lsp-pipe-mode-servers' use a binary pipe
(direct-async, no PTY) so that \\r\\n headers are preserved.  All other
servers use the shared ControlMaster path."
    (if-let* ((connect-fn (plist-get plist :connect)))
        (plist-put
         plist :connect
         (lambda (filter sentinel name environment-fn workspace)
           (if (my/tramp-lsp-pipe-mode-p name)
               (my/tramp-lsp--pipe-connect
                connect-fn filter sentinel name environment-fn workspace)
             (my/tramp-lsp--controlmaster-connect
              connect-fn filter sentinel name environment-fn workspace))))
      plist)))

;;; ── Eglot: disable direct-async for SSH paths ───────────────────────────────
;; /rpc: connections are handled entirely by tramp-rpc-process.el (its own
;; cat-relay, no tramp-sh make-process path), so tramp-direct-async-process-p
;; is irrelevant for them.  Only suppress it for SSH sessions.

(with-eval-after-load 'eglot
  (define-advice eglot--connect (:around (fn &rest args) my/tramp-eglot-no-direct-async)
    "Disable TRAMP direct-async for Eglot SSH stdio sessions."
    (if (my/tramp-rpc-path-p)
        (apply fn args)
      (cl-letf (((symbol-function 'tramp-direct-async-process-p) #'ignore))
        (apply fn args)))))

;;; ── Magit & compile integration ─────────────────────────────────────────────

(with-eval-after-load 'magit
  ;; PTY mode for git pipe commands gives line-buffered output.
  (setq magit-tramp-pipe-stty-settings 'pty))

(with-eval-after-load 'compile
  (with-eval-after-load 'tramp
    ;; tramp-compile-disable-ssh-controlmaster-options would break ControlMaster
    ;; reuse; remove it from compilation-mode-hook.
    (remove-hook 'compilation-mode-hook
                 #'tramp-compile-disable-ssh-controlmaster-options)))

;;; ── find-file timing feedback ────────────────────────────────────────────────

(defcustom my/find-file-feedback-threshold 0.4
  "Report local file opens only when they exceed this many seconds."
  :type 'number
  :group 'my)

(defun my/find-file-feedback-a (orig filename &rest args)
  "Report elapsed time when opening remote or unusually slow local files."
  (let ((t0 (float-time)))
    (prog1 (apply orig filename args)
      (let* ((elapsed (- (float-time) t0))
             (remote  (ignore-errors (file-remote-p filename)))
             (label   (cond ((my/tramp-rpc-path-p filename) "RPC")
                            (remote "TRAMP")
                            (t "Local"))))
        (when (or remote (> elapsed my/find-file-feedback-threshold))
          (message "[%s %.2fs] %s" label elapsed filename))))))

(advice-add 'find-file :around #'my/find-file-feedback-a)

;;; ── Memoization for expensive remote lookups ────────────────────────────────
;; magit-toplevel, project-current, and vc-git-root each do subprocess calls
;; that take hundreds of milliseconds over SSH TRAMP.  Cache per directory.
;;
;; /rpc: paths are excluded: tramp-rpc-magit.el already provides a TTL-based
;; cache with filesystem-watch invalidation.  Wrapping that in our own alist
;; would only serve stale results after tramp-rpc has invalidated its cache.

(defcustom my/tramp-memoize-cache-limit 128
  "Maximum entries retained in each remote lookup memoization cache.
Set to nil to keep caches unbounded."
  :type '(choice (const :tag "Unbounded" nil)
                 (integer :tag "Entries"))
  :group 'tramp)

(defun my/tramp--trim-memoize-cache (cache)
  "Trim symbol CACHE to `my/tramp-memoize-cache-limit' entries."
  (when (integerp my/tramp-memoize-cache-limit)
    (if (<= my/tramp-memoize-cache-limit 0)
        (set cache nil)
      (let* ((entries (symbol-value cache))
             (tail (nthcdr (1- my/tramp-memoize-cache-limit) entries)))
        (when tail
          (setcdr tail nil))))))

(defun my/tramp-memoize (key cache fn &rest args)
  "Return cached result for KEY from symbol CACHE, or call FN with ARGS.
Caches for remote SSH paths only; passes through for local and /rpc: paths."
  (if (and key (file-remote-p key) (not (my/tramp-rpc-path-p key)))
      (if-let* ((cached (assoc key (symbol-value cache))))
          (cdr cached)
        (let ((value (apply fn args)))
          (set cache (cons (cons key value) (symbol-value cache)))
          (my/tramp--trim-memoize-cache cache)
          value))
    (apply fn args)))

(defvar my/tramp--magit-toplevel-cache  nil)
(defvar my/tramp--project-current-cache nil)
(defvar my/tramp--vc-git-root-cache     nil)

(defun my/tramp-clear-caches-h (&rest _)
  "Invalidate all TRAMP memoization caches."
  (setq my/tramp--magit-toplevel-cache  nil
        my/tramp--project-current-cache nil
        my/tramp--vc-git-root-cache     nil))

(with-eval-after-load 'magit
  (define-advice magit-toplevel (:around (fn &optional directory) my/tramp-memoize)
    "Memoize remote `magit-toplevel' lookups."
    (my/tramp-memoize (or directory default-directory)
                      'my/tramp--magit-toplevel-cache
                      fn directory)))

(with-eval-after-load 'project
  (define-advice project-current (:around (fn &optional maybe-prompt directory) my/tramp-memoize)
    "Memoize remote `project-current' lookups."
    (my/tramp-memoize (or directory
                          (bound-and-true-p project-current-directory-override)
                          default-directory)
                      'my/tramp--project-current-cache
                      fn maybe-prompt directory)))

(with-eval-after-load 'vc-git
  (define-advice vc-git-root (:around (fn file) my/tramp-memoize)
    "Memoize remote `vc-git-root' lookups; discard immediate misses."
    (let ((value (my/tramp-memoize (file-name-directory file)
                                   'my/tramp--vc-git-root-cache
                                   fn file)))
      ;; A nil result would cache a miss forever — drop it immediately.
      (unless (cdar my/tramp--vc-git-root-cache)
        (setq my/tramp--vc-git-root-cache (cdr my/tramp--vc-git-root-cache)))
      value)))

(with-eval-after-load 'tramp
  (advice-add 'tramp-cleanup-all-connections :after #'my/tramp-clear-caches-h)
  (when (fboundp 'tramp-cleanup-connection)
    (advice-add 'tramp-cleanup-connection :after #'my/tramp-clear-caches-h)))

(defun my/tramp-remove-named-advice (symbol advice-name)
  "Remove advice named ADVICE-NAME from SYMBOL when present."
  (advice-mapc
   (lambda (advice props)
     (when (equal (alist-get 'name props) advice-name)
       (advice-remove symbol advice)))
   symbol))

(defun my/tramp-projectile-dispatch-handler-a (operation orig-fn &rest args)
  "Call ORIG-FN for OPERATION, dispatching through file handlers when safe.
Unlike the upstream tramp-rpc advice, this only treats the first arg as a path
when it is actually a string, avoiding `stringp' errors for calls such as
`(projectile-get-ext-command 'git)'."
  (let* ((path-arg (car args))
         (handler-path (cond
                        ((and (stringp path-arg)
                              (file-name-absolute-p path-arg))
                         path-arg)
                        ((stringp default-directory)
                         default-directory)))
         (handler (and (stringp handler-path)
                       (find-file-name-handler handler-path operation))))
    (if handler
        (apply handler operation args)
      (apply orig-fn args))))

(with-eval-after-load 'projectile
  ;; tramp-rpc currently advises these Projectile functions via a generic file
  ;; handler wrapper that assumes the first argument is always a pathname. That
  ;; breaks `projectile-get-ext-command', whose argument is a VCS symbol such as
  ;; `git', and leaves project switching stuck in a scratch buffer.
  (my/tramp-remove-named-advice
   'projectile-get-ext-command
   "tramp-advice-projectile-get-ext-command")
  (my/tramp-remove-named-advice
   'projectile-project-files
   "tramp-advice-projectile-project-files")
  (advice-add 'projectile-get-ext-command :around
              (lambda (orig-fn &rest args)
                (apply #'my/tramp-projectile-dispatch-handler-a
                       #'projectile-get-ext-command orig-fn args))
              '((name . "my/tramp-projectile-get-ext-command")))
  (advice-add 'projectile-project-files :around
              (lambda (orig-fn &rest args)
                (apply #'my/tramp-projectile-dispatch-handler-a
                       #'projectile-project-files orig-fn args))
              '((name . "my/tramp-projectile-project-files"))))

;;; ── recentf: don't probe remote paths at startup ────────────────────────────

(with-eval-after-load 'recentf
  ;; Prevent recentf from opening SSH/RPC connections to check if recent remote
  ;; files still exist during Emacs startup.
  (setq recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude tramp-file-name-regexp)
  (add-to-list 'recentf-exclude "\\`/rpc:"))

(provide 'init-tramp)
;;; init-tramp.el ends here
