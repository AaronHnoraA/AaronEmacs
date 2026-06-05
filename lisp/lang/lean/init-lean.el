;;; init-lean.el --- Lean 4 on eglot -*- lexical-binding: t -*-

;;; Commentary:
;; Defines lean-mode derived from prog-mode.  Reuses lean4-mode's proven
;; syntax/indent/unicode-input building blocks without pulling in lean4-mode.el's
;; lsp-mode client registration.  LSP driven by eglot.  Custom Lean notifications
;; ($/lean/fileProgress) live in init-lean-eglot.el.
;; xwidget infoview in init-lean-infoview.el.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)

;;; ── Package: lean4-mode kept for its sub-files only ─────────────────────────

;; We require only the sub-files (lean4-syntax, lean4-eri, lean4-input) and
;; the data/abbreviations.json.  lean4-mode.el itself is NEVER loaded; doing so
;; would trigger its lsp-mode client registration.
(my/package-ensure-vc 'lean4-mode
                      "https://github.com/leanprover-community/lean4-mode.git")

;; Try to load lean4 sub-files eagerly; silently skipped during batch-compile
;; when elpa is not on the path — they are always available at runtime.
(require 'lean4-syntax nil t)
(require 'lean4-eri    nil t)
(require 'lean4-input  nil t)

;;; ── Forward declarations ─────────────────────────────────────────────────────

(declare-function lean4-eri-indent "lean4-eri")
(declare-function lean4-eri-indent-reverse "lean4-eri")
(declare-function quail-show-key "quail")
(declare-function my/package-ensure-vc "init-package-vc")
(declare-function my/register-eglot-server-program "init-lsp")
(declare-function my/project-current-root "init-project")
(declare-function my/symbols-make-file-line-candidate "init-symbols")
(declare-function my/symbols-read-file-line-candidates "init-symbols")
(declare-function my/symbols-register-project-fallback "init-symbols")
(declare-function eldoc-box-hover-at-point-mode "eldoc-box" (&optional arg))
(declare-function eldoc-doc-buffer "eldoc" ())
(declare-function eldoc-mode "eldoc" (&optional arg))
(declare-function eglot "eglot" (managed-major-modes project class contact language-ids
                                                     &optional interactive))
(declare-function eglot-code-actions "eglot" ())
(declare-function eglot-reconnect "eglot")
(declare-function eglot-ensure "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function flymake-mode "flymake" (&optional arg))
(declare-function flymake-start "flymake" (&optional report-fn))
(declare-function lean-setup-flymake-backend "init-lean-eglot" ())
(declare-function lean-setup-sideline "init-lean-eglot" ())
(declare-function lean-refresh-file-dependencies "init-lean-eglot")
(declare-function lean--clear-fringe-overlays "init-lean-eglot")
(declare-function lean-iv-sync-cursor-h "init-lean-infoview")
(declare-function lean-iv-setup-buffer-sync "init-lean-infoview")
(declare-function lean-iv-teardown-h "init-lean-infoview")
(declare-function lean-iv-toggle "init-lean-infoview")
(declare-function lean-iv-restart "init-lean-infoview")
(declare-function lean-iv-node-p "init-lean-infoview")
(defvar lean--iv--script-dir)
(declare-function my/diagnostics-dispatch "init-diagnostics-extra" ())
(declare-function my/flymake-diagnostic-at-point-mode "init-lsp" (&optional arg))
(declare-function my/problems-buffer "init-problems" ())

;; Forward defvar declarations for variables defined in sibling modules
(defvar eldoc-box-hover-at-point-mode)
(defvar eldoc-mode)
(defvar lean--file-progress)
(defvar lean--flymake-counts)
(defvar lean--fringe-overlays)
(defvar eglot-connect-timeout)
(defvar eglot-lsp-context)
(defvar flymake-fringe-indicator-position)
(defvar flymake-mode)
(defvar project-find-functions)
;; From lean4-syntax (loaded above; defvar silences byte-compile for callers)
(defvar lean4-syntax-table)
(defvar lean4-font-lock-defaults)

;;; ── Defcustoms ───────────────────────────────────────────────────────────────

(defgroup lean nil
  "Lean 4 editing support."
  :group 'languages)

(defcustom lean-eglot-connect-timeout 180
  "Seconds before timing out Lean Eglot initialization.
Mathlib projects can legitimately take longer than Eglot's default 30 seconds
while Lake warms the environment.  Set this to nil to never time out."
  :type '(choice (const :tag "Never time out" nil)
                 (integer :tag "Seconds"))
  :group 'lean)

(defcustom lean-eglot-start-delay 0.35
  "Seconds to wait before automatically starting Lean Eglot.
This keeps freshly opened Mathlib buffers responsive while still starting the
language server without user action."
  :type 'number
  :group 'lean)

(defcustom lean-info-window-width 84
  "Width for Lean xwidget infoview side windows."
  :type 'integer
  :group 'lean)

(defcustom lean-dev-log-enabled nil
  "When non-nil, write Lean integration events to `lean-dev-log-buffer-name'."
  :type 'boolean
  :group 'lean)

(defcustom lean-dev-log-buffer-name "*Lean Dev Log*"
  "Buffer name for Lean integration diagnostics."
  :type 'string
  :group 'lean)

;;; ── Development log ─────────────────────────────────────────────────────────

(define-derived-mode lean-dev-log-mode special-mode "Lean Dev Log"
  "Mode for Lean integration diagnostics.")

(defun lean-dev-log-buffer ()
  "Return the Lean development log buffer."
  (let ((buf (get-buffer-create lean-dev-log-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'lean-dev-log-mode)
        (lean-dev-log-mode)))
    buf))

(defun lean-dev-log (format-string &rest args)
  "Append FORMAT-STRING with ARGS to `lean-dev-log-buffer-name'."
  (when lean-dev-log-enabled
    (let ((buf (lean-dev-log-buffer)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (follow (= (point) (point-max))))
          (goto-char (point-max))
          (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
          (insert (condition-case err
                      (apply #'format format-string args)
                    (error
                     (format "log formatting error: %S; format=%S args=%S"
                             err format-string args))))
          (insert "\n")
          (when follow
            (goto-char (point-max))))))))

(defun lean-dev-log-open ()
  "Open the Lean development log buffer."
  (interactive)
  (pop-to-buffer (lean-dev-log-buffer)))

;;; ── Project root helpers ─────────────────────────────────────────────────────

(defun lean-root-dir-p (dir)
  "Return non-nil when DIR looks like a Lean project root."
  (or (file-exists-p (expand-file-name "lakefile.lean" dir))
      (file-exists-p (expand-file-name "lakefile.toml" dir))
      (file-exists-p (expand-file-name "lean-toolchain" dir))))

(defun lean-project-root ()
  "Return the Lean project root for the current buffer."
  (or (when-let* ((f (or buffer-file-name default-directory)))
        (locate-dominating-file f #'lean-root-dir-p))
      (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      default-directory))

(defun lean--project-try-eglot (dir)
  "Return a Lean project for DIR while Eglot is looking for an LSP root."
  (when (and (bound-and-true-p eglot-lsp-context)
             (derived-mode-p 'lean-mode))
    (when-let* ((root (locate-dominating-file dir #'lean-root-dir-p)))
      (cons 'lean-eglot-project
            (file-name-as-directory (expand-file-name root))))))

(cl-defmethod project-root ((project (head lean-eglot-project)))
  "Return the root directory for a Lean Eglot PROJECT."
  (cdr project))

(defun lean--install-project-finder ()
  "Install Lean's Eglot project finder before broader project backends."
  (setq project-find-functions
        (cons #'lean--project-try-eglot
              (remove #'lean--project-try-eglot project-find-functions))))

(with-eval-after-load 'project
  (lean--install-project-finder))

(defun lean--eglot-project-root-candidate ()
  "Return the project root Eglot should use for the current Lean buffer."
  (let ((eglot-lsp-context t))
    (when-let* ((project (project-current nil)))
      (project-root project))))

;;; ── Eglot server contact ─────────────────────────────────────────────────────

(defcustom lean-infoview-proxy-enabled t
  "When non-nil, route Eglot through lean-proxy.mjs for infoview support.
The proxy is a transparent JSON-RPC passthrough to lake serve; it also
serves the official @leanprover/infoview over HTTP+SSE so a single Lean
LSP session drives both editing and the xwidget infoview.
Set to nil for a direct, no-proxy lake serve connection."
  :type 'boolean
  :group 'lean)

(defun lean--proxy-script ()
  "Return the absolute path to lean-proxy.mjs, or nil if not found."
  (let ((script (expand-file-name "lean-proxy.mjs" lean--iv--script-dir)))
    (when (file-exists-p script) script)))

(defun lean--proxy-available-p ()
  "Return non-nil if the infoview proxy can be used for the current buffer."
  (and lean-infoview-proxy-enabled
       (executable-find "node")
       (lean--proxy-script)
       (lean-iv-node-p)           ; dist/index.html must be built
       (not (file-remote-p default-directory))))

(defun lean--proxy-port-file (root)
  "Return the port-file path for ROOT's proxy instance."
  (let* ((hash (md5 (expand-file-name root)))
         (dir  (expand-file-name "lean" (or (bound-and-true-p no-littering-var-directory)
                                             (expand-file-name "var" user-emacs-directory)))))
    (expand-file-name (format "infoview-%s.json" (substring hash 0 12)) dir)))

(defun lean--server-contact (&optional _interactive _project)
  "Return the Lean LSP server command for the current buffer.
When `lean-infoview-proxy-enabled' is non-nil and the proxy script and
dist/ bundle are present, routes Eglot through lean-proxy.mjs so the
infoview shares the single Lean LSP session.  Falls back to a direct
lake serve / lean --server connection otherwise."
  (let* ((root    (file-name-as-directory
                   (expand-file-name (lean-project-root))))
         (in-lake (locate-dominating-file
                   (or buffer-file-name default-directory ".") #'lean-root-dir-p))
         (direct  (if in-lake
                      '("lake" "serve")
                    (list (or (executable-find "lean") "lean") "--server"))))
    (if (lean--proxy-available-p)
        (let ((script    (lean--proxy-script))
              (port-file (lean--proxy-port-file root))
              (downstream direct))
          (lean-dev-log "server-contact: proxy root=%s port-file=%s downstream=%S"
                        root port-file downstream)
          `("node" ,script
            "--root"      ,root
            "--port-file" ,port-file
            "--"          ,@downstream))
      (lean-dev-log "server-contact: direct contact=%S" direct)
      direct)))

;;; ── Mode-line progress ───────────────────────────────────────────────────────

(defvar-local lean--progress-mode-line-string nil)

(defun lean-progress-kind (item)
  "Return the :kind field from a fileProgress ITEM (plist)."
  (plist-get item :kind))

(defun lean--flymake-count (kind)
  "Return cached Lean Flymake diagnostic count for KIND."
  (or (and (boundp 'lean--flymake-counts)
           (plist-get lean--flymake-counts kind))
      0))

(defun lean--status-segment (text face help)
  "Return a propertized mode-line TEXT with FACE and HELP."
  (propertize text
              'face face
              'help-echo help
              'mouse-face 'mode-line-highlight))

(defun lean--progress-mode-line-text ()
  "Compact mode-line string reflecting Lean progress and diagnostics."
  (let* ((items     (and (boundp 'lean--file-progress) lean--file-progress))
         (total     (length items))
         (running   (seq-count (lambda (i) (eq (lean-progress-kind i) 1)) items))
         (blocked   (- total running))
         (errors    (lean--flymake-count :error))
         (warnings  (lean--flymake-count :warning))
         (notes     (lean--flymake-count :note))
         (help      (format "Lean: processing=%d, blocked=%d, errors=%d, warnings=%d, notes=%d"
                            running blocked errors warnings notes))
         parts)
    (when (> running 0)
      (push (lean--status-segment (format "…%d" running) 'warning help) parts))
    (when (> blocked 0)
      (push (lean--status-segment (format "!%d" blocked) 'error help) parts))
    (when (> errors 0)
      (push (lean--status-segment (format "E%d" errors) 'error help) parts))
    (when (> warnings 0)
      (push (lean--status-segment (format "W%d" warnings) 'warning help) parts))
    (when (> notes 0)
      (push (lean--status-segment (format "N%d" notes) 'success help) parts))
    (if parts
        (concat " λ[" (string-join (nreverse parts) " ") "]")
      (lean--status-segment " λ✓" 'success help))))

(defun lean-progress-mode-line-refresh ()
  "Update mode-line Lean progress indicator."
  (setq lean--progress-mode-line-string (lean--progress-mode-line-text))
  (force-mode-line-update t))

(define-minor-mode lean-progress-mode-line-mode
  "Show Lean elaboration state in the mode line."
  :lighter (:eval lean--progress-mode-line-string)
  (if lean-progress-mode-line-mode
      (lean-progress-mode-line-refresh)
    (setq lean--progress-mode-line-string nil)))

;;; ── AaronNote LSP UI integration ────────────────────────────────────────────

(defvar-local lean--eglot-start-timer nil
  "Timer used to start Eglot for the current Lean buffer.")

(defun lean--cancel-eglot-start-timer ()
  "Cancel a pending Lean Eglot startup timer."
  (when (timerp lean--eglot-start-timer)
    (cancel-timer lean--eglot-start-timer))
  (setq lean--eglot-start-timer nil))

(defun lean--setup-diagnostics-ui ()
  "Enable AaronNote diagnostics UI pieces for the current Lean buffer."
  (when (boundp 'flymake-fringe-indicator-position)
    (setq-local flymake-fringe-indicator-position 'right-fringe))
  (when (fboundp 'flymake-mode)
    (flymake-mode 1))
  (when (fboundp 'my/flymake-diagnostic-at-point-mode)
    (my/flymake-diagnostic-at-point-mode -1)))

(defun lean--setup-managed-ui ()
  "Enable Lean-specific UI after Eglot starts managing this buffer.
Completion uses the global corfu+capf surface — no company-mode override."
  (when (fboundp 'lean-setup-flymake-backend)
    (lean-setup-flymake-backend))
  (when (fboundp 'lean-setup-sideline)
    (lean-setup-sideline))
  (lean--setup-diagnostics-ui)
  (when (fboundp 'eldoc-mode)
    (eldoc-mode 1))
  (when (fboundp 'eldoc-box-hover-at-point-mode)
    (eldoc-box-hover-at-point-mode 1))
  (when (and (fboundp 'flymake-start)
             (bound-and-true-p flymake-mode))
    (flymake-start))
  (lean-dev-log "eglot UI active: flymake=%S eldoc=%S eldoc-box=%S fringe=%S"
                (bound-and-true-p flymake-mode)
                (bound-and-true-p eldoc-mode)
                (bound-and-true-p eldoc-box-hover-at-point-mode)
                (and (boundp 'flymake-fringe-indicator-position)
                     flymake-fringe-indicator-position)))

;;; ── Project symbol search (ripgrep) ──────────────────────────────────────────

(defvar lean--project-symbol-history nil)

(defconst lean--rg-regexp
  (concat
   "^[[:space:]]*"
   "(?:@[[:alnum:]_.]+[[:space:]]+)*"
   "(?:(?:private|protected|noncomputable|unsafe|partial|scoped|local)"
   "[[:space:]]+)*"
   "(?:class[[:space:]]+inductive|inductive|instance|structure|class|theorem"
   "|axiom|lemma|definition|def|constant|abbrev|opaque)\\b")
  "Ripgrep pattern for top-level Lean 4 declarations.")

(defun lean--declaration-kind-and-name (text)
  "Extract (kind . name) from a Lean declaration line TEXT."
  (when (string-match
         (rx string-start
             (* (any " \t"))
             (* (seq "@" (+ (not (any " \t\n\r"))) (+ blank)))
             (* (seq (or "private" "protected" "noncomputable"
                         "unsafe" "partial" "scoped" "local")
                     (+ blank)))
             (group (or (seq "class" (+ blank) "inductive")
                        "inductive" "instance" "structure" "class"
                        "theorem" "axiom" "lemma" "definition" "def"
                        "constant" "abbrev" "opaque"))
             (+ blank)
             (group (+ (not (any " \t\n\r:={([,")))))
         text)
    (cons (match-string 1 text) (match-string 2 text))))

(defun lean--project-symbol-candidates ()
  "Return file-line candidates for Lean declarations in the current project."
  (when-let* ((rg   (executable-find "rg"))
              (root (file-name-as-directory (expand-file-name (lean-project-root)))))
    (cl-loop for hit in
             (let ((default-directory root))
               (condition-case nil
                   (process-lines rg "--color" "never" "--line-number"
                                  "--no-heading" "--glob" "*.lean"
                                  "-e" lean--rg-regexp ".")
                 (error nil)))
             when (string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\(.*\\)\\'" hit)
             for file    = (match-string 1 hit)
             for line    = (string-to-number (match-string 2 hit))
             for text    = (string-trim (match-string 3 hit))
             for kn      = (lean--declaration-kind-and-name text)
             for summary = (if kn (format "%s %s" (car kn) (cdr kn)) text)
             collect (my/symbols-make-file-line-candidate
                      root (expand-file-name file root) line summary))))

(defun lean-project-symbols-fallback ()
  "Fallback project symbol search for Lean files (used by my/symbols)."
  (my/symbols-read-file-line-candidates
   (lean--project-symbol-candidates)
   "Lean symbol: "
   'lean--project-symbol-history
   'lean-project-symbol))

;;; ── Keybindings ──────────────────────────────────────────────────────────────

(defun lean--setup-keys ()
  "Install Lean-specific keys in the current buffer."
  ;; Official Lean infoview rendered in xwidget-webkit.
  (local-set-key (kbd "C-c C-i") #'lean-iv-toggle)
  (local-set-key (kbd "C-c i r") #'lean-iv-restart)
  ;; LSP management
  (local-set-key (kbd "C-c C-r") #'eglot-reconnect)
  (local-set-key (kbd "C-c C-d") #'lean-refresh-file-dependencies)
  (local-set-key (kbd "C-c C-a") #'eglot-code-actions)
  (local-set-key (kbd "C-c C-e") #'eldoc-doc-buffer)
  (local-set-key (kbd "C-c C-l") #'lean-dev-log-open)
  (local-set-key (kbd "C-c !")   #'my/problems-buffer)
  (local-set-key (kbd "C-c ?")   #'my/diagnostics-dispatch)
  ;; Unicode / input
  (local-set-key (kbd "C-c C-k") #'quail-show-key)
  ;; Indentation
  (local-set-key (kbd "TAB")       #'lean4-eri-indent)
  (local-set-key (kbd "<backtab>") #'lean4-eri-indent-reverse))

;;; ── Remote tuning ────────────────────────────────────────────────────────────

(defun lean--tune-remote ()
  "Apply Lean editing tweaks for TRAMP buffers."
  nil)

(defun lean--apply-eglot-settings ()
  "Install Lean-specific Eglot settings in the current buffer."
  (when (boundp 'eglot-connect-timeout)
    (setq-local eglot-connect-timeout lean-eglot-connect-timeout))
  (lean-dev-log "lean-mode setup: file=%s root=%s contact=%S eglot-connect-timeout=%S"
                (or buffer-file-name "<no file>")
                (file-name-as-directory (expand-file-name (lean-project-root)))
                (lean--server-contact)
                (and (boundp 'eglot-connect-timeout) eglot-connect-timeout)))

;;; ── Post-command hook: infoview cursor sync ──────────────────────────────────

(defun lean--post-command-h ()
  "On cursor movement in a Lean buffer, sync the xwidget infoview."
  (when (derived-mode-p 'lean-mode)
    (when (fboundp 'lean-iv-sync-cursor-h)
      (lean-iv-sync-cursor-h))))

;;; ── Major mode ───────────────────────────────────────────────────────────────

;;;###autoload
(define-derived-mode lean-mode prog-mode "Lean"
  "Major mode for Lean 4 source files.

Reuses lean4-mode's syntax table, font-lock keywords, indentation
engine (lean4-eri), and the \\='Lean\\=' Quail unicode-input method,
while using eglot (not lsp-mode) as the language server backend."
  :group 'lean
  ;; Ensure lean4 sub-files are loaded (idempotent — already loaded if elpa is ready)
  (require 'lean4-syntax nil t)
  (require 'lean4-eri    nil t)
  (require 'lean4-input  nil t)
  ;; Syntax table: copy lean4's if available, else inherit prog-mode's
  (when (and (boundp 'lean4-syntax-table) lean4-syntax-table)
    (set-syntax-table lean4-syntax-table))
  ;; Comments
  (setq-local comment-start       "--")
  (setq-local comment-start-skip  "[-/]-[ \t]*")
  (setq-local comment-end         "")
  (setq-local comment-end-skip    "[ \t]*\\(-/\\|\\s>\\)")
  (setq-local comment-padding     1)
  (setq-local comment-use-syntax  t)
  ;; Font-lock / indentation
  (setq-local font-lock-defaults  lean4-font-lock-defaults)
  (setq-local indent-tabs-mode    nil)
  (setq-local lisp-indent-function #'common-lisp-indent-function)
  ;; Unicode input (lean4-input defines the "Lean" Quail package)
  (set-input-method "Lean")
  ;; No electric indent — Lean's structural indentation is managed by lean4-eri
  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode -1))
  ;; Trim trailing whitespace on save
  (add-hook 'before-save-hook #'whitespace-cleanup nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lean\\'" . lean-mode))

;;; ── Mode hook ────────────────────────────────────────────────────────────────

(defun lean--mode-hook ()
  "Full lean-mode setup installed via lean-mode-hook."
  ;; Sibling modules — loaded here so they are ready before eglot connects.
  (require 'init-lean-eglot    nil t)
  (require 'init-lean-infoview nil t)
  ;; Projectile may install its project backend after init-lean loads.  Re-pin
  ;; the Lean finder before Eglot asks project.el for an LSP root.
  (lean--install-project-finder)
  ;; Keys
  (lean--setup-keys)
  ;; Eglot startup policy
  (lean--apply-eglot-settings)
  ;; Mode-line progress indicator
  (lean-progress-mode-line-mode 1)
  ;; AaronNote Flymake diagnostics UI; managed-mode hook completes the rest.
  (lean--setup-diagnostics-ui)
  ;; Remote tuning
  (lean--tune-remote)
  ;; Keep the infoview bridge's shadow LSP document in sync while visible.
  (when (fboundp 'lean-iv-setup-buffer-sync)
    (lean-iv-setup-buffer-sync))
  ;; Post-command: goal refresh + infoview cursor sync
  (add-hook 'post-command-hook #'lean--post-command-h nil t)
  ;; Buffer teardown
  (add-hook 'kill-buffer-hook  #'lean--buffer-teardown-h nil t)
  ;; Start Eglot shortly after the mode hook returns.  This keeps the initial
  ;; file visit responsive but still starts `lake serve' automatically.
  (lean--schedule-eglot-start))

(defun lean--schedule-eglot-start ()
  "Schedule automatic Eglot startup for the current Lean buffer."
  (lean--cancel-eglot-start-timer)
  (let ((buf (current-buffer))
        (delay (max 0 (or lean-eglot-start-delay 0))))
    (lean-dev-log "eglot scheduled: delay=%s buffer=%s"
                  delay (buffer-name buf))
    (setq lean--eglot-start-timer
          (run-at-time
           delay nil
           (lambda (buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq lean--eglot-start-timer nil)
                 (when (derived-mode-p 'lean-mode)
                   (lean--ensure-eglot)))))
           buf))))

(defun lean--ensure-eglot ()
  "Start eglot for the current lean-mode buffer if not already managed."
  (cond
   ((not buffer-file-name)
    (lean-dev-log "eglot skipped: buffer has no file"))
   ((file-remote-p default-directory)
    (lean-dev-log "eglot skipped for remote buffer: %s" default-directory))
   ((not (fboundp 'eglot))
    (lean-dev-log "eglot skipped: `eglot' is not available"))
   ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (lean-dev-log "eglot already managing buffer: %s" (buffer-name)))
   (t
    (lean-dev-log "eglot starting: buffer-dir=%s lsp-root=%s command=%S timeout=%S project-finders=%S"
                  default-directory
                  (or (lean--eglot-project-root-candidate) "<none>")
                  (lean--server-contact)
                  (and (boundp 'eglot-connect-timeout) eglot-connect-timeout)
                  project-find-functions)
    (when (and (boundp 'eglot-connect-timeout)
               (numberp eglot-connect-timeout))
      (let ((buf (current-buffer))
            (timeout eglot-connect-timeout))
        (run-at-time
         (1+ timeout) nil
         (lambda ()
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when (and (derived-mode-p 'lean-mode)
                          (not (and (fboundp 'eglot-managed-p)
                                    (eglot-managed-p))))
                 (lean-dev-log
                  "eglot not connected after %s seconds; check Eglot stderr and Messages buffers"
                  timeout))))))))
    (condition-case err
        (call-interactively #'eglot)
      (error
       (lean-dev-log "eglot start error: %s" (error-message-string err))
       (signal (car err) (cdr err)))))))

(add-hook 'lean-mode-hook #'lean--mode-hook)

(defun lean--buffer-teardown-h ()
  "Cancel timers, clear overlays, and close the infoview on buffer kill."
  (lean--cancel-eglot-start-timer)
  (when (fboundp 'lean--clear-fringe-overlays)
    (lean--clear-fringe-overlays))
  (when (fboundp 'lean-iv-teardown-h)
    (lean-iv-teardown-h)))

;;; ── Eglot server registration ────────────────────────────────────────────────

(defun lean--eglot-connect-log-h (_server)
  "Log successful Lean Eglot connections."
  (when (derived-mode-p 'lean-mode)
    (lean-dev-log "eglot connected: buffer=%s root=%s"
                  (buffer-name)
                  (file-name-as-directory (expand-file-name (lean-project-root))))))

(defun lean--eglot-managed-mode-log-h ()
  "Log when Eglot activates editing support in a Lean buffer."
  (when (derived-mode-p 'lean-mode)
    (lean--setup-managed-ui)
    (lean-dev-log "eglot managed mode active: buffer=%s lsp-root=%s flymake=%S eldoc=%S capf=%S"
                  (buffer-name)
                  (or (lean--eglot-project-root-candidate) "<none>")
                  (bound-and-true-p flymake-mode)
                  (bound-and-true-p eldoc-mode)
                  completion-at-point-functions)))

(with-eval-after-load 'eglot
  (add-hook 'eglot-connect-hook #'lean--eglot-connect-log-h)
  (add-hook 'eglot-managed-mode-hook #'lean--eglot-managed-mode-log-h)
  (when (fboundp 'my/register-eglot-server-program)
    (my/register-eglot-server-program
     '(lean-mode :language-id "lean4")
     #'lean--server-contact
     :executables '("lean" "lake")
     :label "Lean Language Server"
     :source "lean4-mode (sub-files only)"
     :note "Uses lake serve when a lakefile is found, else lean --server")))

;;; ── Project symbol fallback ──────────────────────────────────────────────────

(with-eval-after-load 'init-symbols
  (my/symbols-register-project-fallback 'lean-mode
                                        #'lean-project-symbols-fallback))

(provide 'init-lean)
;;; init-lean.el ends here
