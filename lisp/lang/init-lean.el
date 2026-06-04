;;; init-lean.el --- Lean4 config with upstream lsp-mode integration -*- lexical-binding: t -*-

;;; Commentary:
;; Lean 4 works best with the upstream `lean4-mode' + `lsp-mode' integration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(my/package-ensure-installed-list '(dash magit-section lsp-mode))
(my/package-ensure-vc 'lean4-mode "https://github.com/leanprover-community/lean4-mode.git")

(defvar lean4-mode-hook)
(declare-function lean4-lake-find-dir "lean4-lake")
(declare-function lean4-get-executable "lean4-util" (exe-name))
(declare-function lsp-workspace-root "lsp-mode" (&optional path))
(declare-function my/language-server-executable-find "init-lsp" (program))
(declare-function my/project-current-root "init-project")
(declare-function my/symbols-make-file-line-candidate "init-symbols"
                  (root file line match))
(declare-function my/symbols-read-file-line-candidates "init-symbols"
                  (candidates &optional prompt history category))
(declare-function my/symbols-register-project-fallback "init-symbols"
                  (mode function))

(defvar my/lean-project-symbol-history nil)
(defvar lean4-executable-name)
(defvar lean4-fringe-data)
(defvar lean4-show-file-progress)
(defvar company-idle-delay)
(defvar flycheck-after-syntax-check-hook)
(defvar flymake-no-changes-timeout)
(defvar lsp-eldoc-enable-hover)
(defvar lsp-enable-file-watchers)
(defvar lsp-enable-on-type-formatting)
(defvar lsp-enable-symbol-highlighting)
(defvar lsp-file-watch-threshold)
(defvar lsp-idle-delay)
(defvar lsp-inlay-hint-enable)
(defvar lsp-warn-no-matched-clients)
(defvar lsp-semantic-tokens-enable)
(defvar lean4-info-buffer-debounce-delay-sec)
(defvar lean4-info-buffer-debounce-upper-bound-sec)
(defvar-local lean4-fringe-delay-timer nil)
(defvar-local lean4-rootdir nil)
(defvar-local my/lean4-remote-ui-error-reported nil)
(defvar-local my/lean4-info-source-buffer nil)
(defvar-local my/lean4-progress-mode-line-string nil)

(declare-function lean4-info-buffer-refresh "lean4-info" ())
(declare-function lean4-info-buffer-redisplay-debounced "lean4-info" ())
(declare-function lean4-toggle-info "lean4-info" ())
(declare-function lean4-refresh-file-dependencies "lean4-mode" ())
(declare-function lsp-workspace-restart "lsp-mode" (&optional workspace))
(declare-function magit-current-section "magit-section" ())
(declare-function magit-section-toggle "magit-section" (section))
(declare-function magit-section-forward "magit-section" ())
(declare-function magit-section-backward "magit-section" ())

(defcustom my/lean4-info-max-current-messages 12
  "Maximum diagnostics rendered in the expanded current-message section."
  :type 'integer
  :group 'my/language-server)

(defcustom my/lean4-info-max-context-messages 6
  "Maximum diagnostics rendered in collapsed above/below message sections."
  :type 'integer
  :group 'my/language-server)

(defcustom my/lean4-full-ui-local t
  "Enable richer Lean UI features in local buffers."
  :type 'boolean
  :group 'my/language-server)

(defcustom my/lean4-info-window-width 84
  "Preferred width for the Lean info side window."
  :type 'integer
  :group 'my/language-server)

(defun my/lean4-remote-rootdir ()
  "Return Lean's remote executable root as a plain Unix path."
  (when-let* ((executable (ignore-errors
                            (my/language-server-executable-find
                             lean4-executable-name)))
              ((stringp executable))
              (rootdir (file-name-directory
                        (directory-file-name
                         (file-name-directory executable)))))
    (file-name-as-directory rootdir)))

(defconst my/lean-project-symbol-rg-regexp
  (concat
   "^[[:space:]]*"
   "(?:@[[:alnum:]_.]+[[:space:]]+)*"
   "(?:(?:private|protected|noncomputable|unsafe|partial|scoped|local)"
   "[[:space:]]+)*"
   "(?:class[[:space:]]+inductive|inductive|instance|structure|class|theorem"
   "|axiom|lemma|definition|def|constant|abbrev|opaque)\\\\b")
  "Ripgrep regexp used to discover top-level Lean declarations.")

(defun my/lean-root-dir-p (dir)
  "Return non-nil when DIR looks like a Lean project root."
  (or (file-exists-p (expand-file-name "lakefile.lean" dir))
      (file-exists-p (expand-file-name "lakefile.toml" dir))
      (file-exists-p (expand-file-name "lean-toolchain" dir))))

(defun my/lean-project-root ()
  "Return the current Lean project root."
  (or (and (fboundp 'lsp-workspace-root)
           (ignore-errors (lsp-workspace-root)))
      (and (fboundp 'lean4-lake-find-dir)
           (ignore-errors (lean4-lake-find-dir)))
      (when-let* ((file (or buffer-file-name default-directory)))
        (locate-dominating-file file #'my/lean-root-dir-p))
      (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      default-directory))

(defun my/lean--declaration-kind-and-name (text)
  "Extract declaration kind and name from Lean declaration TEXT."
  (when (string-match
         (rx string-start
             (* (any " \\t"))
             (* (seq "@" (+ (not (any " \\t\\n\\r"))) (+ blank)))
             (* (seq (or "private"
                         "protected"
                         "noncomputable"
                         "unsafe"
                         "partial"
                         "scoped"
                         "local")
                     (+ blank)))
             (group (or (seq "class" (+ blank) "inductive")
                        "inductive"
                        "instance"
                        "structure"
                        "class"
                        "theorem"
                        "axiom"
                        "lemma"
                        "definition"
                        "def"
                        "constant"
                        "abbrev"
                        "opaque"))
             (+ blank)
             (group (+ (not (any " \\t\\n\\r:={([,")))))
         text)
    (cons (match-string 1 text)
          (match-string 2 text))))

(defun my/lean--project-symbol-lines (root)
  "Return ripgrep declaration hits under Lean project ROOT."
  (when-let* ((rg (executable-find "rg")))
    (let ((default-directory root))
      (condition-case nil
          (process-lines
           rg
           "--color" "never"
           "--line-number"
           "--no-heading"
           "--glob" "*.lean"
           "-e" my/lean-project-symbol-rg-regexp
           ".")
        (error nil)))))

(defun my/lean--project-symbol-candidates ()
  "Return file-line candidates for Lean declarations in the current project."
  (let ((root (file-name-as-directory (expand-file-name (my/lean-project-root)))))
    (cl-loop for hit in (my/lean--project-symbol-lines root)
             when (string-match "\\\\`\\\\([^:]+\\\\):\\\\([0-9]+\\\\):\\\\(.*\\\\)\\\\'" hit)
             for relative-file = (match-string 1 hit)
             for line = (string-to-number (match-string 2 hit))
             for text = (string-trim (match-string 3 hit))
             for kind-and-name = (my/lean--declaration-kind-and-name text)
             for summary = (if kind-and-name
                               (format "%s %s"
                                       (car kind-and-name)
                                       (cdr kind-and-name))
                             text)
             collect (my/symbols-make-file-line-candidate
                      root
                      (expand-file-name relative-file root)
                      line
                      summary))))

(defun my/lean-project-symbols-fallback ()
  "Fallback project symbol search for Lean projects."
  (my/symbols-read-file-line-candidates
   (my/lean--project-symbol-candidates)
   "Lean project symbol: "
   'my/lean-project-symbol-history
   'my-lean-project-symbol))

(with-eval-after-load 'magit-mode
  ;; `magit-region-highlight-hook' may reference
  ;; `magit-diff-update-hunk-region' before `magit-diff' is loaded.
  (require 'magit-diff nil t))

(defvar magit-region-highlight-hook)

(defun my/lean4-info-buffer ()
  "Return the Lean goal buffer, if it exists."
  (get-buffer "*Lean Goal*"))

(defun my/lean4-info-source-buffer ()
  "Return the source Lean buffer associated with the current info buffer."
  (cond
   ((derived-mode-p 'lean4-mode)
    (current-buffer))
   ((buffer-live-p my/lean4-info-source-buffer)
    my/lean4-info-source-buffer)
   (t
    (seq-find (lambda (buffer)
                (with-current-buffer buffer
                  (derived-mode-p 'lean4-mode)))
              (buffer-list)))))

(defun my/lean4-info-buffer-setup (&rest _)
  "Adjust the Lean goal buffer after it is created or shown."
  (let ((buffer (my/lean4-info-buffer))
        (source (and (derived-mode-p 'lean4-mode)
                     (current-buffer))))
    (when buffer
      (with-current-buffer buffer
        ;; Lean's info buffer uses `magit-section-mode' for rendering, but it
        ;; is not a diff buffer and does not need Magit's diff-specific region
        ;; highlighting hook.
        (setq-local magit-region-highlight-hook nil)
        ;; `lean4-info-mode' derives from `prog-mode', but it is a rendered UI
        ;; buffer.  It should never try to attach its own language server.
        (setq-local lsp-warn-no-matched-clients nil)
        (when (buffer-live-p source)
          (setq-local my/lean4-info-source-buffer source))))))

(defun my/lean4-display-info-buffer ()
  "Show the Lean info view in a stable right-side window."
  (interactive)
  (let ((source (current-buffer)))
    (lean4-toggle-info)
    (when-let* ((buffer (my/lean4-info-buffer)))
      (with-current-buffer buffer
        (setq-local my/lean4-info-source-buffer source))
      (display-buffer-in-side-window
       buffer
       `((side . right)
         (slot . 1)
         (window-width . ,my/lean4-info-window-width)
         (dedicated . t)))
      (with-current-buffer source
        (when (fboundp 'lean4-info-buffer-refresh)
          (lean4-info-buffer-refresh))))))

(defun my/lean4-info-refresh ()
  "Refresh the Lean info view from its source buffer."
  (interactive)
  (if-let* ((source (my/lean4-info-source-buffer)))
      (with-current-buffer source
        (lean4-info-buffer-refresh))
    (user-error "No Lean source buffer associated with this info view")))

(defun my/lean4-info-jump-to-source ()
  "Jump from the Lean info view back to its source buffer."
  (interactive)
  (if-let* ((source (my/lean4-info-source-buffer)))
      (pop-to-buffer source)
    (user-error "No Lean source buffer associated with this info view")))

(defun my/lean4-info-ret ()
  "Activate buttons or toggle sections in the Lean info view."
  (interactive)
  (cond
   ((button-at (point))
    (push-button))
   ((fboundp 'magit-section-toggle)
    (magit-section-toggle (magit-current-section)))
   (t
    (user-error "No action at point"))))

(defun my/lean4-info-mode-keys ()
  "Install practical keys for the Lean info buffer."
  (local-set-key (kbd "g") #'my/lean4-info-refresh)
  (local-set-key (kbd "RET") #'my/lean4-info-ret)
  (local-set-key (kbd "TAB") #'magit-section-toggle)
  (local-set-key (kbd "n") #'magit-section-forward)
  (local-set-key (kbd "p") #'magit-section-backward)
  (local-set-key (kbd "o") #'my/lean4-info-jump-to-source)
  (local-set-key (kbd "q") #'quit-window))

(defun my/lean4-mode-keys ()
  "Install project-oriented Lean editing keys after `lean4-mode' setup."
  (local-set-key (kbd "C-c C-i") #'my/lean4-display-info-buffer)
  (local-set-key (kbd "C-c C-g") #'my/lean4-display-info-buffer)
  (local-set-key (kbd "C-c C-u") #'my/lean4-toggle-rich-ui)
  (local-set-key (kbd "C-c C-r") #'lsp-workspace-restart)
  (local-set-key (kbd "C-c C-d") #'lean4-refresh-file-dependencies)
  (local-set-key (kbd "C-c C-k") #'quail-show-key))

(with-eval-after-load 'lean4-info
  (advice-add 'lean4-ensure-info-buffer :after #'my/lean4-info-buffer-setup)
  (advice-add 'lean4-toggle-info-buffer :after #'my/lean4-info-buffer-setup)
  (add-hook 'lean4-info-mode-hook #'my/lean4-info-mode-keys)
  (add-to-list 'my/language-server-disabled-modes 'lean4-info-mode)
  (setq lean4-info-buffer-debounce-delay-sec 0.2
        lean4-info-buffer-debounce-upper-bound-sec 0.8)

  (define-advice lean4-info--mk-message-section
      (:override (value caption messages buffer) my/fold-noisy-context)
    "Render Lean diagnostics compactly in the info view.

The local section is expanded because it is usually actionable.  Context
sections above and below point are collapsed and capped so old diagnostics do
not dominate redisplay time or screen space."
    (when-let* ((msgs messages))
      (let* ((context-p (memq value '(errors-above errors-below)))
             (limit (if context-p
                        my/lean4-info-max-context-messages
                      my/lean4-info-max-current-messages))
             (total (length msgs))
             (trimmed
              (cond
               ((<= total limit) msgs)
               ((eq value 'errors-above)
                (last msgs limit))
               (t
                (cl-subseq msgs 0 limit))))
             (heading (if (> total (length trimmed))
                          (format "%s (%d, showing %d):"
                                  (string-remove-suffix ":" caption)
                                  total
                                  (length trimmed))
                        caption)))
        (magit-insert-section (magit-section value nil context-p)
          (magit-insert-heading heading)
          (magit-insert-section-body
            (dolist (e trimmed)
              (-let (((&Diagnostic :message
                                   :range (&Range :start
                                                  (&Position :line :character)))
                      e))
                (let ((ln (1+ (lsp-translate-line line)))
                      (col (lsp-translate-column character)))
                  (insert-text-button
                   (format "%d:%d:" ln col)
                   'action #'lean4-info--error-button-action
                   'button-data (list buffer ln col)
                   'face 'magit-section-heading
                   'help-echo "mouse-2: visit this file, line and column"))
                (lean4-info--insert-highlight-inaccessible-names
                 "\n" message "\n"))))))))
  )

(defun my/lean4-tune-responsive-ui ()
  "Enable a richer Lean UI while keeping refreshes reasonably debounced."
  (setq-local lsp-idle-delay 0.8
              flymake-no-changes-timeout 1.2
              company-idle-delay 0.45
              lean4-show-file-progress my/lean4-full-ui-local
              lsp-enable-file-watchers nil
              lsp-file-watch-threshold 0
              lsp-enable-symbol-highlighting my/lean4-full-ui-local
              lsp-enable-on-type-formatting nil
              lsp-inlay-hint-enable my/lean4-full-ui-local
              lsp-semantic-tokens-enable my/lean4-full-ui-local
              lsp-eldoc-enable-hover my/lean4-full-ui-local)
  (my/lean4-progress-mode-line-mode 1))

(defun my/lean4-toggle-rich-ui ()
  "Toggle rich Lean UI features in the current buffer."
  (interactive)
  (setq-local my/lean4-full-ui-local (not my/lean4-full-ui-local))
  (my/lean4-tune-responsive-ui)
  (when (bound-and-true-p lsp-managed-mode)
    (when (and (fboundp 'lsp-inlay-hints-mode)
               (boundp 'lsp-inlay-hints-mode))
      (lsp-inlay-hints-mode (if lsp-inlay-hint-enable 1 -1)))
    (when (and (fboundp 'lsp-semantic-tokens-mode)
               (boundp 'lsp-semantic-tokens-mode))
      (lsp-semantic-tokens-mode (if lsp-semantic-tokens-enable 1 -1))))
  (message "Lean rich UI %s"
           (if my/lean4-full-ui-local "enabled" "disabled")))

(defun my/lean4-setup-remote-rootdir ()
  "Make `lean4-mode' resolve Lean toolchain paths correctly over TRAMP."
  (when (file-remote-p default-directory)
    (when-let* ((rootdir (my/lean4-remote-rootdir)))
      (setq-local lean4-rootdir rootdir))))

(defun my/lean4-tune-remote-ui ()
  "Tune Lean UI for TRAMP connections: disable expensive features, slow refresh."
  (when (file-remote-p default-directory)
    ;; Fringe progress overlays have a dedicated safe wrapper; disable the
    ;; feature itself to avoid unnecessary remote traffic.
    (setq-local lean4-show-file-progress nil)
    ;; Semantic tokens add round-trip overhead with little benefit remotely.
    (setq-local lsp-semantic-tokens-enable nil)
    ;; post-command and flycheck hooks only re-render cached goal data locally —
    ;; they are cheap but generate timer churn; remove them.  The LSP idle hook
    ;; (lean4-info-buffer-refresh) is kept so C-c C-i and cursor-movement goals
    ;; still update, but we slow its cadence to avoid hammering the connection.
    (remove-hook 'post-command-hook #'lean4-info-buffer-redisplay-debounced t)
    (remove-hook 'flycheck-after-syntax-check-hook
                 #'lean4-info-buffer-redisplay-debounced t)
    ;; 2 s idle before sending $/lean/plainGoal — relaxed from the 0.5 s default.
    (setq-local lsp-idle-delay 2.0)
    ;; Cancel any pending fringe timer left over from lean4-mode startup.
    (when (timerp lean4-fringe-delay-timer)
      (cancel-timer lean4-fringe-delay-timer)
      (setq-local lean4-fringe-delay-timer nil))))

(defun my/lean4-lsp-mode-ensure-deferred ()
  "Start Lean's `lsp-mode' after mode setup has returned to the command loop."
  (let ((buffer (current-buffer)))
    (run-at-time
     0 nil
     (lambda (buf)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (my/lsp-mode-ensure))))
     buffer)))

(defun my/lean4-progress-kind (item)
  "Return Lean progress ITEM kind, across lsp-mode data representations."
  (cond
   ((hash-table-p item)
    (or (gethash "kind" item)
        (gethash :kind item)))
   ((and (consp item) (plist-member item :kind))
    (plist-get item :kind))
   ((consp item)
    (or (alist-get 'kind item)
        (alist-get :kind item)))
   (t nil)))

(defun my/lean4-progress-mode-line-text ()
  "Return compact mode-line text for Lean file progress."
  (let* ((items (and (boundp 'lean4-fringe-data)
                     lean4-fringe-data))
         (total (length items))
         (processing (seq-count
                      (lambda (item)
                        (eq (my/lean4-progress-kind item) 1))
                      items))
         (errors (- total processing)))
    (cond
     ((> errors 0)
      (propertize (format " λ✕%d" errors) 'face 'error))
     ((> processing 0)
      (propertize (format " λ…%d" processing) 'face 'warning))
     (t
      (propertize " λ✓" 'face 'success)))))

(defun my/lean4-progress-mode-line-refresh ()
  "Refresh Lean progress state in the mode line."
  (setq my/lean4-progress-mode-line-string
        (my/lean4-progress-mode-line-text))
  (force-mode-line-update t))

(define-minor-mode my/lean4-progress-mode-line-mode
  "Show Lean processing state in the mode line."
  :lighter (:eval my/lean4-progress-mode-line-string)
  (if my/lean4-progress-mode-line-mode
      (my/lean4-progress-mode-line-refresh)
    (setq my/lean4-progress-mode-line-string nil)))

(defun my/lean4-report-remote-ui-error (error)
  "Report remote Lean UI ERROR only once per buffer."
  (unless my/lean4-remote-ui-error-reported
    (setq-local my/lean4-remote-ui-error-reported t)
    (message "Lean remote UI error (non-fatal): %s"
             (error-message-string error))))

(use-package lean4-mode
  :init
  (setq lean4-mode-hook nil)
  (when (fboundp 'my/register-lsp-mode-preference)
    (my/register-lsp-mode-preference 'lean4-mode))
  (add-hook 'lean4-mode-hook #'my/lean4-setup-remote-rootdir)
  (add-hook 'lean4-mode-hook #'my/lean4-tune-responsive-ui)
  (add-hook 'lean4-mode-hook #'my/lean4-tune-remote-ui)
  (add-hook 'lean4-mode-hook #'my/lean4-mode-keys)
  (add-hook 'lean4-mode-hook #'my/lean4-lsp-mode-ensure-deferred)
  :mode ("\\\\.lean\\\\'" . lean4-mode))

(with-eval-after-load 'lean4-mode
  (define-advice lean4--server-cmd (:around (fn) my/lean4-remote-server-cmd)
    "Avoid synchronous version probes when starting Lean over TRAMP."
    (if (file-remote-p default-directory)
        (if (locate-dominating-file default-directory #'my/lean-root-dir-p)
            (list "lake" "serve")
          (list lean4-executable-name "--server"))
      (funcall fn))))

(with-eval-after-load 'lean4-fringe
  (define-advice lean4-fringe-update (:after (&rest _) my/mode-line-progress)
    "Keep Lean mode-line progress in sync with file progress notifications."
    (when (derived-mode-p 'lean4-mode)
      (my/lean4-progress-mode-line-refresh)))

  (define-advice lean4-fringe-update-progress-overlays
      (:around (fn) my/lean4-remote-safe-progress)
    "Ignore invalid remote progress overlays instead of wedging the session."
    (unwind-protect
        (if (file-remote-p default-directory)
            (condition-case err
                (funcall fn)
              (error
               (my/lean4-tune-remote-ui)
               (my/lean4-report-remote-ui-error err)))
          (funcall fn))
      (when (derived-mode-p 'lean4-mode)
        (my/lean4-progress-mode-line-refresh)))))

(with-eval-after-load 'lean4-info
  (define-advice lean4-info-buffer-redisplay
      (:around (fn) my/lean4-remote-safe-info)
    "Swallow rendering errors in the Lean goal buffer.
Unlike the fringe advice, a transient render failure should NOT permanently
kill the info buffer, so we just log the first occurrence and continue."
    (condition-case err
        (funcall fn)
      (error
       (my/lean4-report-remote-ui-error err)))))

(with-eval-after-load 'init-symbols
  (my/symbols-register-project-fallback 'lean4-mode
                                        #'my/lean-project-symbols-fallback))


(provide 'init-lean)

;;; init-lean.el ends here
