;;; init-snippets.el --- yasnippet config -*- lexical-binding: t; -*-

;;; Commentary:
;; - 所有 prog-mode 自动启用 yas-minor-mode
;; - TAB 不给 yas 用（缩进 / company 用）
;; - `C-c y` 是 snippet 前缀
;; - `C-c y y` 展开 snippet
;; - `C-c y i` 打开 snippet 菜单

;;; Code:

(require 'config)
(require 'remote-core)
(require 'seq)

(config-defvar yas-snippet-dirs nil
  "Directories searched by yasnippet."
  :type '(repeat directory)
  :group 'snippets)

(declare-function yas-activate-extra-mode "yasnippet" (mode))
(declare-function yas-reload-all "yasnippet" (&optional no-jit interactive))
(declare-function yas--load-directory-2 "yasnippet" (directory mode-sym))
(declare-function yas-next-field "yasnippet" (&optional arg))
(declare-function yas-prev-field "yasnippet" (&optional arg))
(declare-function yas-minor-mode "yasnippet" (&optional arg))
(declare-function my/copilot-setup-dwim-keys "init-copilot" (keymap))

(defvar my/snippet-action-functions nil
  "Functions that can handle a snippet-like editor action at point.
Each function is called without arguments and returns non-nil after handling
the trigger.  This keeps structural commands in the existing snippet workflow
without representing them as Yasnippet text templates.")

(defcustom my/yas-enable-idle-delay 0.15
  "Idle seconds before enabling a cold Yasnippet library in a source buffer.
An explicit snippet command still loads it immediately.  Warm libraries are
enabled synchronously.  Set to nil to retain synchronous first-buffer load."
  :type '(choice (const :tag "Enable synchronously" nil)
                 (number :tag "Idle seconds"))
  :group 'snippets)

(defvar-local my/yas--pending-enable nil
  "One-shot idle timer for this buffer's cold Yasnippet enablement.")

(defun my/yas--cancel-pending-enable ()
  "Cancel this buffer's pending cold Yasnippet activation."
  (when (timerp my/yas--pending-enable)
    (cancel-timer my/yas--pending-enable))
  (setq my/yas--pending-enable nil))

(defun my/yas--load-local-libraries ()
  "Load the Yasnippet features and their client-side snippet directory."
  (require 'yasnippet)
  (require 'yasnippet-snippets nil t))

(defun my/yas--enable-now ()
  "Enable Yasnippet in the current buffer with client-local first load."
  (my/yas--cancel-pending-enable)
  (unless (and (featurep 'yasnippet)
               (featurep 'yasnippet-snippets))
    (let ((default-directory temporary-file-directory)
          (process-environment (remote-client-process-environment))
          (exec-path (remote-client-exec-path))
          (remote-current-adapter-id nil)
          (remote-current-route nil)
          (remote-current-workspace nil))
      (my/yas--load-local-libraries)))
  (yas-minor-mode 1))

(defun my/yas--enable-after-idle (buffer mode)
  "Enable Yasnippet in BUFFER if it still uses MODE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq major-mode mode)
        (my/yas--cancel-pending-enable)
        (condition-case error
            (my/yas--enable-now)
          (error
           (message "Yasnippet setup failed: %s"
                    (error-message-string error))))))))

(defun my/yas-enable-for-source-buffer ()
  "Enable Yasnippet after a cold source buffer becomes idle.
Yasnippet scans hundreds of client-side snippets on first load.  Keep that
work off the initial file visit while preserving immediate warm enablement."
  (if (or (null my/yas-enable-idle-delay)
          (and (featurep 'yasnippet)
               (featurep 'yasnippet-snippets)))
      (my/yas--enable-now)
    (unless (timerp my/yas--pending-enable)
      (setq-local my/yas--pending-enable
                  (run-with-idle-timer
                   my/yas-enable-idle-delay nil
                   #'my/yas--enable-after-idle
                   (current-buffer) major-mode))
      (add-hook 'kill-buffer-hook #'my/yas--cancel-pending-enable nil t)
      (add-hook 'change-major-mode-hook
                #'my/yas--cancel-pending-enable nil t))))

(defun my/yas--ensure-enabled-a (&rest _)
  "Enable Yasnippet here before a caller expands a snippet programmatically.
Activation is deferred to idle, but lsp-mode, inline completion and Company
expand snippets directly; `yas-expand-snippet' refuses to run until
`yas-minor-mode' is on, which left an accepted completion half-inserted."
  (unless (bound-and-true-p yas-minor-mode)
    (my/yas--enable-now)))

(advice-add 'yas-expand-snippet :before #'my/yas--ensure-enabled-a)

(defun my/snippet-jupyter-cell-id ()
  "Return a fresh nbformat-compatible id for a Jupyter cell snippet."
  (if (fboundp 'my/noema-jupyter-notebook--new-id)
      (my/noema-jupyter-notebook--new-id)
    (format "cell-%s"
            (substring
             (secure-hash 'sha256
                          (format "%s:%s:%s"
                                  (float-time) (random) (emacs-pid)))
             0 12))))

(defun my/snippet-jupyter-prefix ()
  "Return the current notebook projection's comment prefix."
  (or (bound-and-true-p my/noema-jupyter-notebook--comment-prefix) "#"))

(defun my/yas-jupyter-setup ()
  "Share cell templates across notebook languages without global snippets."
  (when (featurep 'yasnippet)
    (if (bound-and-true-p my/noema-jupyter-cell-mode)
        (yas-activate-extra-mode 'jupyter-notebook-mode)
      (yas-deactivate-extra-mode 'jupyter-notebook-mode))))

(add-hook 'yas-minor-mode-hook #'my/yas-jupyter-setup)
(add-hook 'my/noema-jupyter-cell-mode-hook #'my/yas-jupyter-setup)

(defun my/snippet-expand ()
  "Expand a structural editor action or fall back to Yasnippet.
Buffer-local action providers get the first chance to consume a trigger such
as a Noema Jupyter `jcode' command."
  (interactive)
  (unless (run-hook-with-args-until-success 'my/snippet-action-functions)
    (my/yas--enable-now)
    (call-interactively #'yas-expand)))

(defun my/snippet-insert ()
  "Choose a Yasnippet, loading its client-side library when needed."
  (interactive)
  (my/yas--enable-now)
  (call-interactively #'yas-insert-snippet))

(defun my/snippet-new ()
  "Create a Yasnippet, loading its client-side library when needed."
  (interactive)
  (my/yas--enable-now)
  (call-interactively #'yas-new-snippet))

(defun my/snippet-visit ()
  "Visit a Yasnippet file, loading its client-side library when needed."
  (interactive)
  (my/yas--enable-now)
  (call-interactively #'yas-visit-snippet-file))

(defconst my/yas-treesit-extra-modes
  '((bash-ts-mode sh-mode)
    (c-ts-mode c-mode)
    (c++-ts-mode c++-mode)
    (css-ts-mode css-mode)
    (go-ts-mode go-mode)
    (html-ts-mode html-mode)
    (java-ts-mode java-mode)
    (js-ts-mode js-mode js2-mode)
    (json-ts-mode json-mode)
    (python-ts-mode python-mode)
    (rust-ts-mode rust-mode)
    (toml-ts-mode conf-toml-mode)
    (typescript-ts-mode typescript-mode)
    (yaml-ts-mode yaml-mode))
  "Snippet parent modes reused by tree-sitter major modes.")

(defun my/yas-setup-auctex-extra-modes ()
  "Make AUCTeX buffers reuse `latex-mode' and `tex-mode' snippets."
  (when (and (featurep 'yasnippet)
             (derived-mode-p 'LaTeX-mode 'plain-TeX-mode))
    (yas-activate-extra-mode 'latex-mode)
    (yas-activate-extra-mode 'tex-mode)))

(defun my/yas-setup-treesit-extra-modes ()
  "Make tree-sitter buffers reuse snippets from their original major modes."
  (when-let* ((extra-modes (alist-get major-mode my/yas-treesit-extra-modes)))
    (dolist (mode (if (listp extra-modes) extra-modes (list extra-modes)))
      (yas-activate-extra-mode mode))))

(defun my/yas--trim-fieldless-trailing-newline (args)
  "Filter-args advice for `yas-expand-snippet'.
Strip a single trailing newline from field-less templates — those with no
user-interactive fields ($1, $2, …).  The newline is a Unix file artifact,
not intentional content.  Templates with user fields are left untouched."
  (let ((content (car args)))
    (if (and (stringp content)
             (string-suffix-p "\n" content)
             (not (string-suffix-p "\n\n" content))
             (not (string-match-p "\\$\\(?:{?[1-9]\\)" content)))
        (cons (substring content 0 -1) (cdr args))
      args)))

(advice-add 'yas-expand-snippet :filter-args
            #'my/yas--trim-fieldless-trailing-newline)

(defconst my/yas-noema-generated-tex-directory
  (expand-file-name "snippets/tex-mode/generated" user-emacs-directory)
  "Generated LaTeX Workshop/Overleaf snippets shared with Noema.")

(defun my/yas-load-noema-generated-tex-snippets (&rest _)
  "Load provider subdirectories into the existing `tex-mode' YAS table.
YAS normally recurses below a mode directory, but an existing
`.yas-compiled-snippets.el' short-circuits that recursion. Loading this small,
pinned generated subtree after `yas-reload-all' keeps Emacs and Noema on
the same catalog without putting filesystem work on the expansion hot path."
  (when (and (file-directory-p my/yas-noema-generated-tex-directory)
             (fboundp 'yas--load-directory-2))
    (dolist (provider-dir
             (directory-files my/yas-noema-generated-tex-directory
                              t directory-files-no-dot-files-regexp))
      (when (file-directory-p provider-dir)
        (yas--load-directory-2 provider-dir 'tex-mode)))))

(advice-add 'yas-reload-all :after
            #'my/yas-load-noema-generated-tex-snippets)

;; These punctuation shortcuts are editor-specific.  Noema scans the shared
;; snippets/ tree itself and owns its browser-side math shortcuts separately.
(defconst my/yas-emacs-math-snippets
  '((tex-mode
     (";" "$${1:a}$ $0" "Inline math"
      (not (derived-mode-p 'markdown-mode 'noema-research-mode))
      ("LaTeX local"))
     (":" "$$\n${1:a}\n$$\n$0" "Display math"
      (not (derived-mode-p 'markdown-mode 'noema-research-mode))
      ("LaTeX local")))
    (markdown-mode
     (";" "\\\\($1\\\\) $0" "Inline math" nil ("Noema local"))
     (":" "\\\\[\n$1\n\\\\]\n$0" "Display math" nil ("Noema local"))))
  "Yasnippet definitions kept on the Emacs side of the shared catalog.")

(defun my/yas-define-emacs-math-snippets ()
  "Restore Emacs-only punctuation snippets after a Yasnippet reload."
  (dolist (entry my/yas-emacs-math-snippets)
    (yas-define-snippets (car entry) (cdr entry))))

(add-hook 'yas-after-reload-hook #'my/yas-define-emacs-math-snippets)

(defun my/yas-org-cleanup-trailing-newline ()
  "Silently delete a trailing newline left by a snippet at point.
Replicates the per-snippet `inhibit-modification-hooks' cleanup that was
previously inlined in every org-mode snippet file."
  (save-excursion
    (when (and (not (eobp))
               (eq (char-after) ?\n))
      (let ((inhibit-modification-hooks t))
        (ignore-errors (delete-char 1))))))

(defun my/yas-setup-org-behavior ()
  "Keep Org snippet expansion conservative around indentation and newlines."
  (setq-local yas-indent-line 'fixed)
  (setq-local yas-also-indent-empty-lines nil)
  ;; After each snippet exits, clean up any trailing newline it may have left.
  ;; This replicates the per-snippet inline lisp that was removed from snippet
  ;; files, while keeping the inhibit-modification-hooks suppression centralized.
  (add-hook 'yas-after-exit-snippet-hook
            #'my/yas-org-cleanup-trailing-newline nil t))

(defun my/yas--compiled-snippets-stale-p (directory)
  "Return non-nil when DIRECTORY's compiled snippets predate a snippet file.
Yasnippet loads `.yas-compiled-snippets.el' whenever it exists and never
compares it with the sources, so a snippet added after the last
`yas-recompile-all' silently disappears."
  (let ((compiled (expand-file-name ".yas-compiled-snippets.el" directory)))
    (when-let* ((stamp (file-attribute-modification-time
                        (file-attributes compiled))))
      (seq-some
       (lambda (file)
         (time-less-p stamp
                      (file-attribute-modification-time (file-attributes file))))
       (directory-files-recursively
        directory "\\`[^.]" nil
        (lambda (subdirectory)
          (not (string-prefix-p "." (file-name-nondirectory subdirectory)))))))))

(defvar yas--creating-compiled-snippets)

(defun my/yas--skip-stale-compiled-a (function directory mode-sym)
  "Call FUNCTION on DIRECTORY and MODE-SYM, reading sources over a stale cache."
  (if (and (not (bound-and-true-p yas--creating-compiled-snippets))
           (not (file-exists-p (expand-file-name ".yas-skip" directory)))
           (my/yas--compiled-snippets-stale-p directory))
      (yas--load-directory-2 directory mode-sym)
    (funcall function directory mode-sym)))

(advice-add 'yas--load-directory-1 :around #'my/yas--skip-stale-compiled-a)

(use-package yasnippet
  :ensure t
  :defer t
  :commands (yas-expand yas-insert-snippet yas-new-snippet yas-visit-snippet-file)
  :hook
  ((prog-mode . my/yas-enable-for-source-buffer)
   (text-mode . my/yas-enable-for-source-buffer)
   (org-mode . my/yas-enable-for-source-buffer)
   (org-mode . my/yas-setup-org-behavior)
   (yas-minor-mode . my/yas-setup-auctex-extra-modes)
   (yas-minor-mode . my/yas-setup-treesit-extra-modes)
   (LaTeX-mode . my/yas-setup-auctex-extra-modes)
   (plain-TeX-mode . my/yas-setup-auctex-extra-modes))
  :config
  (yas-reload-all)

  (with-eval-after-load 'yasnippet
    ;; snippet 会话中的跳转仍统一走全局 DWIM 调度，避免直接触碰脆弱内部状态。
    (my/copilot-setup-dwim-keys yas-keymap)
    ;; 不让 yas 抢 TAB
    (define-key yas-keymap (kbd "TAB") nil)
    (define-key yas-keymap (kbd "<tab>") nil)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :defer t
  :config
  ;; 把包自带 snippets 也加入
  (add-to-list 'yas-snippet-dirs
               (expand-file-name
                "snippets"
                (file-name-directory
                 (locate-library "yasnippet-snippets"))))
  (yas-reload-all))

;; 全局 snippet 前缀，避免覆盖 `rg' 默认的 `C-c s' 搜索入口。
(define-prefix-command 'my/snippet-map)
(global-set-key (kbd "C-c y") #'my/snippet-map)
(keymap-set my/snippet-map "y" #'my/snippet-expand)
(keymap-set my/snippet-map "i" #'my/snippet-insert)
(keymap-set my/snippet-map "n" #'my/snippet-new)
(keymap-set my/snippet-map "v" #'my/snippet-visit)

(provide 'init-snippets)
;;; init-snippets.el ends here
