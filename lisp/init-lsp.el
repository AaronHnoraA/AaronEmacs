;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;; Restore the original Eglot + Flymake workflow, while keeping a small
;; compatibility layer so explicitly registered modes can still opt into
;; `lsp-mode' when needed.

;;; Code:

(defvar my/lsp-mode-preferred-modes nil
  "Major modes that should use `lsp-mode' instead of `eglot'.")

(defvar my/lsp-mode-required-features nil
  "Alist mapping major modes to extra `lsp-mode' support features.")

(defvar tramp-login-shell)

(defun my/language-server-prepare-remote-eglot-environment ()
  "Prepare shell settings for remote `eglot' buffers."
  (when (file-remote-p default-directory)
    (let ((remote-shell (or (and (boundp 'tramp-login-shell)
                                 tramp-login-shell)
                            "sh")))
      (setq-local shell-file-name remote-shell)
      (setq-local explicit-shell-file-name remote-shell)
      (setq-local shell-command-switch "-c"))))

(defun my/register-lsp-mode-preference (mode &optional feature)
  "Prefer `lsp-mode' over `eglot' for MODE.
When FEATURE is non-nil, require it before starting `lsp-mode'."
  (add-to-list 'my/lsp-mode-preferred-modes mode)
  (when feature
    (setf (alist-get mode my/lsp-mode-required-features nil nil #'eq)
          feature)))

(defun my/lsp-mode-preferred-p ()
  "Return non-nil when current buffer should use `lsp-mode'."
  (and my/lsp-mode-preferred-modes
       (apply #'derived-mode-p my/lsp-mode-preferred-modes)))

(defun my/lsp-mode-required-feature ()
  "Return the extra `lsp-mode' feature required for the current buffer."
  (catch 'feature
    (dolist (entry my/lsp-mode-required-features)
      (when (derived-mode-p (car entry))
        (throw 'feature (cdr entry))))
    nil))

(defun my/lsp-mode-supported-p ()
  "Return non-nil when `lsp-mode' can start for the current buffer."
  (let ((feature (my/lsp-mode-required-feature)))
    (if feature
        (or (featurep feature)
            (require feature nil t))
      t)))

(defun my/current-language-server-backend ()
  "Return the active language server backend for the current buffer."
  (cond
   ((and (fboundp 'eglot-managed-p)
         (eglot-managed-p))
    'eglot)
   ((bound-and-true-p lsp-managed-mode)
    'lsp-mode)
   (t nil)))

(defun my/language-server-stop-eglot ()
  "Shut down the current `eglot' session in this buffer, if any."
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p))
    (ignore-errors
      (eglot-shutdown (eglot-current-server)))))

(defun my/lsp-mode-ensure ()
  "Start `lsp-mode' for explicitly registered major modes."
  (interactive)
  (when (my/lsp-mode-preferred-p)
    (unless (bound-and-true-p lsp-managed-mode)
      (my/language-server-stop-eglot)
      (if (my/lsp-mode-supported-p)
          (lsp-deferred)
        (let ((feature (my/lsp-mode-required-feature)))
          (message "Skip lsp-mode in %s: missing `%s'" major-mode feature))))))

(defun my/eglot-ensure ()
  "Start `eglot' in programming buffers that do not opt into `lsp-mode'."
  (interactive)
  (when (and (derived-mode-p 'prog-mode)
             (not (my/lsp-mode-preferred-p)))
    (unless (or (bound-and-true-p lsp-managed-mode)
                (and (fboundp 'eglot-managed-p)
                     (eglot-managed-p)))
      (my/language-server-prepare-remote-eglot-environment)
      (eglot-ensure))))

(defun my/language-server-ensure ()
  "Start the preferred language server backend for the current buffer."
  (interactive)
  (if (my/lsp-mode-preferred-p)
      (my/lsp-mode-ensure)
    (my/eglot-ensure)))

(defun my/language-server-call (eglot-fn lsp-fn)
  "Call EGLOT-FN or LSP-FN for the active language server backend."
  (pcase (my/current-language-server-backend)
    ('eglot
     (call-interactively eglot-fn))
    ('lsp-mode
     (call-interactively lsp-fn))
    (_
     (user-error "No active language server in current buffer"))))

(defun my/language-server-code-actions ()
  "Run a code action using the active language server backend."
  (interactive)
  (my/language-server-call #'eglot-code-actions #'lsp-execute-code-action))

(defun my/language-server-format-buffer ()
  "Format the current buffer using the active language server backend."
  (interactive)
  (my/language-server-call #'eglot-format-buffer #'lsp-format-buffer))

(defun my/language-server-rename ()
  "Rename the symbol at point using the active language server backend."
  (interactive)
  (my/language-server-call #'eglot-rename #'lsp-rename))

(defun my/language-server-find-implementation ()
  "Find implementation using the active language server backend."
  (interactive)
  (my/language-server-call #'eglot-find-implementation #'lsp-find-implementation))

(defun my/language-server-find-type-definition ()
  "Find type definition using the active language server backend."
  (interactive)
  (my/language-server-call #'eglot-find-typeDefinition #'lsp-find-type-definition))

;; -------------------------
;; 1. Company Mode (Completion)
;; -------------------------
;; [https://company-mode.github.io/manual/](https://company-mode.github.io/manual/)

(defconst my/company-lsp-backends
  '((company-capf
     company-files
     :with company-tempo
     company-yasnippet))
  "LSP-first company backends for code buffers.")

(defconst my/company-text-backends
  '((company-capf
     company-files
     :with company-yasnippet
     company-dabbrev))
  "Company backends for prose and document buffers.")

(defconst my/company-shell-backends
  '((company-capf
     company-files
     :with company-dabbrev-code
     company-dabbrev))
  "Company backends for interactive shell buffers.")

(defun my/company-setup-text-backends ()
  "Use company popup completion in document buffers instead of `*Completions*'."
  (setq-local company-backends my/company-text-backends))

(defun my/company-setup-shell-backends ()
  "Enable popup completion for Eshell with CAPF/pcomplete."
  (company-mode 1)
  (setq-local company-backends my/company-shell-backends)
  (setq-local company-idle-delay 0.08)
  (setq-local company-minimum-prefix-length 1))

(use-package company
  :ensure t
  :demand t
  :hook ((eglot-managed-mode . company-mode)
         (lsp-managed-mode . company-mode)
         (org-mode . company-mode)
         (text-mode . company-mode)
         (text-mode . my/company-setup-text-backends))
  :init
  :bind (:map company-mode-map
         ([remap completion-at-point] . company-complete)
         :map company-active-map
         ("C-s"     . company-filter-candidates)
         ([tab]     . company-complete-selection))
  :config
  (define-advice company-capf--candidates (:around (func &rest args))
    "Try default completion styles."
    (let ((completion-styles '(basic partial-completion)))
      (apply func args)))
  (setq company-idle-delay 0.12
        company-minimum-prefix-length 1
        company-show-numbers t
        company-show-quick-access t
        company-require-match nil
        company-tooltip-width-grow-only t
        company-tooltip-align-annotations t
        company-format-margin-function nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-code-everywhere t
        company-files-exclusions '(".git/" ".DS_Store")
        company-backends my/company-lsp-backends)
  (setq-default company-backends my/company-lsp-backends))

(with-eval-after-load 'esh-mode
  (add-hook 'eshell-mode-hook #'my/company-setup-shell-backends))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'company-backends))

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-doc-delay 0.2)
  (company-box-scrollbar nil))

(use-package company-prescient
  :ensure t
  :after company
  :defer 2
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode 1))


;; -------------------------
;; 2. Aggressive Indent
;; -------------------------
(use-package aggressive-indent
  :ensure t
  :hook ((elisp-mode . aggressive-indent-mode)
         (python-mode  . aggressive-indent-mode)
         (c++-mode     . aggressive-indent-mode)
         (c-mode       . aggressive-indent-mode)))


;; -------------------------
;; 3. Flymake (Diagnostics)
;; -------------------------
;; Eglot / lsp-mode 均统一走 Flymake 诊断
(use-package flymake
  :ensure nil ; Emacs built-in
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c !" . flymake-show-buffer-diagnostics))
  :custom
  (flymake-no-changes-timeout nil) ; 不在输入停顿时自动检查
  (flymake-indicator-type 'fringes))

;; 光标停在报错位置时，在 minibuffer 显示诊断
(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :custom
  (flymake-diagnostic-at-point-display-diagnostic-function
   #'flymake-diagnostic-at-point-display-minibuffer))


;; -------------------------
;; 4. lsp-mode (for explicit opt-in languages)
;; -------------------------
(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp
             lsp-deferred
             lsp-execute-code-action
             lsp-find-implementation
             lsp-find-type-definition
             lsp-format-buffer
             lsp-inlay-hints-mode
             lsp-rename)
  :hook ((lsp-managed-mode . (lambda ()
                               (when (fboundp 'lsp-inlay-hints-mode)
                                 (lsp-inlay-hints-mode 1)))))
  :init
  (setq lsp-completion-provider :capf
        lsp-diagnostics-provider :flymake
        lsp-headerline-breadcrumb-enable nil
        lsp-inlay-hint-enable t
        lsp-log-io nil)
  :config
  (define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c d") #'eldoc-doc-buffer)
  (define-key lsp-mode-map (kbd "C-c a") #'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename)
  (define-key lsp-mode-map (kbd "C-h e") #'xref-find-definitions)
  (define-key lsp-mode-map (kbd "C-h r") #'xref-find-references)
  (define-key lsp-mode-map (kbd "C-h i") #'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "C-h t") #'lsp-find-type-definition))


;; -------------------------
;; 5. Eglot (LSP Client)
;; -------------------------
(use-package eglot
  :ensure nil ; Built-in since Emacs 29
  :hook ((prog-mode . my/eglot-ensure)
         (eglot-managed-mode . (lambda ()
                                 (when (fboundp 'eglot-inlay-hints-mode)
                                   (eglot-inlay-hints-mode 1)))))
  :bind (:map eglot-mode-map
         ("C-c f" . eglot-format-buffer)
         ("C-c d" . eldoc-doc-buffer)
         ("C-c a" . eglot-code-actions)
         ("C-c r" . eglot-rename)
         ("C-h e" . xref-find-definitions)
         ("C-h r" . xref-find-references)
         ("C-h i" . eglot-find-implementation)
         ("C-h t" . eglot-find-typeDefinition))
  :custom
  (eglot-sync-connect 0)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0)
  (read-process-output-max (* 1024 1024)))


;; -------------------------
;; 6. UI Emulation (Doc Box & Breadcrumb)
;; -------------------------

;; 替代 lsp-ui-doc：提供光标处悬浮文档框
(use-package eldoc-box
  :ensure t
  :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode)
         (lsp-managed-mode . eldoc-box-hover-at-point-mode))
  :bind (:map eglot-mode-map
         ("C-h d" . eldoc-box-help-at-point)
         ("C-h c" . eldoc-box-quit-frame))
  :custom
  (eldoc-box-max-pixel-width 600)
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-clear-with-C-g t)
  :config
  (with-eval-after-load 'lsp-mode
    (define-key lsp-mode-map (kbd "C-h d") #'eldoc-box-help-at-point)
    (define-key lsp-mode-map (kbd "C-h c") #'eldoc-box-quit-frame)))

;; 替代 lsp-headerline-breadcrumb：Eglot 作者出品的面包屑
(use-package breadcrumb
  :ensure t
  :hook ((prog-mode . breadcrumb-local-mode)
         (org-src-mode . breadcrumb-local-mode)))

;; -------------------------
;; 7. Dape (Debugging)
;; -------------------------
;;
;; dape 是更适合 Eglot / 原生 Emacs 工作流的 DAP 客户端。
;; 入口命令是 `M-x dape`。
;; 推荐打开 `repeat-mode`，这样单步调试体验会更顺。
;;
;; 说明：
;; - 不再使用 dap-ui-mode / dap-auto-configure-mode
;; - 不再依赖 lsp-mode
;; - 部分 dap-mode 命令在 dape 中没有 1:1 同名接口，
;;   这里改成 dape 当前公开可用的命令体系
;;
(use-package dape
  :ensure t
  :after hydra
  :commands (dape
             dape-next
             dape-step-in
             dape-step-out
             dape-continue
             dape-pause
             dape-restart
             dape-quit
             dape-breakpoint-toggle
             dape-breakpoint-log
             dape-breakpoint-expression
             dape-breakpoint-hits
             dape-breakpoint-remove-at-point
             dape-evaluate-expression
             dape-watch-dwim
             dape-repl
             dape-repl-threads
             dape-repl-stack
             dape-repl-breakpoints
             dape-repl-scope
             dape-repl-watch)
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  :custom
  (dape-buffer-window-arrangement 'right)
  :config
  (repeat-mode 1)

  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  (add-hook 'dape-start-hook
            (lambda () (save-some-buffers t t)))

  (defhydra hydra-dape-mode
    (:color pink :hint nil :foreign-keys run)
    "
^Stepping^          ^Switch/View^             ^Breakpoints^         ^Debug^                     ^Eval / Watch^
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
_n_: Next           _ss_: Sessions(REPL)      _bb_: Toggle          _dd_: Debug (dape)          _ee_: Eval
_i_: Step in        _st_: Threads             _bd_: Delete here     _dr_: Restart               _er_: Eval region
_o_: Step out       _sf_: Stack               _ba_: Log message     _dq_: Quit                  _es_: Eval thing
_c_: Continue       _sl_: Locals(scope)       _bc_: Condition       _dR_: REPL                  _ea_: Add watch
_p_: Pause          _sb_: Breakpoints         _bh_: Hit count
"
    ("n" dape-next)
    ("i" dape-step-in)
    ("o" dape-step-out)
    ("c" dape-continue)
    ("p" dape-pause)

    ("ss" dape-repl)
    ("st" dape-repl-threads)
    ("sf" dape-repl-stack)
    ("sl" dape-repl-scope)
    ("sb" dape-repl-breakpoints)

    ("bb" dape-breakpoint-toggle)
    ("ba" dape-breakpoint-log)
    ("bd" dape-breakpoint-remove-at-point)
    ("bc" dape-breakpoint-expression)
    ("bh" dape-breakpoint-hits)

    ("dd" dape)
    ("dr" dape-restart)
    ("dR" dape-repl)
    ("dq" dape-quit :color blue)

    ("ee" dape-evaluate-expression)
    ("ea" dape-watch-dwim)
    ("er" (if (use-region-p)
              (dape-evaluate-expression
               (or (ignore-errors (dape--live-connection 'stopped t))
                   (ignore-errors (dape--live-connection 'last)))
               (buffer-substring-no-properties
                (region-beginning) (region-end)))
            (user-error "No active region")))
    ("es" (let ((sym (thing-at-point 'symbol t)))
            (if sym
                (dape-evaluate-expression
                 (or (ignore-errors (dape--live-connection 'stopped t))
                     (ignore-errors (dape--live-connection 'last)))
                 sym)
              (user-error "No symbol at point"))))

    ("q" nil "quit" :color blue)))


;; -------------------------
;; 8. Misc & Language Init
;; -------------------------

(setq tab-always-indent t)

;; Org-mode specific company setup
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local company-backends
                        '((company-files
                           company-yasnippet
                           company-capf
                           company-dabbrev)))))

;; 让 C-c ' 打开的窗口自动启动对应 language server
(add-hook 'org-src-mode-hook
          (lambda ()
            (my/language-server-ensure)))


;; Load other language specific configurations
(require 'init-cpp)
(require 'init-rust)
(require 'init-ocaml)
(require 'init-bazel)
(require 'init-haskell)
(require 'init-python)
(require 'init-elisp)
(require 'init-vale)
(require 'init-sh)
(require 'init-java)
(require 'init-lean)
(require 'init-md)
(require 'init-nix)
(require 'init-sage)
(require 'init-html)
(require 'init-js2)
(require 'init-latex)

;; eglot：永不自动重连（需要你手动 M-x eglot 重新连）
(setq-default eglot-autoreconnect nil)

;; 保存时检查（当前 buffer）
(add-hook 'after-save-hook
          (lambda ()
            (when (bound-and-true-p flymake-mode)
              (flymake-start))))


(provide 'init-lsp)
;;; init-lsp.el ends here
