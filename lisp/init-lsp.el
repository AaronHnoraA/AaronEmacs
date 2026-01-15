;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; The completion engine
;;
;; 'company-mode' has an online manual now.
;;
;; https://company-mode.github.io/manual/

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind (:map company-mode-map
         ([remap completion-at-point] . company-complete)
         :map company-active-map
         ("C-s"     . company-filter-candidates)
         ([tab]     . company-complete-common-or-cycle)
         ([backtab] . company-select-previous-or-abort)
         )
  :after yasnippet
  :config
  (define-advice company-capf--candidates (:around (func &rest args))
    "Try default completion styles."
    (let ((completion-styles '(basic partial-completion)))
      (apply func args)))
  :custom
  (company-idle-delay 0)
  ;; Easy navigation to candidates with M-<n>
  (company-show-quick-access t)
  (company-require-match nil)
  (company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (company-minimum-prefix-length 3)
  (company-tooltip-width-grow-only t)
  (company-tooltip-align-annotations t)
  ;; complete `abbrev' only in current buffer and make dabbrev case-sensitive
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  ;; make dabbrev-code case-sensitive
  (company-dabbrev-code-ignore-case nil)
  (company-dabbrev-code-everywhere t)
  ;; call `tempo-expand-if-complete' after completion
  (company-tempo-expand t)
  ;; Ignore uninteresting files. Items end with a slash are recognized as
  ;; directories.
  (company-files-exclusions '(".git/" ".DS_Store"))
  ;; No icons
  (company-format-margin-function nil)
  (company-backends '((company-capf :with company-tempo company-yasnippet)
                      company-files
                      (company-dabbrev-code company-keywords)
                      company-dabbrev)))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp-deferred)
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-region)
         ("C-c d" . lsp-describe-thing-at-point)
         ("C-c a" . lsp-execute-code-action)
         ("C-c r" . lsp-rename))
  :config
  (with-no-warnings
    (lsp-enable-which-key-integration t))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-links nil)                    ;; no clickable links
  (lsp-enable-folding nil)                  ;; use `hideshow' instead
  (lsp-enable-snippet nil)                  ;; no snippets, it requires `yasnippet'
  (lsp-enable-file-watchers nil)            ;; performance matters
  (lsp-enable-text-document-color nil)      ;; as above
  (lsp-enable-symbol-highlighting nil)      ;; as above
  (lsp-enable-on-type-formatting nil)       ;; as above
  (lsp-semantic-tokens-enable nil)          ;; optional
  (lsp-semantic-tokens-apply-modifiers nil) ;; don't override token faces
  (lsp-headerline-breadcrumb-enable nil)    ;; keep headline clean
  (lsp-modeline-code-actions-enable nil)    ;; keep modeline clean
  (lsp-modeline-diagnostics-enable nil)     ;; as above
  (lsp-log-io nil)                          ;; debug only
  (lsp-auto-guess-root t)                   ;; Yes, I'm using projectile
  (lsp-completion-provider :none)           ;; don't add `company-capf' to `company-backends'
  (lsp-keep-workspace-alive nil)            ;; auto kill lsp server
  (lsp-eldoc-enable-hover nil)              ;; disable eldoc hover
  (lsp-completion-enable-additional-text-edit nil))

(use-package eglot
  :disabled
  :hook (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c f" . eglot-format)
         ("C-c d" . eldoc-doc-buffer)
         ("C-c a" . eglot-code-actions)
         ("C-c r" . eglot-rename)
         ("C-c l" . eglot-command-map))
  :config
  (defvar-keymap eglot-command-map
    :prefix 'eglot-command-map
    ;; workspaces
    "w q" #'eglot-shutdown
    "w r" #'eglot-reconnect
    "w s" #'eglot
    "w d" #'eglot-show-workspace-configuration

    ;; formatting
    "= =" #'eglot-format-buffer
    "= r" #'eglot-format

    ;; goto
    "g a" #'xref-find-apropos
    "g d" #'eglot-find-declaration
    "g g" #'xref-find-definitions
    "g i" #'eglot-find-implementation
    "g r" #'xref-find-references
    "g t" #'eglot-find-typeDefinition

    ;; actions
    "a q" #'eglot-code-action-quickfix
    "a r" #'eglot-code-action-rewrite
    "a i" #'eglot-code-action-inline
    "a e" #'eglot-code-action-extract
    "a o" #'eglot-code-action-organize-imports)
  :custom
  (eglot-sync-connect 0)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-ignored-server-capabilities '(:documentLinkProvider
                                       :documentOnTypeFormattingProvider
                                       :foldingRangeProvider
                                       :colorProvider
                                       :inlayHintProvider)))


(use-package aggressive-indent
  :ensure t
  :hook (
         (clojure-mode    . aggressive-indent-mode)
         (python-mode    . aggressive-indent-mode)
         (c++-mode    . aggressive-indent-mode)
         (c-mode    . aggressive-indent-mode)
         ;; 你也可以对其它语言开，但我建议先从括号语言开始
         ))
;;
;; lsp-mode completion 允许 snippets（函数补全可插参数模板）
(setq lsp-completion-enable t)
(setq lsp-enable-snippet t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-signature-auto-activate t)   ;; 自动弹出签名
  (lsp-signature-render-documentation t)
  (lsp-signature-doc-lines 5))

(add-hook 'after-init-hook 'global-company-mode)
;; -------------------------
;; LSP diagnostics inline (Flymake)
;; -------------------------
(use-package flymake
  :ensure nil ; Emacs built-in
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c !" . flymake-show-buffer-diagnostics))
  :custom
  ;; 让 Flymake 更积极一点（默认也行，但这个更“明显”）
  (flymake-no-changes-timeout 0.3)
  ;; 只在行尾显示标记时更干净；如果你想更显眼，可以改成 'right-margin
  (flymake-indicator-type 'fringes))
;; lsp-mode diagnostics visibility baseline
(with-eval-after-load 'lsp-mode
  ;; lsp-mode 默认走 flymake；这里不改 provider
  (setq lsp-diagnostics-provider :auto))

;; 光标停在报错位置时，在 minibuffer/eldoc 显示诊断
(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :custom
  (flymake-diagnostic-at-point-display-diagnostic-function
   #'flymake-diagnostic-at-point-display-minibuffer))

(with-eval-after-load 'lsp-mode
  (setq lsp-diagnostics-provider :flycheck))

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-imenu
             lsp-ui-doc-show
             lsp-ui-doc-hide
             lsp-ui-doc-glance
             lsp-ui-peek-find-definitions
             lsp-ui-peek-find-references
             lsp-ui-peek-find-implementation)
  :bind
  (;; ========== Hyper + l ==========
   ("C-h m" . lsp-ui-imenu)                     ; 符号大纲
   ("C-h d" . lsp-ui-doc-glance)                ; 快速看文档
   ("C-h D" . lsp-ui-doc-toggle)                  ; 强制显示文档
   ("C-h c" . lsp-ui-doc-hide)                  ; 隐藏文档

   ("C-h e" . lsp-ui-peek-find-definitions)   ; 定义
   ("C-h r" . lsp-ui-peek-find-references)    ; 引用
   ("C-h i" . lsp-ui-peek-find-implementation)) ; 实现
  :custom
  (lsp-ui-imenu-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.3))


(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))


(use-package company-tabnine
  :ensure t
  :after company
  :config
  ;; TabNine 放到第一优先级
  (add-to-list 'company-backends
               '(company-tabnine company-capf)))


;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)



(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))


(use-package dap-mode
  :ensure t
  :after hydra lsp-mode
  :commands dap-debug
  :custom
  (dap-auto-configure-mode t)
  :config
  (dap-ui-mode 1)
  :hydra
  (hydra-dap-mode
   (:color pink :hint nil :foreign-keys run)
   "
^Stepping^          ^Switch^                 ^Breakpoints^         ^Debug^                     ^Eval
^^^^^^^^----------------------------------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bb_: Toggle          _dd_: Debug                 _ee_: Eval
_i_: Step in        _st_: Thread             _bd_: Delete          _dr_: Debug recent          _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add             _dl_: Debug last            _es_: Eval thing at point
_c_: Continue       _su_: Up stack frame     _bc_: Set condition   _de_: Edit debug template   _ea_: Add expression.
_r_: Restart frame  _sd_: Down stack frame   _bh_: Set hit count   _ds_: Debug restart
_Q_: Disconnect     _sl_: List locals        _bl_: Set log message
                  _sb_: List breakpoints
                  _sS_: List sessions
"
   ("n" dap-next)
   ("i" dap-step-in)
   ("o" dap-step-out)
   ("c" dap-continue)
   ("r" dap-restart-frame)
   ("ss" dap-switch-session)
   ("st" dap-switch-thread)
   ("sf" dap-switch-stack-frame)
   ("su" dap-up-stack-frame)
   ("sd" dap-down-stack-frame)
   ("sl" dap-ui-locals)
   ("sb" dap-ui-breakpoints)
   ("sS" dap-ui-sessions)
   ("bb" dap-breakpoint-toggle)
   ("ba" dap-breakpoint-add)
   ("bd" dap-breakpoint-delete)
   ("bc" dap-breakpoint-condition)
   ("bh" dap-breakpoint-hit-condition)
   ("bl" dap-breakpoint-log-message)
   ("dd" dap-debug)
   ("dr" dap-debug-recent)
   ("ds" dap-debug-restart)
   ("dl" dap-debug-last)
   ("de" dap-debug-edit-template)
   ("ee" dap-eval)
   ("ea" dap-ui-expressions-add)
   ("er" dap-eval-region)
   ("es" dap-eval-thing-at-point)
   ("q" nil "quit" :color blue)
   ("Q" dap-disconnect :color red)))



(require 'init-cpp)
(require 'init-rust)
(require 'init-ocaml)
(require 'init-bazel)
(require 'init-haskell)
(require 'init-python)
(require 'init-elisp)
(require 'init-vale)
(require 'init-sh)


(provide 'init-lsp)
;;; init-lsp.el ends here
